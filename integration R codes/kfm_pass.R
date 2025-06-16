## Config ##
rm(list= ls())
na_val = ""

####Header####
kfm_common <- function(data){
  #rename cols
  data <- rename(data, scientific_name = ScientificName, CommonName_old = CommonName, native_taxa_code = Species)
  #Split CommonName_old into common_name and lifestage
  tray <- str_split_fixed(data$CommonName_old, ", ", 2)
  data$common_name <- tray[,1]
  data$lifestage <- tray[,2]
  data$lifestage[data$lifestage == "all" | data$lifestage == ""] <- NA
  
#for the RPC data, there is a common name that has comma in it, so we need to reassign those values. 
  data_1 <- data %>%
    mutate(common_name = if_else(data$scientific_name=="Macrocystis, Pterygophora, and Eisenia combined","giant kelp, California sea palm, Southern sea palm",common_name),
           lifestage = if_else(data$scientific_name=="Macrocystis, Pterygophora, and Eisenia combined",as.character(NA),lifestage))
  
  return(data_1)
}

#date lookup #The RPC site 12, year 2015 used to have two survey date which is a mistake, remove 2015-05-14 and keep 2015-05-27 in the original datasheet (Josh suggested)
dates <- read.csv("kfm/data/raw_in/lookups/KFM_ListOfSurveyDates_1982-2023.csv", header = TRUE, na.strings = na_val)
dates$X1mQUAD <- as.Date(dates$X1mQUAD,"%m/%d/%Y")
dates$X5mQUAD <- as.Date(dates$X5mQUAD,"%m/%d/%Y")
dates$BANDTRAN <- as.Date(dates$BANDTRAN,"%m/%d/%Y")
dates$RDFC <- as.Date(dates$RDFC,"%m/%d/%Y")
dates$RPC <- as.Date(dates$RPC,"%m/%d/%Y")
dates$FISHTRAN <- as.Date(dates$FISHTRAN,"%m/%d/%Y")
dates <- dates[,c("SiteNumber","SurveyYear","X1mQUAD","X5mQUAD","BANDTRAN","RDFC","RPC","FISHTRAN")]

#check the survey dates and make sure there is only one date for a year. fish transect and RDFC has multiple surveys in a year. 
# peace <- dates %>%
#   melt(id.var=c("SiteNumber","SurveyYear"),value.name="date",variable.name="method") %>%
#   filter(!is.na(date)) %>%
#   group_by(SiteNumber,SurveyYear,method) %>%
#   summarise(freq=n()) %>%
#   ungroup()%>%
#   filter(freq>1)

############
###############################################################################################
#1m_quad
data <- read.csv("kfm/data/raw_in/KFM_1mQuadrat_RawData_1982-2023.csv", header = TRUE, na.strings = na_val)

#  20170213:there are pair plots A and B. We combine A and B and treat the 2 m2 plots as one experimental unit
data$CountB[is.na(data$CountB)]<-0
data$count<-data$CountA+data$CountB

#20181121 Because quad data have "all" species categories which are the sum of juvenile and adult, we remove the "all". 
# For macrosystics, the stipe count was added in 2007 and terminated in 2014. To maintain consistency, we remove the stipe and keep only the adult and juvenile. 

spp<- data %>%
  distinct(Species,ScientificName,CommonName) 

#check to see the survey date at each site, 1 survey date only. 
# peace<-data %>%
#   group_by(SiteNumber,IslandCode,IslandName,SiteCode,SiteName,SurveyYear,QuadratNumber,Species) %>%
#   summarise(freq=n(),.groups='drop')

data1 <- data %>%
  filter(!(SiteNumber==32&SurveyYear==2022&SiteCode=="EFC"&SurveyDate=="5/17/2022 0:00:00")) %>%#remove survey date 2022-05-17 at site 32 and survey year 2022; There were 2 survey dates and we removed one of it. 
  filter(!Species=='2002.99') %>%# remove stipe counts
  dcast(SiteNumber+IslandCode+IslandName+SiteCode+SiteName+SurveyYear+QuadratNumber~Species,value.var="count")

colnames(data1) <-paste0("Z",colnames(data1))

data<-data1 %>%
  mutate(Z2002=if_else(is.na(Z2002)&!is.na(Z2010),Z2010,Z2002),
         Z2004=if_else(is.na(Z2004)&!is.na(Z2012),Z2012,Z2004),
         Z2005=if_else(is.na(Z2005)&!is.na(Z2013),Z2013,Z2005),
         Z2006=if_else(is.na(Z2006)&!is.na(Z2014),Z2014,Z2006)) %>%
  select(-Z2010,-Z2012,-Z2013,-Z2014) %>% # remove all the "all category
  melt(id.var=c("ZSiteNumber","ZIslandCode","ZIslandName","ZSiteCode","ZSiteName","ZSurveyYear","ZQuadratNumber"),value.name="count",variable.name="Species",na.rm=T) %>%
  rename(SiteNumber=ZSiteNumber,
         IslandCode=ZIslandCode,
         IslandName=ZIslandName,
          SiteCode= ZSiteCode,
          SiteName= ZSiteName,
          SurveyYear=ZSurveyYear,
          QuadratNumber= ZQuadratNumber)%>%
  mutate(Species=as.numeric(gsub("Z","",Species,fixed=T))) %>%
  left_join(spp,by="Species")
  
  
data$data_source <- "kfm"
data$sample_method <- "1mquad"


#merge with date
data <- data %>%#check the survey date and only 1 day in a year, it is good
  left_join(dates[!is.na(dates$X1mQUAD),c("SiteNumber","SurveyYear","X1mQUAD")], by = c("SiteNumber","SurveyYear"))  #check the survey date and only 1 day in a year, it is good
data <- rename(data, date = X1mQUAD) 


#survey area changes over time
data$area = NA
data$area[data$SurveyYear < 1985] = 1
data$area[data$SurveyYear >= 1985] = 2
  
# #trial test the unmatch calculation
# trial<-data %>%
#   filter(Species==2010|Species==2002|Species==2002.5|Species==2002.99) %>%
#   dcast(SiteNumber+date+QuadratNumber~Species,value.var = "count") %>%
#   mutate(total=.[,4]+.[,5])%>%
#   filter(is.na(.[,4]))
# #write.csv(trial,"quality_check_kelp.csv",row.names = F,na="")

data <- kfm_common(data)
write.csv(data,"kfm/data/raw_pass/kfm_1mquad_pass.csv", row.names = FALSE,na="")

#5m_quad
data <- read.csv("kfm/data/raw_in/KFM_5mQuadrat_RawData_1996-2023.csv", header = TRUE, na.strings = na_val)
data$data_source <- "kfm"
data$sample_method <- "5mquad"
#merge with date
data <- merge(data,dates[!is.na(dates$X5mQUAD),c("SiteNumber","SurveyYear","X5mQUAD")], by = c("SiteNumber","SurveyYear"), all.x = TRUE)#check the survey date and only 1 day in a year, it is good

data <- rename(data, date = X5mQUAD,count = Count)
data$area = 10  #  20170213:each replicate is the sum of two adjacent two 5 m quads. 
data <- kfm_common(data)

# 20170213: Rass suggests 5m quad 1-20 are paralleled to 21-40. Because quad 1 and 21 were adjacent to each other, we combined quad 1 and 21 (or 2 and 22) and treat it as 1 replicate. 
data$side[data$QuadratNumber %in% 1:20]<-"A"
data$side[data$QuadratNumber %in% 21:40]<-"B"
data$QuadratNumber[data$side=="B"]<-data$QuadratNumber[data$side=="B"]-20

data1<-data %>%
  arrange(-count) %>% #there are two surveys at each location at site 34 in 2021, we choose the one that has the biggest count. 
  group_by(SiteNumber, SurveyYear, QuadratNumber, side,native_taxa_code,date) %>%
  slice(1) %>%
  dcast(SiteNumber+SurveyYear+IslandCode+IslandName+SiteCode+SiteName+native_taxa_code+scientific_name+CommonName_old+QuadratNumber+data_source+sample_method+date+area+common_name+lifestage~side,value.var="count",fill=NA)

#check to see the number of replicate, 2021 site 34 has duplicate. we remove the lowest value
# peace2<- data %>%
#   group_by(SiteNumber, SurveyYear, QuadratNumber, side,native_taxa_code,date,area,lifestage) %>%
#   summarise(replicate = n(),.groups = 'drop') 
data1$count=data1$A+data1$B
write.csv(data1,"kfm/data/raw_pass/kfm_5mquad_pass.csv", row.names = FALSE,na="")

#band_transect
data <- read.csv("kfm/data/raw_in/KFM_BandTransect_RawData_1982-2023.csv", header = TRUE, na.strings = na_val)
data$data_source <- "kfm"
data$sample_method <- "band"
#merge with date
data <- merge(data,dates[!is.na(dates$BANDTRAN),c("SiteNumber","SurveyYear","BANDTRAN")], by = c("SiteNumber","SurveyYear"), all.x = TRUE) #check the survey date and only 1 day in a year, it is good
data <- rename(data, date = BANDTRAN)

#  20170213:plots A and B are opposite of the transect. The sum of A and B are the total number for an experimental plot with 60 m2 area
data$CountB[is.na(data$CountB)]<-0
data$count<-data$CountA+data$CountB

data$area <- NA
data$area[data$SurveyYear < 1985] = 40
data$area[data$SurveyYear >= 1985] = 60 #20170213: the new protocol indicates area after 1985 changes to 60 m^2
data <- kfm_common(data)
write.csv(data,"kfm/data/raw_pass/kfm_band_pass.csv", row.names = FALSE,na="")


####################################################################
#rpc
# note, in the data sent in 2023, the survey date from 2022 and 2023 were not listed in the survey date file, I mannually add in based on the info from other survey
data <- read.csv("kfm/data/raw_in/KFM_RandomPointContact_RawData_1982-2023.csv", header = TRUE, na.strings = na_val)

data<-data %>%
  #filter(SurveyYear<2018) %>% # change this next year
  mutate(data_source = "kfm",
         sample_method = "rpc") 


# #site 30, 2014 appear twice, removed the zero fill one (first row of each record)  ### This issue was fixed in 2018 updates
# peace1<-data %>%
#   group_by(SiteNumber,IslandCode,IslandName,SiteCode,SiteName,SurveyYear,Species,ScientificName,CommonName,data_source,sample_method,Quadrat_Number) %>%
#   slice(n()) %>%
#   ungroup()

# site 12, 13, 32, 34 have duplicate records, we keep the records that count C and D are present (except site 32, we choose MAY 20 and remove May 17)
# trial <- data %>%
#   group_by(SiteNumber, SurveyYear, Species, ScientificName, CommonName, data_source, sample_method, Quadrat_Number) %>%
#   summarise(replicate = n(),.groups = 'drop') %>%
#   filter(replicate > 1) 

data_upc <- data %>%
  filter(!(SiteNumber==32&SurveyYear==2022&SurveyDate=="5/17/2022 0:00:00")) %>%
  group_by(SiteNumber, SurveyYear, Species, ScientificName, CommonName, data_source, sample_method, Quadrat_Number) %>%
  mutate(replicate = n()) %>%
  # if there are multiple rows in a group, remove any rows that have a count in C or D
  filter(!(replicate > 1 & is.na(CountC) & is.na(CountD))) %>%
  ungroup()

  
#merge with date # check to see there is only 1 day a year, good.
data_upc2 <- merge(data_upc,dates[!is.na(dates$RPC),c("SiteNumber","SurveyYear","RPC")], by = c("SiteNumber","SurveyYear"), all.x = TRUE)

# note data received in 2023, site 2 in year 1986, 1987, 1988 had the wrong survey date, same for site 12 in 2015. We mannually change the date data before merging.  
# trial2 <- data_upc2 %>%
#   group_by(SiteNumber, SurveyYear, Species, ScientificName, CommonName, data_source, sample_method, Quadrat_Number) %>%
#   summarise(replicate = n(),.groups = 'drop') %>%
#   filter(replicate > 1) %>%
#   distinct(SiteNumber, SurveyYear) 

data <- data_upc2

data <- rename(data, year = SurveyYear,date = RPC)
data$count <- rowSums(data[,c("CountA","CountB","CountC","CountD")], na.rm = TRUE)
data$points <- NA
data$points[data$year == 1982] <- 20
data$points[data$year == 1983] <- 10
data$points[data$year == 1984] <- 50
data$points[data$year > 1984] <- 40
data <- kfm_common(data)
####
# #testing to check substrate. The output below shows that there is no substrate or all bare substrate

# trial<-data %>%
#   filter(native_taxa_code %in% c(15002,15003,15004)) %>%
#   mutate(percent=count/points) %>%
#   group_by(SiteNumber,year,date,Quadrat_Number) %>%
#   summarise(sumall=sum(percent),.groups='drop') %>%
#   ungroup() %>%
#   filter(sumall<0.99)
# 
# write.csv(trial,"substrate_not_100.csv",row.names = F)
#--------------------------------------------------------------------------------------
write.csv(data,"kfm/data/raw_pass/kfm_rpc_pass.csv", row.names = FALSE,na="")

#####################################################################################################
#rdfc
data <- read.csv("kfm/data/raw_in/KFM_RovingDiverFishCount_RawData_1982-2023.csv", stringsAsFactors = F,header = TRUE, na.strings = na_val,encoding = "UTF-8")

#error checking: the data has some errors on NA fills . 
#remove NA in both abundance and count
data <- data %>%
  filter(!(is.na(Abundance)&is.na(Count))) %>%
  filter(ExperienceLevel == "E")

# assign the categorical abundance to the numerical count
data$data_source <- "kfm"
data$sample_method <- "rdfc"
data$Abundance <- tolower(data$Abundance)
data$Abundance[data$Count == 0] <- "-"
data$Abundance[data$Count == 1] <- "s"
data$Abundance[data$Count >= 2 & data$Count <= 10] <- "f"
data$Abundance[data$Count >= 11 & data$Count <= 100] <- "c"
data$Abundance[data$Count > 100] <- "m"
data$Count[data$Abundance == "-"] <- 0
data$Count[data$Abundance == "s"] <- 1
data$Species <- NA

# species list
# spplist<-data %>%
#   distinct(CommonName, ScientificName) %>%
#   arrange(CommonName) 

#the yellowtail rock fish can't be distinguished in the juvenile stage. so it combined with flavidus species in the survey. However, it is safe to say the yellowtail juvenile is not found in the survey
# change some name spelling
data2 <- data %>%
  dplyr::mutate(ScientificName=if_else(ScientificName=="Sebastes serranoides/flavidus","Sebastes serranoides",ScientificName),
         CommonName=if_else(CommonName=="olive/yellowtail rockfish, juvenile","olive rockfish, juvenile",CommonName))
         #CommonName=if_else(str_detect(CommonName, "^se"),"senorita",CommonName))

# species list
# spplist2<-data2 %>%
#   distinct(CommonName, ScientificName) %>%
#   arrange(CommonName) 

data3 <- kfm_common(data2)

# one fish spelling can't be handled in R, so we change it 
data <- data3 %>%
  mutate(common_name=if_else(str_detect(common_name, "^se"),"senorita",common_name))

# test <- data %>%
#   filter(str_detect(common_name, "^se"))

#Abundance-count map: calculate the specie-specific count and assign to the abundance value. 
map <- data %>%
  select(scientific_name,lifestage,Abundance,Count) %>%
  filter(!is.na(Count),Abundance != "-",Abundance!="s") %>%
  group_by(scientific_name,lifestage,Abundance) %>%
  summarise(count_mean = as.integer(mean(Count)),.groups='drop') %>%
  ungroup()

cfill <- data %>% #count_fill, only work on the data that is after 2003 that count is NA but Abundance has value. 
  filter(SurveyYear >= 2003 & is.na(Count)& !is.na(Abundance)) %>%
  select(-Count) %>%
  left_join(map,by = c("scientific_name", "lifestage", "Abundance")) %>%
  rename(Count = count_mean)

data <- data %>%   
  filter(!(SurveyYear >= 2003 & is.na(Count) & !is.na(Abundance)))

data <- rbind(data,cfill)

#minor qc
data$date <- as.Date(data$SurveyDate,"%m/%d/%Y")
data$lifestage[is.na(data$lifestage)] <- "all"
data <- rename(data,count = Count)

#use the -99999 to distinguish between NA-filled or has abundance value, -99999 equal to has abundance data
data$count[is.na(data$count)]<--99999

# datak <-data %>%
#   group_by(PermanentObserverNumber,SiteCode , date ,scientific_name, lifestage) %>%
#   summarise(freq=n())%>%
#   ungroup() %>%
#   filter(freq==2)
#cast by lifestage
peace <- dcast(data,PermanentObserverNumber + SiteCode + date + scientific_name ~ lifestage, value.var = "count",fill = NA)



peace <- peace %>%
  mutate(adult=ifelse((!is.na(female)|!is.na(male)),rowSums(cbind(female,male),na.rm=T),adult),#sum male/female to adult. If there is any record has either male or female, assume the none one is 0.
         adult=ifelse(subadult>0&!is.na(subadult),subadult+adult,adult),# starting in 2018, the species Hypsypops rubicundus add subadult category, we group this into "adult". 
         adult = ifelse(all==0&!is.na(all),0,adult),#zero-fill !adult !juvenile & all. if all=0, then adult and juvenile should be 0. 
         juvenile = ifelse(all==0&!is.na(all),0,juvenile),
         adult=ifelse(is.na(adult)& is.na(juvenile),all,adult),#20170302: fill in adult in the case of adult is na and juvenile is na. the adult == all(Josh suggested) 
         adult = ifelse(is.na(adult)& is.na(all),0,adult), #fill in 0 for the adult in the case of adult is NA and all is NA
         all=rowSums(cbind(adult, juvenile),na.rm=T)) %>% #20170213: sum to all when adult & juvenile & !all, as well as if adult+juvenile <> all
  select(-female,-male,-subadult) 
         
#trail1<-peace1 %>% mutate(aa = adult+juvenile) %>%filter(aa!=all)         
         
##20170228: some records has only adult or juvenile but the all was zero-fill. we correct this by assigning adult or juvenile to all (problem was fixed by 20170313). 
# peace[which(peace$all==0& peace$juvenile!=0 & peace$adult==0),]$all<- peace[which(peace$all==0& peace$juvenile!=0 & peace$adult==0),]$juvenile 
# peace[which(peace$all==0& peace$juvenile==0 & peace$adult!=0),]$all<- peace[which(peace$all==0& peace$juvenile==0 & peace$adult!=0),]$adult
##20170228: some records has all >0, adult==0, and NA for juvenile. we correct this by assigning all to adult (problem was fixed by 20170313). 
#peace[which(peace$all!=0& is.na(peace$juvenile) & peace$adult==0),]$adult<- peace[which(peace$all!=0& is.na(peace$juvenile) & peace$adult==0),]$all
#20170228: all the sebastes serranoides species has na for juvenile which can be 0 filled. Also, the adult always less than the all. We correct this by assigning all to adult (problem was fixed by 20170313)
#peace[which(peace$scientific_name=="Sebastes serranoides"),]$juvenile<-0
#peace[which(peace$scientific_name=="Sebastes serranoides"),]$adult<-peace[which(peace$scientific_name=="Sebastes serranoides"),]$all


#zero fill in the rest of NA. We had check that the juvenile that is NA only exist in certain species
peace[is.na(peace)]<-0

#from now on, any non-converted abundance data get a NA
peace[peace<0]<-NA
#

# #produce data table for Josh from KFM to check the case that adult_juvenile<> all
# peace$all_cal[!is.na(peace$adult) & !is.na(peace$juvenile)] <- rowSums(peace[!is.na(peace$adult) & !is.na(peace$juvenile),c("adult","juvenile")])
# aa<-peace %>%
#   filter(!(is.na(all)|is.na(all_cal))) %>%
#   mutate(diff=all-all_cal) %>%
#   filter(!diff==0)
# 
# dataa<-data %>%
#   select(SiteCode,PermanentObserverNumber,scientific_name,CommonName_old,date,Abundance,count) 
# 
# bb<-aa %>%
#   left_join(dataa,by=c("PermanentObserverNumber","SiteCode","date","scientific_name")) %>%
#   select(-adult,-all,-juvenile,-all_cal,-diff)
# 
# rem<-bb %>%
#   group_by(PermanentObserverNumber,SiteCode,date,scientific_name)%>%
#   summarise(count=mean(count)) %>%
#   ungroup() %>%
#   filter(!is.na(count)) %>%
#   select(PermanentObserverNumber,SiteCode,date,scientific_name) %>%
#   distinct()
# 
# cc<-rem %>%
#   left_join(bb,by=c("PermanentObserverNumber","SiteCode","date","scientific_name")) 
#   
# 
# write.csv(cc,"kfm/data/data_check.csv", row.names = FALSE)


#zero-fill divers. If a diver didn't see a fish, we assign 0 for the diver. 
peace1 <- melt(peace, id=c("PermanentObserverNumber","SiteCode", "date","scientific_name"), na.rm = F, value.name = "count")  

observation<- unique(peace1[c("SiteCode","date")])

newdata <- data.frame()
for (i in 1:nrow(observation)) {
    peace_fil <- peace1 %>%
     filter(SiteCode==observation[i,"SiteCode"],date==observation[i,"date"]) %>%
     mutate(count=ifelse(is.na(count),"NAAN",count)) %>%
     dcast(SiteCode+date+scientific_name+variable ~ PermanentObserverNumber,value.var="count",fill=0) %>%
     rename(lifestage=variable) %>%
     melt(id=c("SiteCode", "date","scientific_name","lifestage"), na.rm = F, value.name = "count") %>%
     rename(PermanentObserverNumber=variable) %>%
     mutate(count=ifelse(count=="NAAN",NA,count)) %>%
     dcast(PermanentObserverNumber+SiteCode+date+scientific_name~lifestage,value.var="count") %>%
     mutate(PermanentObserverNumber=as.integer(levels(PermanentObserverNumber)[PermanentObserverNumber]),adult=as.numeric(adult),all=as.numeric(all),juvenile=as.numeric(juvenile)) 
     newdata <- rbind(newdata,peace_fil)
} 
 #merge the data 
data_name<- data %>%
  select(scientific_name,common_name) %>%
  distinct() %>%
  right_join(newdata,by="scientific_name")

data2 <- data %>%
  select(-lifestage,-count,-Abundance,-Score,-CommonName_old,-scientific_name,-common_name) %>%
  distinct() %>%
  left_join(data_name, by = c("PermanentObserverNumber","SiteCode","date"))

# #aggregate over divers, if there is NA (abundance), the data remain as NA (Abundance). 20171012, Rass suggested to keep all divers in the dataset and make them into replicates. Any fish data after 2017-10-12 have 2-8 divers for each transects. 
# data2 <- data2 %>%
#   group_by(data_source,sample_method,SiteNumber,IslandCode,IslandName,SiteCode,SiteName,SurveyYear,date,scientific_name,common_name,native_taxa_code) %>%
#   summarise(adult = round(mean(adult)), juvenile = round(mean(juvenile)), all = round(mean(all))) %>%
#   ungroup()

#cleanup
data2$sample_subtype <- "FUL"
data2$height <- NA
data2$area <- 2000
data2$lifestage <- NA

write.csv(data2,"kfm/data/raw_pass/kfm_rdfc_pass.csv", row.names = FALSE,na="")

#visual_fish
data <- read.csv("kfm/data/raw_in/KFM_VisualFishTransect_RawData_1985-2023.csv", header = TRUE, na.strings = na_val)
data$data_source <- "kfm"
data$sample_method <- "visualfish"
data$sample_subtype <- "BOT"
data$height <- 3

#error
#one date is not correct "1993-07-29" because there was only 1 species in this survey and it should change to the previous date 1993-07-28( issue fixed in 2018). 
#data[data$SurveyDate=="7/29/1993 0:00",]$SurveyDate<-"7/28/1993 0:00"


data$date <- as.Date(data$SurveyDate,"%m/%d/%Y")
data$SurveyYear <- as.integer(format(data$date, "%Y"))
data$area[data$SurveyYear < 1996] = 200
data$area[data$SurveyYear >= 1996] = 100

#20170213: transect 3 and 4 prior to 1996 is repeated survey. We removed it. 
data<-data %>%
  filter(!(SurveyYear<1996&Transect_Number %in% c(3,4)))


##20170213: restrict to "A" counts, remove "B" counts. 
data$count <- data$CountA
data <- kfm_common(data)

data <- data %>%
  mutate(common_name=if_else(str_detect(common_name, "^se"),"senorita",common_name))


#cast by lifestage
peace <- dcast(data,SiteCode + Transect_Number + date + scientific_name ~ lifestage, value.var = "count",fill = NA)
#sum male/female to adult
peace <- peace %>%
  mutate(adult=ifelse((!is.na(adult) | !is.na(female) | !is.na(male)),rowSums(cbind(female,male, adult),na.rm=T),adult),#sum male/female to adult. If there is any record has either male or female, assume the none one is 0.
          adult=ifelse(subadult>0&!is.na(subadult),subadult+adult,adult)) %>% # starting in 2018, the species Hypsypops rubicundus add subadult category, we group this into "adult".
  select(-male,-female,-subadult)

#trail1<-peace1 %>%filter(!is.na(adult) | !is.na(female) | !is.na(male))         


#zero-fill !adult & juvenile
peace$adult[is.na(peace$adult) & !is.na(peace$juvenile)] <- 0
#zero-fill adult & !juvenile
peace$juvenile[!is.na(peace$adult) & is.na(peace$juvenile)] <- 0
peace <- peace %>%
  mutate(all = adult + juvenile)
ntc <- data %>%
  select(native_taxa_code,scientific_name) %>%
  distinct() %>%
  mutate(ntc = sprintf("%5.2f",native_taxa_code)) %>%
  group_by(scientific_name) %>%
  summarise(native_taxa_code = paste(ntc, collapse = "_"))
data <- data %>%
  select(-lifestage,-CountA,-CountB,-count,-CommonName_old,-native_taxa_code) %>%
  distinct() %>%
  left_join(ntc,by = "scientific_name") %>%
  left_join(peace, by = c("SiteCode","Transect_Number","date","scientific_name"))
data$lifestage <- NA

write.csv(data,"kfm/data/raw_pass/kfm_visualfish_pass.csv", row.names = FALSE,na="")
