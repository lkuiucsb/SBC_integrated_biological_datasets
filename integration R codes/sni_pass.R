## Config ##
rm(list = ls())
na_val = ""

sni_common <- function(data){
  data$native_taxa_code = factor(sprintf("%04s",data$SpeciesCode))
  data$date <- as.Date(data$Date,"%m/%d/%Y")
  #data <- data[data$Period < 70,] #remove any navy-funded records
  data$ID_cha <- data$ID
  data$ID_cha <- sub('-B','', data$ID_cha) #create ID for the replicate
  data$ID_cha <- sub('-M','', data$ID_cha)
  data$ID_tra<- sub(".*-(.*?)-.*", "\\1", data$ID) #create ID for the transect
  data<-data %>%
    mutate(Station=case_when(
      Station==1|Station==8 ~ 11,
      Station==2|Station==3 ~ 12,
      Station==4|Station==5 ~ 13,
      Station==6|Station==9 ~ 14,
      Station==7 ~ 7
    ))
  return(data)
}

#fish
data_1 <- read.csv("sni/data/raw_in/tblSNI6ZeroFill.csv", header = TRUE,stringsAsFactors = F, na.strings = na_val)
data_2 <- read.csv("sni/data/raw_in/tblSNI6ZeroFill_2016_2017.csv", header = TRUE, stringsAsFactors = F,na.strings = na_val)

data <-bind_rows(data_1,data_2)

# #join with header

header_1 <- read.csv("sni/data/raw_in/tblSNI6Header.csv", stringsAsFactors = F,na.strings = na_val)
header_2 <- read.csv("sni/data/raw_in/tblSNI6Header_2016_2017.csv", stringsAsFactors = F,na.strings = na_val)
 
header <-bind_rows(header_1,header_2)

# data cleanning and zero- fill the species that was added later. Open species list
 
 
data  <- data %>%
  rename(adult = AdultCount, juvenile = JuvCount, size= Size) %>%
  mutate(size=as.numeric(size),size=if_else(is.na(size)&!is.na(Min),(Min+Max)/2,size),
         SpeciesCode=as.character(SpeciesCode)) %>%
         #adult=if_else(adult==999999,as.integer(NA),adult)) #there were two cases that the fish number were rediculous
  group_by (ID,SpeciesCode)%>%
  summarise(juvenile=sum(juvenile),adult=sum(adult),size=mean(size,na.rm=T))%>%
  ungroup()  
    
    
data_zero <- data %>%  
    select(ID,SpeciesCode) %>%
    mutate(value=as.numeric(1)) %>%
    dcast(ID~SpeciesCode,value.var = "value",fill=as.integer(0)) %>%
    melt(id=c("ID"),variable.name="SpeciesCode",value.name = "value")
  

data <- data_zero %>%
  left_join(data,by=c("ID","SpeciesCode")) %>%
  left_join(header,by="ID") %>%
  mutate(juvenile=if_else(value==0&is.na(juvenile),as.integer(0),juvenile),
         adult=if_else(value==0&is.na(adult),as.integer(0),adult)) %>%
  select(-value)
 
  
# data1<-data %>%

data$all = rowSums(data[,c("adult","juvenile")])
data$data_source = "sni"
data$sample_method = "fish"
data$sample_subtype[data$Depth == "B"] <- "BOT"
data$area[data$Depth == "B"] <- 100
data$height[data$Depth == "B"] <- 2
data$sample_subtype[data$Depth == "M"] <- "MID"
data$area[data$Depth == "M"] <- 250
data$lifestage <- NA

data <-sni_common(data)

write.csv(data,"sni/data/raw_pass/sni_fish_pass.csv", row.names = FALSE,na="")

###########################################################
#swath
data_1 <- read.csv("sni/data/raw_in/tblSNI1ZeroFill.csv", header = TRUE, stringsAsFactors = F,na.strings = na_val)
data_2 <- read.csv("sni/data/raw_in/tblSNI1ZeroFill_2016_2017.csv", header = TRUE, stringsAsFactors = F,na.strings = na_val)
data <-bind_rows(data_1,data_2)


#join with header
header_1 <- read.csv("sni/data/raw_in/tblSNI1Header.csv", stringsAsFactors = F,na.strings = na_val)
header_2 <- read.csv("sni/data/raw_in/tblSNI1Header_2016_2017.csv", stringsAsFactors = F,na.strings = na_val)
header <-bind_rows(header_1,header_2)

#replace some of the station name: 11 12 13 14

data <- merge(data, header, by = "ID")

#NA filled for the species that was added after 2014 but was not surveyed in the early year. 
data<-data %>%
  dcast(ID+Period+Station+Swath+Observer+Date+Status~SpeciesCode,value.var = "Count",fill=NA)%>%
  melt(id=c("ID","Period","Station","Swath","Observer","Date","Status"),variable.name="SpeciesCode",value.name = "Count")%>%
  mutate(SpeciesCode= as.character(SpeciesCode))

#data cleanning
data$data_source = "sni"
data$sample_method = "swath"
data$lifestage <- NA
data$area <- 20
data <- rename(data, count = Count)

data <-sni_common(data)
write.csv(data,"sni/data/raw_pass/sni_swath_pass.csv", row.names = FALSE,na="")

#########################################
#rpc
data_1 <- read.csv("sni/data/raw_in/tblSNI2ZeroFill.csv", header = TRUE, stringsAsFactors = F,na.strings = na_val)
data_2 <- read.csv("sni/data/raw_in/tblSNI2ZeroFill_2016_2017.csv", header = TRUE, stringsAsFactors = F,na.strings = na_val)

data <-bind_rows(data_1,data_2) %>%
  mutate(SpeciesCode=as.character(SpeciesCode))

# data cleanning and zero- fill the species that was added later. Open species list
data_zero <- data %>%  
  dcast(ID~SpeciesCode,value.var = "Count",fill=as.integer(0)) %>%
  melt(id=c("ID"),variable.name="SpeciesCode",value.name = "Count")

#join with header
header_1 <- read.csv("sni/data/raw_in/tblSNI2Header.csv", stringsAsFactors = F,na.strings = na_val)
header_2 <- read.csv("sni/data/raw_in/tblSNI2Header_2016_2017.csv", stringsAsFactors = F,na.strings = na_val)

header <-bind_rows(header_1,header_2)

data <- merge(data_zero, header, by = "ID")


data$data_source = "sni"
data$sample_method = "rpc"
data$lifestage <- NA
data$points <- 20
data <- rename(data, count = Count)
data <-sni_common(data)

# #test for zero-filled species
# peace <- data %>%
#   select(ID,SpeciesCode) %>%
#   group_by(SpeciesCode) %>%
#   summarise(freq=n()) %>%
#   ungroup()

write.csv(data,"sni/data/raw_pass/sni_rpc_pass.csv", row.names = FALSE,na="")

#################################################
#quad
data_1 <- read.csv("sni/data/raw_in/tblSNI2aZeroFill.csv",stringsAsFactors = F, header = TRUE, na.strings = na_val)
data_2 <- read.csv("sni/data/raw_in/tblSNI2aZeroFill_2016_2017.csv", header = TRUE, stringsAsFactors = F,na.strings = na_val)
data <-bind_rows(data_1,data_2)%>%
  mutate(SpeciesCode=as.character(SpeciesCode))

#join with header
header_1 <- read.csv("sni/data/raw_in/tblSNI2Header.csv",stringsAsFactors = F, na.strings = na_val)
header_2 <- read.csv("sni/data/raw_in/tblSNI2Header_2016_2017.csv", stringsAsFactors = F,na.strings = na_val)

header <-bind_rows(header_1,header_2)

data <- merge(data, header, by = "ID") 
  

data$data_source = "sni"
data$sample_method = "quad"
data$lifestage <- NA
data$area <- 1
data <- rename(data, count = Count)
data <-sni_common(data)

# #test for zero-filled species. No need to zero/NA fill
# peace <- data %>%
#   select(ID,SpeciesCode) %>%
#   group_by(SpeciesCode) %>%
#   summarise(freq=n()) %>%
#   ungroup()


write.csv(data,"sni/data/raw_pass/sni_quad_pass.csv", row.names = FALSE,na="")
