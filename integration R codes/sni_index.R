rm(list = ls())

#index prefix key:
#s - site
#t - taxa
#a - survey
#b - transect
#c - replicate

dataset <- c(
  "fish",
  "swath",
  "quad",
  "rpc"
)

taxa_lookup <- read.csv("sni/data/raw_in/lookups/tblSpecies_2018_08_22.csv")
taxa_lookup$scientific_name <- as.factor(gsub(" spp.","",taxa_lookup$SpeciesName))   #remove " .spp" strings
taxa_lookup$native_taxa_code = factor(taxa_lookup$SpeciesCode)
taxa_lookup <- rename(taxa_lookup, common_name = CommonNames)

data <- list()
#read in files
for (ii in 1:length(dataset)){
  readpath <- paste0("sni/data/raw_pass/sni_",dataset[ii],"_pass.csv")
  data[[ii]] <- read.csv(readpath,stringsAsFactors = F,header = TRUE,na="")
}

#assign replicate_id's

replicate_key <- "ID_cha"

for (ii in 1:length(dataset)){
  #define replicate id
  rep_key = c("data_source",replicate_key[!is.na(replicate_key)])
  rep <-  select_(data[[ii]],.dots = rep_key) %>%
    distinct() %>%
    arrange_(rep_key)
  rep$replicate_id <- factor(1:dim(rep)[1])
  data[[ii]] <- left_join(data[[ii]],rep,by = rep_key)
}

#This code is for generating the index the first time.
#The updated should be use the code in line 123

#scrape sites
site_key <- c("data_source","Station")
subsite_key <- c(site_key)
subsite <- list()
for (ii in 1:length(dataset)){
  subsite <- rbind(subsite, data[[ii]][,subsite_key])
}
subsite <- subsite %>%
  distinct() %>%
  arrange_(.dots = subsite_key)
site <- subsite %>%
  select_(.dots = site_key) %>%
  distinct()
site$site_id <- factor(sprintf("a-s-%02d",1:dim(site)[1]))
subsite <- inner_join(subsite,site, by = site_key)
subsite$subsite_id <- factor(sprintf("b-s-%02d",1:dim(subsite)[1]))
for (ii in 1:length(dataset)){
  data[[ii]] <- merge(data[[ii]],subsite,by = subsite_key)
}
#load and merge latlons
site_lookup <- read.csv("integrated/data/community_structure/lookups/components/sites_lat_lon_KFM_SNI_PISCO_LTER.csv",header = TRUE)
subsite <- merge(subsite,site_lookup[,c("data_source","number","latitude","longitude")],by.x = c("data_source","Station"), by.y = c("data_source","number"), all.x = TRUE)
subsite <- rename(subsite, sitevar_1 = Station)
subsite$sitevar_2 <- NA
write.csv(subsite,"sni/data/raw_index/lookups/sni_sites.csv", row.names = FALSE,na="")

#scrape transect
transect_key <- c("data_source","site_id","subsite_id","ID_tra")
transect <- list()
for (ii in 1:length(dataset)){
  transect <- rbind(transect, data[[ii]][,transect_key])
}
transect <- unique(transect)
transect <- transect[with(transect, order(site_id)), ]
transect$transect_id <- factor(sprintf("c-s-%05d",1:dim(transect)[1]))
for (ii in 1:length(dataset)){
  data[[ii]] <- merge(data[[ii]],transect,by = c(transect_key))
}
tr_read_key <- c(transect_key,"sample_method","date","transect_id")
transect <- list()
for (ii in 1:length(dataset)){
  transect <- rbind(transect, data[[ii]][,tr_read_key])
}
transect <- unique(transect)
transect <- transect[with(transect, order(site_id,sample_method)), ]
transect$zone <- NA

write.csv(transect,"sni/data/raw_index/lookups/sni_transects.csv", row.names = FALSE,na="")

#scrape taxa
taxa_key <- c("data_source","sample_method","native_taxa_code","lifestage")
taxa <- list()
for (ii in 1:length(dataset)){
  taxa <- rbind(taxa, data[[ii]][,taxa_key])
}
#assign unique id
taxa <- unique(taxa)
taxa$taxa_id <- factor(sprintf("t-s-%03d",1:dim(taxa)[1]))
#mege id with data
for (ii in 1:length(dataset)){
  data[[ii]] <- merge(data[[ii]],taxa[,c(taxa_key,"taxa_id")],by = taxa_key)
}
#format
taxa <- merge(taxa,taxa_lookup[,c("native_taxa_code","scientific_name","common_name")], by = c("native_taxa_code"), all.x = TRUE)

#remove " .spp" strings
taxa$common_name[taxa$common_name == ""] <- NA
taxa <- taxa[with(taxa, order(scientific_name,lifestage,native_taxa_code)), ]
#subset
taxa <- taxa[,c("taxa_id","data_source","sample_method","native_taxa_code","scientific_name","lifestage","common_name")]
write.csv(taxa,"sni/data/raw_index/lookups/sni_taxa.csv", row.names = FALSE,na="")

#write to files
for (ii in 1:length(dataset)){
  writepath <- paste0("sni/data/raw_index/sni_",dataset[ii],"_index.csv")
  write.csv(data[[ii]],writepath, row.names = FALSE,na="")
}

#if we already have all the subsite/site/transect/taxa table, we use the code below. 
# 
# subsite <- read.csv("sni/data/raw_index/lookups/sni_sites.csv") %>%
#   rename(site=sitevar_1) %>%
#   select(data_source,site,site_id,subsite_id) %>%
#   rename(Station=site)
# 
# transect<-read.csv("sni/data/raw_index/lookups/sni_transects.csv") %>%
#   select(site_id,ID_tra,transect_id)%>%
#   distinct()
# 
# taxa<-read.csv("sni/data/raw_index/lookups/sni_taxa.csv") %>%
#   select(taxa_id,sample_method,native_taxa_code)%>%
#   mutate(sample_method=as.character(sample_method))

# 
# 
# for (ii in 1:length(dataset)){
#   peace <- data[[ii]] 
#   peace$sample_method <- as.character(peace$sample_method)
#   peace1<-peace %>%
#     #mutate(native_taxa_code=as.factor(native_taxa_code)) %>%
#     left_join(subsite,by=c("data_source","Station")) %>%
#     left_join(transect,by=c("site_id","ID_tra")) %>%
#     left_join(taxa,by=c("sample_method","native_taxa_code"))
#   
#   #filter out non-matching ones
#   # 
#   #kk<-peace1 %>%
#   #  filter(is.na(site_id)|is.na(subsite_id)|is.na(transect_id)|is.na(taxa_id))
#    # print(kk)
#   
#   writepath <- paste0("sni/data/raw_index/sni_",dataset[ii],"_index.csv")
#   write.csv(peace1,writepath, row.names = FALSE,na="")
# }


#####################################
# #checking dataset
# data <- list()
# #read in files
# for (ii in 1:length(dataset)){
#   readpath <- paste0("sni/data/raw_index/sni_",dataset[ii],"_index.csv")
#   data[[ii]] <- read.csv(readpath,header = TRUE,na="")
# }
# 
# data1 <-data[[1]]
# 
# unique(data1$ID_cha)
