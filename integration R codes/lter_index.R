rm(list = ls())

#index prefix key:
#a - site
#b -subsite
#t - taxa
#c - transect
#  - replicate

dataset <- list(
  "fish",
  "crypticfish",
  "quad",
  "swath",
  "upc"
)

data <- list()
#read in files
for (ii in 1:length(dataset)){
  readpath <- paste0("lter/data/raw_pass/lter_",dataset[ii],"_pass.csv")
  data[[ii]] <- read.csv(readpath,header = TRUE,na="")
}

#assign replicate_id's
replicate_key <- list(
  NA,
  NA,
  c("quad","side"),
  c("quad","side"),
  NA
)

for (ii in 1:length(dataset)){
  #define replicate id
  rep_key = c("data_source",replicate_key[[ii]][!is.na(replicate_key[[ii]])])
  
  rep <- data[[ii]]%>%
    dplyr::select(all_of(rep_key)) %>%
    distinct() %>%
    arrange(.)
  
  rep$replicate_id <- factor(1:dim(rep)[1])
  data[[ii]] <- left_join(data[[ii]],rep,by = rep_key)
}

# #scrape sites
# site_key <- c("data_source","site")
# subsite_key <- c(site_key)
# subsite <- list()
# for (ii in 1:length(dataset)){
#   subsite <- rbind(subsite, data[[ii]][,subsite_key])
# }
# subsite <- subsite %>%
#   distinct() %>%
#   arrange_(.dots = subsite_key)
# site <- subsite %>%
#   select_(.dots = site_key) %>%
#   distinct()
# site$site_id <- factor(sprintf("a-l-%02d",1:dim(site)[1]))
# subsite <- inner_join(subsite,site, by = site_key)
# subsite$subsite_id <- factor(sprintf("b-l-%02d",1:dim(subsite)[1]))
# for (ii in 1:length(dataset)){
#   data[[ii]] <- merge(data[[ii]],subsite,by = subsite_key)
# }
# #load and merge latlons
# site_lookup <- read.csv("integrated/data/community_structure/lookups/components/sites_lat_lon_KFM_SNI_PISCO_LTER.csv",header = TRUE)
# site_lookup$data_source <- as.character(site_lookup$data_source)
# site_lookup$site_code <- as.character(site_lookup$site_code)
# subsite$data_source <- as.character(subsite$data_source)
# subsite$site <- as.character(subsite$site)
# subsite <- subsite %>%
#   left_join(site_lookup[,c("data_source","site_code","latitude","longitude")],by = c("data_source", "site" = "site_code")) %>%
#   rename(sitevar_1 = site) %>%
#   group_by(data_source,sitevar_1,site_id,subsite_id) %>%
#   summarise(latitude = mean(latitude, na.rm = TRUE),longitude = mean(longitude, na.rm = TRUE)) %>%
#   ungroup()
# subsite$sitevar_2 <- NA
# write.csv(subsite,"lter/data/raw_index/lookups/lter_sites.csv", row.names = FALSE)
# 
# #scrape transect
# transect_key <- c("data_source","site_id","subsite_id","transect")
# transect <- list()
# for (ii in 1:length(dataset)){
#   transect <- rbind(transect, data[[ii]][,transect_key])
# }
# transect <- unique(transect)
# transect <- transect[with(transect, order(site_id)), ]
# transect$transect_id <- factor(sprintf("c-l-%05d",1:dim(transect)[1]))
# for (ii in 1:length(dataset)){
#   data[[ii]] <- merge(data[[ii]],transect,by = c(transect_key))
# }
# tr_read_key <- c(transect_key,"sample_method","date","transect_id")
# transect <- list()
# for (ii in 1:length(dataset)){
#   transect <- rbind(transect, data[[ii]][,tr_read_key])
# }
# transect <- unique(transect)
# transect <- transect[with(transect, order(site_id,sample_method)), ]
# transect$zone <- NA
# write.csv(transect,"lter/data/raw_index/lookups/lter_transects.csv", row.names = FALSE)
# 

#scrape taxa----------------------------------------------------
# taxa_key <- c("data_source","sample_method","worms_name","lifestage","common_name","native_taxa_code")
# taxa <- list()
# for (ii in 1:length(dataset)){
#   taxa <- rbind(taxa, data[[ii]][,taxa_key])
# }
# 
# #assign unique id
# # we need the worms name instead of scientific name assign by the scientist.
# taxa <- unique(taxa) %>%
#   rename(scientific_name=worms_name)
# 
# taxa$taxa_id <- factor(sprintf("t-l-%03d",1:dim(taxa)[1]))
# taxa <- taxa[with(taxa, order(scientific_name,lifestage,native_taxa_code)), ]
# 
# # #subset
#  taxa <- taxa[,c("taxa_id","data_source","sample_method","native_taxa_code","scientific_name","lifestage","common_name")]
# write.csv(taxa,"lter/data/raw_index/lookups/lter_taxa.csv",na="", row.names = FALSE)
######################################


#We have existing site information, merge with current data

# merge to get site, subsite, and transect

subsite <- read.csv("lter/data/raw_index/lookups/lter_sites.csv",na="") %>%
  rename(site=sitevar_1) %>%
  select(data_source,site,site_id,subsite_id)

transect<-read.csv("lter/data/raw_index/lookups/lter_transects.csv",na="") %>%
  select(site_id,transect,transect_id)%>%
  distinct() 

taxa <-read.csv("lter/data/raw_index/lookups/lter_taxa.csv",na="",stringsAsFactors = F)

taxa <- taxa %>%
  select(taxa_id,sample_method,native_taxa_code)

#merge all info together
for (ii in 1:length(dataset)){
   peace<-data[[ii]] 
   
   peace1<-peace %>%  
    mutate(native_taxa_code=as.factor(native_taxa_code)) %>%
    left_join(subsite,by=c("data_source","site")) %>%
    left_join(transect,by=c("site_id","transect")) %>%
    left_join(taxa,by=c("sample_method","native_taxa_code"))
   
   #filter out non-matching ones
   # 
    kk<-peace1 %>%
      filter(is.na(site_id)|is.na(subsite_id)|is.na(transect_id)|is.na(taxa_id))
      print(kk)
      print(paste0("original data is ",nrow(peace), " and the merged data is ",nrow(peace1)))
   
  writepath <- paste0("lter/data/raw_index/lter_",dataset[ii],"_index.csv")
  write.csv(peace1,writepath, row.names = FALSE,na="")
}


