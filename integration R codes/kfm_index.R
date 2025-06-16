rm(list = ls())

#index prefix key:
#a - site
#t - taxa
#
#b - transect
#c - replicate

dataset <- list(
  "1mquad",
  "5mquad",
  "band",
  "rpc",
  "rdfc",
  "visualfish"
)

data <- list()
#read in files
for (ii in 1:length(dataset)){
  readpath <- paste0("kfm/data/raw_pass/kfm_",dataset[ii],"_pass.csv")
  data[[ii]] <- read.csv(readpath,header = TRUE,na="")
}

#assign replicate_id's
replicate_key <- list(
  "QuadratNumber",
  "QuadratNumber",
  "TransectNumber",
  "Quadrat_Number",
  "PermanentObserverNumber",
  "Transect_Number"
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
# site_key <- c("data_source","SiteNumber")
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
# site$site_id <- factor(sprintf("a-k-%02d",1:dim(site)[1]))
# subsite <- inner_join(subsite,site, by = site_key)
# subsite$subsite_id <- factor(sprintf("b-k-%02d",1:dim(subsite)[1]))
# for (ii in 1:length(dataset)){
#   data[[ii]] <- merge(data[[ii]],subsite,by = subsite_key)
# }
# #load and merge latlons
# site_lookup <- read.csv("integrated/data/community_structure/lookups/components/sites_lat_lon_KFM_SNI_PISCO_LTER.csv",header = TRUE)
# subsite <- merge(subsite,site_lookup[,c("data_source","number","latitude","longitude")],by.x = c("data_source","SiteNumber"), by.y = c("data_source","number"), all.x = TRUE)
# subsite <- rename(subsite, sitevar_1 = SiteNumber)
# subsite$sitevar_2 <- NA
# write.csv(subsite,"kfm/data/raw_index/lookups/kfm_sites.csv", row.names = FALSE)
# 
# #scrape transect
# transect_key <- c("data_source","site_id","subsite_id")
# transect <- list()
# for (ii in 1:length(dataset)){
#   transect <- rbind(transect, data[[ii]][,transect_key])
# }
# transect <- unique(transect)
# transect <- transect[with(transect, order(site_id)), ]
# transect$transect_id <- factor(sprintf("c-k-%05d",1:dim(transect)[1]))
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
# write.csv(transect,"kfm/data/raw_index/lookups/kfm_transects.csv", row.names = FALSE)

# #scrape taxa
# taxa_key <- c("data_source","sample_method","scientific_name","lifestage","common_name","native_taxa_code")
# taxa <- list()
# for (ii in 1:length(dataset)){
#   taxa <- rbind(taxa, data[[ii]][,taxa_key])
# }
# #assign unique id
# taxa <- unique(taxa)
# taxa$taxa_id <- factor(sprintf("t-k-%03d",1:dim(taxa)[1]))
# #mege id with data
# for (ii in 1:length(dataset)){
#   data[[ii]] <- merge(data[[ii]],taxa,by = taxa_key)
# }
# #format
# taxa$lifestage[taxa$lifestage == ""] <- NA
# taxa$scientific_name[taxa$scientific_name == ""] <- NA
# taxa$common_name[taxa$common_name == ""] <- NA
# taxa <- taxa[with(taxa, order(scientific_name,native_taxa_code)), ]
# #subset
# taxa <- taxa[,c("taxa_id","data_source","sample_method","native_taxa_code","scientific_name","lifestage","common_name")]
# write.csv(taxa,"kfm/data/raw_index/lookups/kfm_taxa.csv", row.names = FALSE,na="")


#check for if there is more taxa in the new set of data
# taxa<-read.csv("kfm/data/raw_index/lookups/kfm_taxa.csv",na="")
# 
# extra<-data.frame()
# for (ii in 1:length(dataset)){
#   peace <- data[[ii]]
#   peace1<-peace %>%
#     mutate(native_taxa_code=as.factor(native_taxa_code)) %>%
#     left_join(select(taxa,taxa_id,sample_method,native_taxa_code,scientific_name),by=c("sample_method","scientific_name","native_taxa_code")) %>%
#     filter(is.na(taxa_id))%>%
#     select(data_source,sample_method,native_taxa_code,scientific_name,lifestage,common_name) %>%
#     distinct()
#   extra<-rbind(extra,peace1)
# }
# 
# id<-taxa$taxa_id
# max_num<-as.integer(max(substr(id,5,7)))
# extra$taxa_id <- factor(sprintf("t-k-%03d",(max_num+1):(max_num+dim(extra)[1])))
# 
# taxa_final<-rbind(taxa,extra)
# 
# write.csv(taxa_final,"kfm/data/raw_index/lookups/kfm_taxa.csv",row.names = F,na="")

#We have existing site information, merge with current data

site_key <- c("data_source","SiteNumber")


subsite <- read.csv("kfm/data/raw_index/lookups/kfm_sites.csv",na="") %>%
  rename(SiteNumber=sitevar_1) %>%
  select(data_source,SiteNumber,site_id,subsite_id)

transect<-read.csv("kfm/data/raw_index/lookups/kfm_transects.csv",na="") %>%
  select(site_id,transect_id)%>%
  distinct() %>%
  left_join(subsite,by="site_id")

taxa<-read.csv("kfm/data/raw_index/lookups/kfm_taxa.csv",na="") %>%
  select(taxa_id,sample_method,native_taxa_code,scientific_name)

# taxa_new2<-taxa_new %>%
#   select(taxa_id,sample_method,native_taxa_code,scientific_name)%>%
#   right_join(taxa,by=c("sample_method","native_taxa_code","scientific_name"))

for (ii in 1:length(dataset)){
   peace <- data[[ii]] 
   peace1<-peace %>%
     mutate(native_taxa_code=as.factor(native_taxa_code)) %>%
     left_join(transect,by=c("data_source","SiteNumber")) %>%
     left_join(taxa,by=c("sample_method","scientific_name","native_taxa_code"))
   
   #filter out non-matching ones
   # 
     kk<-peace1 %>%
       filter(is.na(site_id)|is.na(subsite_id)|is.na(transect_id)|is.na(taxa_id))
     print(kk)
     print(paste0("original data is ",nrow(peace), " and the merged data is ",nrow(peace1)))
    
   writepath <- paste0("kfm/data/raw_index/kfm_",dataset[ii],"_index.csv")
   write.csv(peace1,writepath, row.names = FALSE,na="")
}


# 
# #write to files
# for (ii in 1:length(dataset)){
#   writepath <- paste0("kfm/data/raw_index/kfm_",dataset[ii],"_index.csv")
#   write.csv(data[[ii]],writepath, row.names = FALSE)
# }