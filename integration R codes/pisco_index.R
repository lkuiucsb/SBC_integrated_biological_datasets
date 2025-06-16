rm(list = ls())

#index prefix key:
#s - site
#t - taxa
#a - survey
#b - transect
#c - replicate

dataset <- c(
  "fish",
  "quad",
  "swath",
  "upc"
)

taxa_lookup <- read.csv("pisco/data/raw_in/lookups/PISCO_kelpforest_taxon_table.1.2.csv",stringsAsFactors = F,na=NA)
taxa_lookup <- rename(taxa_lookup, sample_method = sample_type,native_taxa_code = classcode, scientific_name=species_definition)
taxa_lookup$sample_method <- tolower(taxa_lookup$sample_method)
#names(taxa_lookup)[9] <-"common_name" (issue fixed in 2019 version)
taxa_lookup <- unique(taxa_lookup[taxa_lookup$campus == "UCSB",c("native_taxa_code","sample_method","scientific_name","common_name")])

#consult with Katie Davis and she said the OCTOPUS can be assigned a genus name "OCTOPUS" in 2016 data (issue fixed in 2019)
#taxa_lookup[which(taxa_lookup$native_taxa_code== "OCTOPUS"),]$genus <- "Octopus"

#taxa in 2023 has changed format, so we need to reformat it. 
taxa_new <- read.csv("pisco/data/raw_in/lookups/PISCO_taxon_lookup_table.csv",stringsAsFactors = F,na=c(NA,""))%>%
  rename_all(tolower)%>%
  rename(sample_method = sample_type,native_taxa_code = pisco_classcode) %>%
  mutate(scientific_name=paste(genus, species),sample_method=tolower(sample_method))%>%
  select(sample_method,native_taxa_code,scientific_name,common_name)


data <- list()
#read in files
for (ii in 1:length(dataset)){
  readpath <- paste0("pisco/data/raw_pass/pisco_",dataset[ii],"_pass.csv")
  data[[ii]] <- read.csv(readpath,header = TRUE,na="")
}

#assign replicate_id's
replicate_key <- list(
  "transect",
  c("transect","quad"),
  "transect",
  "transect"
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
# ########################################
# site_key <- c("data_source","site")
# subsite_key <- c(site_key, "side")
# subsite <- list()
# for (ii in 1:length(dataset)){
#   subsite <- rbind(subsite, data[[ii]][,subsite_key])
# }
# subsite <- subsite %>%
#   distinct() %>%
#   arrange(across(all_of(subsite_key)))
# 
# site <- subsite %>%
#   select(all_of(site_key)) %>%
#   distinct()
# 
# site$site_id <- factor(sprintf("a-p-%02d",1:dim(site)[1]))
# subsite <- inner_join(subsite,site, by = site_key)
# subsite$subsite_id <- factor(sprintf("b-p-%03d",1:dim(subsite)[1]))
# for (ii in 1:length(dataset)){
#   data[[ii]] <- merge(data[[ii]],subsite,by = subsite_key)
# }
# #load and merge latlons
# site_lookup <- read.csv("pisco/data/raw_in/lookups/PISCO_site_lookup_table.csv",header = TRUE) %>%
#   #change all column names to lower case
#   rename_all(tolower) %>%
#   rename(site_com=site,latitude=lat_wgs84,longitude=lon_wgs84) %>%
#   select(site_com,latitude,longitude)%>%
#   group_by(site_com) %>%
#   summarise_all(mean,na.rm = TRUE) %>%
#   ungroup() 
# 
# subsite1 <- subsite %>%
#   mutate(site_com = paste0(site,"_",side)) %>%
#   left_join(site_lookup,by = c("site_com"="site_com")) %>%
#   mutate(site_com = ifelse(is.na(latitude),gsub("_CEN","",site_com),site_com)) %>% # some sites the lat and long don't associate with the subside
#   left_join(site_lookup,by = c("site_com"="site_com")) %>% # so we remove the _CEN and try again
#   select(-site_com,-latitude.x,-longitude.x) %>%
#   rename(latitude = latitude.y,longitude = longitude.y) %>%
#   rename(sitevar_1 = site,sitevar_2 = side)
# 
# subsite <- subsite1 
#  write.csv(subsite,"pisco/data/raw_index/lookups/pisco_sites.csv", row.names = FALSE)
# # 
# # #scrape transect
# transect_key <- c("data_source","site_id","subsite_id","zone")
# transect <- list()
# for (ii in 1:length(dataset)){
#   transect <- rbind(transect, data[[ii]][,transect_key])
# }
# transect <- unique(transect)
# transect <- transect[with(transect, order(site_id)), ]
# transect$transect_id <- factor(sprintf("c-p-%05d",1:dim(transect)[1]))
# for (ii in 1:length(dataset)){
#   data[[ii]] <- merge(data[[ii]],transect,by = c(transect_key))
# }
# tr_read_key <- c(transect_key,"sample_method","date","transect_id","zone")
# transect <- list()
# for (ii in 1:length(dataset)){
#   transect <- rbind(transect, data[[ii]][,tr_read_key])
# }
# transect <- unique(transect)
# transect <- transect[with(transect, order(site_id,sample_method)), ]
# write.csv(transect,"pisco/data/raw_index/lookups/pisco_transects.csv", row.names = FALSE)
# # 
# # #scrape taxa
# # taxa_key <- c("data_source","sample_method","native_taxa_code")
# # taxa <- list()
# # for (ii in 1:length(dataset)){
# #   taxa <- rbind(taxa, data[[ii]][,taxa_key])
# # }
# # # #assign unique id
# # taxa <- unique(taxa)
# # taxa$taxa_id <- factor(sprintf("t-p-%03d",1:dim(taxa)[1]))
# # #mege id with data
# # for (ii in 1:length(dataset)){
# #   data[[ii]] <- merge(data[[ii]],taxa[,c(taxa_key,"taxa_id")],by = taxa_key)
# # }
# # #format
# # taxa <- merge(taxa,taxa_lookup, by = c("native_taxa_code","sample_method"), all.x = TRUE)
# # 
# # taxa$lifestage=NA
# # #taxa <- rename(taxa, lifestage = notes)
# # #taxa$species[taxa$species == "spp" | taxa$species == ""] <- NA
# # #taxa$scientific_name <- as.character(taxa$genus)
# # #taxa$scientific_name[!is.na(taxa$species)] <- paste(taxa$genus[!is.na(taxa$species)], taxa$species[!is.na(taxa$species)], sep = " ")
# # #taxa$lifestage[taxa$lifestage == ""] <- NA
# # #taxa$scientific_name[taxa$scientific_name == ""] <- NA
# # #taxa$common_name[taxa$common_name == ""] <- NA
# # #taxa <- taxa[with(taxa, order(scientific_name,lifestage,native_taxa_code)), ]
# # #subset
# # taxa <- taxa[,c("taxa_id","data_source","sample_method","native_taxa_code","scientific_name","lifestage","common_name")]
# # # write.csv(taxa,"pisco/data/raw_index/lookups/pisco_taxa.csv", row.names = FALSE,na="")
# 
# 
# #-------------------------------------
# #-------------------------------------
# #if add more years in the data, can skip "scrape site, subsite, transect, and taxa" sections above and start checking the extra species. 
# #check if there are more taxa in the new set of data and create a taxa table for merging
# #------------------------------------------
# # 
# taxa<-read.csv("pisco/data/raw_index/lookups/pisco_taxa_old.csv")
# 
# extra<-data.frame()
# for (ii in 1:length(dataset)){
#   peace <- data[[ii]]
#   peace1<-peace %>%
#     mutate(native_taxa_code=as.factor(native_taxa_code)) %>%
#     left_join(select(taxa,taxa_id,sample_method,native_taxa_code,scientific_name,lifestage),by=c("sample_method","native_taxa_code")) %>%
#     filter(is.na(taxa_id)) %>%
#     select(data_source,sample_method,native_taxa_code,scientific_name,lifestage) %>%
#     distinct()
#   extra<-rbind(extra,peace1)
# }
# 
# id<-taxa$taxa_id
# max_num<-as.integer(max(substr(id,5,7)))
# extra$taxa_id <- factor(sprintf("t-p-%03d",(max_num+1):(max_num+dim(extra)[1])))
# # 
#  extra1 <-extra%>%
#    select(-scientific_name,-lifestage) %>%
#    left_join(taxa_new, by = c("native_taxa_code","sample_method")) %>%
#    mutate(lifestage=ifelse(str_detect(common_name,"juvenile"),"juvenile",NA),
#           lifestage = ifelse(str_detect(common_name,"adult"),"adult",lifestage))
#    
#  # extra$species[extra$species == "spp" | extra$species == ""] <- NA
#  # extra$scientific_name <- as.character(extra$genus)
#  # extra$scientific_name[!is.na(extra$species)] <- paste(extra$genus[!is.na(extra$species)], extra$species[!is.na(extra$species)], sep = " ")
#  # extra$common_name[extra$common_name == ""] <- NA
#  extra1 <- extra1[,c("taxa_id","data_source","sample_method","native_taxa_code","scientific_name","lifestage","common_name")]
# 
# taxa_final<-bind_rows(taxa,extra1)
# # 
#  write.csv(taxa_final,"pisco/data/raw_index/lookups/pisco_taxa.csv",row.names = F)


###################
#one-time code
## # in 2019, we examined the site and it hasn't been changed, but more lat and long were available. So we merge the site index table with the new lat and long info
# lonlat<-read.csv("pisco/data/raw_in/lookups/PISCO_kelpforest_site_table.1.2.csv") %>%
#    filter(campus=="UCSB")%>%
#   distinct(site,latitude,longitude)
#   
# siteold <- read.csv("pisco/data/raw_index/lookups/pisco_sites.csv",na="") %>%
#    rename(sitecode=sitevar_1,side=sitevar_2) %>%
#    select(data_source,sitecode,side,site_id,subsite_id) 
# 
# # read in conversion table
# sitenew <-read.csv("pisco/data/raw_in/lookups/generated_site_side.csv",na="") %>%
#   right_join(siteold,by=c("sitecode","side"))%>%
#   left_join(lonlat,by="site") %>%
#   rename(sitevar_1=sitecode,sitevar_2=side)%>%
#   select(data_source,sitevar_1,sitevar_2,site_id,subsite_id,latitude,longitude)
# 
# write.csv(sitenew,"pisco/data/raw_index/lookups/pisco_sites.csv",row.names = F,na="")
##############################


#We have existing site information, merge with current data
 
subsite <- read.csv("pisco/data/raw_index/lookups/pisco_sites.csv",na="") %>%
  rename(site=sitevar_1,side=sitevar_2) %>%
  select(data_source,site,side,site_id,subsite_id)

transect<-read.csv("pisco/data/raw_index/lookups/pisco_transects.csv",na="") %>%
  select(subsite_id,zone,transect_id)%>%
  distinct() %>%
  left_join(subsite,by="subsite_id")

taxa<-read.csv("pisco/data/raw_index/lookups/pisco_taxa.csv",na="") %>%
  select(taxa_id,sample_method,native_taxa_code,scientific_name)

for (ii in 1:length(dataset)){
  peace <- data[[ii]] 
  peace1<-peace %>%
    mutate(native_taxa_code=as.factor(native_taxa_code)) %>%
    left_join(transect,by=c("data_source","site","side","zone")) %>%
    left_join(taxa,by=c("sample_method","native_taxa_code"))
  
  #filter out non-matching ones

  kk<-peace1 %>%
    filter(is.na(site_id)|is.na(subsite_id)|is.na(transect_id)|is.na(taxa_id))
    print(kk)
    print(paste0("original data is ",nrow(peace), " and the merged data is ",nrow(peace1)))
    
  writepath <- paste0("pisco/data/raw_index/pisco_",dataset[ii],"_index.csv")
  write.csv(peace1,writepath, row.names = FALSE,na="")
}

# 
# 
# #write to files
# for (ii in 1:length(dataset)){
#   writepath <- paste0("pisco/data/raw_index/pisco_",dataset[ii],"_index.csv")
#   write.csv(data[[ii]],writepath, row.names = FALSE,na="")
# }