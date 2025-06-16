##Configure environment
rm(list = ls())

#integrate sites
datasource <- c(
  "kfm",
  "lter",
  "pisco",
  "sni"
)

#merge sites
site_key <- c("subsite_id","site_id","data_source","sitevar_1","sitevar_2","latitude","longitude")
sites <- list()
for (ii in 1:length(datasource)){
  readpath <- paste0(datasource[ii],"/data/raw_index/lookups/",datasource[ii],"_sites.csv")
  tray <-  read.csv(readpath,header = TRUE,stringsAsFactors = F,na="" ,strip.white=TRUE)
  sites <- rbind(sites,tray[,site_key])
}

#if at the site level, take average of coordinators across subsites
#sites1 <- sites %>%
#  group_by(site_id,data_source,sitevar_1) %>%
#  summarise(latitude=mean(latitude,na.rm=T),longitude=mean(longitude,na.rm=T)) %>%
#  ungroup()
#sites<-sites1

write.csv(sites,"integrated/data/community_structure/lookups/sites_integrated_subsite.csv", row.names = FALSE,na="")

#merge transects
transect_key <- c("transect_id","data_source","sample_method","date","site_id","subsite_id","zone")
transects <- list()
for (ii in 1:length(datasource)){
  readpath <- paste0(datasource[ii],"/data/raw_index/lookups/",datasource[ii],"_transects.csv")
  tray <-  read.csv(readpath,header = TRUE, strip.white=TRUE,stringsAsFactors = F, na="")
  transects <- rbind(transects,tray[,transect_key])
}
write.csv(transects,"integrated/data/community_structure/lookups/transects_integrated.csv", row.names = FALSE,na="")

#merge taxa
taxa_key <- c("taxa_id","data_source","sample_method","native_taxa_code","scientific_name","lifestage","common_name")
taxa1 <- list()
for (ii in 1:length(datasource)){
  readpath <- paste0(datasource[ii],"/data/raw_index/lookups/",datasource[ii],"_taxa.csv")
  tray <-  read.csv(readpath,header = TRUE,stringsAsFactors = F, na="")
  if (datasource[ii] == "kfm"){
   #properly format kfm native_taxa_code
   tray$native_taxa_code <- as.character(tray$native_taxa_code)
   #tray$native_taxa_code[!is.na(tray$native_taxa_code) & tray$sample_method != "visualfish"] = sprintf("%5.2f",as.numeric(tray$native_taxa_code[!is.na(tray$native_taxa_code) & tray$sample_method != "visualfish"]))
  }
  if (datasource[ii] == "sni"){tray$native_taxa_code = as.character(tray$native_taxa_code)} 
  #factor(sprintf("%04d",tray$native_taxa_code))
  taxa1 <- rbind(taxa1,tray[,taxa_key])

} 
write.csv(taxa1,"integrated/data/community_structure/lookups/components/taxa_raw.csv", row.names = FALSE,na="")
