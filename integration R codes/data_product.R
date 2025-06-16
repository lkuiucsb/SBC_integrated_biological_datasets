rm(list = ls())

library(reshape2)
library(dplyr)
library(ggplot2)


#---------------------------
taxa <- read.csv("integrated/data/community_structure/lookups/taxa_integrated.csv",stringsAsFactors = FALSE)

site <- read.csv("integrated/data/community_structure/lookups/sites_integrated_subsite.csv") %>%
  select(-data_source)

fish <- read.csv("integrated/data/community_structure/fish_all_integrated.csv",stringsAsFactors = F,na="")

cover <- read.csv("integrated/data/community_structure/cover_integrated.csv",stringsAsFactors = F,na="")

quad <- read.csv("integrated/data/community_structure/quadswath_integrated.csv",stringsAsFactors = F,na="")


#------------------------
#fish

taxa$query[is.na(taxa$query)]<-taxa$common_name[is.na(taxa$query)]

taxa$scientific_name[is.na(taxa$aphia_id)]<-taxa$query[is.na(taxa$aphia_id)]

taxa1<-taxa %>%
  select(taxa_id,aphia_id,taxa_dictionary,scientific_name)

fish1 <- fish %>%
  left_join(taxa1,by = "taxa_id") 

cover1 <- cover %>%
  left_join(taxa1,by = "taxa_id") 

quad1 <- quad %>%
  left_join(taxa1,by = "taxa_id") 


fish2 <- fish1 %>% left_join(site,by = c("site_id","subsite_id")) 

cover2 <- cover1 %>% left_join(site,by = c("site_id","subsite_id"))

quad2 <- quad1 %>% left_join(site,by = c("site_id","subsite_id"))

fish3<-fish2 %>%
  rename(proj_taxon_id=taxa_id,auth_taxon_id=aphia_id,auth_name=taxa_dictionary,taxon_name=scientific_name,site_name=sitevar_1,subsite_name=sitevar_2) 
  

cover3<-cover2 %>%
  rename(proj_taxon_id=taxa_id,auth_taxon_id=aphia_id,auth_name=taxa_dictionary,taxon_name=scientific_name,site_name=sitevar_1,subsite_name=sitevar_2)

quad3<-quad2 %>%
  rename(proj_taxon_id=taxa_id,auth_taxon_id=aphia_id,auth_name=taxa_dictionary,taxon_name=scientific_name,site_name=sitevar_1,subsite_name=sitevar_2)

taxa3<-taxa %>%
  rename(proj_taxon_id=taxa_id,auth_taxon_id=aphia_id,auth_name=taxa_dictionary,taxon_name=scientific_name,native_taxon_code=native_taxa_code)%>%
  dplyr::select(-query,-status)

  
  
write.csv(fish3,"integrated/data/community_structure/data_products/fish_all_integrated_20231022.csv",row.names = F,na=".")
write.csv(cover3,"integrated/data/community_structure/data_products/cover_integrated_20231022.csv",row.names = F,na=".")
write.csv(quad3,"integrated/data/community_structure/data_products/quadswath_integrated_20231022.csv",row.names = F,na=".")
write.csv(taxa3,"integrated/data/community_structure/data_products/taxa_integrated_20231022.csv",row.names = F,na=".")


