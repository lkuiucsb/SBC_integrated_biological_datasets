##Configure environment
rm(list = ls())

#merge taxa with WoRMS query results
taxa <- read.csv("integrated/data/community_structure/lookups/components/taxa_raw.csv",header = TRUE, na.strings = "",stringsAsFactors=FALSE)
# taxa$scientific_name <- as.character (taxa$scientific_name)
# taxa$native_taxa_code<- as.character (taxa$native_taxa_code)
# taxa$lifestage <- as.character (taxa$lifestage)
# taxa$common_name <- as.character (taxa$common_name)

#trim leading and trailing whitespace
taxa$scientific_name <- gsub("^\\s+|\\s+$", "", taxa$scientific_name)

#remove repeated spaces
taxa$scientific_name <- gsub("\\s+", " ", taxa$scientific_name)


# #comment out the ones that have been fixed. 
# taxa[taxa$scientific_name == "Laminaria spp juvenile",c("scientific_name","lifestage","common_name")] <- c("Laminaria","juvenile < 10cm width","Laminaria spp.")
# 
# taxa[taxa$scientific_name == "Semicossyphus pulcher (f)" ,"scientific_name"] <- "Semicossyphus pulcher"
# taxa[taxa$scientific_name == "Semicossyphus pulcher (m)" , "scientific_name"] <- "Semicossyphus pulcher"
# 
# taxa[taxa$scientific_name == "Hexagrammos decagrammus (m)" ,"scientific_name"] <- "Hexagrammos decagrammus"
# taxa[taxa$scientific_name == "Hexagrammos decagrammus (f)", "scientific_name"] <- "Hexagrammos decagrammus"
# 
# taxa[taxa$scientific_name == "Cystoseira osmundacea juvenile","scientific_name"] <- "Cystoseira osmundacea"
# taxa[taxa$scientific_name == "Eisenia arborea juvenile","scientific_name"] <- "Eisenia arborea"
# taxa[taxa$scientific_name == "Macrocystis pyrifera juvenile","scientific_name"] <- "Macrocystis pyrifera"
# taxa[taxa$scientific_name == "Macrocystis <1m","scientific_name"] <- "Macrocystis pyrifera"
# taxa[taxa$scientific_name == "Laminaria setchellii juvenile","scientific_name"] <- "Laminaria setchellii"
# taxa[taxa$scientific_name == "Leptasterias hexactis recruit","scientific_name"] <- "Leptasterias hexactis"
# taxa[taxa$scientific_name == "Patiria miniata recruit","scientific_name"] <- "Patiria miniata"
# taxa[taxa$scientific_name == "Pisaster spp recruit","scientific_name"] <-"Pisaster"
# taxa[taxa$scientific_name == "Pterygophora californica juvenile","scientific_name"] <- "Pterygophora californica"
# taxa[taxa$scientific_name == "Strongylocentrotus franciscanus recruit","scientific_name"] <- "Strongylocentrotus franciscanus"
# taxa[taxa$scientific_name == "Strongylocentrotus purpuratus recruit","scientific_name"] <- "Strongylocentrotus purpuratus"
# taxa[taxa$scientific_name == "Acanthacora cyanocrypta","scientific_name"] <- "Acanthancora cyanocrypta"
# 
# taxa[taxa$scientific_name == "Mola Mola","scientific_name"] <- "Mola mola"
# taxa[taxa$scientific_name == "Mytilus Californianus","scientific_name"] <- "Mytilus californianus"
# taxa[taxa$scientific_name == "Phanerodon spp.","scientific_name"] <- "Phanerodon"
# taxa[taxa$scientific_name == "Pholad Unidentified","scientific_name"] <- "Pholadidae"
# taxa[taxa$scientific_name == "Syngnathus spp.","scientific_name"] <- "Syngnathus"
# 
# 
# taxa[taxa$data_source == "sni" & taxa$sample_method == "swath" & taxa$native_taxa_code == "589","scientific_name"] <- "Macrocystis pyrifera"
# taxa[taxa$data_source == "sni" & taxa$sample_method == "rpc" & taxa$native_taxa_code == "589","scientific_name"] <- "Macrocystis pyrifera"

taxa_m <-taxa %>%
  mutate(scientific_name=gsub(" juvenile","",scientific_name),
         scientific_name=gsub(" recruit","",scientific_name),
         scientific_name=gsub(" spp.","",scientific_name),
         scientific_name=gsub(" spp","",scientific_name),
         scientific_name=gsub(" \\(f)","",scientific_name),
         scientific_name=gsub(" \\(m)","",scientific_name),
         scientific_name=gsub("Mola Mola","Mola mola",scientific_name),
         scientific_name=gsub("Mytilus Californianus","Mytilus californianus",scientific_name),
         scientific_name=gsub(" <1m","",scientific_name),
         scientific_name=gsub("Acanthacora cyanocrypta","Acanthancora cyanocrypta",scientific_name),
         scientific_name=gsub("Unidentified","",scientific_name),
         scientific_name=gsub(" >1m","",scientific_name),
         # everything below was changed in Thomas's suggestion file ("integrated/data/community_structure/lookups/components/Uniden_alternat_Thomas.csv")
         scientific_name=gsub("Dictyoneuropsis reticulata/Agarum fimbriatum","Costariaceae",scientific_name),
         scientific_name=gsub("Macrocystis, Pterygophora, and Eisenia combined","Phaeophyceae",scientific_name),
         scientific_name=gsub("Parastichopus parvimensis","Parastichopus parvimensis",scientific_name),
         scientific_name=gsub("Sebastes atrovirens/carnatus/caurinus/chrysomelas","Sebastes",scientific_name),
         scientific_name=gsub("Sebastes chrysomelas/carnatus","Sebastes",scientific_name),
         scientific_name=gsub("Sebastes serranoides/flavidus","Sebastes",scientific_name),
         scientific_name=gsub("_"," ",scientific_name),
         # This is only the kfm RDFC survey and names are corrected in DAN/SHANNON's list
         scientific_name=gsub("Artedius creaseri","Ruscarius creaseri",scientific_name),
         scientific_name=gsub("Coryphopterus nicholsi","Rhinogobiops nicholsii",scientific_name),
         scientific_name=gsub("baitfish, unidentified","Engraulidae",scientific_name),
         scientific_name=gsub("larval fish","Actinopteri",scientific_name),
         scientific_name=gsub("Gobiesox","Gobiesox maeandricus",scientific_name)
         ) 
    
# 
#Li updated Thomas's list and shannon/Dan's list to have all the species name change information in one file. . 
#most of lter names have been cleared during the data recompiling process. 
revi <- read.csv("integrated/data/community_structure/lookups/components/species_name_change.csv",stringsAsFactors = F, na="")  
  
#IMPORT the additiona unit type table
unittype<- read.csv("integrated/data/community_structure/lookups/components/unit_type_addition.csv",stringsAsFactors = F, na="")  

taxa_revi <-taxa_m %>%
  left_join(revi,by=c("data_source","sample_method","native_taxa_code")) %>%
  mutate(scientific_name=if_else(!is.na(name_sug),name_sug,scientific_name)) %>%
  select(taxa_id,data_source,sample_method,native_taxa_code,scientific_name,lifestage,common_name) %>%
  mutate(unit_type = case_when (
    sample_method=="upc"|sample_method=="rpc"~"percent_cover",
    TRUE ~"count"
  ) ) %>%
 left_join(unittype,by=c("data_source","sample_method","native_taxa_code")) %>%
  mutate(unit_type=if_else(!is.na(unit_type_app),unit_type_app,unit_type))


write.csv(taxa_revi,"integrated/data/community_structure/lookups/components/taxa_manipulated.csv", row.names = FALSE,na="")



#######################################
# #Thomas had suggested to revised some of species' scientific name. This piece of the code was originally applied to an earlier version of the dataset (prior to 2018-08). 
# In the later version, the LTER species was all fixed. And the the kfm and pisco was applied as the code above. 
# revi <- read.csv("integrated/data/community_structure/lookups/components/Uniden_alternat_Thomas.csv")
# 
# revi <- revi %>%
#   filter(!(name_sug=="NA"|rank_sug=="CHECK")) %>%
#  select(data_source,sample_method,native_taxa_code,common_name,name_sug)
# 
# taxa_out <- taxa %>%
#   filter(!(data_source %in% revi$data_source & sample_method %in% revi$sample_method & native_taxa_code %in% revi$native_taxa_code & common_name %in% revi$common_name)) 
# 
# taxa_replace <- taxa %>%
#   filter(data_source %in% revi$data_source & sample_method %in% revi$sample_method & native_taxa_code %in% revi$native_taxa_code & common_name %in% revi$common_name) %>%
#   left_join(revi,by= c("data_source", "sample_method", "native_taxa_code", "common_name")) %>%
#   select(-scientific_name) %>%
#   rename(scientific_name=name_sug) %>%
#   rbind(taxa_out)


# # Dan and Kevin worked on checking the species list again and make changes 
# revi2 <- read.csv("integrated/data/community_structure/lookups/components/taxa_check.csv",na.strings = "",stringsAsFactors=FALSE)
# 
# revi21<- revi2 %>%
#    select(taxa_id,data_source,sample_method,native_taxa_code,query,query_correction,unit_type) %>%
#    right_join(taxa_replace,by=c("taxa_id","data_source","sample_method","native_taxa_code"))
# 
# revi22<-revi21 %>%
#   filter(is.na(query_correction)) %>%
#   mutate(query_correction=scientific_name)
# 
# revi23<-revi21 %>%
#   filter(!is.na(query_correction)) %>%
#   rbind(revi22)%>%
#   select(-scientific_name) %>%
#   rename(scientific_name=query_correction)
# 

# PISCO has a superlayer and we like to distinct the code




