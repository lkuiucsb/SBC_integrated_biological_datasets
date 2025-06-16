rm(list = ls())

#merge taxa with WoRMS query results
taxa1 <- read.csv("integrated/data/community_structure/lookups/components/taxa_manipulated.csv",header = TRUE, na.strings = "",stringsAsFactors=FALSE)
taxa1 <- rename(taxa1, query = scientific_name)

worms <- read.csv("integrated/data/community_structure/lookups/components/taxa_worms.csv",header = TRUE, na.strings = "",stringsAsFactors=FALSE)

taxa1 <- merge(taxa1,worms,by = "query",all.x = TRUE)
taxa1 <- taxa1[,c("taxa_id","data_source","sample_method","unit_type","native_taxa_code","query","lifestage","common_name","kingdom","phylum","class","order","family","genus","scientific_name","rank","status","aphia_id")]
taxa1$status[is.na(taxa1$status)] <- "not_queried"
taxa1$taxa_dictionary<-"WORMS"
taxa1$taxa_dictionary[is.na(taxa1$aphia_id)]<-NA

taxa2<-taxa1 %>%
  select(taxa_id,data_source,sample_method,unit_type,native_taxa_code,lifestage,common_name,query,status,aphia_id,taxa_dictionary,kingdom,phylum,class,order,family,genus,scientific_name)%>%
  mutate(common_name=if_else(is.na(scientific_name)&is.na(common_name),as.character(query),common_name))

write.csv(taxa2,"integrated/data/community_structure/lookups/taxa_integrated.csv", row.names = FALSE,na="")
