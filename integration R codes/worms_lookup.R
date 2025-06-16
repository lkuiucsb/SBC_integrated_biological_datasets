##Configure environment
rm(list = ls())

#WORMS DATA BASE
#remotes::install_github("ropensci/worrms")
library(worrms)
library(dplyr)
library(stringr)


taxa <- read.csv("integrated/data/community_structure/lookups/components/taxa_manipulated.csv",stringsAsFactors = F)

df1 <- taxa %>% #filter out the non species code
  filter(!(unit_type %in% c("note","relief","substrate")|native_taxa_code=="UNDDET")) %>%
  distinct(scientific_name) 
  

df2 <- data.frame()
#kk<-wm_records_name(name="Hexagrammos lagocephalu")
#wm_record(id=245237)


for (i in 1:nrow(df1)) {
  
  input<-df1$scientific_name[i]
  resu<-data.frame()
  print(paste0(i," ",input))
  
  tryCatch({
  resu<-wm_records_name(name=input)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  if (nrow(resu)==0) { 
    df2 <- bind_rows(df2,data.frame(query=input,
                              valid_AphiaID=NA,
                              valid_name=NA,
                              rank=NA,
                              status=NA,
                              kingdom=NA,
                              phylum=NA,
                              class=NA,
                              order=NA,
                              family=NA,
                              genus=NA)) 
   
  } else {
 resu1<-resu %>% select(valid_AphiaID,valid_name,rank,status,kingdom, phylum,class,order,family,genus)  

  df2<-bind_rows(df2,bind_cols(query=input,resu1[1,]))
  }
  resu<-data.frame()
  
}
df3<-df2

#some corrections
df4<-df3 %>%
  rename(aphia_id=valid_AphiaID,scientific_name=valid_name) %>%
  select(query,status,rank,kingdom,phylum,class,order,family,genus,scientific_name,aphia_id)%>%
  filter(!is.na(query))

write.csv(df4,"integrated/data/community_structure/lookups/components/taxa_worms.csv",row.names = F,quote = F,na="")

