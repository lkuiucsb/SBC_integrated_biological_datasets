rm(list = ls())

######## Define and merge data ########
library(tidyverse)
# c(data_source, dataset, unique_method, zero_fill)
  # unique_method:
    #drop - drops all indistinct records
    #sum - sums indistinct records accross count field
  # zero_fill -- none, closed, open
    #none - does not zero fill (use if already zero filled)
    #open - zero fills accross all transects with at least one record on given date, for all taxa found over all dates
qs_sets <- list(
  c("kfm","rpc","drop","none"),
  c("lter","upc","drop","none"),
  c("pisco","upc","drop","open"),
  c("sni","rpc","drop","none")  # the pass code already zero-filled the sni cover data. 
)

id_vars <- c("data_source","sample_method","date","site_id","subsite_id","transect_id","replicate_id","taxa_id")
sample_vars <- c("points")
measure_vars <- c("count")


all <- data.frame()
na_review <- data.frame()
drop_review <- data.frame()

for (ii in 1:length(qs_sets)){
  readpath <- paste0(qs_sets[[ii]][1],"/data/raw_index/",qs_sets[[ii]][1],"_",qs_sets[[ii]][2],"_index.csv")
  data <-  read.csv(readpath,header = TRUE,stringsAsFactors = F, na="")
  data <- data[,c(id_vars, sample_vars, measure_vars)]
  data$date <- as.Date(data$date)
  
  #unique records
  ##Ensure unique observations (either drop or sum nonunique)
  if (qs_sets[[ii]][3] == "drop"){
    #Drop conflicting records
    duplicates <- duplicated(data[,id_vars],incomparables = FALSE)
    #unique(duplicates)
    drop_review <- rbind(drop_review,data[duplicates,])
    data_unique <- data[!duplicates,]
  } else if (qs_sets[[ii]][3] == "sum"){
    data_unique <- data %>%
      group_by(across(all_of(id_vars))) %>%
      summarise(count = sum(count),.groups='drop')%>%
      ungroup()
  }
  
  #zero_fill
  if (qs_sets[[ii]][4] == "none"){
    zf <- data_unique
  } else {
    #"open"# only pisco use open, so we edit this by taking into account that pisco's total point could be different at a given location because it was back calculated from species percent cover. 
    #zero fill
    totalpoint <- data_unique  %>%
      group_by(data_source,sample_method,date,site_id,subsite_id,transect_id,replicate_id) %>%
      summarise(totalpoints = min(points),.groups='drop') %>%
      ungroup()

    zf <- data_unique %>%
      select(-points) %>%
      complete(taxa_id,nesting(data_source,sample_method,date,site_id,subsite_id,transect_id,replicate_id),fill=list(count=0)) %>%
      left_join(data_unique,by=c("data_source","sample_method","date","site_id","subsite_id","transect_id","replicate_id","taxa_id","count")) %>%
      left_join(totalpoint,by=c("data_source","sample_method","date","site_id","subsite_id","transect_id","replicate_id")) %>%
      mutate(points=ifelse(is.na(points),totalpoints,points)) %>%
      select(-totalpoints)
     
  }
    
  #push NA's to review
  na_review <- rbind(na_review,zf[is.na(zf$count),])
  
  zf <- zf[,c(id_vars,sample_vars,measure_vars)]
  
  all <- bind_rows(all,zf)
  
}

 #write to file
write.csv(all,"integrated/data/community_structure/cover_integrated.csv", row.names = FALSE, na = "", quote=F)


write.table(drop_review,"integrated/data/community_structure/reviews/cover_drop_review.csv", sep = ",", row.names = FALSE, na = "")
write.table(na_review,"integrated/data/community_structure/reviews/cover_na_review.csv", sep = ",", row.names = FALSE, na = "")

###################
#check the old data

#check old data and new data (2023 version has a lot less records). The conclusion is pisco has clean the data a little.
# there used to be multiple number of total points (point) for each given species, and they have change the percent cover to make it more consistent (so only 1-2 total number points by 1 off, instead of )
# new1 <- zf_new %>%
#   distinct(date,site_id,subsite_id,transect_id,replicate_id,points) %>%
#   mutate(data_new=1)
# old1 <- zf_old %>%
#   distinct(date,site_id,subsite_id,transect_id,replicate_id,points) %>%
#   mutate(data_old=1) %>%
#   full_join(new1,by=c("date","site_id","subsite_id","transect_id","replicate_id","points")) %>%
#   filter(is.na(data_new)) 
#   # mutate(year=substr(date,1,4)) %>%
#   # group_by(year) %>%
#   # summarise(n=n()) 
# 
# new2 <- data %>%
#   distinct(date,site_id,subsite_id,transect_id,replicate_id,points) %>%
#   group_by(date,site_id,subsite_id,transect_id,replicate_id) %>%
#   summarise(n=n()) 

