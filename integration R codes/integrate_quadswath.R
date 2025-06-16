rm(list = ls())

######## Define and merge data ########

# c(data_source, dataset, unique_method, zero_fill)
  # unique_method:
    #drop - drops all indistinct records
    #first - uses first (arbetrary) record
    #sum - sums indistinct records accross count field
  # zero_fill -- none, closed, open
    #none - does not zero fill (use if already zero filled)
    #open - zero fills accross all transects with at least one record on given date, for all taxa found over all dates
qs_sets <- list(
  c("kfm","1mquad","drop","none"), # the dataset was not zero filled and we don't zero filled because some species were moved between survey methods. 
  c("kfm","5mquad","drop","none"),
  c("kfm","band","drop","none"),
  c("lter","quad","drop","none"),
  c("lter","swath","drop","none"),
  c("pisco","quad","drop","open"),
  c("pisco","swath","sum","open"),
  c("sni","swath","drop","none"),
  c("sni","quad","drop","none")
)

id_vars <- c("data_source","sample_method","date","site_id","subsite_id","transect_id","replicate_id","taxa_id")
sample_vars <- c("area")
measure_vars <- c("count")


all <- data.frame()
na_review <- data.frame()
drop_review <- data.frame()
for (ii in 1:length(qs_sets)){
  readpath <- paste0(qs_sets[[ii]][1],"/data/raw_index/",qs_sets[[ii]][1],"_",qs_sets[[ii]][2],"_index.csv")
  data <-  read.csv(readpath,header = TRUE,stringsAsFactors = F,na="")
  data <- data[,c(id_vars, sample_vars, measure_vars)]
  data$date <- as.Date(data$date)
  
  #unique records
  ##Ensure unique observations (either drop or sum nonunique)
  if (qs_sets[[ii]][3] == "drop"){
    #Drop conflicting records
    duplicates <- duplicated(data[,id_vars],incomparables = FALSE)
    unique(duplicates)
    drop_review <- rbind(drop_review,data[duplicates,])
    data_unique <- data[!duplicates,]
  } else if (qs_sets[[ii]][3] == "sum"){
    data_unique <- data %>%
      group_by(across(all_of(id_vars))) %>%
      summarise(area = head(area,1), count = sum(count),.groups='drop') %>%
      ungroup() 

  }   

  #zero_fill
  if (qs_sets[[ii]][4] == "none"){
    zf <- data_unique
  } else {
    #"open"
    zf <- data_unique  %>%
      mutate(count=as.numeric(count), count=if_else(is.na(count),9999999,count) ) %>% # incase there are na for the count. 
      dcast(data_source+sample_method+date+site_id+subsite_id+transect_id+replicate_id+area~taxa_id,value.var="count",fill=0) %>%
      melt(id.vars=c("data_source","sample_method","date","site_id","subsite_id","transect_id","replicate_id","area"),variable.name="taxa_id",value.name="count" ) %>%
      mutate(count=if_else(count==9999999,as.numeric(NA),count) )
  }

  #push NA's to review
  na_review <- rbind(na_review,zf[is.na(zf$count),])
  zf <- zf[,c(id_vars,sample_vars,measure_vars)]
  
  all <- bind_rows(all,zf)
  
}
#write to file

write.csv(all,"integrated/data/community_structure/quadswath_integrated.csv", row.names = FALSE, na = "", quote=F)

write.table(drop_review,"integrated/data/community_structure/reviews/quadswath_drop_review.csv", sep = ",", row.names = FALSE, na = "")
write.table(na_review,"integrated/data/community_structure/reviews/quadswath_na_review.csv", sep = ",", row.names = FALSE, na = "")

#############################

