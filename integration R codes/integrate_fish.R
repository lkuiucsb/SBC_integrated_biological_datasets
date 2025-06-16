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
fish_sets <- list(
  c("kfm","rdfc","drop","open"), #no duplicated records. so it doesn't matter whether is drop or sum
  c("kfm","visualfish","drop","none"),
  c("lter","fish","sum","none"), #althought it is open for LTER. the 2018 updated fish file already zero filled. 
  c("lter","crypticfish","sum","none"),
  c("pisco","fish","sum","open"), #pisco each of the fish count might appear in separated rows. Should sum them all up. 
  c("sni","fish","drop","none")
)
#select lifestage for output
countcol = "all"

id_vars <- c("data_source","sample_method","sample_subtype","date","site_id","subsite_id","transect_id","replicate_id","taxa_id")
sample_vars <- c("area","height")
measure_vars <- c("count")

all <- data.frame()
na_review <- data.frame()
drop_review <- data.frame()

for (ii in 1:length(fish_sets)){
  readpath <- paste0(fish_sets[[ii]][1],"/data/raw_index/",fish_sets[[ii]][1],"_",fish_sets[[ii]][2],"_index.csv")
  data <-  read.csv(readpath,header = TRUE,stringsAsFactors = F, na="")
  data$count <- data[,countcol]
  data <- data[,c(id_vars, sample_vars, measure_vars)]
  data$date <- as.Date(data$date)
  
  #unique records
  ##Ensure unique observations (either drop or sum nonunique)
  if (fish_sets[[ii]][3] == "drop"){
    #Drop conflicting records
    duplicates <- duplicated(data[,id_vars],incomparables = FALSE)
    #unique(duplicates)
    drop_review <- rbind(drop_review,data[duplicates,])
    data_unique <- data[!duplicates,]
  } else if (fish_sets[[ii]][3] == "sum"){
    data_unique <- data %>%
      group_by(across(all_of(id_vars))) %>%
      summarise(area = head(area,1), height = head(height,1), count = sum(count),.groups='drop') %>%
      ungroup()
  }
  
  #zero_fill
  if (fish_sets[[ii]][4] == "none"){
    zf <- data_unique
  } else {
    #"open"
    zf <- data_unique  %>%
      mutate(count=as.numeric(count), count=if_else(is.na(count),9999999,count) ) %>% # incase there are na for the count. 
      dcast(data_source+sample_method+sample_subtype+date+site_id+subsite_id+transect_id+replicate_id+area+height~taxa_id,value.var="count",fill=0) %>%
      melt(id.vars=c("data_source","sample_method","sample_subtype","date","site_id","subsite_id","transect_id","replicate_id","area","height"),variable.name="taxa_id",value.name="count" ) %>%
      mutate(count=if_else(count==9999999,as.numeric(NA),count) )
  }
    
   #push NA's to review
  na_review <- rbind(na_review,zf[is.na(zf$count),])
  
  zf <- zf[,c(id_vars,sample_vars,measure_vars)]

  all <- bind_rows(all,zf)
  
}

#write to file

write.csv(all,paste0("integrated/data/community_structure/fish_",countcol,"_integrated.csv"), row.names = FALSE, na = "", quote=F)

write.table(drop_review,paste0("integrated/data/community_structure/reviews/fish_",countcol,"_drop_review.csv"), sep = ",", row.names = FALSE, na = "")
write.table(na_review,paste0("integrated/data/community_structure/reviews/fish_",countcol,"_na_review.csv"), sep = ",", row.names = FALSE, na = "")

##------------------------
# fish_all <- read.csv("integrated/data/community_structure/data_2018/fish_all_integrated_201801.csv")
# fish_adult <- read.csv("integrated/data/community_structure/fish_adult_integrated.csv")
# fish_juvenile <- read.csv("integrated/data/community_structure/fish_juvenile_integrated.csv")
#   
# adult<- fish_adult[,"count"] 
# juvenile <- fish_juvenile[,"count"]
# 
# fish_comb <- cbind(fish_all,cbind(adult,juvenile)) %>%
#   rename (all= count)
# 
# write.csv(fish_comb,"integrated/data/community_structure/fish_comb_integrated.csv", row.names = FALSE)


#old <- read.csv("integrated/data/community_structure/fish_all_integrated.csv")

peace <- old %>%
  filter(data_source == "kfm"&sample_method=="rdfc") %>%
  filter(is.na(count))

