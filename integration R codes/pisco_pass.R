## Config ##
##Configure environment
rm(list = ls())

na_val = NA

pisco_common <- function(data){
  data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))
  data <- rename(data, native_taxa_code = classcode)
  #after 2019, the site info combine side, we split it back here
  data <-data %>%
    left_join(st,by="site")%>%
    select(-site)%>%
    rename(site=sitecode)
  
  return(data)
}


#read in the site table to parse the site and side info. 
st <-read.csv("pisco/data/raw_in/lookups/generated_site_side.csv", header = TRUE, stringsAsFactors = F)


#fish

data <- read.csv("pisco/data/raw_in/PISCO_FISH.csv", header = TRUE, na.strings = na_val,stringsAsFactors = F)

data$data_source = "pisco"
data$sample_method = "fish"
data <- rename(data,sample_subtype = level)
data$height <- 2
data$area <- 60
data <- rename(data,size = fish_tl)
data <- data[data$method == "SBTL_FISH" & data$campus == "UCSB",]

#st1 <- unique(data$site)

#fish transect has fish survey two times a year in 1 site, we removed the Oct survey (issue fixed in 2019)
# trail <- data %>%
#   filter(year==2011&site=="IV_REEF_E")
# 
# data<-data %>%
#   filter(!(year==2011&month==10&day==4&site=="IV_REEF"&side=="E"&zone=="OUTER"))

#in 2023, some site were change names: SMI_HARRIS_PT_RESERVE_W, we need to change it before putting data through the function
data <- data %>%
  mutate(site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_W","SMI_HARRIS_PT_RESERVE_W",site),
         site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_E","SMI_HARRIS_PT_RESERVE_E",site))


data <- pisco_common(data)

#mutate adult/juvenile/all
data <- data %>%
  mutate(adult = count * as.integer(size >= 10),juvenile = count * as.integer(size < 10)) %>%
  rename(all = count)

write.csv(data,"pisco/data/raw_pass/pisco_fish_pass.csv", row.names = FALSE,na="")

#quad
data <- read.csv("pisco/data/raw_in/PISCO_QUAD.csv", header = TRUE, na.strings = na_val,stringsAsFactors = F)

#in 2023, there is no site_name_old anymore
# data<-data %>% #in 2019 data, the name change in the quad, we changed it back, otherwise there is no lat and lon match it. . 
#   mutate(site=ifelse(!is.na(site_name_old),site_name_old,site))

data$data_source = "pisco"
data$sample_method = "quad"
data <- data[data$method == "SBTL_QUAD" & data$campus == "UCSB",]
data$area <- 1

#st2 <- unique(data$site)
#in 2023, some site were change names: SMI_HARRIS_PT_RESERVE_W, we need to change it before putting data through the function
data <- data %>%
  mutate(site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_W","SMI_HARRIS_PT_RESERVE_W",site),
         site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_E","SMI_HARRIS_PT_RESERVE_E",site))

data <- pisco_common(data)
write.csv(data,"pisco/data/raw_pass/pisco_quad_pass.csv", row.names = FALSE,na="")

#swath
# data <- read.csv("pisco/data/raw_in/UCSB_SWATH.csv", header = TRUE, na.strings = na_val)
# add_swath<-read.csv("pisco/data/raw_in/UCSB_SWATH_2016.csv", header = TRUE, na.strings = na_val)
# data<-rbind(data,add_swath)

data <- read.csv("pisco/data/raw_in/PISCO_SWATH.csv", header = TRUE, na.strings = na_val,stringsAsFactors = F)

#in 2023, there is no site_name_old anymore
# data<-data %>% #in 2019 data, the name change in the quad, we changed it back, otherwise there is no lat and lon match it. . 
#   mutate(site=ifelse(!is.na(site_name_old),site_name_old,site))

data$data_source = "pisco"
data$sample_method = "swath"
data <- data[data$method == "SBTL_SWATH" & data$campus == "UCSB",]
data$area = 60

#There are three algae survey twice, one for all algae and one for stipe greater than 30 cm. 
# We remove the one for stipe over 30 cm because it is redundent to the survey that includes all algae height. (issue fixed in 2019 data)
# trail <- data %>%
#   filter((classcode=="EISARBSTIPEGT30"|classcode=="PTECALSTIPEGT30"|classcode=="LAMSETSTIPEGT30"))
# data<-data %>%
#   filter(!(classcode=="EISARBSTIPEGT30"|classcode=="PTECALSTIPEGT30"|classcode=="LAMSETSTIPEGT30"))
#st3 <- unique(data$site)
#in 2023, some site were change names: SMI_HARRIS_PT_RESERVE_W, we need to change it before putting data through the function
data <- data %>%
  mutate(site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_W","SMI_HARRIS_PT_RESERVE_W",site),
         site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_E","SMI_HARRIS_PT_RESERVE_E",site))

data <- pisco_common(data)
write.csv(data,"pisco/data/raw_pass/pisco_swath_pass.csv", row.names = FALSE,na="")

#upc

data <- read.csv("pisco/data/raw_in/PISCO_UPC.csv", header = TRUE, na.strings = na_val,stringsAsFactors =F )

#in 2023, there is no site_name_old anymore
# data<-data %>% #in 2019 data, the name change in the quad, we changed it back, otherwise there is no lat and lon match it. . 
#   mutate(site=ifelse(!is.na(site_name_old),site_name_old,site))


data$data_source = "pisco"
data$sample_method = "upc"
data <- data[data$method == "SBTL_UPC" & data$campus == "UCSB",]

#st4 <- unique(data$site)

data$points <- round(data$count * 100/data$pct_cov)

#in 2023, some site were change names: SMI_HARRIS_PT_RESERVE_W, we need to change it before putting data through the function
data <- data %>%
  mutate(site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_W","SMI_HARRIS_PT_RESERVE_W",site),
         site=ifelse(site=="SMI_HARRIS_POINT_RESERVE_E","SMI_HARRIS_PT_RESERVE_E",site))


data <- pisco_common(data)
write.csv(data,"pisco/data/raw_pass/pisco_upc_pass.csv", row.names = FALSE,na="")


###########################################
#
# #As we receive data for 2019, the data structure has been changed. The side were merged into the site info. We made this table just to separated them. 
# library(stringr)
# library(tidyr)
# trail1 <- data.frame(site=c(st1,st2,st3,st4)) %>%
#   mutate(site=as.character(site),
#          sitenew=ifelse(site %in% c("SMI_HARE_ROCK","SMI_BAY_POINT","CAT_BLUE_CAVERN","CAT_PUMPERNICKEL","CAT_INTAKE_PIPES","SCI_SCORPION_ANCHORAGE",
#                                  "SRI_FORD_POINT","BULLITO","POINT_DUME","COUNTY_LINE","LITTLE_DUME","EL_MATADOR","LEO_CARRILLO","ANACAPA_BLACK_SEA_BASS","DUNES"),
#                      paste0(site,"_CEN"),site),
#          sitenew=ifelse(str_detect(sitenew,"_FAR_WEST"),gsub("_FAR_WEST","_FARWEST",sitenew,fixed=T),sitenew),
#          aa = strsplit(sitenew, "_(?!.*_)", perl=TRUE)) %>%
#   unnest(aa) %>%
#   group_by(sitenew) %>%
#   mutate(col=seq_along(sitenew)) %>% #add a column indicator
#   spread(key=col, value=aa)%>%
#   data.frame() %>%
#   mutate(X2=ifelse(X2=="FARWEST","FAR_WEST",X2))%>%
#   rename(sitecode=X1,side=X2)%>%
#   select(site,sitecode,side)
# 
#
# write.csv(trail1,"pisco/data/raw_in/lookups/generated_site_side.csv",row.names = F)

################################



