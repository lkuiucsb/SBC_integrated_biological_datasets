## Config ##
rm(list = ls())
na_val = "-99999"


lter_common <- function(data,survey_method){
  #rename cols
  data <- merge(data,taxa_lookup[taxa_lookup$SURVEY == survey_method,c("SP_CODE","lifestage","WORMS_NAME")], by = c("SP_CODE"), all.x = TRUE)
  names(data) <- tolower(names(data))
  data$date <- as.Date(data$date)
  data <- rename(data, native_taxa_code = sp_code)
  return(data)
}

#load lookups
taxa_lookup <- read.csv("lter/data/raw_in/lookups/SBCLTER_Species_List_Master.csv", na.strings = c(".",""),stringsAsFactors = F)
taxa_lookup <- rename(taxa_lookup, lifestage = SIZE)
#Note, there is a NA species code, be cautious!

#fish & crypticfish
data <- read.csv("lter/data/raw_in/Annual_fish_comb.csv", header = TRUE, na.strings = na_val)

data$data_source = "lter"
data$sample_method[data$SURVEY == "FISH"] = "fish"
data$sample_method[data$SURVEY == "CRYPTIC FISH"] = "crypticfish"
data$sample_subtype = "BOT"
data$height <- 2
#sum over quad/side
data <- data %>%
  group_by(data_source,sample_method,sample_subtype,DATE,SITE,TRANSECT,SP_CODE,SCIENTIFIC_NAME,COMMON_NAME,height) %>%
  summarise(size = weighted.mean(SIZE,COUNT,na,rm=T),count = sum(COUNT),.groups = 'drop') %>% # note, LTER had NA filled in some sites for the early year (missing survey sheet).
  ungroup()
data$area <- 80
#mutate adult/juvenile/all
data <- data %>%
  mutate(adult = count * as.integer(size >= 10),juvenile = count * as.integer(size < 10)) %>%
  rename(all = count) %>%
  mutate(adult=if_else(all==0&!is.na(all)&is.na(adult),as.integer(0),adult),
         juvenile=if_else(all==0&!is.na(all)&is.na(juvenile),as.integer(0),juvenile))

#fish

data_fish<- droplevels(subset(data,sample_method == "fish"))
data_fish <- lter_common(data_fish,"FISH")
write.csv(data_fish,"lter/data/raw_pass/lter_fish_pass.csv", row.names = FALSE,na = "")

#crypticfish
data_crypticfish <- data[data$sample_method == "crypticfish",]
data_crypticfish <- lter_common(data_crypticfish,"CRYPTIC FISH") 

write.csv(data_crypticfish,"lter/data/raw_pass/lter_crypticfish_pass.csv", row.names = FALSE,na = "")

############################################################
#upc
data <- read.csv("lter/data/raw_in/Annual_Cover_All_Years.csv", header = TRUE, na.strings = na_val,stringsAsFactors=FALSE)

#The new dataset separate the substrate and the organism data
sub <- read.csv("lter/data/raw_in/Annual_Substrate_All_Years.csv", header = TRUE, na.strings = na_val,stringsAsFactors=FALSE)

#substrate didn't have common name, we should use the master spp list.
sub1<-sub %>%
  rename(SP_CODE=SUBSTRATE_TYPE) %>%
  mutate(GROUP="SUBSTRATE",SURVEY="UPC")

#the new data in 2018 have the percent cover at the swath level instead of transect, so we should calculate it at the transect level first. 
data <-bind_rows(data,sub1) %>%
  group_by(YEAR,MONTH,DATE,SITE,TRANSECT,SP_CODE,SCIENTIFIC_NAME,COMMON_NAME,TAXON_KINGDOM,TAXON_PHYLUM,TAXON_CLASS,TAXON_ORDER,TAXON_FAMILY,TAXON_GENUS,GROUP,SURVEY,MOBILITY,GROWTH_MORPH) %>%
  summarise(PERCENT_COVER=mean(PERCENT_COVER),.groups='drop') %>%
  ungroup() 

data$data_source = "lter"
data$sample_method = "upc"
data$points <- 80
data$count <- data$PERCENT_COVER * data$points/100
data <-lter_common(data,"UPC")

write.csv(data,"lter/data/raw_pass/lter_upc_pass.csv", row.names = FALSE,na = "")

#quad & swath
data <- read.csv("lter/data/raw_in/Annual_Quad_Swath_All_Years.csv", header = TRUE, na.strings = na_val, stringsAsFactors=FALSE)
data$data_source = "lter"
data$sample_method = tolower(data$SURVEY)

#quad
data_quad <- data %>%
  filter(sample_method == "quad")

data_quad <-lter_common(data_quad,"QUAD")

write.csv(data_quad,"lter/data/raw_pass/lter_quad_pass.csv", row.names = FALSE,na = "")

#swath
data_swath <- data %>%
  filter(sample_method == "swath")

data_swath <-lter_common(data_swath,"SWATH")

#combine kelp species
data1 <- read.csv("lter/data/raw_in/Annual_Kelp_All_Years.csv", header = TRUE, na.strings = na_val, stringsAsFactors = F)

data2<-data1 %>%
  mutate(FRONDS=if_else(FRONDS>0,as.integer(1),FRONDS)) %>% # we are counting the number of plants instead of number of fronds 
  group_by(YEAR,MONTH,DATE,SITE,TRANSECT,QUAD,SIDE,SP_CODE,AREA,SCIENTIFIC_NAME,COMMON_NAME,TAXON_KINGDOM,TAXON_PHYLUM,TAXON_CLASS,TAXON_ORDER,TAXON_FAMILY,TAXON_GENUS,GROUP,SURVEY,MOBILITY,GROWTH_MORPH ) %>%
  summarise(count=sum(FRONDS,na.rm=F),.groups='drop') %>% 
  ungroup() %>%
  mutate(data_source = "lter",sample_method = "swath")

data3<-lter_common(data2,"KELP")

# #rbind for two dataset
data4<- bind_rows(data_swath,data3)

write.csv(data4,"lter/data/raw_pass/lter_swath_pass.csv", row.names = FALSE,na = "")

