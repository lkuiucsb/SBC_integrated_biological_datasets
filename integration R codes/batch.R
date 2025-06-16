##Configure environment
rm(list = ls())

setwd("Path_to_your_folder")
library(reshape2)
library(dplyr)
library(stringr)
  
#Pass - survey-specific normalization
source("kfm_pass.R")
source("lter_pass.R")
source("pisco_pass.R")
source("sni_pass.R")

#Index - project-specific normalization
source("kfm_index.R")
source("lter_index.R")
source("pisco_index.R")
source("sni_index.R") 

#parent lookups organization
source("lookups_merge.R") 
source("taxa_manipulate.R") 
source("worms_lookup.R") #new code for worms query
source("taxa_worms_merge.R")

#integrated normalization (ex. zero-filling)
source("integrate_fish.R") #fish abundance
source("integrate_quadswath.R") #algae/invertebrate abundance
source("integrate_cover.R") #percentage seafloor cover

# data product for 4 data packages
source("data_product.R") #data product

