rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path("C:/Users/", user)
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  dataDir <- file.path(DriveDir, "data")
  shapefileDir <- file.path(dataDir, "nigeria", "shapefiles")
} else if  ("Chilo Chiziba" %in% user) {
  user_path <- file.path("C:/Users/", user)
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  dataDir <- file.path(DriveDir, "data")
  shapefileDir <- file.path(dataDir, "nigeria", "shapefiles")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, "OneDrive - Northwestern University", "urban_malaria")
  dataDir <- file.path(DriveDir, "data")
  shapefileDir <- file.path(dataDir, "nigeria", "shapefiles")
  DHSshf <- file.path(dataDir, "DHS", "Downloads", "NG_2021_MIS_12052022_1735_141460")
  DHSdata <- file.path(dataDir, "DHS", "Downloads", "NG_2021_MIS_12052022_1735_141460")
  results <- file.path(DriveDir, "projects", "Malaria_prevalence", "Malaria_prevalence_Nigeria_Dec-2022", "outputs", "new_results")
}



# ### Required functions and settings
## -----------------------------------------
library(tidyverse)
library(ggplot2)
library(haven)
library(labelled)
library(labelled)
library(labeling)
library(sf)
library(arsenal)
library(srvyr)
library(survey)
library(janitor)
library(ggrepel)
library(usmap)
library(ggspatial)
library(rgdal)
library(tmap)
library(ggpubr)
library(dplyr)
library(data.table)
library(epiDisplay)
#Loading PR and KR datasets
DHSdata <- read_dta(file.path(DHSdata, "NGPR81DT", "NGPR81FL.DTA")) 
DHSKRdata <- read_dta(file.path(dataDir, "DHS", "Downloads", "NG_2021_MIS_12052022_1735_141460", "NGKR81DT", "NGKR81FL.DTA")) 

## Filtering malaria positive children
u5children <- DHSdata %>% dplyr::filter(hv103==1 & hml16 < 5 ) 


#Subset the data
nig_pr_data <- u5children[,c("hv001", "hv002","shlga", "hml32", "hv227", "hv228", "hv005","hv022",
                          "hv021", "hv023")]%>%
  data.table()

#Rename the variables
nig_pr_data <- dplyr::rename(nig_pr_data, HHID="hv002", clusterID="hv001",lga="shlga", mal_status="hml32", net_use="hv227",
                             u5_netuse="hv228", weight="hv005", strata="hv022",
                             s_unit="hv021", design_strata="hv023")

#subset DHS KR data
nig_kr_data <- DHSKRdata[, c("v001","v002", "h47", "m49a", "ml13e", "h32z")]%>%
  data.table()

#Rename the variables
nig_kr_data <- dplyr::rename(nig_kr_data,HHID= "v002", clusterID="v001",u5_diagnostic="h47", IPTp="m49a",
                              artinizine_comb="ml13e",
                             u5_fever= "h32z")
data<-nig_kr_data %>% left_join(nig_pr_data, by = c("HHID", "clusterID"))

# labeling variables
data <- mutate(data,across(where(is.labelled), to_factor)) 
#removing out missing values
df<-data[complete.cases(data),] 


output <- df %>% mutate(wt=weight/1000000,strat=strata, id=s_unit, num_p=1)

output1<-tableby(mal_status ~ u5_netuse +  artinizine_comb + net_use +
                   u5_fever  + u5_diagnostic + IPTp , data = output, 
                    control=tableby.control(total=TRUE, test=TRUE),  na.tableby(T), 
                     stats.labels = list())

Output<-summary(output1, text=TRUE)
Output
write.csv(Output, file = "Univariate_Output_updated.csv")
