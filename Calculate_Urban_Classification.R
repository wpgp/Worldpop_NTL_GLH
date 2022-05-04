# Rscript to Categorise Admin Units of any Africa country into a
# Defined urban classification eg (Urban,Periurban,Rural)
# G.E.Rogers@soton.ac.uk

library(raster)
library(exactextractr)
library(sf)
library(rgdal)
library(dplyr)

#set working directory
working_directory <- "C:/Users/gr1d19/Documents/NTL Paper/R_PROJECT"
setwd(working_directory)

# 1.	Load global GHS SMOD land classification raster (converted to WGS84) and clip to L0 country of interest
# 2.	Aggregate the 8 SMOD classes into 3 (urban, peri urban and rural) 
# 3.	Extract each class into separate rasters and convert to binary (0,1)
# 4.	Load Africa population raster and clip to L0 country of interest
# 5.	Multiply population raster by binary classification rasters
# 6.	Run zonal statistics with country L2 shapefile to calculate sum of population for each 
#     urban classification in each admin unit
# 7.  Determine which classification has the highest population and assign this category to the admin unit
# 8.  Output Results to CSV
#
# Expected Files
# SMOD GLOBAL (WWW):            "POPULATION_RASTERS/GHS_SMOD_GLOBAL_WGS84.tif"
# POPULATION AFRICA (WORLDPOP): "POPULATION_RASTERS/ppp_2020_africa.tif"
# COUNTRY L2 (WORLDPOP):        "GLH_SHAPEFILES/NAM/NAM_L2_UTM_GLH.shp"
# COUNTRY L0 (WORLDPOP):        "GLH_SHAPEFILES/NAM/NAM_L0_buffer.shp"

#Define Country of Interest (alpha3 ISO) ****
country_of_interest   <- "KEN"

#Load L0 Country Shapefile
country_L0_filename   <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L0_buffer.shp",sep="")
country_L0_shapefile  <- st_read(country_L0_filename)

#Load Global SMOD Raster and clip to Country L0 100km Buffer (for efficiency)
smod_raster_global    <- raster("POPULATION_RASTERS/GHS_SMOD_GLOBAL_WGS84.tif")
smod_raster_local     <- crop(smod_raster_global, country_L0_shapefile)

# reclassify the SMOD values into three groups 
# 10-13 -> 0 (Rural)
# 21-23 -> 1 (PeriUrban)
# 30 -> 2 (Urban)
reclass_matrix                  <- matrix(c(9, 13, 0,  20, 23, 1,  29, 30, 2), ncol=3, byrow=TRUE)
smod_raster_local_reclassified  <- reclassify(smod_raster_local, reclass_matrix)
remove(reclass_matrix)

# Extract Rural Component of SMOD Raster
mask_raster_rural     <- smod_raster_local_reclassified
mask_raster_rural[mask_raster_rural != 0] <- NA
rural_raster          <- mask(smod_raster_local_reclassified,mask_raster_rural)
# Reclass Rural Component to 1
rural_raster[rural_raster == 0] <- 1

# Extract Periurban Component of SMOD Raster
mask_raster_periurban   <- smod_raster_local_reclassified
mask_raster_periurban[mask_raster_periurban != 1] <- NA
periurban_raster        <- mask(smod_raster_local_reclassified,mask_raster_periurban)
# Reclass Periurban Component to 1
periurban_raster[periurban_raster == 1] <- 1

# Extract Urban Component of SMOD Raster
mask_raster_urban       <- smod_raster_local_reclassified
mask_raster_urban[mask_raster_urban != 2] <- NA
urban_raster            <- mask(smod_raster_local_reclassified,mask_raster_urban)
# Reclass Urban Component to 1
urban_raster[urban_raster == 2] <- 1

# Cleanup unused Variables
remove(mask_raster_rural,mask_raster_periurban,mask_raster_urban)
remove(smod_raster_global,smod_raster_local,smod_raster_local_reclassified)

#Load World Pop Population Raster
population_raster_global  <- raster("POPULATION_RASTERS/ppp_2020_africa.tif") 

#Clip Population Raster to Country L0 100km Buffer (for efficiency)
population_raster_local   <- crop(population_raster_global, country_L0_shapefile)

#Multiply Population by Resampled Classification Rasters
rural_pop_raster          <- resample(rural_raster,population_raster_local)*population_raster_local
periurban_pop_raster      <- resample(periurban_raster,population_raster_local)*population_raster_local
urban_pop_raster          <- resample(urban_raster,population_raster_local)*population_raster_local

#Load Country L2 Admin Unit Shapefile
country_L2_filename       <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L2_UTM_GLH.shp",sep="")
if (!file.exists(country_L2_filename))
  country_L2_filename     <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L2_Albers_GLH.shp",sep="")
country_L2_shapefile      <- st_read(country_L2_filename)

#Setup duplicate for Zonal Stats
country_shapefile_stats   <- country_L2_shapefile

#Run Zonal Stats to obtain the Sum of Population for Each Urban Classification
country_shapefile_stats$rural_sum     <- exact_extract(rural_pop_raster, country_shapefile_stats,'sum')
country_shapefile_stats$periurban_sum <- exact_extract(periurban_pop_raster, country_shapefile_stats,'sum')
country_shapefile_stats$urban_sum     <- exact_extract(urban_pop_raster, country_shapefile_stats,'sum')

#Determine Total Population in Country
country_total_population <- sum(country_shapefile_stats$rural_sum) + sum(country_shapefile_stats$periurban_sum) + sum(country_shapefile_stats$urban_sum)

#Convert Country Shapefile To Dataframe and Drop unneeded Columns
country_df          <- as.data.frame(country_shapefile_stats)
country_df$geometry <- NULL
country_df$ADM2     <- NULL
#Rename Admin ID Column
names(country_df)[names(country_df) == "ADM2_id"] <- "UID"
#Create Urban Classification Columns 'Class'
country_df$class <- "0"

#Determine Which Column has highest value and assign classification accordingly
for(i in 1:length(country_df[[1]]))
{
   max_column_id <- which(country_df == max(country_df$urban_sum[i],country_df$rural_sum[i],country_df$periurban_sum[i]),arr.ind=TRUE)[2]
   #Assign Classification by string extraction
   country_df$class[i] <- gsub('.{4}$', '', colnames(country_df)[max_column_id])
}

#Extract Only UID & Classification Columns for Simple CSV Output
country_df_simple = country_df[, grepl(paste("UID|class",sep="|") , names(country_df) )]

#Setup Output Filenames  
classification_output_filename <- paste("CLASSIFICATION_DATA/",country_of_interest,"_Urban_Classifications.csv",sep="")

#Output Classification to CSV
if (file.exists(classification_output_filename))
  file.remove(classification_output_filename)
  write.csv(country_df_simple,classification_output_filename,row.names=FALSE)
