#run zonal stats

library(sf)
library(raster)
library(exactextractr)

#Load Source file for Function Defintions
source("FunctionDefs.R")

country_of_interest = "KEN"

#set working directory
working_directory <- "C:\\Users\\gr1d19\\Documents\\NTL Paper\\R_PROJECT"
setwd(working_directory)
      
# load admin unit shapefile


shp_file_path <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L2_UTM_GLH.shp",sep="")
if(!file.exists(shp_file_path))
{
  shp_file_path <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L2_Albers_GLH.shp",sep="")
}
if(!file.exists(shp_file_path))
{
  stop("Source Shapefile Does Not Exist")
}
  

shp_file_original <- st_read(shp_file_path)
shp_file <- shp_file_original

#rename ADM2_ID column to UID for merging
names(shp_file)[names(shp_file) == "ADM2_id"] <- "UID"

#populate list of rasters according to processing level and year
range_of_years <- c("2017","2018","2019")
processing_levels <- c("_rade9h","_outlier_smth")

#Final Year for CSV Output
final_year_output = substring("2018",3,4)

#Which Zonal Stat Metric To Use
#Sum, Mean, Median, etc
zonal_stat_metric = "sum"

#Loop over all years in range
for(current_year in range_of_years)
{
  #define raster directory according to current year
  raster_directory <- paste("NTL_RASTERS/",country_of_interest,"/",current_year,sep="")

  for(proc_level in processing_levels)
  {
    #populate list of rasters according to processing level 
    raster_list <- list.files(path=raster_directory,pattern=paste(proc_level,"*.tif$",sep=""),recursive = TRUE, full.names= TRUE)
    
    for(i in 1:length(raster_list))
    {
      #Extract Month Number
      month_number <- substr(basename(raster_list[i]), 5, 6)
      
      #Define Column Header Name according to processing level and date
      column_name_date_proc_level = paste(month.abb[as.numeric(month_number)],substr(current_year, 3, 4),proc_level,"_",zonal_stat_metric,sep="")
    
      #Create New Column for insertion into shape_file data frame
      shp_file[, column_name_date_proc_level] <- NA
      
      # --- Run Zonal Stats - *Sum*  ---
      shp_file[, column_name_date_proc_level] <- exact_extract(raster(raster_list[i]), shp_file,zonal_stat_metric)
    }
  }
}

#Write Final Shapefile With Zonal Stats
#st_write(shp_file,"ZonalStats_NAM_2018.shp",append = TRUE)

# #Write Zonal Stats to CSV File
output_filename_nodiff <- paste("ZONAL_STATS/","ZonalStats_",country_of_interest,"_",final_year_output,"_NODIFF.csv",sep="")

if (file.exists(output_filename_nodiff))
  file.remove(output_filename_nodiff)
  st_write(shp_file, output_filename_nodiff)

#Add Sum Difference Columns ---
    
#load Zonal Stats to CSV File
NTL_CSV = read.csv(output_filename_nodiff,check.names = FALSE)

#Calculate NTL Difference of Sum Between Months for Rade9
NTL_Sum_Diff_Rade9 = Calculate_NTL_Sum_Diff(NTL_CSV,"rade9h_sum",final_year_output)

#Add Results to Final DataFrame
NTL_CSV_2 = merge(NTL_CSV, NTL_Sum_Diff_Rade9,by="UID")

#Calculate NTL Difference of Sum Between Months for Smth
NTL_Sum_Diff_Smth = Calculate_NTL_Sum_Diff(NTL_CSV_2,"_outlier_smth",final_year_output)

#Add Results to Final DataFrame
NTL_CSV_3 = merge(NTL_CSV_2, NTL_Sum_Diff_Smth,by="UID")

#Extract 2018 & UID Column
NTL_CSV_YEAR_SUBSET = NTL_CSV_3[ , grepl(paste(final_year_output,"UID",sep="|") , names(NTL_CSV_3) ) ]

# #Write Zonal Stats to CSV File
output_filename_final <- paste("ZONAL_STATS/","ZonalStats_",country_of_interest,"_",final_year_output,".csv",sep="")

if (file.exists(output_filename_final))
  file.remove(output_filename_final)
  write.csv(NTL_CSV_YEAR_SUBSET,output_filename_final,row.names=FALSE)
  
if (file.exists(output_filename_nodiff)) 
    file.remove(output_filename_nodiff)
