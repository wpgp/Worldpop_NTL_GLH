library(ggplot2)
library(reshape2)
library(ggpubr)
library(sf)

#set working directory
working_directory   <- "C:\\Users\\gr1d19\\Documents\\NTL Paper\\R_PROJECT"
setwd(working_directory)

#Load Source file for Function Definitions
source("FunctionDefs.R")

#Define the country of interest
country_of_interest <- "KEN"

#Either Urban Classification 'class' or Admin Units 'UID'
Category_Type       <- "class"

#Load the NTL & GLH Data
NTL_GLH_COMBINED    <- Load_GLH_NTL_Data(working_directory,country_of_interest)

#Extract Classification List
Classification_List <- as.character(unique(NTL_GLH_COMBINED[[Category_Type]]))

#All GLH Variables 
GLH_Variables       <- c("_int_frto","_ntl_fr" ,"_intl_fr","_ext_fr","_ntl_to","_intl_to","_ext_to","_ntl_net","_intl_net","_ext_net")

#Define NTL Metric Of Interest 
NTL_Variable        <- "_smth_sum"

#Remains this value
year_of_interest    <- "2018-2019"

#Create Correlation Coefficient Table
Correlation_Table   <- data.frame(Metric_Name=NA,Class=NA, CC=NA,p_value=NA)[numeric(0), ]

for(GLH_Variable_Current in GLH_Variables)
{
  #Loop over all Admin Regions or Urban Classification
  for(Classification_Index in Classification_List)
  {
    #Process GLH Data for Plotting
    GLH_For_plot <- Process_GLH_For_Plot(NTL_GLH_COMBINED,GLH_Variable_Current,Classification_Index,Category_Type)
    
    #Process NTL Data for Plotting
    NTL_For_plot <- Process_NTL_For_Plot(NTL_GLH_COMBINED,NTL_Variable,Classification_Index,Category_Type)
     
    #Run Comparison Plot and output to PNG #Produces Garbage
    #Create_Comparison_Plot(GLH_For_plot,NTL_For_plot,GLH_Variable_Current,NTL_Variable,Category_Type,year_of_interest,country_of_interest)
    
    Metric_Name <- paste(Category_Type," - ",NTL_Variable," & ",GLH_Variable_Current,sep="")
    
    #Calculate Correlation Coefficient between GLH & NTL
    CC <- Calculate_Correlation(GLH_For_plot,NTL_For_plot,GLH_Variable_Current,NTL_Variable,Category_Type,
                               year_of_interest,country_of_interest,makeplot=FALSE)
    
    #Append output to Correlation Table
    Correlation_Table[nrow(Correlation_Table) + 1,] <- list(Metric_Name,Classification_Index,CC$estimate,CC$p.value)
  }
  #Create a Density plot of Correlations for Each NTL-GLH Variable Pair
  #Produce_CC_Density_Plot(Correlation_Table,GLH_Variable_Current,NTL_Variable,Metric_Name,year_of_interest,country_of_interest,Category_Type)
  
  #Create a Bar Chart plot of Correlations for Each NTL-GLH Variable Pair
  Produce_CC_Bar_Plot(Correlation_Table,GLH_Variable_Current,NTL_Variable,Metric_Name,year_of_interest,country_of_interest,Category_Type)
}
#Merge Plots to Final PDF
#Produce_PDF_Grid(year_of_interest,NTL_Variable,country_of_interest,Category_Type,"bar")
#Produce_PDF_Grid(year_of_interest,NTL_Variable,country_of_interest,Category_Type,"density")
