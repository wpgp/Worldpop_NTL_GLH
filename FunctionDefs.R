#Function Definitions for Seasonality Project 
#Nightlights vs Google History Data vs CDR 

Load_GLH_NTL_Data = function(working_directory,country_of_interest)
{
  #load Google Location History CSV
  GLH_CSV_2018 <- read.csv(paste(working_directory,"\\GLH_DATA\\",country_of_interest,"_2018_google_units.csv",sep=""),fileEncoding="UTF-8-BOM")
  GLH_CSV_2019 <- read.csv(paste(working_directory,"\\GLH_DATA\\",country_of_interest,"_2019_google_units.csv",sep=""),fileEncoding="UTF-8-BOM")
  
  #load VIIRS Nightlights CSV
  NTL_CSV_2018 <- read.csv(paste(working_directory,"\\ZONAL_STATS\\","ZonalStats_",country_of_interest,"_18.csv",sep=""),check.names = FALSE)
  NTL_CSV_2019 <- read.csv(paste(working_directory,"\\ZONAL_STATS\\","ZonalStats_",country_of_interest,"_19.csv",sep=""),check.names = FALSE)
  
  GLH_CSV_ALL <- merge(GLH_CSV_2018,GLH_CSV_2019,by="UID")
  NTL_CSV_ALL <- merge(NTL_CSV_2018,NTL_CSV_2019,by="UID")
  
  #load urban rural classification CSV
  CLASS_CSV <- read.csv(paste(working_directory,"\\CLASSIFICATION_DATA\\",country_of_interest,"_Urban_Classifications.csv",sep=""),check.names = FALSE)
  
  #merge data frames into one and drop any 
  #admin regions without 1-1 correspondence
  NTL_GLH_COMBINED <- merge(GLH_CSV_ALL,NTL_CSV_ALL,by="UID")
  
  #merge classification with NTL_GLH
  NTL_GLH_CLASS <- merge(NTL_GLH_COMBINED,CLASS_CSV,by="UID")
  
  return(NTL_GLH_CLASS)
}

#Calculate the difference of radiance sums for NTL Stats
Calculate_NTL_Sum_Diff = function(input_df,NTL_Variable_input,final_year_output)
{
  #Extract columns by NTL processing Variable & Keep UID Column
  NTL_Subset <- input_df[ , grepl(paste(NTL_Variable_input,"UID",sep="|") , names(input_df) ) ]
  
  #create new column based on 'sum' difference between current month and previous 
  
  for(i in 3:length(NTL_Subset))
  {
    column_name_date_proc_level_diff <- paste(colnames(NTL_Subset[i]),"diff",sep="_")
    
    #Create New Column for difference 
    NTL_Subset[, column_name_date_proc_level_diff] <- NA
    
    if(i==3) { NTL_Subset[, column_name_date_proc_level_diff] <- 0 }
    else     { NTL_Subset[, column_name_date_proc_level_diff] <- NTL_Subset[,i] - NTL_Subset[,i-1] }
  }
  
  #Refine Data Frame to Specific Output Year & Keep UID Column
  
  NTL_Subset_Year <- NTL_Subset[ , grepl(paste("UID|",final_year_output,".*diff",sep=""), names( NTL_Subset ) ) ]
  
  return(NTL_Subset_Year)
}

#Produce Simple Plot of input dataframe
Create_Simple_Plot = function(input_df)
{
  #Begin Plot GLH
  p <- ggplot(input_df, aes(x = variable, y = value, color = UID)) 
  p <- p + theme(axis.text.x = element_text(face="bold", size=08, angle=90))
  p <- p + geom_point() + geom_path()
  return(p)
}

#Produce PDF from Mosaic of PNG Files
Produce_PDF_Grid = function(year_of_interest,NTL_Variable,country_of_interest,Classification_Type,Plot_Type)
{
  if(Plot_Type == "density") { directory_suffix   <- paste("_CORRELATION_DENSITY_",sep="")}
  else if(Plot_Type == "bar") { directory_suffix  <- paste("_CORRELATION_BARCHART_",sep="")}
  else { stop("No Plot Type Specified")}
  
  directory_output    <- paste("RESULTS//",country_of_interest,"_",Classification_Type,"_",year_of_interest,directory_suffix,NTL_Variable,"/",sep="")  
  
  png_file_list       <- lapply(list.files(path=directory_output,pattern="png",full.names = TRUE), png::readPNG)
  
  png_grid            <- lapply(png_file_list, grid::rasterGrob)
  
  png_grid_arranged   <- gridExtra::grid.arrange(grobs=png_grid)
  
  pdf_filename        <- paste(directory_output,country_of_interest,"_",Classification_Type,"_",year_of_interest,"_CC_MOSAIC.pdf",sep="")
  
  ggplot2::ggsave(file=pdf_filename, png_grid_arranged)
}

#Produce Density of Various R-Square Values
Produce_CC_Density_Plot = function(Correlation_Table,GLH_Variable_Current,NTL_Variable,Metric_Name,year_of_interest,country_of_interest,Classification_Type)
{
  #Filter the Correlation Table According to Current GLH Metric
  Correlation_Table_Filter <- Correlation_Table[grepl(GLH_Variable_Current,Correlation_Table$Metric_Name),]
  
  #Extract Positive and Negative CC Values
  CC_Positive <- Correlation_Table_Filter[Correlation_Table_Filter$CC>0,]
  CC_Negative <- Correlation_Table_Filter[Correlation_Table_Filter$CC<0,]
  
  #Get max length of CC Vectors
  n <- max(length(CC_Positive$CC), length(CC_Negative$CC))
  
  #Create Correlation Table for Density Plot
  Correlation_Table_For_Plot <- data.frame(CC_Positive$CC[1:n],CC_Negative$CC[1:n])
  
  #Label Colums
  names(Correlation_Table_For_Plot)[1] <- "CC_Positive"
  names(Correlation_Table_For_Plot)[2] <- "CC_Negative"
  
  #Create Two way Density Plot
  
  Plot_Title <- paste(country_of_interest,"_",Classification_Type,"_",year_of_interest," - NTL: ",NTL_Variable," ~  GLH: ",GLH_Variable_Current,sep="")
  
  p <- ggplot(Correlation_Table_For_Plot, fill = "CC -/+") 
  
  p <- p + geom_density( aes(x=CC_Positive, y = ..density.. ), fill="green")
  
  p <- p + geom_density( aes(x=CC_Negative, y = -..density.. ), fill= "blue")
  
  p <- p + labs(title=Plot_Title,x=paste("R Squared (R2) Over ",Classification_Type,sep=""), y = "Density")
  
  p <- p + scale_x_continuous(limits=c(-1,1))
  
  if (!dir.exists("RESULTS")){ dir.create("RESULTS")}
  
  directory_output <- paste("RESULTS//",country_of_interest,"_",Classification_Type,"_",year_of_interest,"_CORRELATION_DENSITY_",NTL_Variable,"/",sep="")

  if (!dir.exists(directory_output)){ dir.create(directory_output)}
  
  filename_output <- paste(directory_output,Metric_Name,".png",sep="")
  
  #Export Plot To file
  ggarrange(p) %>% ggexport(filename = filename_output,width = 600, height = 350)
  
  #print(p)
}

#Produce Density of Various R-Square Values
Produce_CC_Bar_Plot = function(Correlation_Table,GLH_Variable_Current,NTL_Variable,Metric_Name,year_of_interest,country_of_interest,Classification_Type)
{
  #Filter the Correlation Table According to Current GLH Metric
  Correlation_Table_GLH_Filtered <- Correlation_Table[grepl(GLH_Variable_Current,Correlation_Table$Metric_Name),]
  
  Correlation_Table_For_Plot <- data.frame(Correlation_Table_GLH_Filtered$Class,Correlation_Table_GLH_Filtered$CC)
  
  #Label Colums
  names(Correlation_Table_For_Plot)[1] <- "Class"
  names(Correlation_Table_For_Plot)[2] <- "CC"
  
  Plot_Title <- paste(country_of_interest,"_",year_of_interest," - NTL: ",NTL_Variable," ~  GLH: ",GLH_Variable_Current,sep="")
  
  p <- ggplot(data=Correlation_Table_For_Plot, aes(x=Class,y=CC,fill=Class)) 
  p <- p + geom_bar(stat="identity")
  p <- p + theme(legend.position="none")
  p <- p + labs(title=Plot_Title,y = "R Squared")
  
  #Remove Any Rows with NA for CC
  Correlation_Table_For_Plot <- na.omit(Correlation_Table_For_Plot)
  
  if(min(Correlation_Table_For_Plot$CC)<0){   p = p + ylim(-1,1) }
  else { p = p + ylim(0,1) }
  
  p = p + theme(  axis.text.x <- element_text(size=12, face="bold", colour = "black"), 
                  axis.line.x <- element_line(color="black", size = 0.3),
                  axis.line.y <- element_line(color="black", size = 0.3),
                  axis.text.y <- element_text(size=12,  face="bold",colour = "black"),
                  #axis.title.x = element_text(size=13, face="bold", colour = "black",margin = margin(t = 5, r = 0, b = 0, l = 0)),    
                  axis.title.y <- element_text(size=13, face="bold", colour = "black",margin = margin(r = 10)),
                  plot.title <- element_text(size=14, face="bold", colour = "black",margin = margin(t = 3, r = 0, b = 10, l = 0)),
                  axis.title.x <- element_blank()
                )
  
  if (!dir.exists("RESULTS")){ dir.create("RESULTS")}
  
  directory_output <- paste("RESULTS//",country_of_interest,"_",Classification_Type,"_",year_of_interest,"_CORRELATION_BARCHART_",NTL_Variable,"/",sep="")
  
  if (!dir.exists(directory_output)){ dir.create(directory_output)}
  
  filename_output <- paste(directory_output,Metric_Name,".png",sep="")
  
  #Export Plot To file
  ggarrange(p) %>% ggexport(filename = filename_output,width = 600, height = 350)
  
  #print(p)
}

#Calculate Correlation Coefficient across Urban Classification
Calculate_Correlation = function(GLH_input,NTL_input,GLH_Variable,NTL_Variable,Category_Type,year_of_interest,country_of_interest,makeplot)
{
  Classification_Index <- as.character(GLH_input[1,1])
  
  #rename columns for merging
  names(GLH_input)[names(GLH_input) == "value"] <- "GLH"
  names(NTL_input)[names(NTL_input) == "value"] <- "NTL"
  
  #Create DataFrame for combined NTL & GLH Values
  Combined_Data <- GLH_input
  #Add NTL Column 
  Combined_Data$NTL <- NTL_input$NTL
  
  #Just to ensure any 0 are treated as numbers
  Combined_Data$GLH <- as.numeric(Combined_Data$GLH)
  Combined_Data$NTL <- as.numeric(Combined_Data$NTL)
  
  Combined_Data <- Combined_Data
  
  #Calculate Correlation Factor between NTL & GLH
  Correlation_Factor <- cor.test(Combined_Data$NTL, Combined_Data$GLH, method = "spearman")
  
  if(makeplot == TRUE)
  {
    #define plot directory name
    plot_dir <- paste("RESULTS//",country_of_interest,"_",Category_Type,"_",year_of_interest,"_CORRELATION_PLOTS//",NTL_Variable," & ",GLH_Variable,"//",sep="")

    #create directories
    if (!dir.exists("RESULTS")){ dir.create("RESULTS")}
    if (!dir.exists(paste("RESULTS//",country_of_interest,"_",Category_Type,"_",
                          year_of_interest,"_CORRELATION_PLOTS//",sep=""))){dir.create(paste("RESULTS//",country_of_interest,"_",Category_Type,"_",
                                                                                             year_of_interest,"_CORRELATION_PLOTS//",sep=""))} 
    if (!dir.exists(plot_dir)){ dir.create(plot_dir)}
    
    #Create Scatter plot with Correlation Coefficient
    ggscatter(Combined_Data, x = "GLH", y = "NTL",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "spearman",
              title = paste("CC - ",Category_Type," ",Classification_Index,sep=""),
              xlab = GLH_Variable, ylab = NTL_Variable)  %>%
      ggexport(filename = paste(plot_dir,country_of_interest,"_",Classification_Index,".png",sep=""),width = 600, height = 350)
  }
  
  return(Correlation_Factor)
}

#Create plot with 2 independent y axis 
Create_Comparison_Plot = function(GLH_input,NTL_input,GLH_Variable,NTL_Variable,Category_Type,year_of_interest,country_of_interest)
{
  #Define Plot Directory
  plot_dir <- paste(country_of_interest,"_",Category_Type,"_",year_of_interest,"_COMPARISON_PLOT - ",NTL_Variable," & ",GLH_Variable,"//",sep="")
  #Make Dir if not exist
  if (!dir.exists(plot_dir)){dir.create(plot_dir)} 
  
  #Take Admin Unit as Filename
  Classification_Index <- as.character(GLH_input[1,1])
  
  #Start Plot Routine for File Export
  png(filename=paste(plot_dir,country_of_interest,"_",Classification_Index,".png",sep=""),width = 600, height = 350)
  
  par(mar = c(5, 5, 3, 5))
  
  plot(GLH_input$variable,GLH_input$value,type="l",ylab = GLH_Variable,
       main = paste("GLH vs NTL - ",Category_Type,Classification_Index,sep=""), xlab = "Date", col = "blue",lwd=2)
  
  par(new = TRUE)
  
  plot(NTL_input$variable,NTL_input$value,type="l",xaxt = "n", 
       yaxt = "n", ylab = "", xlab = "", col = "red",lwd=2)
  
  axis(side = 4)
  
  legend("topleft", c(GLH_Variable, NTL_Variable),col = c("blue", "red"), lty = c(1, 1),lwd=2)
  
  mtext(NTL_Variable, side = 4, line = 3)
  
  #Write output to PNG
  dev.off()
}

#Prepare GLH Data for Plotting
Process_GLH_For_Plot = function(input_df,GLH_Variable,Classification_Index,Category_Type)
{
  #Extract by GLH Variable
  GLH_Subset            <- input_df[ , grepl(paste(GLH_Variable,Category_Type,sep="|") , names(input_df) ) ]

  #Extract by Admin ID
  GLH_Subset_Class      <- GLH_Subset[GLH_Subset[[Category_Type]]==Classification_Index,]

  #melt for plotting
  GLH_Subset_Class.melt <- melt(GLH_Subset_Class,id=c(Category_Type))

  #Convert Date Format for GGPlot
  GLH_Subset_Class.melt$variable <- as.Date(paste0("01-", substr(GLH_Subset_Class.melt$variable, 0, 5)),format = "%d-%b%y")

  return(GLH_Subset_Class.melt)
}

#Prepare NTL Data for Plotting
Process_NTL_For_Plot = function(input_df,NTL_Variable,Classification_Index,Category_Type)
{
  #Extract columns by NTL processing Variable & Keep UID Column
  NTL_Subset            <- input_df[ , grepl(paste(paste(NTL_Variable,"$",sep=""),Category_Type,sep="|") , names(input_df) ) ]
  
  #Extract by Admin ID
  NTL_Subset_Class      <- NTL_Subset[NTL_Subset[[Category_Type]]==Classification_Index,]
  
  #melt for plotting
  NTL_Subset_Class.melt <- melt(NTL_Subset_Class,id=c(Category_Type))
  
  #Convert Date Format for GGPlot
  NTL_Subset_Class.melt$variable <- as.Date(paste0("01-", substr(NTL_Subset_Class.melt$variable, 0, 5)),format = "%d-%b%y")
  
  return(NTL_Subset_Class.melt)
}