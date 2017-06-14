############### SESYNC Research Support: Fisheries and food security ########## 
## Functions used in the processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 06/14/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: adding function to process date and read in data from feed2go
##
## Links to investigate:

###################################################
#

###### Library used

library(gtools)                              # loading some useful tools 
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(rgeos)                               # Geometric, topologic library of functions
library(gridExtra)                           # Combining lattice plots
library(colorRamps)                          # Palette/color ramps for symbology
library(ggplot2)

###### Functions used in this script sourced from other files

import_data_survey <- function(infile_name,in_dir=".",date="",out_dir=".", out_suffix=""){
  #Import data sent by google and format columns for future processing and visualization.
  
  ##### BEGIN FUNCTION ####
  
  ### Add quote="" otherwise EOF warning and error in reading
  df <- read.table(file.path(in_dir,infile_name),sep=",",fill=T,quote="",header=F)
  
  #remove first row that contains names
  #rename columns using:
  #n_col_start_date <- 3 #may need to be changed
  n_col <- ncol(df)
  nt <- n_col - n_col_start_date #we are dropping the last date because it is often incomplete
  
  ##dates and number not allowed in df column name
  range_dates <- seq.Date(from=as.Date(start_date),by="month",length.out = nt )
  
  date_year <- strftime(range_dates, "%Y")
  date_month <- strftime(range_dates, "%m") # current month of the date being processed
  date_day <- strftime(range_dates , "%d")
  #range_dates_format <- paste(date_year,date_month,date_day,sep="_")
  
  range_dates_str <- as.character(range_dates)
  range_dates_format <- range_dates_str
  #class(range_dates)
  #start_date
  
  names_col <- c("g_id","sci_name","country",range_dates_format)
  df <- df[-1,-n_col] #remove the first row with incomplete header and last column with incomplete data
  names(df) <- names_col
  
  
  df[,n_col_start_date] <- sub('"',"",df[,n_col_start_date])
  
  out_filename_tmp <- sub(extension(infile_name),"",infile_name)#
  out_filename <- file.path(out_dir,paste0(out_filename_tmp,"_",out_suffix,".csv"))
  
  #write.table(df,"test.txt")
  #write.table(df,"test.txt",sep=",")
  write.table(df,file=out_filename,sep=",")
  
  return(out_filename)
}

extract_date_feed2go <- function(string_val){
  list_str <- strsplit(string_val,"_"); 
  date_val <- list_str[[1]][3]
  #substr(x, start, stop)
  date_val <- substr(date_val, start=1, stop=8)
  date_val <- as.Date(date_val,format = "%Y%m%d")
  date_val <- as.character(date_val)
  return(date_val)
}

read_file_feed2go <- function(in_filename,in_dir="."){
  df <- read.table(file.path(in_dir,in_filename),sep=";",fill=T,head=T)
}

summary_data_table <- function(list_lf){
  
  #list_df <- lapply(list_lf_r[[1]],read_file_feed2go,out_dir)
  list_df <- lapply(list_lf,read_file_feed2go,out_dir)
  #lapply(list_df,summary_table_df)
  dim_df <- dim_surveys_df(list_df)
  dim_df$filename <- basename(list_lf)
  dim_df$zip_file <- dirname(list_lf)
  #View(dim_df)
  
  ### Prepare return object
  obj_summary <- list(dim_df,list_df)
  names(obj_summary)<- c("dim_df","list_df")
  return(obj_summary)
}

dim_surveys_df <- function(list_df){
  dim_df<- (lapply(list_df,function(x){data.frame(nrow=dim(x)[1],ncol=dim(x)[2])}))
  dim_df <- do.call(rbind,dim_df)
  #View(dim_df)
  return(dim_df)
}

#################################  END OF SCRIPT  ##################################
