############### SESYNC Research Support: Fisheries and food security ########## 
## Functions used in the processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 06/19/2017
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


extract_date_feed2go <- function(string_val){
  list_str <- strsplit(string_val,"_"); 
  date_val <- list_str[[1]][3]
  #substr(x, start, stop)
  date_val <- substr(date_val, start=1, stop=8)
  date_val <- as.Date(date_val,format = "%Y%m%d")
  date_val <- as.character(date_val)
  return(date_val)
}

read_file_feed2go <- function(in_filename,in_dir=NULL){
  ##Quick function to read in the feed2go data
  #if in_dir is null then the path is contained in the filename
  
  if(is.null(in_dir)){
    df <- read.table(in_filename,sep=";",fill=T,head=T)
  }else{
    df <- read.table(file.path(in_dir,in_filename),sep=";",fill=T,head=T)
  }
  return(df)
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

combine_by_id_survey<- function(i,surveys_names,list_filenames,out_suffix,out_dir){
  #
  #
  
  list_lf <- grep(surveys_names[i],list_filenames,value=T)
  list_dir <- test_dim_df$zip_file[grep(surveys_names[i],list_filenames,value=F)]
  list_lf <- file.path(out_dir,list_dir,list_lf)
  
  ###
  list_df <- lapply(list_lf,read_file_feed2go) # use default option
  
  df_survey <- do.call(rbind.fill,list_df)
  #for(list_I)
  ### Note there could be replicated information!!!, screen for identical rows later on...
  #add id from filename before binding...
  names(list_df) <- list_lf
  
  #repeat filename to fill in new columns with ID
  list_column_filename <- lapply(1:length(list_df),
                                 FUN=function(i,x,y){rep(y[i],nrow(x[[i]]))},x=list_df,y=names(list_df))
  
  df_survey$filename <- unlist(list_column_filename) #adding identifier for tile
  out_filename <- paste0(surveys_names[i],out_suffix,".txt")
  write.tabel(df_survey,file.path(out_dir,out_filename))
  
  return(out_filename)
}


combine_by_surveys<- function(list_filenames,surveys_names,num_cores,out_suffix,out_dir){
  #
  #
  #
  if(is.null(surveys_names)){
    
    list_combined_df_file_ID <- strsplit(list_filenames," ")
    
    list_combined_df_file_ID[[2]]
    list_ID_char <- mclapply(1:length(list_combined_df_file_ID),
                             FUN=function(i){list_combined_df_file_ID[[i]][1]},
                             mc.preschedule = F,
                             mc.cores = num_cores)
    
    surveys_names <- unique(unlist(list_ID_char))
    
    list_filenames <- test_dim_df$filename
  }
  
  ##### Now loop through and bind data.frames
  
  #combine_by_id_survey<- function(i,surveys_names,list_filenames,out_suffix,out_dir){
  
  list_survey_df <- mclapply(1:length(surveys_names),
                             FUN=combine_by_id_survey,
                             list_filenames=list_filenames,
                             out_suffix=out_suffix,
                             out_dir=out_dir,
                             mc.preschedule = F,
                             mc.cores = num_cores)
  
  #### prepare obj
  
  
  return()
}


#################################  END OF SCRIPT  ##################################
