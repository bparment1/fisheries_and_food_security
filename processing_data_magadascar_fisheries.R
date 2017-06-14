############### SESYNC Research Support: Fisheries and food security ########## 
## Importing and processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 06/14/2017
## AUTHORS: Benoit Parmentier and Elizabeth Daut 
## Version: 1
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: unzipping app data for specific month
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
library(lubridate)
library(dplyr)

###### Functions used in this script and sourced from other files

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

#Used to load RData object saved within the functions produced.
load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

### Other functions ####

#function_processing_data <- "processing_data_google_search_time_series_functions_06012017b.R" #PARAM 1
script_path <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/scripts" #path to script #PARAM 
#source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.

############################################################################
#####  Parameters and argument set up ###########

in_dir <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/workflow_preprocessing/data" #local bpy50 , param 1
out_dir <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/workflow_preprocessing/outputs" #param 2

num_cores <- 2 #param 8
create_out_dir_param=TRUE # param 9

out_suffix <-"processing_fisheries_magadascar_06142017" #output suffix for the files and ouptut folder #param 12
unzip_files <- T #param 15

############## START SCRIPT ############################

######### PART 0: Set up the output dir ################

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
  
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #can modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

### PART I READ AND PREPARE DATA #######
#set up the working directory
#Create output directory

lf_dir <- list.files(in_dir,full.names=T) #this is the list of folder with RAW data information
##Get zip files in each input RAW dir
lf_zip <- unlist(lapply(lf_dir,function(x){list.files(pattern=paste("*.zip$",sep=""),
                                                                  path=x,full.names=T)}))
#Record list of files to unzip and path directory
df_zip <- data.frame(file_zip=basename(lf_zip))
df_zip$dir <- dirname(lf_zip)

df_zip$file_zip <- as.character(df_zip$file_zip)
function(x){list_str <- strsplit(df_zip$file_zip,"_"); list_str[3][1:8]}

extract_date_feed2go <- function(string_val){
  list_str <- strsplit(string_val,"_"); 
  date_val <- list_str[[1]][3]
  #substr(x, start, stop)
  date_val <- substr(date_val, start=1, stop=8)
  date_val <- as.Date(date_val,format = "%Y%m%d")
  date_val <- as.character(date_val)
  return(date_val)
}

#debug(extract_date_feed2go)
#extract_date_feed2go(df_zip$file_zip[1])
list_date <- lapply(df_zip$file_zip,FUN=extract_date_feed2go)

df_zip$date <- unlist(list_date)

#reorder by date

#head(df_zip)
#class(ymd((df_zip$date)))
#class((df_zip$date))
df_zip$date <- ymd(df_zip$date)

df_zip <- arrange(df_zip, df_zip$date)

df_zip_fname <- file.path(out_dir,paste("df_zip","_",out_suffix,".txt",sep=""))
write.table(df_zip,file=df_zip_fname,sep=",")

###### unzip files:

#if unzip_files is TRUE
if(unzip_files==T){
  nb_zipped_file <- length(lf_zip)
  list_lf_r <- vector("list",length=nb_zipped_file)
  for(i in 1:nb_zipped_file){
    out_dir_zip <- sub(".zip","",(basename(lf_zip[[i]])))
    lf_r <- lapply(lf_zip[[i]], unzip,exdir= out_dir_zip)
    lf_r <- list.files(pattern="*csv$",path=out_dir_zip,full.names = T)
    #lf_r <- file.path(out_dir_zip,lf_r)
    list_lf_r[[i]] <- lf_r
  }
}
  

### then read in

### Add quote="" otherwise EOF warning and error in reading
#df <- read.table(file.path(df_zip$dir[1],df_zip$file_zip[1]),sep=",",fill=T,header=F)

read_file_feed2go <- function(in_filename,in_dir="."){
  df <- read.table(file.path(in_dir,in_filename),sep=";",fill=T,head=T)
}

#quick test of reading in some data
list_df <- lapply(list_lf_r[[1]],read_file_feed2go,out_dir)



#############
