############### SESYNC Research Support: Fisheries and food security ########## 
## Importing and processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 07/05/2017
## AUTHORS: Benoit Parmentier 
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: developing options to combine by column based on dir
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

function_processing_data <- "processing_data_magadascar_fisheries_functions_07052017.R" #PARAM 1
script_path <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/scripts" #path to script #PARAM 
source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.

############################################################################
#####  Parameters and argument set up ###########

in_dir <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/workflow_preprocessing/data" #local bpy50 , param 1
out_dir <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/workflow_preprocessing/outputs" #param 2

num_cores <- 2 #param 8
create_out_dir_param=TRUE # param 9

out_suffix <-"processing_fisheries_magadascar_07052017" #output suffix for the files and ouptut folder #param 12
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

#debug(extract_date_feed2go)
#extract_date_feed2go(df_zip$file_zip[1])
list_date <- lapply(df_zip$file_zip,FUN=extract_date_feed2go)
df_zip$date <- unlist(list_date)

#reorder by date

#head(df_zip)
#class(ymd((df_zip$date)))
#class((df_zip$date))
df_zip$date <- ymd(df_zip$date) #coerce to date using lubridate function, year-month-day format
df_zip <- arrange(df_zip, df_zip$date) #order by date using dplyr function

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
  

### Reading in all the datasets and summarizing information

#quick test of reading in some data
#undebug(summary_data_table)
test_summary <- summary_data_table(list_lf_r[[1]])
names(test_summary)

#list_obj_summary <- lapply(list_lf_r[1:2],summary_data_table)
list_obj_summary <- mclapply(list_lf_r, 
                             FUN=summary_data_table,
                             mc.preschedule = F,
                             mc.cores = num_cores)

#> warnings()
#Warning message:
#  In mclapply(list_lf_r, FUN = summary_data_table, mc.preschedule = F,  ... :
#                11 function calls resulted in an error
              
#30x46
length(list_obj_summary)

list_dim_df <- mclapply(1:length(list_obj_summary),
                        FUN=function(i){list_obj_summary[[i]]$dim_df},
                        mc.preschedule = F,
                        mc.cores = num_cores)

test_dim_df <- do.call(rbind,list_dim_df)
dim(test_dim_df)
View(test_dim_df)

out_filename <- paste0("summary_table_df_",out_suffix,".txt")
write.table(test_dim_df,file= file.path(out_dir,out_filename),sep=",")

#undebug(summary_data_table)
test_summary <- summary_data_table(list_lf_r[[1]])
names(test_summary)

## To combine data: use
#1) extracted names from the file names (use first char??)

#undebug(combine_by_surveys)
#list_filenames <- test_dim_df$filename
list_filenames <- file.path(test_dim_df$zip_file,test_dim_df$filename)
basename(list_filenames)  
list_in_dir_zip <- unique(dirname(list_filenames))


### First go through all the folders/dirs
#for(i in 1:in_dir_zip){
#  if 1,2,3
#  
#}


list_combined_df_file_ID <- strsplit(list_filenames," ")

list_combined_df_file_ID[[2]]
list_ID_char <- mclapply(1:length(list_combined_df_file_ID),
                         FUN=function(i){list_combined_df_file_ID[[i]][1]},
                         mc.preschedule = F,
                         mc.cores = num_cores)

surveys_names <- unique(unlist(list_ID_char))
surveys_names <- grep("Error",surveys_names,invert=T,value=T)

#2) Using survey names pre-given
### Survey names:
#Fahasalamana
#Fahasalamana isanbolana
#Karazan-tsakafo 
#Laoko 
#Measure Sakafo
#Mpanjono
#Vola isambolana


#grep("Karazan-tsakafo",list_filenames,value = T)

#, Laoko, Mpanjono
#combine_survey_by_colum <- 
  
suvey_names_updated <-  c("Fahasalamana",
                          "Fahasalamana isanbolana",
                           "Karazan-tsakafo", 
                           "Laoko", 
                           "Measure Sakafo",
                           "Mpanjono",
                           "Vola isambolana")

#debug(combine_by_surveys)
#list_survey_df_filename <- combine_by_surveys(list_filenames,surveys_names=NULL,num_cores,out_suffix,out_dir)
list_survey_df_filename <- combine_by_surveys(list_filenames,surveys_names=suvey_names_updated,num_cores,combine_by_dir=T,out_suffix,out_dir)

#> test <- combine_by_surveys(list_filenames,surveys_names,num_cores,out_suffix,out_dir)
#Warning message:
#  In mclapply(1:length(surveys_names), FUN = combine_by_id_survey,  :
#                7 function calls resulted in an error
#> test
#NULL

survey2_df <- read.table(list_survey_df_filename[2],sep=",",header=T,check.names = F)

############################ END OF SCRIPT #####################################
