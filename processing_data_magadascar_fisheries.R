############### SESYNC Research Support: Fisheries and food security ########## 
## Importing and processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 01/31/2018
## AUTHORS: Benoit Parmentier 
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: clean up and testing
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
library(rowr)                                # Contains cbind.fill
library(car)

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

function_processing_data <- "processing_data_madagascar_fisheries_functions_01312018.R" #PARAM 1
script_path <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/scripts" #path to script #PARAM 
source(file.path(script_path,function_processing_data)) #source all functions used in this script 1.

############################################################################
#####  Parameters and argument set up ###########

in_dir <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/workflow_preprocessing/data" #local bpy50 , param 1
out_dir <- "/nfs/bparmentier-data/Data/projects/Fisheries_and_food_security/workflow_preprocessing/outputs" #param 2

num_cores <- 2 #param 8
create_out_dir_param=TRUE # param 9

out_suffix <-"processing_fisheries_madagascar_01312018" #output suffix for the files and ouptut folder #param 12
unzip_files <- T #param 15

survey_names_updated <-  c("Fahasalamana",
                          "Fahasalamana isanbolana",
                          "Karazan-tsakafo", 
                          "Laoko", 
                          "Measure Sakafo",
                          "Mpanjono",
                          "Vola isambolana")

combine_by_dir <- TRUE #if TRUE then examine in each directory if files are split (keyword "avy") in parts and/or month
#1. find "avy"
#2. find word before and after (number: 1,2 or 3)
#3. find month Aout, July, June 
#other option find the month by examining 2nd word from the end after eliminating the extension

combine_option <- "byrow" #This is the option to combine avy byrow or column

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
lf_zip <- unlist(lapply(lf_dir,
                        function(x){list.files(pattern=paste("*.zip$",sep=""),
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

##if unzip_files is TRUE
if(unzip_files==T){
  nb_zipped_file <- length(lf_zip)
  list_lf_r <- vector("list",length=nb_zipped_file)
  for(i in 1:nb_zipped_file){
    out_dir_zip <- sub(".zip","",(basename(lf_zip[[i]])))
    lf_r <- lapply(lf_zip[[i]], unzip,exdir= out_dir_zip)
    lf_r <- list.files(pattern="*csv$",path=out_dir_zip,full.names = T)
    list_lf_r[[i]] <- lf_r
  }
}

names(list_lf_r) <- basename(lf_zip)

#undebug(summary_data_table)
#list_obj_summary_test <- summary_data_table(list_lf_r[[1]]) 

list_obj_summary <- mclapply(list_lf_r, 
                             FUN=summary_data_table,
                             mc.preschedule = F,
                             mc.cores = num_cores)

length(list_obj_summary)
class(list_obj_summary)
list_dim_df <- mclapply(1:length(list_obj_summary),
                        FUN=function(i){list_obj_summary[[i]]$dim_df},
                        mc.preschedule = F,
                        mc.cores = num_cores)

summary_df <- do.call(rbind,list_dim_df)
dim(summary_df)
View(summary_df)
#note we have 1369 files instead of 1006 before.
out_filename <- paste0("summary_table_df_",out_suffix,".txt")
write.table(summary_df,file= file.path(out_dir,out_filename),sep=",")

#> range(summary_df$ncol)
#[1]   1 153
#> table(summary_df$ncol)

#1  77  79  87 133 134 138 141 143 145 148 149 151 153 
#12  80   8  41 122 123 121 121 122  93 154 129 121 122 
### --> May be useful to check the 12 files that only have one column!!!!

#> table(summary_df$nrow)


#1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#41  36  19  21  25  38  20  28  20  18  10   5   6  15  28  10  23   7   5  24 
#21  22  23  24  25  26  27  28  30  31  32  33  34  35  36  37  38  39  40  41 
#9  19  37  26  30  28  31  37   6  25   4  24  12   8   9  17  19  26  10   8 
#42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61 
#10   9   5  14   3  11   7  31 115  79  45  26  23  19  10   7   2  16   2   9 
#62  63  64  65  66  67  71  72  74  77  82  86  89  92  96  98  99 104 105 106 
#4   4   2   4   5   3   5   2   1   4   1   2   3   4   1   6   3   1   2   4 
#107 112 113 115 117 119 121 123 124 125 127 128 131 132 135 136 138 139 144 146 
#3   4   5   1   3   3   5   2   2   2   3   3   1   3   3   3   5   3   2   5 
#147 148 152 155 156 159 163 164 166 169 170 173 175 195 223 
#2   1   1   1   3   1   1   1   1   1   1   1   1   1   3 

## To combine data: use
#1) extracted names from the file names (use first char??)

#undebug(combine_by_surveys)
#list_filenames <- test_dim_df$filename
list_filenames_original <-  file.path(summary_df$zip_file,summary_df$filename)
list_filenames <- file.path(summary_df$zip_file,summary_df$filename)
basename(list_filenames)  
list_in_dir_zip <- unique(dirname(list_filenames))
unique_filenames <- unique(basename(list_filenames))

#2) Using survey names pre-given
### Survey names:
#Fahasalamana
#Fahasalamana isanbolana
#Karazan-tsakafo 
#Laoko 
#Measure Sakafo
#Mpanjono
#Vola isambolana

#undebug(combine_by_surveys)
#list_survey_df_filename <- combine_by_surveys(list_filenames,surveys_names=NULL,num_cores,out_suffix,out_dir)
list_survey_df_filename <- combine_by_surveys(list_filenames,
                                              surveys_names=survey_names_updated,
                                              num_cores,
                                              combine_by_dir=combine_by_dir,
                                              combine_option=combine_option,
                                              out_suffix,
                                              out_dir)

survey2_df <- read.table(list_survey_df_filename[7],sep=",",header=T,check.names = F,fill=T)

survey2_df <- read.table(list_survey_df_filename[3],sep=",",header=T,check.names = F)

############################ END OF SCRIPT #####################################

###to look for:
Hi Benoit,

Thanks for updating the scripts, great work! I ran if for the new updated
dataset in October. The whole file runs, but I noticed the following issues:
  
  1) When compiling the "Mpanjono" surveys, I get the following error: All
inputs to rbind.fill must be data.frames (attached a screenshot)
2) The problematic dataset you flagged, "Karazan-tsakafo
June_Feed2Go_csv_20160630102140700_byrow.csv" is included in the dataset
but it's data is offset. It's not clear why, but given that it's a single
dataset I can try and fix that on my end.
3) Finally, I've noticed that some data entries have commas in them, which
means they are read as separate variables, leading to offsetting. I don't
know how we would deal with this, though maybe we can exploit the //" 
feature that delimits the data.

That being said, I think the script looks great. I feel comfortable
tackling 2) and 3) on my own. if you have time it'd be great if you could
look into 1).

Thanks!
  
  Erwin