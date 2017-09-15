############### SESYNC Research Support: Fisheries and food security ########## 
## Functions used in the processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 09/13/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: data processing issues: follow up on meeting with Erwin and Jessica 
##
## Links to investigate:

###################################################
#
#Function in this script:

#[1] "combine_by_dir_surveys_part" 
#[2] "combine_by_id_survey" : helper, carries out the process of combining by row table using survey name      
#[3] "combine_by_surveys" : main function to generate data.frame by survey name          
#[4] "dim_surveys_df": helper, extracts dimension of data.frames for metadata info             
#[5] "extract_date_feed2go": helper function, extracts dates from feed2go file names        
#[6] "get_val_present": find if values/keywords are present in strings (file names): e.g. "avy"              
#[7] "read_file_feed2go": reading in raw files from feed2go, note the ";" separator          
#[8] "summary_data_table" : generate summary of data collected for the survey (over >200)         
#[9] "survey_combine_by_column": helper function, carries out the process of combining by column using group_id   


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
library(ggplot2)                             # Plotting package using grammar of graphics
library(rowr)                                # Contains cbind.fill


###### Functions used in this script sourced from other files


extract_date_feed2go <- function(string_val){
  
  #### Extract dates from feed2go files
  
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
  #
  #Problem with coding: some of the inputs have a latin1 instead of UTF-8
  #A possibility is to generate a full function to guess the file encoding
  #codepages <- setNames(iconvlist(), iconvlist())
  #This is not implemented here. If a file read using UTF-8 returns a number
  #of rows equals to zero then the function will attempt to read the file
  #again using "latin1" file encoding.
  
  ##### Begin script ######
  
  if(is.null(in_dir)){
    #df <- read.table(in_filename,sep=";",fill=T,head=T, fileEncoding = "UTF-8")
    df <- try(read.table(in_filename,sep=";",fill=T,
                         header=T, quote = "",
                         stringsAsFactors = F,fileEncoding="UTF-8"))
    if(nrow(df)==0){
      df <- try(read.table(in_filename,sep=";",fill=T,
                           header=T,quote = "",
                           stringsAsFactors = F,fileEncoding="latin1"))
    }
    if(nrow(df==1)){
      df <- try(read.table(in_filename,sep=";",
                           header=T,stringsAsFactors = F))
    }
    
    #This error is related to some files being encoded in ASCII "latin1", this
    #encoding is being replaced by the newer UTF-8 iso standard
    #In read.table the fileEncoding option is replaced from "UTF-8" by "latin1", 
    #Adding encoding resolved the following error:
    #Error in type.convert(data[[i]], as.is = as.is[i], dec = dec, numerals = numerals,  : 
    #invalid multibyte string at '<b0>'
    
  }else{
    #df <- try(read.table(file.path(in_dir,in_filename),sep=";",fill=T,header=T,
    #                     stringsAsFactors = F,fileEncoding="latin1"))
    df <- try(read.table(file.path(in_dir,in_filename),sep=";",fill=T,
                         header=T,quote = "",
                         stringsAsFactors = F,fileEncoding="UTF-8"))
    if(nrow(df)==0){
      df <- try(read.table(file.path(in_dir,in_filename),sep=";",fill=T,
                           header=T,quote = "",
                           stringsAsFactors = F,fileEncoding="latin1"))
    }
  }
  return(df)
}

summary_data_table <- function(list_lf){
  
  ### This function lists all input files from the Magadascar suvey and generate tables of summary.
  
  
  ##### Begin script #####
  
  #test_df <- read.table(list_lf[[13]],sep=";")
  #debug(read_file_feed2go)
  test_df <- read_file_feed2go(list_lf[1])
  #debug(read_file_feed2go)
  #list_df <- lapply(list_lf[13],try(read_file_feed2go),out_dir)
  list_df <- lapply(list_lf,read_file_feed2go,out_dir)
  
  test_df2 <- read.table(list_lf[1],sep=";",header=T)
  
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
  #Generate dimension of list of data.frame
  #This is useful metadata to summarize data table collected during the survey
  #
  
  dim_df<- (lapply(list_df,function(x){data.frame(nrow=dim(x)[1],ncol=dim(x)[2])}))
  dim_df <- do.call(rbind,dim_df)
  #View(dim_df)
  return(dim_df)
}

combine_by_id_survey<- function(i,surveys_names,list_filenames,num_cores=1,out_suffix="",out_dir="."){
  # This function combines input files based on survey names id
  #
  #
  
  #### Beging script #####
  
  survey_selected <- surveys_names[i]
  #"Fahasalamana isanbolana", there is a matching problem 
  #survey_selected <- "Fahasalamana_isanbolana"
  #survey_selected <- "Fahasalamana_isanbolana"
  
  list_lf <- grep(survey_selected, list_filenames,value=T)
  list_lf <- grep(survey_selected,list_filenames_original,value=T)
  list_dir <- test_dim_df$zip_file[grep(survey_selected,list_filenames,value=F)]
  ##change on 07/27 double path...
  #list_lf <- file.path(out_dir,list_dir,list_lf)
  
  ###
  #list_df <- lapply(list_lf,read_file_feed2go) # use default option
  #debug(read_file_feed2go)
  #list_df <- read_file_feed2go(list_lf[1]) # use default option
  
  list_df <- mclapply(list_lf,
                      read_file_feed2go,
                      mc.preschedule = FALSE,
                      mc.cores = num_cores) # use default option
  
  test_error <- lapply(list_df,function(x){inherits(x,"try-error")})
  #sum(unlist(lapply(list_df,function(x){inherits(x,"try-error")})))
  #0 #this means no try-error
  
  dim_list_df <-lapply(list_df,FUN=function(x){dim(x)})
  dim_df <- as.data.frame(do.call(rbind,dim_list_df))
  range(dim_df$V2,na.rm = T)  
  histogram(dim_df$V2)
  View(dim_df)
  
  df_survey <- do.call(rbind.fill,list_df)

  
  #for(list_I)
  ### Note there could be replicated information!!!, screen for identical rows later on...
  #add id from filename before binding...
  names(list_df) <- list_lf
  
  #repeat filename to fill in new columns with ID
  list_column_filename <- lapply(1:length(list_df),
                                 FUN=function(i,x,y){rep(y[i],nrow(x[[i]]))},x=list_df,y=names(list_df))
  
  df_survey$filename <- unlist(list_column_filename) #adding identifier for table
  out_filename <- paste0(surveys_names[i],"_",out_suffix,".txt")
  write.table(df_survey,file.path(out_dir,out_filename),sep=",",row.names = F)
  
  return(out_filename)
}

get_val_present <- function(i,df_strings){
  row_df <- df_strings[i,]
  #row_df[row_df > 0]
  val_present <- names(row_df)[row_df > 0] #select names with value present!, -1 means not present
  return(val_present)
}

survey_combine_by_column <- function(out_filenames_selected,df_data){
  #Combine survey by column based on list of files defined in data.frame
  #
  
  ###### Begin script ####
  
  df_subset <- subset(df_data,df_data$out_filenames==out_filenames_selected)
  
  df_subset <- df_subset[with(df_subset, order(filenames)), ]
  list_df <-lapply(df_subset$filenames,FUN=function(x){read.table(x,sep=";",header=T)})
  
  df_subset_combined <- do.call(cbind.fill,list_df) # Note that cbind.fill is found in rowr package
  #df_subset_combined <- do.call(rbind.fill,list_df)
  write.table(df_subset_combined,out_filenames_selected,sep=";")
  
  return(out_filenames_selected)
  
}

combine_by_dir_surveys_part <- function(in_dir_zip,surveys_names,list_filenames){
  # This functions combines files of surveys using survey names, common dir and date information.
  # The first step is to identify for each directory:
  # 1) the existence of multiple parts for each month and survey - this is done by identifying the presence of "avy" 
  #    and months in French or English
  # 2) Grouping files by survey names, common dates and common directory
  # 3) Binding by column (wide format) the information
  # 4) return the list of files were combine by column for further processing
  
  ##### INPUTS
  # 1) in_dir_zip: input directory used to group files to combine
  # 2) surveys_names: survey id keywords used in the combine process
  # 3) list_filenames: input file names from survey app
  ##### OUTPUTS
  # 1) list_filenames2: output file name combined by survey names and parts "avy" as well as months
  
  ###### Begin script ##########
  
  ###############
  ### Step 1:  Subset relevant columns
  
  list_filenames_subset <- grep(in_dir_zip,list_filenames,invert=F,value=T)

  ## Find the presence of keyword "avy" which signal the present of mulitple parts in surveys
  keywords <- c("avy")
  
  ## Find the presence of multiple month names to group parts by unique months (names may be in French or English)
  English_months <- month.name
  French_months <- c("Janvier","Fevrier","Mars","Avril","Main","Juin","Juillet","Aout",
                     "Septembre","Octobre","Novembre","Decembre")
  keywords_month <- c(English_months,French_months)
  
  ##############
  ###  Step 2: Generate data.frame with combination  "avy" keywords from list of file names from the survey app
  df_strings <- as.data.frame(sapply(keywords, regexpr, list_filenames_subset, ignore.case=TRUE))
  df_strings$filenames <- basename(list_filenames_subset)
  
  ##############
  ### Step 3: data.frame with combination months keywords from list of file names from the survey app
  df_strings_month <- as.data.frame(sapply(keywords_month, regexpr, list_filenames_subset, ignore.case=TRUE))
  
  #############
  ### Step 4: data.frame with combination survey_name keywords from list of file names from the survey app
  
  keywords <- surveys_names 
  df_strings_surveys <- as.data.frame(sapply(keywords, regexpr, list_filenames_subset, ignore.case=TRUE))
  
  #############
  ### Step 5: Combine data.frame keywords in a common data.frame "df_strings"
  
  survey_val <- unlist(lapply(1:nrow(df_strings_surveys),FUN=get_val_present,df_strings=df_strings_surveys))
  month_val<- unlist(lapply(1:nrow(df_strings_month),FUN=get_val_present,df_strings=df_strings_month))
  
  #### Add month and survey info to df_strings
  
  df_strings$filenames <- basename(list_filenames_subset)
  df_strings$month_val <- month_val
  df_strings$survey_id <- survey_val
  
  ##############
  ### Step 6: Generate group_id used to combine files by columns
  
  ##Not using "_" to see if we can resolve the issue of indentification of survey names
  df_strings$group_id <- paste0(df_strings$survey_id," ",df_strings$month_val)
  
  #concatenate survey id with month, this is the unit used to combine
  #drop all rows without "avy"

  df_strings$out_filenames <- file.path(out_dir,paste0(df_strings$group_id,"_",in_dir_zip,".csv"))
  
  df_strings[df_strings$avy==-1,]$out_filenames <- file.path(out_dir,in_dir_zip,df_strings[df_strings$avy==-1,]$filenames)
  
  ################
  #### Step 7: Select data to combine: only if "avy" is found in the name
  
  df_test <- df_strings[df_strings$avy>0,]
  df_test$filenames <- file.path(out_dir,in_dir_zip,df_test$filenames)
  
  group_val <- unique(df_test$group_id)
  list_out_filenames <- unique(df_test$out_filenames)

  ################
  #### Step  8: Combine files by group id
  
  #undebug(survey_combine_by_column)
  list_df_col_combined <- survey_combine_by_column(list_out_filenames[1],df_data=df_test)
  
  list_df_col_combined <- lapply(group_val,FUN= survey_combine_by_column,df_data=df_test)
  names(list_df_col_combined)<- list_out_filenames
  
  list_filenames2 <- unique(df_strings$out_filenames)
  
  return(list_filenames2)
}


### THis is the main function using all others above

combine_by_surveys<- function(list_filenames,surveys_names,num_cores,combine_by_dir=T,out_suffix="",out_dir="."){
  # This functions combines data based on the survey names. 
  # Data is combined by row using input files.
  #
  ##### INPUTS
  # 1) in_dir_zip: input directory used to group files to combine
  # 2) surveys_names: survey id keywords used in the combine process
  # 3) list_filenames: input file names from survey app
  ##### OUTPUTS
  # 1) list_filenames2: output file name combined by survey names and parts "avy" as well as months
  
  ####### Begin script ##########
  
  ##########
  ## Step 1
  
  ### If no survey names provided then generate it from the list of files
  if(is.null(surveys_names)){
    
    list_combined_df_file_ID <- strsplit(list_filenames," ")
    
    #list_combined_df_file_ID[[2]]
    list_ID_char <- mclapply(1:length(list_combined_df_file_ID),
                             FUN=function(i){list_combined_df_file_ID[[i]][1]},
                             mc.preschedule = F,
                             mc.cores = num_cores)
    
    surveys_names <- unique(unlist(list_ID_char))
    surveys_names <- grep("Error",surveys_names,invert=T,value=T)
  }
  
  ##########
  ## Step 2
  
  browser()
  
  if(combine_by_dir==T){
    
    #dirname(list_filenames)
    #basename(list_filenames[1:10])
    ### First go through all the folders/dirs
    
    #1. find "avy"
    #2. find word before and after (number: 1,2 or 3)
    #3. find month Aout, July, June 
    #other option find the month by examining 2nd word from the end after eliminating the extension
    
    list_combined_df_file_ID <- strsplit(list_filenames," ")
    list_in_dir_zip <- unique(dirname(list_filenames))
    
    #undebug(combine_by_dir_surveys_part)
    #browser()
    
    test_filenames2 <- combine_by_dir_surveys_part(list_in_dir_zip[1],
                                                   surveys_names = surveys_names,
                                                   list_filenames=list_filenames)
    
    list_filenames2 <- mclapply(list_in_dir_zip,
                                FUN=combine_by_dir_surveys_part,
                                surveys_names = surveys_names,
                                list_filenames=list_filenames,
                                mc.preschedule = FALSE,
                                mc.cores = num_cores)

    
    #Now remove filenames that have been cbind
    #tt <- unlist(list_filenames2)
    list_filenames2 <- unlist(list_filenames2)
    #
  }else{
    #list_filenames2 <- list_filenames
    #list_lf <- file.path(out_dir,list_dir,list_lf)
    list_filenames2 <- file.path(out_dir,list_filenames)
  }
  
  ###########
  ### Step 3
  
  ##### Now loop through and bind data.frames
  browser()
  
  #undebug(combine_by_id_survey)
  ## Suvey_names 2 does not work: need to check when using option combine by dir
  #test_filename<- combine_by_id_survey(2,surveys_names,list_filenames2,out_suffix,out_dir)
  test_filename <- combine_by_id_survey(3,surveys_names,list_filenames2,num_cores,out_suffix,out_dir)
  
  #test_df <- read.table(test_filename,sep=",",header=T,check.names = F)
  # Start writing to an output file
  if(file.exists("warnings_messages.txt")){
    file.remove("warnings_messages.txt")
  }
  sink('warnings_messages.txt')
  warnings()
  # Stop writing to the file
  sink()
  
  num_cores_tmp <- 1 #set to 1 since we are already using mclapply and could go over the number of cores
  list_survey_df <- mclapply(1:length(surveys_names),
                             FUN=combine_by_id_survey,
                             surveys_names = surveys_names,
                             list_filenames=list_filenames2,
                             num_cores= num_cores_tmp,
                             out_suffix=out_suffix,
                             out_dir=out_dir,
                             mc.preschedule = F,
                             mc.cores = num_cores)
  
  #index_error <- lapply(list_survey_df,FUN=function(x){!inherits(x,"try-error")})
  #test_df <- read.table(test_filename,sep=",",header=T,check.names = F)
  list_survey_df <- file.path(out_dir,unlist(list_survey_df))
  
  #### prepare return obj

  return(list_survey_df)
}


#################################  END OF SCRIPT  ##################################
