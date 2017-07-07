############### SESYNC Research Support: Fisheries and food security ########## 
## Functions used in the processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 07/07/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: testing options to combine by column based on in_dir, survey id and month
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
    #df <- read.table(in_filename,sep=";",fill=T,head=T, fileEncoding = "UTF-8")
    df <- read.table(in_filename,sep=";",fill=T,header=T, stringsAsFactors = F)
    
  }else{
    df <- read.table(file.path(in_dir,in_filename),sep=";",fill=T,header=T)
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
  #
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
  lapply(list_df,function(x){inherits(x,"try-error")})
  #sum(unlist(lapply(list_df,function(x){inherits(x,"try-error")})))
  #0 #this means no try-error
  
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

combine_by_surveys<- function(list_filenames,surveys_names,num_cores,combine_by_dir=T,out_suffix="",out_dir="."){
  #
  #
  #
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
  
  #browser()
  
  if(combine_dir==T){
    
    #dirname(list_filenames)
    #basename(list_filenames[1:10])
    ### First go through all the folders/dirs
    
    #1. find "avy"
    #2. find word before and after (number: 1,2 or 3)
    #3. find month Aout, July, June 
    #other option find the month by examining 2nd word from the end after eliminating the extension
    
    list_combined_df_file_ID <- strsplit(list_filenames," ")
    list_in_dir_zip <- unique(dirname(list_filenames))
    
    #add identifier for combined by column
    #
    for(i in 1:length(list_in_dir_zip)){
      in_dir_zip <- list_in_dir_zip[i]
      #if 1,2,3
      list_filenames_subset <- grep(in_dir_zip,list_filenames,invert=F,value=T)
      #keywords <- c("1", "2", "3", "avy","Aout","July","June")
      #keywords <- c("1", "2", "3", "avy")
      
      keywords <- c("avy")
      English_months <- month.name
      French_months <- c("Janvier","Fevrier","Mars","Avril","Main","Juin","Juillet","Aout",
                         "Septembre","Octobre","Novembre","Decembre")
      keywords_month <- c(English_months,French_months)
      
      df_strings <- as.data.frame(sapply(keywords, regexpr, list_filenames_subset, ignore.case=TRUE))
      df_strings$filenames <- basename(list_filenames_subset)
      
      df_strings_month <- as.data.frame(sapply(keywords_month, regexpr, list_filenames_subset, ignore.case=TRUE))
      
      ## repeat for each row

      keywords <- surveys_names 
      df_strings_surveys <- as.data.frame(sapply(keywords, regexpr, list_filenames_subset, ignore.case=TRUE))
      survey_val <- unlist(lapply(1:nrow(df_strings_surveys),FUN=get_val_present,df_strings=df_strings_surveys))
      
      month_val<- unlist(lapply(1:nrow(df_strings_month),FUN=get_val_present,df_strings=df_strings_month))
      df_strings$filenames <- basename(list_filenames_subset)
      df_strings$month_val <- month_val
      df_strings$survey_id <- survey_val
      #df_strings$survey_id <- ?
      View(df_strings)
      
      #concatenate survey id with month, this is the unit used to combine
      #drop all rows without "avy"
      dim(df_strings)
      df_test <- df_strings[df_strings$avy>0,]
      dim(df_test)
      
      df_test$group_id <- paste0(df_test$survey_id,"_",df_test$month_val)
      group_val <- unique(df_test$group_id)
      
      #for(i in 1:length(group_val)){
        
      df_test$filenames <- file.path(out_dir,in_dir_zip,df_test$filenames)  
      survey_combine_by_column <- function(group_selected,df_data)  
        #group_selected <- group_val[j]
        
        df_subset <- subset(df_test,df_test$group_id==group_selected)
        #cbind(...) #based on 1,2,3
        #splitted_filenames <- strsplit(df_subset$filenames," ")
        #df_splitted <- as.data.frame(do.call(rbind,splitted_filenames))
        #df_subset <- cbind(df_subset,df_splitted )
        #grep(df_test$filenames,"avy",)
        #df_subset <- sort(df_subset$filenames)
        #y <- df_subset
        #y <- y[with(y, order(filenames)), ]
        df_subset <- df_subset[with(df_subset, order(filenames)), ]
        lapply(df_subset$filenames,function(x){read.table(x)})
        #cbind each of df_subset
        return(df_subset)
        
      }
      ### Using df_strings table
      ### 1) Flag for avy surrounded by 1, 2, 3
      ## 2) Find month value: can be anything from (january to december in English to Janvier to december in French)
      ## 3) For each survey + month value, group 
      ## 4) combined the groups


    }
    #
    #
  }
  
  ##### Now loop through and bind data.frames
  #browser()
  
  #debug(combine_by_id_survey)
  test_filename<- combine_by_id_survey(2,surveys_names,list_filenames,out_suffix,out_dir)
  #test_df <- read.table(test_filename,sep=",",header=T,check.names = F)
  # Start writing to an output file
  if(file.exists("warnings_messages.txt")){
    file.remove("warnings_messages.txt")
  }
  sink('warnings_messages.txt')
  warnings()
  # Stop writing to the file
  sink()
  
  list_survey_df <- mclapply(1:length(surveys_names),
                             FUN=combine_by_id_survey,
                             surveys_names = surveys_names,
                             list_filenames=list_filenames,
                             out_suffix=out_suffix,
                             out_dir=out_dir,
                             mc.preschedule = F,
                             mc.cores = num_cores)
  
  #index_error <- lapply(list_survey_df,FUN=function(x){!inherits(x,"try-error")})
  #test_df <- read.table(test_filename,sep=",",header=T,check.names = F)
  list_survey_df <- file.path(out_dir,unlist(list_survey_df))
  
  #### prepare obj
  

  return(list_survey_df)
}


#################################  END OF SCRIPT  ##################################
