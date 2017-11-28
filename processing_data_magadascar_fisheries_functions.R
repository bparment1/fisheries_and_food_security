############### SESYNC Research Support: Fisheries and food security ########## 
## Functions used in the processing data from survey for the fisheries project at SESYNC.
## 
## DATE CREATED: 06/06/2017
## DATE MODIFIED: 11/09/2017
## AUTHORS: Benoit Parmentier 
## Version: 1
## PROJECT: Fisheries by Jessica Gephart
## ISSUE: 
## TO DO:
##
## COMMIT: change to readfeed2go warning handling option and chekc.names, impact outputs
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

#[10] recode_val_fun
#[11] recode_string_fun
#[12] add_single_quote

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
library(car)

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
                         stringsAsFactors = F,
                         fileEncoding="UTF-8",
                         check.names=F))
    if(nrow(df)==0){
      df <- try(read.table(in_filename,sep=";",fill=T,
                           header=T,quote = "",
                           stringsAsFactors = F,
                           fileEncoding="latin1",
                           check.names=F))
    }
    if(nrow(df)==1){
      df <- try(read.table(in_filename,sep=";",
                           header=T,
                           stringsAsFactors = F,
                           check.names=F))
    }
    
    #This error is related to some files being encoded in ASCII "latin1", this
    #encoding is being replaced by the newer UTF-8 iso standard
    #In read.table the fileEncoding option is replaced from "UTF-8" by "latin1", 
    #Adding encoding resolved the following error:
    #Error in type.convert(data[[i]], as.is = as.is[i], dec = dec, numerals = numerals,  : 
    #invalid multibyte string at '<b0>'
    
    ### Handling warning message:
    warning_messages <- warnings()
    str_warning <- "incomplete final line found by readTableHeader"
    #str_warning <- "tt"
    match_val <- grepl(str_warning,names(warning_messages))
    if(sum(match_val)>0){ #warning message macthinb
      df <- try(read.table(in_filename,
                                sep=";",
                                fill=T,
                                header=T, 
                                quote = "",
                                stringsAsFactors = F,
                                #fileEncoding="UTF-8",
                                check.names = F))
    }

  }
  
  if(!is.null(in_dir)){
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
    
    if(nrow(df)==1){
      df <- try(read.table(file.path(in_dir,in_filename),sep=";",
                           header=T,stringsAsFactors = F))
    }
    
    ### Handling warning message:
    warning_messages <- warnings()
    str_warning <- "incomplete final line found by readTableHeader"
    #str_warning <- "tt"
    match_val <- grepl(str_warning,names(warning_messages))
    if(sum(match_val)>0){ #warning message macthinb
      df <- try(read.table(file.path(in_dir,in_filename),
                           sep=";",
                           fill=T,
                           header=T, 
                           quote = "",
                           stringsAsFactors = F,
                           #fileEncoding="UTF-8",
                           check.names = F))
      
    }
  }
  
  ### return file read in
  return(df)
}

summary_data_table <- function(list_lf){
  
  ### This function lists all input files from the Madagascar survey 
  ### and generate tables of summary.
  ### Summary includes number of rows and columns.
  
  
  ##### Begin script #####
  
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
  #Generate dimension of list of data.frame
  #This is useful metadata to summarize data table collected during the survey
  #
  
  dim_df<- (lapply(list_df,function(x){data.frame(nrow=dim(x)[1],ncol=dim(x)[2])}))
  dim_df <- do.call(rbind,dim_df)
  #View(dim_df)
  return(dim_df)
}

add_single_quote <- function(char_val){
  #Quick function to add single quote
  char_val <- paste0("'",char_val,"'")
  return(char_val)
}

recode_string_fun <- function(string_input,string_val,string_ref){
  #Take a string input and replaces values given string_values matching reference values (to be recoded)
  #string_input: input vector of character type
  #string_val: values to be changed (from)
  #string_ref: values matched (to)
  
  ## Begin ##
  
  string_recode <- paste0(add_single_quote(string_val),"=",add_single_quote(string_ref))
  string_recode <- paste(string_recode,collapse=";")
  string_output <- car::recode(string_input,string_recode)
  return(string_output)
}


recode_val_fun <- function(month_val,string_val,string_ref){
  #
  # This function recodes a string of character using input and reference values
  #
  #INPUTS:
  #1) month_val: input characters to recode
  #2) string_val:  input string of values to be matched
  #3) string_ref:  ref string of values 
  #OUTPUTS:
  #month_val_recoded: recoded string using input
  
  ##### Begin script ####
  
  month_val_recoded <- month_val
  
  if(class(string_val)=="character"){
    month_val_recoded <- recode_string_fun(month_val_recoded,
                                        string_val=string_val,
                                        string_ref=string_ref)
  }
  
  for(i in 1:length(string_val)){
    #test <- recode_string_fun(month_val,string_val=French_months_l,string_ref=English_months_u)
    month_val_recoded <- recode_string_fun(month_val_recoded,
                                        string_val=string_val[[i]],
                                        string_ref=string_ref)
  }
  
  return(month_val_recoded)
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
  ## Utility function used after grep usage
  ## Select rows matching keywords
  
  row_df <- df_strings[i,]
  #row_df[row_df > 0]
  val_present <- names(row_df)[row_df > 0] #select names with value present!, -1 means not present
  if(length(val_present)==0){
    val_present <- NA
  }
  if(length(val_present)>1){
    val_present<- paste0(length(val_present)," matched")
  }
  
  return(val_present)
}

survey_combine_by_column <- function(out_filenames_selected,df_data,method_opt="byrow"){
  #
  # This function Combines survey by column based on list of files defined in data.frame
  ##Inputs:
  # 1) out_filenames_selected: output names for files being combined
  # 2) df_data: input data to be combined
  # 3) method_opt: "byrow" or "bycolum", combine files splitted using byrow or by colum
  #                     - byrow: removes duplicate columns, add NA in missing
  #                     - bycolumn: keeps all columns even if duplicate, ad NA in mssing
  ##Outputs:
  # 1) out_filenames_selected: output name (modified from input)
  
  ###### Begin script ####
  
  df_subset <- subset(df_data,df_data$out_filenames==out_filenames_selected)
  df_subset <- df_subset[with(df_subset, order(filenames)), ]

  list_df <-lapply(df_subset$filenames,FUN=read_file_feed2go)
  #debug(read_file_feed2go)
  #undebug(read_file_feed2go)

  ### Remove quotes in the header first:
  remove_quotes_header <- function(df_tmp){
     
    names_col <- gsub("\"","",names(df_tmp))
    names(df_tmp) <- noquote(names_col)
    return(df_tmp)
  }
  
  #debug(remove_quotes_header)
  #remove_quotes_header(list_df[[1]])
  #list_df_tmp <- lapply(list_df,FUN=remove_quotes_header)
  #l_names <- unlist(lapply(list_df,FUN=function(x){names(x)}))
  #length(unique(l_names)) #should be number of columns for method1
  #length(l_names) #should be number of columns for method2
  
  #### Now combine by column and row options

  #require(plyr) # requires plyr for rbind.fill()
  
  #
  #https://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
  #cbind_fill <- function(...) {                                                                                                                                                       
  #  transpoted <- lapply(list(...),t)
  #  #col_names <-rownames(transpoted)
  #  transpoted_dataframe <- lapply(transpoted, as.data.frame) 
  #  df_out <- data.frame(t(rbind.fill(transpoted_dataframe)))
  #  #names(df_out) <- col_names
  #  return (df_out)                                                                                                                          
  #} 

  #modify here
  cbind_fill <- function(list_df_val){
    #Function to bind data column-wise.
    #It is modified from this url:
    #https://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
    #
    #INPUTS
    #1) list_df_val: data.frame list
    #OUTPUT
    #1) df_out:
    #
    
    ###### Begin ######
    
    transpoted <- lapply(list_df_val,t)
    col_names <- lapply(transpoted, rownames)
    transpoted_dataframe <- lapply(transpoted, as.data.frame) 
    df_out <- data.frame(t(rbind.fill(transpoted_dataframe)))
    names(df_out) <- unlist(col_names)
    
    return (df_out)                                                                                                                          
  } 
  
  df_subset_combined_method1 <- do.call(rbind.fill,list_df_tmp) #294 unique column names, only unique columns are retained
  #use rowr package
  #df_subset_combined_method2 <- do.call(cbind.fill,list_df_tmp) # All columns are retained, even duplicates
  #df_subset_combined_method2 <- do.call(cbind_fill,list_df_tmp) # All columns are retained, even duplicates
  df_subset_combined_method2 <- cbind_fill(list_df_tmp)
  rownames(df_subset_combined_method2) <- NULL
  dim(df_subset_combined_method2)
  # cbind.fill is in rowsr
  # rbind.fill in plyr
  #
  #for the time being
  
  out_filenames_selected_method1 <- sub("\\.csv$","_byrow.csv",out_filenames_selected)
  write.table(df_subset_combined_method1,
              out_filenames_selected_method1,
              sep=";",
              row.names = FALSE,
              #fileEncoding = "UTF-8",
              col.names=T,
              quote=F)
  
  out_filenames_selected_method2 <- sub("\\.csv$","_bycol.csv",out_filenames_selected)
  write.table(df_subset_combined_method2,
              out_filenames_selected_method2,
              sep=";",
              row.names = FALSE,
              #fileEncoding = "UTF-8",
              col.names=T,
              quote=F)
  
  unique(l_names)[1:10]
  names(df_subset_combined_method1)[1:10]
  (l_names)[1:10]
  names(df_subset_combined_method2)[1:10]
  #sub("[\]","",l_names)
  #sub("\\","",l_names) #remove back slashes, note that we 
  #sub("\"","",l_names) #remove back slashes, note that we 
  #l_names1 <- gsub("\"","",l_names) #remove back slashes, note that we 
  #l_names2 <- noquote(l_names1)
  #l_names3 <- gsub("\"","",l_names2) #remove back slashes, note that we 
  #noquotes(l_names3)
  
  ### Prepare 
  
  #byrow is option 1
  #bycolumn is option 2
  
  if(method_opt=="byrow"){
    df_subset_combined <- df_subset_combined_method1
    out_filename <- out_filenames_selected_method1
  }
  
  if(method_opt=="bycolumn"){
    df_subset_combined <- df_subset_combined_method2
    out_filename <- out_filenames_selected_method2
  }

  #df_data$
  return(out_filename)
  
}

combine_by_dir_surveys_part <- function(in_dir_zip,surveys_names,list_filenames,combine_option="byrow",out_dir=NULL){
  #
  # This functions combines files of surveys using survey names, common dir and date information.
  # The first step is to identify for each directory:
  # 1) the existence of multiple parts for each month and survey - this is done by identifying the presence of "avy" 
  #    and months in French, English, Malagasy, upper and lower cases
  # 2) Grouping files by survey names, common dates and common directory
  # 3) Binding by column (wide format) the information or by row (see combine_option)
  # 4) return the list of files were combine by column for further processing
  
  ##### INPUTS
  #
  # 1) in_dir_zip: input directory used to group files to combine
  # 2) surveys_names: survey id keywords used in the combine process
  # 3) list_filenames: input file names from survey app
  # 4) combine_option: "byrow" or "bycolum", combine files splitted using byrow or by colum
  #                     - byrow: removes duplicate columns, add NA in missing
  #                     - bycolumn: keeps all columns even if duplicate, ad NA in mssing
  # ) out_suffix: if NULL, empty string
  # 5) out_dir: if NULL, defaults to current directory
  
  #
  ##### OUTPUTS
  #
  # 1) list_filenames2: output file name combined by survey names and parts "avy" as well as months
  
  
  ###### Begin script ##########
  
  ###############
  ### Step 1:  Subset relevant columns
  
  if(is.null(out_dir)){
    out_dir="."
  }
  
  list_filenames_subset <- grep(in_dir_zip,list_filenames,invert=F,value=T)

  ## Find the presence of keyword "avy" which signal the present of mulitple parts in surveys
  keywords <- c("avy")
  
  ## Find the presence of multiple month names to group parts by unique months (names may be in French or English)
  English_months_u <- month.name
  English_months_l <- c("january","february","march","april","may","june","july","august",
                        "september","october","november","december")
  French_months_u <- c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout",
                     "Septembre","Octobre","Novembre","Decembre")
  French_months_l <- c("janvier","fevrier","mars","avril","mai","juin","juillet","aout",
                     "septembre","octobre","novembre","decembre")
  Malagasy_months_u <- c("Janoary", "Febroary", "March", "Aprily", "Mey", "Jona", "Jolay", "Aogositra",
                       "Septambra", "Oktobra", "Novambra", "Desambra")
  Malagasy_months_l <- c("janoary", "febroary", "marsa", "aprily", "mey", "jona", "jolay", "aogositra",
                         "septambra", "oktobra", "novambra", "desambra")
  
  keywords_month <- c(English_months_u,English_months_l,French_months_u,
                      French_months_l,Malagasy_months_u,Malagasy_months_l,
                      "Novembra","novembra")
  ##############
  ###  Step 2: Generate data.frame with combination  "avy" keywords from list of file names from the survey app
  df_strings <- as.data.frame(sapply(keywords, 
                                     regexpr, 
                                     list_filenames_subset, 
                                     ignore.case=TRUE))
  
  df_strings$filenames <- basename(list_filenames_subset)
  
  ##############
  ### Step 3: data.frame with combination months keywords from list of file names from the survey app
  ## use case sensitive option 
  #df_strings_month <- as.data.frame(sapply(keywords_month, regexpr, list_filenames_subset, ignore.case=FALSE))
  df_strings_month <- as.data.frame(sapply(keywords_month, 
                                           regexpr, 
                                           list_filenames_subset, 
                                           ignore.case=FALSE))
  
  #############
  ### Step 4: data.frame with combination survey_name keywords from list of file names from the survey app
  
  keywords <- surveys_names 
  df_strings_surveys <- as.data.frame(sapply(keywords, regexpr, list_filenames_subset, ignore.case=TRUE))
  
  #############
  ### Step 5: Combine data.frame keywords in a common data.frame "df_strings"
  
  survey_val <- unlist(lapply(1:nrow(df_strings_surveys),
                              FUN=get_val_present,df_strings=df_strings_surveys))
  #undebug(get_val_present)
  
  #get_val_present(49,df_strings_month)
  month_val<- unlist(lapply(1:nrow(df_strings_month),
                            FUN=get_val_present,df_strings=df_strings_month))
  
  #### Add month and survey info to df_strings
  
  df_strings$filenames <- basename(list_filenames_subset)
  list_months_values <- list(English_months_l,English_months_u,French_months_l, French_months_u,Malagasy_months_u,Malagasy_months_l)
  #debug(recode_val_fun)
  month_val_recoded <- recode_val_fun(month_val=month_val,
                         string_val=list_months_values,
                         string_ref=English_months_u)
  ## Recode addtional values if necessary:
  month_val_recoded <- recode_val_fun(month_val_recoded,
                                      string_val=c("novembra","Novembra"),
                                      string_ref=c("November","November"))
  
  df_strings$month_val <- month_val_recoded
  df_strings$survey_id <- survey_val
  
  ##############
  ### Step 6: Generate group_id used to combine files by columns
  
  ##Not using "_" to see if we can resolve the issue of indentification of survey names
  df_strings$group_id <- paste0(df_strings$survey_id," ",df_strings$month_val)
  
  #concatenate survey id with month, this is the unit used to combine
  #drop all rows without "avy"
  out_filenames <- paste0(df_strings$group_id,"_",in_dir_zip,".csv")
  
  df_strings$out_filenames <- file.path(out_dir,out_filenames)
  df_strings[df_strings$avy==-1,]$out_filenames <- file.path(out_dir,in_dir_zip,df_strings[df_strings$avy==-1,]$filenames)
  
  ################
  #### Step 7: Select data to combine: only if "avy" is found in the name
  
  #Selected inputs:
  df_selected <- df_strings[df_strings$avy>0,]
  ### Need to check output directory
  #[16] "./Feed2Go_csv_20160902074190400/./Feed2Go_csv_20160902074190400/Mpanjono 2 avy 2 july_201609020742416016.csv"       

  df_selected$filenames <- file.path(out_dir,in_dir_zip,df_selected$filenames)
  
  group_val <- unique(df_selected$group_id) #grouping values 
  list_out_filenames <- unique(df_selected$out_filenames)

  ################
  #### Step  8: Combine files by group id
  
  #undebug(survey_combine_by_column)
  list_df_col_combined <- survey_combine_by_column(list_out_filenames[4],
                                                   df_data=df_selected,
                                                   method_opt="byrow")
  
  list_df_col_combined <- lapply(list_out_filenames,
                                 FUN= survey_combine_by_column,
                                 df_data=df_selected,
                                 method_opt= combine_option
                                 )
  
  #byrow is option 1
  #bycolumn is option 2
  #combine_option <- c("byrow")
  
  list_df_col_combined <- mclapply(list_out_filenames,
                                   FUN= survey_combine_by_column,
                                   df_data=df_test,
                                   method_opt=combine_option,
                                   mc.preschedule = FALSE,
                                   mc.cores=num_cores
                                   )
  
  #list_df_col_combined <- lapply(group_val,FUN= survey_combine_by_column,df_data=df_test)
  names(list_df_col_combined)<- list_out_filenames
  
  #this might need to be changed
  list_filenames2 <- c(list_df_col_combined,df_strings[df_strings$avy<0,c("out_filenames")])
  list_filenames2 <- unique(df_strings$out_filenames)
  
  return(list_filenames2)
}

###############################
### THis is the main function using all others above

combine_by_surveys<- function(list_filenames,surveys_names,num_cores=1,combine_by_dir=T,combine_option="byrow",out_suffix=NULL,out_dir=NULL){
  # This functions combines data based on the survey names. 
  # Data is combined by row using input files.
  #
  ##### INPUTS
  # 1) list_filenames: input file names from survey app
  # 2) surveys_names: survey id keywords used in the combine process
  #                   if NULL, survey names are found from the names
  # 3) num_cores: number of cores to use when combine files, default to 1 in case of no multicores system
  # 4) combine_by_dir: if TRUE, combine files that are splitted using "avy" and monthly names
  # 5) combine_option: "byrow" or "bycolum", combine files splitted using byrow or by colum
  #                     - byrow: removes duplicate columns, add NA in missing
  #                     - bycolumn: keeps all columns even if duplicate, add NA in mssing
  # 6) out_suffix: if NULL, empty string
  # 7) out_dir: if NULL, defaults to current directory
  #
  ##### OUTPUTS
  #
  # 1) list_survey_df: names of output files with combined data by surveys 
  #
  
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
    
    #debug(combine_by_dir_surveys_part)
    ### By default byrow and bycolumn are produced but only one file is selected to be combined in the survey name
    test_filenames2_zip <- combine_by_dir_surveys_part(list_in_dir_zip[4],
                                                   surveys_names = surveys_names,
                                                   list_filenames=list_filenames,
                                                   combine_option=combine_option, #byrow or bycolumn
                                                   out_dir=out_dir)
    

    #Example:
    #Karazan-tsakafo August_Feed2Go_csv_20160902074190400_byrow.csv
    #Karazan-tsakafo August_Feed2Go_csv_20160902074190400_bycol.csv
    #[12] "./Feed2Go_csv_20160902074190400/Vola isambolana Aout_20160902074246308.csv": unmodified file       
    #5: In read.table(in_filename, sep = ";", fill = T, header = T, quote = "",  :
    #                   incomplete final line found by readTableHeader on './Feed2Go_csv_20160902074190400/Laoko 2 avy 3 Aout_201609020742078013.csv'
                     
    list_filenames2 <- mclapply(list_in_dir_zip,
                                FUN=combine_by_dir_surveys_part,
                                surveys_names = surveys_names,
                                list_filenames=list_filenames,
                                combine_option=combine_option,
                                out_dir=out_dir,
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
  #test_filename <- combine_by_id_survey(3,surveys_names,list_filenames2,num_cores,out_suffix,out_dir)
  
  #test_df <- read.table(test_filename,sep=",",header=T,check.names = F)
  # Start writing to an output file
  if(file.exists("warnings_messages.txt")){
    file.remove("warning_messages.txt")
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
