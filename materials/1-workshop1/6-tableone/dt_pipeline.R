#start task name____________________________________________________________----
(title_name<-'Data management pipeline')
dt.path<-'Workshop_I/mock_datasets/instructional_dataset' #raw data path
tableone_path<-'Teaching_Modules/table_one' #table_one project path [TEMPORARY]

##start directory setup____-----------------------------------------------------
###path package----
require(fs) #installed for directory management
###path names----
home.path<-path_home() #users home path (ensure that anyone can use your script)
project.path<-'Dropbox/SA_Workshops' #project path
wrk_dir<-file.path(home.path,project.path) #name working directory path
setwd(wrk_dir) #set working directory
##end directory setup]----

##start loading packages____----------------------------------------------------
###data manipulation----
require(tidyverse)  #installed for data manipulation and exploration
require(data.table) #installed for %like% function
require(janitor)    #data cleaning *presets
##end loading package]----

##data management pipeline____--------------------------------------------------
###start reading raw data to list____----
csv.list<-list.files(path=dt.path,pattern = '*.csv') #get all csv files
dt_ls<-list()              #start an empty list
for(csv.file in csv.list){
  dt.<-read.csv(
    file.path(dt.path,csv.file) #read each file from the csv.list
  )                             #name the read as dt_
  dt.name<-paste0(
    'dt_',                #data name prefix
    gsub('\\d{2}_', '',   #replace a two digits and underscore character ('00_')
         gsub(
           c('_UKZN_workshop_2023.csv'),'',
           csv.file,fixed=T))  #remove '_UKZN_workshop_2023.csv' in the name
  )
  dt_ls[[dt.name]]<-dt.   #assign a name to dataframe within the dt_project
}
###end reading raw data to list]----

###start data manipulation and cleaning____----
dt_ls$dt_participant_metadata$sex<-
  ifelse(
    dt_ls$dt_participant_metadata$sex==FALSE,
    'Female',dt_ls$dt_participant_metadata$sex
  )
###end data manipulation and cleaning]----

##end data management pipeline]-----

##save final project database----
save(dt_ls,file=file.path(
  tableone_path,'data',
  'UKZN_workshop_2023.RData')
)
#end task]_________________________________________________________________-----
