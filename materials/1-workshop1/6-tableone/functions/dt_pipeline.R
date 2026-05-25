

dt.path<-"datasets/instructional_dataset"
##data management pipeline____--------------------------------------------------
csv.list<-list.files(path=here(dt.path),pattern = '*.csv')
## Lets go back to our data list
csv.list
## read and assign shorter names (----create a list with the datasets)
ls_of_dt<-list()              #start an empty list
for(csv.file in csv.list){
  dt.<-read.csv(
    file=here(file.path(dt.path,csv.file)) #read each file from the csv.list
  )                             #name the read as dt_
  dt.name<-paste0(
    'dt_',                #data name prefix
    gsub('\\d{2}_', '',   #replace a two digits and underscore character ('00_')
         gsub(
           c('_UKZN_workshop_2023.csv'),'',
           csv.file,fixed=T))  #remove '_UKZN_workshop_2023.csv' in the name
  )
  assign(paste0(dt.name),dt.) #assign a name to dataframe
  ls_of_dt[[dt.name]]<-dt.   #assign a name to dataframe within the dt_project
}

## data management pipeline for tableone____------------------------------------
names(ls_of_dt)
for(name in names(ls_of_dt)){
  print(paste0(name,'--------------------------------------------------------'))
  print(head(ls_of_dt[[name]]))
}
##Renaming of variable
names.vector<-c(
  'arm','ARM',
  'age','Age (years)',
  'smoker','Smoker',
  'sex','Sex',
  'education','Education',
  'nugent_score', 'Nugent Score', 
  'crp_blood','CRP Blood',
  'ph','pH',
  'live_cd19_negative','Live CD19-',
  'cd45_negative', 'CD45-',
  'cd45_positive','CD45+',
  'neutrophils','Neutrophils',
  'non_neutrophils','Non-Neutrophils',
  'cd3_negative','CD3-',
  'cd3_positive','CD3+',
  'cd4_t_cells','CD4 T-cells',
  'cd3_negative','CD3-',
  'cd3_positive','CD3+',
  'cd8_t_cells','CD8 T-cells'
)
for(name in names(ls_of_dt)){
  print(paste0(name,'--------------------------------------------------------'))
  ls_of_dt[[name]]<-
    func_rename_columns(ls_of_dt[[name]],
                        func_make_dt_clean_names(names.vector,2)%>%
                          rename(old_names=v1,new_names=v2))
  
}

for(name in names(ls_of_dt)){
  print(paste0(name,'--------------------------------------------------------'))
  print(head(ls_of_dt[[name]]))
}
# extracting baseline data from each of our datasets
dt_baseline_all<-
  #get baseline sample identifier
  ls_of_dt$dt_sample_ids%>%
  relocate(pid,sample_id)%>%
  filter(time_point=='baseline')%>%
  arrange(pid)%>%
  left_join(
    ls_of_dt$dt_participant_metadata%>%
      mutate(Sex=ifelse(
        Sex==F,'Female','Male'
      ))
  )%>%
  left_join(
    ls_of_dt$dt_visit_clinical_measurements
  )%>%
  left_join(
    ls_of_dt$dt_elisa_cytokines%>%
      select(-limits)%>%
      spread(cytokine,conc)
  )%>%
  left_join(
    ls_of_dt$dt_flow_cytometry
  )

dt_baseline<-dt_baseline_all[c(4:13)]

strata<- names(dt_baseline)[which(names(dt_baseline)%in%'ARM')]
vars<- dt_baseline %>% select(-all_of(strata)) %>% names()
factorvars<- dt_baseline %>% select(where(is.character)) %>% names()
nonnormal<- dt_baseline %>% select(where(is.numeric)) %>% names()

TABLE_ONE=
  as.data.frame(
  print(#printing table one results
    CreateTableOne(#attributes of table on
      data=dt_baseline,
      strata = strata,
      factorVars = factorvars,
      vars=vars,
      addOverall = T
    ),#
    nonnormal = nonnormal,
    printToggle = F,
    noSpaces=T,
    catDigits = 1,
    contDigits = 1
  ))%>%
    rownames_to_column(var='Variable')%>%
    select(-test)%>%
    rename_with(.,str_to_title)%>%
    mutate_all(
      ~str_replace_all(
        .,c(
          "NA"="-",
          " \\(median \\[IQR\\]\\)"="; median (IQR)",
          "\\["="\n(",
          "\\]"=")"
        )
      ))%>%
    mutate_if(is.character,~ifelse(.==" [","\n(",.))%>%
    mutate_if(is.character,~ifelse(.=="]",")",.))%>%
    relocate(Overall,.before = P)%>%
  regulartable()
