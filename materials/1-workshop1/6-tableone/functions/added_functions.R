# Function removing redundant objects
##redundant objects are indicated by a punctuation of your choice
##the default is a period '.'
func_remove_redundant<-function(list=ls()){
  # List all objects in the global environment
  all_objects <- ls(globalenv())
  
  # Use a regular expression pattern to select objects with a '.' in their names
  objects_with_period <- grep("\\.", all_objects, value = TRUE)
  
  # Remove the selected objects
  if (length(objects_with_period) > 0) {
    rm(list = objects_with_period, envir = globalenv())
    cat("Removed the following objects:", objects_with_period, "\n")
  } else {
    cat("No objects with '.' found in the global environment.\n")
  }
}

# make clean names
func_make_dt_clean_names<-function(vector,num_col=3){
  dt_clean_names<-as_tibble(matrix(vector,
                                   byrow=T,
                                   ncol=num_col))
  names(dt_clean_names)<-str_to_lower(names(dt_clean_names))
  return(dt_clean_names)
}

# rename variables
func_rename_columns <- function(df, name_df) {
  for(i in 1:nrow(name_df)) {
    if(name_df$old_names[i] %in% names(df)) {
      df <- rename(df, !!name_df$new_names[i] := !!name_df$old_names[i])
    }
  }
  return(df)
}

# label columns
func_label_dataframe <- function(df, col_names, col_labels) {
  for(i in seq_along(col_names)) {
    df <- df %>%
      rename(!!col_labels[i] := !!col_names[i])
  }
  return(df)
}

# generating missing values on a dataset
func_generate_missing_values <- function(data, column_name, percentage) {
  # Check if the column exists in the data
  if (!(column_name %in% names(data))) {
    stop("The specified column does not exist in the dataset.")
  }
  # Calculate the number of values to replace
  num_values <- length(data[[column_name]])
  num_missing <- round(num_values * percentage / 100)
  # Generate random row indices
  row_indices <- sample(1:num_values, num_missing, replace = TRUE)
  # Replace the data at these indices with NA
  data[row_indices, column_name] <- NA
  
  return(data)
}

# Function to clean up table one results
func_clean_tableone<-function(TABLEONE){
  N<-as.numeric(TABLEONE$Overall[1])
  TABLE <- as_tibble(
    TABLEONE %>%
      rownames_to_column('Variable') %>%
      mutate_all(
        ~str_replace_all(., 
                         c("NA" = "-", 
                           "\\[" = "\n(", 
                           "\\]" = ")", 
                           "\\[I" = "(I"))) %>%
      mutate(
        Variable = ifelse(Variable%like%'^X','',Variable),
        Variable = str_replace_all(
          Variable,
          c(
            '__'='-',
            '_'=' ',
            ' =' = ';',
            ' \\(m' = 'm',
            ' \\(I' = 'I',
            ' \\(S' = 'S',
            '\\)\\)' = '',
            ' \\(%\\)' = '; n (%)',
            '\\.' = '',
            'medianIQR' = '; median (IQR)',
            'meanSD' = '; mean (SD)'
          )
        )
      )%>%
      mutate(
        missing = round((as.numeric(Missing)/100)*N, 0),
        n_used = N - (missing),
        Variable = ifelse(
          'level' %in% names(TABLEONE) & !is.na(missing) & level != '', 
          paste0(Variable, '; n (%)'), 
          Variable)
      )%>%
      select(-Missing)
  )
  
  if('test'%in%names(TABLE)){
    TABLE<-TABLE%>%
      select(-test)
  }else{
    TABLE<-TABLE
  }
  return(TABLE)
}

# Function to create word document with a table----
func_create_flexdocx <- 
  function(output_file, dataframes, table_names) {
    # Check if the lengths of dataframes and table_names are equal
    if(length(dataframes) != length(table_names)) {
      stop("The number of dataframes and table names provided do not match.")
    }
    
    # Create a temporary Rmd file
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    # Initialize an empty string for Rmd content
    rmd_content <- c("---", 
                     "output: word_document", 
                     "---", 
                     "")
    # Loop over each dataframe and table name
    for(i in seq_along(dataframes)) {
      # Create a flextable object from the current dataframe
      ft <- dataframes[[i]] %>%
        regulartable() %>%
        set_caption(paste(table_names[i]),
                    autonum = run_autonum(seq_id = "tab",
                                          bkm = paste0("tab", i))) %>%
        theme_booktabs() %>%
        merge_v(j = 1) %>%
        valign(valign = "top",part = "all") %>%
        valign(part='header',valign = "bottom") %>%  
        align(align = "left",part = "all") %>%
        align(j=c(2:length(names(dataframes[[i]]))),
              align = "center",part = "header") %>%
        align(j=c(2:length(names(dataframes[[i]]))),
              align = "center",part = "body") %>%
        align(j=1,align = "left") %>%
        align(j=1,i=1,align = "left") %>%
        bold(part = "header") %>%
        set_table_properties(layout = 'autofit',width=.99) %>%
        merge_h(i=4)
      if('level'%in%names(dataframes[[i]])){
        ft<-ft%>%
          align(j=c(2),
                align = "left",part = "body")
      }
      # Add the flextable to the Rmd content
      rmd_content <- c(rmd_content, "", "```{r, echo=FALSE}", 
                       paste0("ft", i), "```")
      
      # Assign the flextable to a variable in the global environment
      assign(paste0("ft", i), ft, envir = .GlobalEnv)
    }
    
    # Write the Rmd content
    writeLines(rmd_content, temp_rmd)
    
    # Render the Rmd file
    render(temp_rmd, output_file = paste0(output_file,'.docx'))
  }