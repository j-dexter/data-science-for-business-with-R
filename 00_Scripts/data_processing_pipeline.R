# Customer Segmentation ----
# DATA PREPARATION ----
# data_processing_pipeline.R ----



library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

# Processing pipeline for the Customer Level Data

process_customer_data_readable <- function(data, definitions_tbl) {
     
     # Tidy the data definition table 
     definitions_list <- definitions_tbl %>% 
          fill(X__1, .direction = "down") %>% 
          filter(!is.na(X__2)) %>% 
          rename(column_name = X__1, key = X__2, value = X__3) %>% 
          
          # Split Columns into a list of tbl dataframes
          split(.$column_name) %>% 
          map(~ select(., -column_name)) %>% 
          map(~ mutate(., value = as_factor(value)))
     
     # For Loop to Rename Columns by original names
     for (i in seq_along(definitions_list)) {
          list_name <- names(definitions_list)[i]
          colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value")
          )
     }
     
     # Merging Data ----
     
     ## Here we Iteratively Join the Dataframes within
     # the definitions list with the main dataframe (data_raw_tbl)
     
     # Add data_raw_tbl to new list + combine definition_list too
     data_merged_tbl <- list(customer_data = data) %>% 
          append(definitions_list, after = 1) %>% 
          reduce(left_join) %>% 
          select(-one_of(names(definitions_list))) %>% 
          set_names(str_replace_all(names(.), pattern = "_value", "")) 
     
     # Data Processing Step (this is ONLY if a variables ORDER Matters)
     data_processed_tbl <- data_merged_tbl %>% 
          mutate_if(is.character, as.factor) %>% 
          mutate(
               Channel = Channel %>% fct_relevel("Store Only", "Web Only", "Omni Shopper")
          )
     
     return(data_processed_tbl)
     
}
















