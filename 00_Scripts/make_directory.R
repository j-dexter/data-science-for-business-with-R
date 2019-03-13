# FUNCTION FOR SETTING UP BSPF PROJECT DIRECTORY

# Setup for projects following the Business Science Problem Framework

# Load package
library(fs) 
# For working w/and manipulating the file system
# Useful for working with folders, directories and file paths.



make_project_dir <- function() {
    
    dir_names <- c(
        "00_Data",
        "00_Scripts",
        "01_Business_Understanding",
        "02_Data_Understanding",
        "03_Data_Preparation",
        "04_Modeling",
        "05_Evaluation",
        "06_Deployment")
    
    dir_create(dir_names)
    
    dir_ls()
}


