# DATA UNDERSTANDING ----

# Libraries 
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

definitions_raw_tbl

train_raw_tbl

glimpse(train_raw_tbl)

# Exploratory Data Analysis (EDA) ----

# Step 1: Data Summarization -----

skim(train_raw_tbl)


# Character Data Type

train_raw_tbl %>%
    select_if(is.character) %>%
    glimpse()

train_raw_tbl %>%
    select_if(is.character) %>%
    map(unique)
    
train_raw_tbl %>%
    select_if(is.character) %>%
    map(~ table(.) %>% prop.table())

# Numeric Data

train_raw_tbl %>%
    select_if(is.numeric) %>%
    map(~ unique(.) %>% length())


train_raw_tbl %>%
    select_if(is.numeric) %>%
    map_df(~ unique(.) %>% length()) %>%
    gather() %>%
    arrange(value) %>%
    filter(value <= 10)


# Step 2: Data Visualization ----

train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    ggpairs() 

train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
            diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) +
    theme(legend.position = "bottom")


data <- train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome)

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
    
    color_expr <- enquo(color)
    
    if (rlang::quo_is_null(color_expr)) {
        
        g <- data %>%
            ggpairs(lower = "blank") 
        
    } else {
        
        color_name <- quo_name(color_expr)
        
        g <- data %>%
            ggpairs(mapping = aes_string(color = color_name), 
                    lower = "blank", legend = 1,
                    diag = list(continuous = wrap("densityDiag", 
                                                  alpha = density_alpha))) +
            theme(legend.position = "bottom")
    }
    
    return(g)
    
}

train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    plot_ggpairs(color = Attrition)


# Explore Features by Category

#   1. Descriptive features: age, gender, marital status 
train_raw_tbl %>%
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
    plot_ggpairs(Attrition)

#   2. Employment features: department, job role, job level
train_raw_tbl %>%
    select(Attrition, contains("employee"), contains("department"), contains("job")) %>%
    plot_ggpairs(Attrition) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
train_raw_tbl %>%
    select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
    plot_ggpairs(Attrition)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
train_raw_tbl %>%
    select(Attrition, contains("satisfaction"), contains("life")) %>%
    plot_ggpairs(Attrition)

#   5. Performance Data: Job Involvment, Performance Rating
train_raw_tbl %>%
    select(Attrition, contains("performance"), contains("involvement")) %>%
    plot_ggpairs(Attrition)

#   6. Work-Life Features 
train_raw_tbl %>%
    select(Attrition, contains("overtime"), contains("travel")) %>%
    plot_ggpairs(Attrition)

#   7. Training and Education 
train_raw_tbl %>%
    select(Attrition, contains("training"), contains("education")) %>%
    plot_ggpairs(Attrition)

#   8. Time-Based Features: Years at company, years in current role
train_raw_tbl %>%
    select(Attrition, contains("years")) %>%
    plot_ggpairs(Attrition)