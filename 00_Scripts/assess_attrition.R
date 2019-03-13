# HR 201: PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# CHAPTER 1: BUSINESS UNDERSTANDING ----
# These are a collection of functions built during the first chapter
# of analyzing the business. You can source() this file for later use.


library(tidyverse)
library(tidyquant)
library(forcats)
library(stringr)

# Function to calculate attrition cost within a mutate() function
calculate_attrition_cost <- function(
    
    # Employee
    n                    = 1,
    salary               = 80000,
    
    # Direct Costs
    separation_cost      = 500,
    vacancy_cost         = 10000,
    acquisition_cost     = 4900,
    placement_cost       = 3500,
    
    # Productivity Costs
    net_revenue_per_employee = 250000,
    workdays_per_year        = 240,
    workdays_position_open   = 40,
    workdays_onboarding      = 60,
    onboarding_efficiency    = 0.50
    
) {
    
    # Direct Costs
    direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
    
    # Lost Productivity Costs
    productivity_cost <- net_revenue_per_employee / workdays_per_year * 
        (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
    
    # Savings of Salary & Benefits (Cost Reduction)
    salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
    
    # Estimated Turnover Per Employee
    cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
    
    # Total Cost of Employee Turnover
    total_cost <- n * cost_per_employee
    
    return(total_cost)
    
}

# Function to convert counts to percentages. Works well with dplyr::count()
count_to_pct <- function(data, ..., col = n) {
    
    grouping_vars_expr <- quos(...)
    col_expr <- enquo(col)
    
    ret <- data %>%
        group_by(!!! grouping_vars_expr) %>%
        mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
        ungroup()
    
    return(ret)
    
}

# Function to assess attrition versus a baseline
assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
    
    attrition_col_expr <- enquo(attrition_col)
    
    data %>% 
        filter((!! attrition_col_expr) %in% attrition_value) %>%
        arrange(desc(pct)) %>%
        mutate(
            above_industry_avg = case_when(
                pct > baseline_pct ~ "Yes",
                TRUE ~ "No"
            )
        )
    
}

# Function to plot attrition
plot_attrition <- function(data, ..., .value, 
                           fct_reorder = TRUE, 
                           fct_rev = FALSE, 
                           include_lbl = TRUE, 
                           color = palette_light()[[1]], 
                           units = c("0", "K", "M")) {
    
    
    # Inputs
    
    group_vars_expr <- quos(...)
    if (length(group_vars_expr) == 0) 
        group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
    
    value_expr <- enquo(.value)
    value_name <- quo_name(value_expr)
    
    units_val <- switch(units[[1]],
                        "M" = 1e6,
                        "K" = 1e3,
                        "0"  = 1)
    if (units[[1]] == "0") units <- ""
    
    
    # Data Manipulation
    usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
    
    data_manipulated <- data %>%
        mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>% 
        mutate(value_text = str_c(usd(!! value_expr / units_val), 
                                  units[[1]], sep = ""))
    
    
    if (fct_reorder) {
        data_manipulated <- data_manipulated %>%
            mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
            arrange(name)
    }
    
    if (fct_rev) {
        data_manipulated <- data_manipulated %>%
            mutate(name = forcats::fct_rev(name)) %>%
            arrange(name)
    }
    
    # Visualization
    
    g <- data_manipulated %>%
        ggplot(aes_string(x = value_name, y = "name")) +
        geom_segment(aes(xend = 0, yend = name), color = color) +
        geom_point(aes_string(size = value_name), color = color) +
        scale_x_continuous(labels = scales::dollar) +
        theme_tq() +
        scale_size(range = c(3, 5)) +
        theme(legend.position = "none")
    
    
    if (include_lbl) {
        g <- g +
            geom_label(aes_string(label = "value_text", size = value_name), 
                       hjust = "inward", color = color) 
    }
    
    return(g)
    
}