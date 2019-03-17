# Customer Segmentation ----
# DATA PREPARATION ----
# plot_hist_facet.R ----

library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

## Stepping Down to Understand the Function

#1 To Inspect Functions: Save parameters w/same name as the function inputs
#data <- smpl_data_raw_tbl %>% 
#    select(-subcustomer_sc_id, -last_trxn)

# custom historgram plot:
# purpose: To inspect feature distributions + 
# check feature skewdness + identify transformations needed.

# These Historams allow us to really inspect for different things that are going
# on in the data that we might not have otherwise seen.

# A great way to visualize lots of features at once!


plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
     
     data_factored <- data %>%
          mutate_if(is.character, as.factor) %>%  # so plot doesn't fail by getting character data
          mutate_if(is.factor, as.numeric) %>%    # so plot doesn't fail by getting factor data
          gather(key = key, value = value, factor_key = TRUE) # gathers wide data AND converts to long data
     
     if (fct_reorder) {
          data_factored <- data_factored %>%
               mutate(key = as.character(key) %>% as.factor())
     }
     
     if (fct_rev) {
          data_factored <- data_factored %>%
               mutate(key = fct_rev(key))
     }
     
     g <- data_factored %>%
          ggplot(aes(x = value, group = key)) +
          geom_histogram(bins = bins, fill = fill, color = color) +
          facet_wrap(~ key, ncol = ncol, scale = scale) + 
          theme_tq()
     
     return(g)
     
}

