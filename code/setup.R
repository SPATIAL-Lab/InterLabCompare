library(dplyr); library(googlesheets4); library(tidyr)
library(stringr)

wide <- read_sheet('https://docs.google.com/spreadsheets/d/1h6h5yPlmJBaqjx_mOwyS9ETybus44Z3kRqqMZBp80zw')

df <- wide %>% 
  pivot_longer(!Sample, 
               names_to = 'type', 
               values_to = 'value')

df$lab = df$iso = df$treated = df$temp = character(nrow(df))

df <- df%>% 
  mutate(lab = ifelse(str_detect(type, 'utah'), 'UU', 'DPAA'), 
         iso = ifelse(str_detect(type, 'd13c'), 'C', 'O'), 
         treated = ifelse(str_detect(type, 'untreated'), F, T),
         temp = case_when(str_detect(type, '30') ~ '30', 
                          str_detect(type, '50') ~ '50'), 
         d18O = ifelse(str_detect(type, 'd18o'), value, NA), 
         d13C = ifelse(str_detect(type, 'd13c'), value, NA)
         ) %>%
  rename(sample = Sample)

# okay next up we want columns of the offset between samples (Delta) of:
# DPAA bake
# DPAA temp
# DPAA UU untreated
# DPAA UU ba
