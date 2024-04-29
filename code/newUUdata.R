library(dplyr); libray(tidyr); library(readr)

df <- read_csv("data/UU_0424.csv") %>% 
  select(-c(batch_id))

avg = aggregate(.~sample_id, df, mean) %>% 
  select(sample_id, d13C, d18O)

