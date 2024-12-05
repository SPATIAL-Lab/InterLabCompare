# This integrated the new UU data generated in April 2024. Doesn't need to be re-run at this point
# for reference, this was untreated, unbaked, at 30 C for 24 hours
library(dplyr); library(tidyr); library(readr)

df <- read_csv("data/UU_0424.csv") %>% 
  select(-c(batch_id))

avg = aggregate(.~sample_id, df, mean) %>% 
  select(sample_id, d13C, d18O)

