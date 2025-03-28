# Initial cleaning of data. Doesn't need to be run now as the needed data are in the data folder. 
# Creates the csvs called singlevalues, interlab, intralab, and intralab2
library(dplyr); library(tidyr);
library(stringr); library(ggplot2); library(readxl)

long <- read_excel("data/baseInterlabData.xlsx")

df <- long %>% 
  mutate(lab = ifelse(str_detect(treatment, 'utah'), 'UU', 'DPAA'), 
         treated = ifelse(str_detect(treatment, 'untreated'), F, T),
         temp = case_when(str_detect(treatment, '30') ~ '30', 
                          str_detect(treatment, '50') ~ '50'), 
         baked = ifelse(str_detect(treatment, 'unbaked'), F, T), #assuming it's 30 for now
         treatment = substring(treatment, 6), 
         rxn_time = as.character(rxn_time)
  ) 
df$d13C <- round(df$d13C, 2)
df$d18O <- round(df$d18O, 2)

# let's drop DPAA's untreated, unbaked, 50 for 24 hours group
df <- subset(df, treatment != 'untreated_unbaked_50' | rxn_time != 24)
write.csv(df, file = "data/singlevalues.csv")

# Intra-lab comparison, Untreated, Bakesd, 30 -----------------------------------
uu <- df %>% 
  filter(lab == 'UU')

uu$dC.off = uu$dO.off = rep(0)

sid = unique(uu$sample)
for(i in sid){
  uu$dC.off[uu$sample == i] = 
    uu$d13C[uu$sample == i] - 
    uu$d13C[uu$sample == i & uu$treatment == 'untreated_baked_30']
}

for(i in sid){
  uu$dO.off[uu$sample == i] = 
    uu$d18O[uu$sample == i] - 
    uu$d18O[uu$sample == i & uu$treatment == 'untreated_baked_30']
}

dpaa <- df %>% 
  filter(lab == 'DPAA')

dpaa$dC.off = dpaa$dO.off = rep(0)

sid = unique(uu$sample)
for(i in sid){
  dpaa$dC.off[dpaa$sample == i] = 
    dpaa$d13C[dpaa$sample == i] - 
    dpaa$d13C[dpaa$sample == i & dpaa$treatment == 'untreated_baked_30']
}

for(i in sid){
  dpaa$dO.off[dpaa$sample == i] = 
    dpaa$d18O[dpaa$sample == i] - 
    dpaa$d18O[dpaa$sample == i & dpaa$treatment == 'untreated_baked_30']
}

il <- rbind(uu, dpaa) %>% 
  mutate(treatment = recode(treatment, 
                            'treated_unbaked_30' = 'Treated, Unbaked, 30 Rxn Temp',
                            'treated_unbaked_50' = 'Treated, Unbaked, 50 Rxn Temp', 
                            'treated_baked_30' = 'Treated, Baked, 30 Rxn Temp', 
                            'untreated_unbaked_30' = 'Untreated, Unbaked, 30 Rxn Temp', 
                            'untreated_unbaked_50' = 'Untreated, Unbaked, 50 Rxn Temp', 
                            'untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp'
         )) 
write.csv(il, file = "data/intralab.csv")

# Interlab for 1:1 Plots --------------------------------------------------
uu <- df %>% 
  filter(lab == 'UU') %>% 
  rename(d13Cuu = d13C, 
         d18Ouu = d18O) %>% 
  select(sample, d13Cuu, d18Ouu, treatment) %>% 
  mutate(treatment = recode(treatment, 
                            'treated_unbaked_30' = 'Treated, Unbaked, 30 Rxn Temp',
                            'treated_unbaked_50' = 'Treated, Unbaked, 50 Rxn Temp', 
                            'treated_baked_30' = 'Treated, Baked, 30 Rxn Temp', 
                            'untreated_unbaked_30' = 'Untreated, Unbaked, 30 Rxn Temp', 
                            'untreated_unbaked_50' = 'Untreated, Unbaked, 50 Rxn Temp', 
                            'untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp'
  ))

dpaa <- df %>% 
  filter(lab == 'DPAA') %>% 
  rename(d13Cdpaa = d13C, 
         d18Odpaa = d18O) %>% 
  select(-c(lab)) %>% 
  mutate(treatment = recode(treatment,
                            'treated_unbaked_30' = 'Treated, Unbaked, 30 Rxn Temp',
                            'treated_unbaked_50' = 'Treated, Unbaked, 50 Rxn Temp', 
                            'treated_baked_30' = 'Treated, Baked, 30 Rxn Temp', 
                            'untreated__unbaked_30' = 'Untreated, Unbaked, 30 Rxn Temp', 
                            'untreated__unbaked_50' = 'Untreated, Unbaked, 50 Rxn Temp', 
                            'untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp'
  ))

il1 <- left_join(uu, dpaa, by = c("sample", "treatment"))

write.csv(il1, file = 'data/interlab.csv')

# Intralab for 1:1 Plots --------------------------------------------------
#wide <- long %>% pivot_wider(names_from = treatment, values_from = c(d18O, d13C))

list_df <- split(df, df$treatment) #split the dataset into a list of datasets 
list2env(list_df, envir= .GlobalEnv)

untreated_baked_30 <- untreated_baked_30 %>% 
  rename(d13Ccompare = d13C, 
         d18Ocompare = d18O) %>% 
  select(sample, d13Ccompare, d18Ocompare, treatment, lab)

treated_unbaked_30 <- treated_unbaked_30 %>% 
  select(sample, treatment, d13C, d18O, lab)
treated_unbaked_50 <- treated_unbaked_50 %>% 
  select(sample, treatment, d13C, d18O, lab)
treated_baked_30 <- treated_baked_30 %>% 
  select(sample, treatment, d13C, d18O, lab)
untreated_unbaked_30 <- untreated_unbaked_30 %>% 
  select(sample, treatment, d13C, d18O, lab)
untreated_unbaked_50 <- untreated_unbaked_50 %>% 
  select(sample, treatment, d13C, d18O, lab)

il2 <- rbind(treated_unbaked_30, treated_unbaked_50, treated_baked_30, untreated_unbaked_30, untreated_unbaked_50)
il2_bound <- full_join(untreated_baked_30, il2, join_by(sample, lab)) %>% 
  select(-c(treatment.x)) %>% 
  rename(treatment = treatment.y) %>% 
  mutate(treatment = recode(treatment, 
                            'treated_unbaked_30' = 'Treated, Unbaked, 30 Rxn Temp', 
                            'treated_unbaked_50' = 'Treated, Unbaked, 50 Rxn Temp', 
                            'treated_baked_30' = 'Treated, Baked, 30 Rxn Temp',
                            'untreated_unbaked_30' = 'Untreated, Unbaked, 30 Rxn Temp',
                            'untreated_unbaked_50' = 'Untreated, Unbaked, 50 Rxn Temp'
  ))
write.csv(il2_bound, file = 'data/intralab2.csv')


# Comparing Un/Treated with own rxn temp, unbaked -------------------------
# so we want untreated_unbaked_50 for UU, untreated_unbaked_30 for DPAA
# and then treated_unbaked_50 for UU, treated_unbaked_30 for DPAA

uu <- df %>% 
  filter(treatment == 'untreated_unbaked_50' | treatment == 'treated_unbaked_50') %>% 
  filter(lab == 'UU') %>% 
  mutate(treatment = recode(treatment, 
                             'untreated_unbaked_50' = 'Untreated', 
                             'treated_unbaked_50' = 'Treated')
  )

dpaa <- df %>% 
  filter(treatment == 'untreated_unbaked_30' | treatment == 'treated_unbaked_30') %>% 
  filter(lab == 'DPAA') %>% 
  mutate(treatment = recode(treatment, 
                            'untreated_unbaked_30' = 'Untreated', 
                            'treated_unbaked_30' = 'Treated')
  )

il <- rbind(uu, dpaa) 

list_df <- split(il, il$treatment) #split the dataset into a list of datasets 
list2env(list_df, envir= .GlobalEnv)

Treated <- Treated %>% 
  rename(d13Ctreated = d13C, 
         d18Otreated = d18O) %>% 
  select(sample, d13Ctreated, d18Otreated, treatment, lab)

Untreated <- Untreated %>% 
  select(sample, d13C, d18O, treatment, lab)

il_bound <- full_join(Treated, Untreated, join_by(sample, lab)) %>% 
  select(-c(treatment.x)) %>% 
  rename(treatment = treatment.y) %>% 
  mutate(diffC = d13Ctreated - d13C, 
         diffO = d18Otreated - d18O)
write.csv(il_bound, file = 'data/intralab3.csv')
