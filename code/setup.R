library(dplyr); library(googlesheets4); library(tidyr);
library(stringr); library(ggplot2); library(readxl)

long <- read_excel("data/rawInterlabData.xlsx")

df <- long %>% 
  mutate(lab = ifelse(str_detect(treatment, 'utah'), 'UU', 'DPAA'), 
         treated = ifelse(str_detect(treatment, 'untreated'), F, T),
         temp = case_when(str_detect(treatment, '30') ~ '30', 
                          str_detect(treatment, '50') ~ '50'), 
         baked = ifelse(str_detect(treatment, 'baked'), T, F), #assuming it's 30 for now
         treatment = substring(treatment, 6)
  ) 

write.csv(df, file = "data/singlevalues.csv")

# Intra-lab comparison, treated-untreated--------------------------------------
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
                            'treated_30' = 'Treated, Unbaked, 30 Rxn Temp',
                            'treated_50' = 'Treated, Unbaked, 50 Rxn Temp', 
                            'treated_baked_30' = 'Treated, Baked, 30 Rxn Temp', 
                            'untreated_30' = 'Untreated, Unbaked, 30 Rxn Temp', 
                            'untreated_50' = 'Untreated, Unbaked, 50 Rxn Temp', 
                            'untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp'
         )) 
write.csv(il, file = "data/intralab.csv")

ggplot(data = subset(il, treatment != 'Untreated, Baked, 30 Rxn Temp'), aes(x = treatment, y = dC.off, fill = lab)) + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot()


# Interlab for 1:1 Plots --------------------------------------------------
uu <- df %>% 
  filter(lab == 'UU') %>% 
  rename(d13Cuu = d13C, 
         d18Ouu = d18O) %>% 
  select(sample, d13Cuu, d18Ouu, treatment)
dpaa <- df %>% 
  filter(lab == 'DPAA') %>% 
  rename(d13Cdpaa = d13C, 
         d18Odpaa = d18O)

il1 <- left_join(uu, dpaa, by = c("sample", "treatment")) %>% 
  mutate(treatment = recode(treatment, 
                            'treated_30' = 'Treated, Unbaked, 30 Rxn Temp',
                            'treated_50' = 'Treated, Unbaked, 50 Rxn Temp', 
                            'treated_baked_30' = 'Treated, Baked, 30 Rxn Temp', 
                            'untreated_30' = 'Untreated, Unbaked, 30 Rxn Temp', 
                            'untreated_50' = 'Untreated, Unbaked, 50 Rxn Temp', 
                            'untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp'
  ))
write.csv(il1, file = 'data/intralab1.csv')

# Delta Calculated from Excel ---------------------------------------------

library(readxl)
delta <- read_excel("data/deltaValues.xlsx")
  
long <- delta %>% 
  pivot_longer(!sample, 
               names_to = 'type', 
               values_to = 'value') %>% 
  mutate(iso = ifelse(str_detect(type, 'DC'), 'C', 'O'),
         type = recode(type, 
                       'DC_untreated' = 'Untreated',
                       'DO_untreated' = 'Untreated', 
                       'DC_treated' = 'Treated', 
                       'DO_treated' = 'Treated', 
                       'DC_untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp', #something's up with this one...
                       'DO_untreated_baked_30' = 'Untreated, Baked, 30 Rxn Temp',
                       'DC_untreated_50' = 'Untreated, 50 Rxn Temp', 
                       'DO_untreated_50' = 'Untreated, 50 Rxn Temp'
                        ) 
         )

write.csv(long, file = "data/delta.csv")
