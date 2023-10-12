library(dplyr); library(googlesheets4); library(tidyr);
library(stringr); library(ggplot2)

long <- read_sheet('https://docs.google.com/spreadsheets/d/1h6h5yPlmJBaqjx_mOwyS9ETybus44Z3kRqqMZBp80zw')

df <- long %>% 
  mutate(lab = ifelse(str_detect(treatment, 'utah'), 'UU', 'DPAA'), 
         treated = ifelse(str_detect(treatment, 'untreated'), F, T),
         temp = case_when(str_detect(treatment, '30') ~ '30', 
                          str_detect(treatment, '50') ~ '50'), 
         baked = ifelse(str_detect(treatment, 'baked'), T, F), #assuming it's 30 for now
         treatment = substring(treatment, 6)
  ) 

write.csv(df, file = "singlevalues.csv")

# Intra-lab comparison, treated-untreated--------------------------------------
uu <- df %>% 
  filter(lab == 'UU')

uu$dC.off = uu$dO.off = rep(0)

sid = unique(uu$sample)
for(i in sid){
  uu$dC.off[uu$sample == i] = 
    uu$d13C[uu$sample == i] - 
    uu$d13C[uu$sample == i & uu$treatment == 'untreated']
}

for(i in sid){
  uu$dO.off[uu$sample == i] = 
    uu$d18O[uu$sample == i] - 
    uu$d18O[uu$sample == i & uu$treatment == 'untreated']
}

dpaa <- df %>% 
  filter(lab == 'DPAA')

dpaa$dC.off = dpaa$dO.off = rep(0)

sid = unique(uu$sample)
for(i in sid){
  dpaa$dC.off[dpaa$sample == i] = 
    dpaa$d13C[dpaa$sample == i] - 
    dpaa$d13C[dpaa$sample == i & dpaa$treatment == 'untreated']
}

for(i in sid){
  dpaa$dO.off[dpaa$sample == i] = 
    dpaa$d18O[dpaa$sample == i] - 
    dpaa$d18O[dpaa$sample == i & dpaa$treatment == 'untreated']
}

df <- rbind(uu, dpaa)
write.csv(df, file = "intralab.csv")

ggplot(data = subset(df, treatment != 'untreated'), aes(x = treatment, y = dC.off, fill = lab)) + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot()
# Delta Calculated from Excel ---------------------------------------------

library(readxl)
delta <- read_excel("data/DeltaValues.xlsx")
  
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

write.csv(long, file = "delta.csv")


#code graveyard
lab = case_when(str_detect(type, 'lab') ~ 'interlab',
                str_detect(type, 'utah') ~ 'UU',
                str_detect(type, 'dpaa') ~'DPAA' 
), 


'DC_dpaa_treated_50v30' = '50-30, Treated',
'DO_dpaa_treated_50v30' = '50-30, Treated',
'DC_dpaa_treated_untreated' = 'Treated-Untreated',
'DO_dpaa_treated_untreated' = 'Treated-Untreated',
'DC_dpaa_untreated_50v30' = '50-30, Untreated',
'DO_dpaa_untreated_50v30' = '50-30, Untreated', 
'DC_dpaa_untreated_baking' = 'Baking-Not Baking',
'DO_dpaa_untreated_baking' = 'Baking-Not Baking', 
'DC_dpaa_treated_baking' = 'Baking-Not Baking, Treated', 
'DO_dpaa_treated_baking' = 'Baking-Not Baking, Treated',

'DC_utah_untreated_baking' = 'Baking-Not Baking',
'DO_utah_untreated_baking' = 'Baking-Not Baking', 
'DC_utah_treated_untreated' = 'Treated-Untreated', 
'DO_utah_treated_untreated' = 'Treated-Untreated', 
'DC_utah_treated_baking' = 'Baking-Not Baking, Treated', 
'DO_utah_treated_baking' = 'Baking-Not Baking, Treated'