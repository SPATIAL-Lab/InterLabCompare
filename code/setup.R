library(dplyr); library(googlesheets4); library(tidyr);
library(stringr); library(ggplot2)

wide <- read_sheet('https://docs.google.com/spreadsheets/d/1h6h5yPlmJBaqjx_mOwyS9ETybus44Z3kRqqMZBp80zw')

df <- wide %>% 
  pivot_longer(!Sample, 
               names_to = 'type', 
               values_to = 'value')

df <- df%>% 
  mutate(lab = ifelse(str_detect(type, 'utah'), 'UU', 'DPAA'), 
         iso = ifelse(str_detect(type, 'd13c'), 'C', 'O'), 
         treated = ifelse(str_detect(type, 'untreated'), F, T),
         temp = case_when(str_detect(type, '30') ~ '30', 
                          str_detect(type, '50') ~ '50'), 
         baked = ifelse(str_detect(type, 'baked'), T, F), #assuming it's 30 for now
         d18O = ifelse(str_detect(type, 'd18o'), value, NA), 
         d13C = ifelse(str_detect(type, 'd13c'), value, NA)
         ) %>%
  rename(sample = Sample)

write.csv(df, file = "singlevalues.csv")
# First looks at sample values --------------------------------------------
df %>% group_by(sample) %>% 
  summarise(Count= n(),Mean=mean(d13C, na.rm = T),SD=sd(d13C, na.rm = T))

df %>% group_by(sample) %>% 
  summarise(Count= n(),Mean=mean(d18O, na.rm = T),SD=sd(d18O, na.rm = T))
#is that a lot of oxygen variance?

#okay next up we want columns of the offset
#between samples (Delta) of: DPAA bake


# DPAA UU untreated -------------------------------------------------------
treatment <- df %>% 
  filter(treated == F & is.na(temp) & baked == F)

treatment$dC.off = treatment$dO.off = rep(0)

treatmentC <- treatment %>% 
  filter(iso == "C")

sid = unique(treatment$sample)
for(i in sid){
  treatmentC$dC.off[treatmentC$sample == i] = 
    treatmentC$d13C[treatmentC$sample == i & treatmentC$lab == 'DPAA'] - 
    treatmentC$d13C[treatmentC$sample == i & treatmentC$lab == 'UU']
}

treatmentO <- treatment %>% 
  filter(iso == "O")

sid = unique(treatmentO$sample)
for(i in sid){
  treatmentO$dO.off[treatmentO$sample == i] = 
    treatmentO$d18O[treatmentO$sample == i & treatmentC$lab == 'DPAA'] - 
    treatmentO$d18O[treatmentO$sample == i & treatmentC$lab == 'UU']
}

treatmentC <- treatmentC %>%  
  distinct(sample, .keep_all = T)  
treatmentC$compare = 'DPAA-UU Untreated'

treatmentO <- treatmentO %>%  
  distinct(sample, .keep_all = T)  

treatmentO$compare = 'DPAA-UU Untreated'

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = treatmentO, aes(y = dO.off, fill = lab)) + 
  theme_classic() +
  scale_fill_manual(values = c("pink", "purple")) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Lab Comparison, untreated", 
       y = expression(paste(Delta^18, "O", " (\u2030, VPDB)")))

overallO <- treatmentO
overallC <- treatmentC
# 50-30 comparison WIP---------------------------------------------------------------
# hrm the 50-30 comparison is actually four types of comparisons: 50-30 untreated or 50-30 treated, 
# or 50 T-U *or* 30 T-U

# ugh so first 50-30 untreated
treatment <- df %>% 
  filter(!is.na(temp) & treated == F)

treatment$dC.off = treatment$dO.off = rep(0)
# note that Utah only baked at 30, then compared treated and untreated
treatment5030un <- treatment %>% 
  filter(iso == "C")

sid = unique(treatmentCUU$sample)

for(i in sid){
  treatmentCUU$dC.off[treatmentCUU$sample == i] = 
    treatmentCUU$d13C[treatmentCUU$sample == i & treatmentCUU$temp == 50] - 
    treatmentCUU$d13C[treatmentCUU$sample == i & treatmentCUU$temp == 30]
}

treatmentOUU <- treatment %>% 
  filter(iso == "O" & lab == 'UU')

sid = unique(treatmentOUU$sample)
for(i in sid){
  treatmentOUU$dO.off[treatment$sample == i] = 
    treatmentOUU$d18O[treatment$sample == i & treatmentOUU$treated == T] - 
    treatmentOUU$d18O[treatment$sample == i & treatmentOUU$treated == F]
}

treatmentCDPAA <- treatment %>% 
  filter(iso == "C" & lab == "DPAA")
sid = unique(treatmentCUU$sample)

for(i in sid){
  treatmentCDPAA$dC.off[treatmentCDPAA$sample == i] = 
    treatmentCDPAA$d13C[treatmentCDPAA$sample == i & treatmentCDPAA$treated == T] - 
    treatmentCDPAA$d13C[treatmentCDPAA$sample == i & treatmentCDPAA$treated == F]
  
}

treatmentODPAA <- treatment %>% 
  filter(iso == "O" & lab == 'DPAA')
sid = unique(treatmentODPAA$sample)
for(i in sid){
  treatmentODPAA$dO.off[treatmentODPAA$sample == i] = 
    treatmentODPAA$d18O[treatmentODPAA$sample == i & treatmentODPAA$treated == T] - 
    treatmentODPAA$d18O[treatmentODPAA$sample == i & treatmentODPAA$treated == F]
  
}

treatmentC <- rbind(treatmentCDPAA, treatmentCUU)
treatmentC <- treatmentC %>% 
  group_by(lab) %>% 
  distinct(sample, .keep_all = T)  

treatmentC$compare = 'Treated-Untreated'

treatmentO <- rbind(treatmentODPAA, treatmentOUU)
treatmentO <- treatmentO %>% 
  group_by(lab) %>% 
  distinct(sample, .keep_all = T)  

treatmentO$compare = 'Treated-Untreated'

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = treatmentO, aes(x = lab, y = dO.off, fill = lab, color = lab)) + 
  theme_classic() +
  scale_fill_manual(values = c("pink", "purple")) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^18, "O", " (\u2030, VPDB)")))

# Treated versus Untreated ------------------------------------------------
treatment <- df %>% 
  filter(
    str_detect(type, 'baked', negate = T)
  ) 

treatment$dC.off = treatment$dO.off = rep(0)

treatmentCUU <- treatment %>% 
  filter(iso == "C" & lab == "UU")

sid = unique(treatmentCUU$sample)

for(i in sid){
  treatmentCUU$dC.off[treatmentCUU$sample == i] = 
    treatmentCUU$d13C[treatmentCUU$sample == i & treatmentCUU$treated == T] - 
    treatmentCUU$d13C[treatmentCUU$sample == i & treatmentCUU$treated == F]
}

treatmentOUU <- treatment %>% 
  filter(iso == "O" & lab == 'UU')

sid = unique(treatmentOUU$sample)
for(i in sid){
  treatmentOUU$dO.off[treatment$sample == i] = 
    treatmentOUU$d18O[treatment$sample == i & treatmentOUU$treated == T] - 
    treatmentOUU$d18O[treatment$sample == i & treatmentOUU$treated == F]
}

treatmentCDPAA <- treatment %>% 
  filter(iso == "C" & lab == "DPAA")
sid = unique(treatmentCUU$sample)

for(i in sid){
  treatmentCDPAA$dC.off[treatmentCDPAA$sample == i] = 
    treatmentCDPAA$d13C[treatmentCDPAA$sample == i & treatmentCDPAA$treated == T] - 
    treatmentCDPAA$d13C[treatmentCDPAA$sample == i & treatmentCDPAA$treated == F]
  
}

treatmentODPAA <- treatment %>% 
  filter(iso == "O" & lab == 'DPAA')
sid = unique(treatmentODPAA$sample)
for(i in sid){
  treatmentODPAA$dO.off[treatmentODPAA$sample == i] = 
    treatmentODPAA$d18O[treatmentODPAA$sample == i & treatmentODPAA$treated == T] - 
    treatmentODPAA$d18O[treatmentODPAA$sample == i & treatmentODPAA$treated == F]
  
}

treatmentC <- rbind(treatmentCDPAA, treatmentCUU)
treatmentC <- treatmentC %>% 
  group_by(lab) %>% 
  distinct(sample, .keep_all = T)  

treatmentC$compare = 'Treated-Untreated'

treatmentO <- rbind(treatmentODPAA, treatmentOUU)
treatmentO <- treatmentO %>% 
  group_by(lab) %>% 
  distinct(sample, .keep_all = T)  

treatmentO$compare = 'Treated-Untreated'

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = treatmentO, aes(x = lab, y = dO.off, fill = lab, color = lab)) + 
  theme_classic() +
  scale_fill_manual(values = c("pink", "purple")) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^18, "O", " (\u2030, VPDB)")))

# Delta Calculated from Excel ---------------------------------------------

delta <- read_sheet('https://docs.google.com/spreadsheets/d/1Xah5oyJbzPvUrd-tPMr9mOExBpUDIu0AhFo7mUxgfOQ')
  
long <- delta %>% 
  pivot_longer(!sample, 
               names_to = 'type', 
               values_to = 'value') %>% 
  mutate(lab = case_when(str_detect(type, 'lab') ~ 'interlab',
                         str_detect(type, 'utah') ~ 'UU',
                         str_detect(type, 'dpaa') ~'DPAA' 
                         ), 
         iso = ifelse(str_detect(type, 'DC'), 'C', 'O'),
         type = recode(type, 
                       'DC_lab_untreated_baked' = 'Untreated & Baked',
                       'DO_lab_untreated_baked' = 'Untreated & Baked',
                       'DC_lab_untreated' = 'Untreated',
                       'DO_lab_untreated' = 'Untreated', 
                       'DC_lab_treated' = 'Treated', 
                       'DO_lab_treated' = 'Treated', 
                       'DC_lab_untreated_30' = 'Untreated (30)', #something's up with this one...
                       'DO_lab_untreated_30' = 'Untreated (30)',
                       'DC_lab_untreated_50' = 'Untreated (50)', 
                       'DO_lab_untreated_50' = 'Untreated (50)',
                       
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
                       'DO_utah_treated_baking' = 'Baking-Not Baking, Treated',
                        ) 
         )

write.csv(long, file = "delta.csv")