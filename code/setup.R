library(dplyr); library(googlesheets4); library(tidyr);
library(stringr); library(ggplot2)

wide <- read_sheet('https://docs.google.com/spreadsheets/d/1h6h5yPlmJBaqjx_mOwyS9ETybus44Z3kRqqMZBp80zw')

df <- wide %>% 
  pivot_longer(!Sample, 
               names_to = 'type', 
               values_to = 'value')

long <- df%>% 
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
# 
# Treated versus Untreated ------------------------------------------------
treatment <- df %>% 
  filter(
    str_detect(type, 'baked', negate = T)
  )

treatment$dC.off = treatment$dO.off = rep(0)

sid = unique(treatment$sample)

for(i in sid){
  treatment$dC.off[treatment$sample == i] = 
    treatment$d13C[treatment$sample == i & treatment$treated == T] - 
    treatment$d13C[treatment$sample == i & treatment$treated == F]
  
}

sid = unique(treatment$sample)
for(i in sid){
  treatment$dO.off[treatment$sample == i] = 
    treatment$d18O[treatment$sample == i & treatment$treated == T] - 
    treatment$d18O[treatment$sample == i & treatment$treated == F]
  
}

treatmentCUU <- treatment %>% 
  filter(iso == "C" & lab == "UU") %>% 
  distinct(sample, .keep_all = T)

treatmentCDPAA <- treatment %>% 
  filter(iso == "C" & lab == "DPAA") %>% 
  distinct(sample, .keep_all = T)  
treatmentC <- rbind(treatmentCDPAA, treatmentCUU)

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = treatmentC, aes(x = lab, y = dC.off, fill = lab, color = lab)) + 
  theme_classic() +
  scale_fill_manual(values = c("pink", "purple")) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^13, "C", " (\u2030, VPDB)")))

# Delta Calculated from Excel ---------------------------------------------

delta <- read_sheet('https://docs.google.com/spreadsheets/d/1Xah5oyJbzPvUrd-tPMr9mOExBpUDIu0AhFo7mUxgfOQ')
  
df <- delta %>% 
  pivot_longer(!sample, 
               names_to = 'type', 
               values_to = 'value') 
df <- df %>% 
  mutate(iso = ifelse(str_detect(type, 'DC'), 'C', 'O'),
         lab = case_when(str_detect(type, 'lab') ~ 'interlab', 
                         str_detect(type, 'dpaa') ~ 'DPAA',
                         str_detect(type, 'uu') ~ 'Utah'),  
         type = recode(type, #haven't finished refining these, WIP
                       'DC_lab_untreated_baking' = 'DPAA-UU, Untreated & Baked',
                       'DC_lab_untreated' = 'DPAA-UU, Untreated',
                       'DC_utah_bakeduntreated' = 'Baking-Not Baking, Utah, Treated', 
                       'DC_dpaa_untreated_baking' = 'Baking-Not Baking, DPAA, Untreated',
                       'DC_dpaa_untreated_50v30' = '50-30, DPAA untreated', 
                       'DO_lab_untreated' = 'DPAA-UU, Untreated', 
                       'DO_lab_untreated_50' = 'DPAA-UU, Untreated (50)', 
                       'DC_dpaa_treated_50v30' = '50-30, DPAA treated', 
                       'DC_dpaa_treated_baking' = 'Baking-Not Baking, DPAA, Treated', 
                       'DC_dpaa_treated_untreated' = 'Treated-Untreated, DPAA', 
                       'DC_lab_treated' = 'DPAA-UU, Treated', 
                       'DC_lab_untreated_30' = 'DPAA-UU, Untreated (30)', 
                       'DO_lab_treated' = 'DPAA-UU, Treated',
                       'DO_lab_untreated_30' = 'DPAA-UU, Untreated (30)' 
                       )
  )


  
 ggplot() + 
   geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
   geom_boxplot(data = subset(df, lab == 'interlab'), aes(x = type, y = value, color = iso, fill = iso)) + 
   theme_classic() +
   scale_fill_manual(values = c("pink", "purple")) +
   scale_color_manual(values = c("#F05039", "#1F449C")) +
   theme(legend.position = "bottom", 
         axis.text.x = element_text(size = 12, angle = 270, vjust = 0.5),
         axis.text.y = element_text(size = 12),
         axis.title = element_text(size = 14), ) +
   labs(x = "Treatment", 
        y = expression(paste(Delta,"\u2030")), 
        color = 'Isotopic Group')
  