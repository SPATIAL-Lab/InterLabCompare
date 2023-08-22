library(dplyr); library(googlesheets4); library(tidyr);
library(stringr); library(ggplot2)

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
  
  
 ggplot() + 
   geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
   geom_boxplot(data = delta, aes(x = Treat, y = dO.off, fill = Treat, color = Time)) + 
   theme_classic() +
   scale_fill_manual(values = palette) +
   scale_color_manual(values = c("#F05039", "#1F449C")) +
   theme(legend.position = "none", 
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         axis.title = element_text(size = 14), ) +
   labs(x = "Treatment", 
        y = expression(paste(Delta^18, "O", " (\u2030, VPDB)")))
  