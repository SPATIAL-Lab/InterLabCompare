# Setup -------------------------------------------------------------------
# run this before anything else, it loads librarys and the data frames 
library(dplyr); library(tidyr);library(ggplot2); library(stringr); 
library(ggpubr); library(lsr)

delta <- read.csv('data/delta.csv')
intralab1 <- read.csv('data/intralab.csv') %>% 
  filter(treatment != 'Untreated, Baked, 30 Rxn Temp')
interlab1 <- read.csv('data/interlab.csv')
intralab2 <- read.csv('data/intralab2.csv')
intralab3 <- read.csv('data/intralab3.csv')
sv <- read.csv("data/singlevalues.csv")


# Results in Paper --------------------------------------------------------

# first, compare samples run via usual SOP to when we follow SOP, except for not treating

t.test(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'C')$value)
round(cohensD(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'C')$value), 1)

t.test(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'O')$value)
round(cohensD(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'O')$value), 1)

t.test(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'O')$value)

# what does it look like in terms of repeatability?
mean(subset(intralab3, lab == 'UU')$diffC, na.rm = T)
mean(subset(intralab3, lab == 'DPAA')$diffC, na.rm = T)

mean(subset(intralab3, lab == 'UU')$diffO, na.rm = T)
mean(subset(intralab3, lab == 'DPAA')$diffO, na.rm = T)

# next, compare samples run untreated, unbaked, but different reaction temperatures
t.test(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'C')$value)
round(cohensD(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'C')$value), 1)

t.test(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'O')$value)
round(cohensD(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'O')$value), 1)

# finally, compare samples run untreated, 30C, but un/baked
t.test(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'C')$value)
round(cohensD(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'C')$value), 1)

t.test(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'O')$value)
round(cohensD(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'O')$value), 1)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)
round(cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value), 1)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)
round(cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value), 1)

# Summarizing interlab Values/RID --------------------------------------------------

summInterlab <- delta %>% 
  group_by(iso, type) %>% 
  summarize(min = min(value),
            max = max(value),
            mean = round(mean(value), 1), 
            sd = round(sd(value), 1), 
            min = min(value),
            max = max(value), 
            range = round(max - min, 1), 
            RID = round(mean + 2*sd, 1)
  ) %>% 
  select(-c(min, max))


# Intralab comparisons ----------------------------------------------------

summIntra <- intralab1 %>% 
  group_by(treatment, lab) %>% 
  summarize(
    Omean = round(mean(dO.off), 1),
    Osd = round(sd(dO.off), 1), 
    #Omin = min(dO.off),
    #Omax = max(dO.off), 
    Orange = max(dO.off) - min(dO.off),
    #Osrd = (sd(dO.off)*100)/mean(dO.off),
    Cmean = round(mean(dC.off), 2),
    Csd = round(sd(dC.off), 1), 
    #Cmin = min(dC.off),
    #Cmax = max(dC.off), 
    Crange = max(dC.off) - min(dC.off),
    #Csrd = (sd(dC.off)*100)/mean(dC.off)
  )   
