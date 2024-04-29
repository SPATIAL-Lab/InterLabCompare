# Setup -------------------------------------------------------------------
# run this before anything else, it loads librarys and the data frames 
library(dplyr); library(tidyr);library(ggplot2); library(stringr); 
library(ggpubr); library(lsr)

delta <- read.csv('data/delta.csv')
intralab1 <- read.csv('data/intralab.csv') %>% 
  filter(treatment != 'Untreated, Baked, 30 Rxn Temp')
interlab1 <- read.csv('data/interlab.csv')
intralab2 <- read.csv('data/intralab2.csv')
sv <- read.csv("data/singlevalues.csv")

# Single Value Data -------------------------------------------------------
#how do the samples look, regardless of treatment? 
summSingle <- sv %>% 
  group_by(lab, sample) %>% 
  summarize(C_mean = round(mean(d13C), 2), 
            C_sd = round(sd(d13C), 2), 
            C_min = min(d13C),
            C_max = max(d13C), 
            O_mean = round(mean(d18O), 2), 
            O_sd = round(sd(d18O), 2), 
            O_min = min(d18O),
            O_max = max(d18O)) # huh some samples seem to just have higher SD 

# Summarizing interlab Values --------------------------------------------------

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
# Treated v Untreated by Lab ----------------------------------------------

t.test(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'C')$value)
round(cohensD(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'C')$value), 1)

t.test(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'O')$value)
round(cohensD(subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' & iso == 'O')$value), 1)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)


# Baking and Reaction Temp -----------------------------------------------------------

t.test(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, Own Rxn Temp' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, 50 Rxn Temp' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Unbaked, 30 Rxn Temp' & iso == 'O')$value)

# Intralab stats ----------------------------------------------------------

t.test(subset(il, treatment == 'Treated, Baked, 30 Rxn Temp')$dO.off)
cohensD(subset(il, treatment == 'Treated, Baked, 30 Rxn Temp')$dO.off)

t.test(subset(delta, type == 'Treated' & iso == 'O')$value)
cohensD(subset(delta, type == 'Treated' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated' & iso == 'O')$value)

# Intralab visualization --------------------------------------------------
summIntra <- intralab1 %>% 
  group_by(treatment, lab) %>% 
  summarize(
    Omean = round(mean(dO.off), 1),
    Osd = round(sd(dO.off), 1), 
    #Omin = min(dO.off),
    #Omax = max(dO.off), 
    Orange = max(dO.off) - min(dO.off),
    #Osrd = (sd(dO.off)*100)/mean(dO.off),
    Cmean = round(mean(dC.off), 1),
    Csd = round(sd(dC.off), 1), 
    #Cmin = min(dC.off),
    #Cmax = max(dC.off), 
    Crange = max(dC.off) - min(dC.off),
    #Csrd = (sd(dC.off)*100)/mean(dC.off)
  )


# Intralab ttests, oxygen -------------------------------------------------

intra_t_test <- sort(unique(intralab1$treatment))
ttest_intralab_DPAA = data.frame(intra_t_test)
datacols = c("O")
for(i in 1:5){
  tt = t.test(subset(intralab1, treatment == paste(intra_t_test[i]) & lab == 'DPAA')$dO.off)
  cd = cohensD(subset(intralab1, treatment == intra_t_test[i] & lab == 'DPAA')$dO.off)
  ttest_intralab_DPAA$estimate[i] = tt$estimate
  ttest_intralab_DPAA$stat[i] = round(tt$statistic, 2)
  ttest_intralab_DPAA$pvalue[i] = round(tt$p.value, 3)
  ttest_intralab_DPAA$cohens[i] = round(cd, 1)
  }

intra_t_test <- sort(unique(subset(intralab1, lab == 'UU')$treatment))
ttest_intralab_SIRFER = data.frame(intra_t_test)
datacols = c("O")
for(i in 1:3){
  tt = t.test(subset(intralab1, treatment == paste(intra_t_test[i]) & lab == 'UU')$dO.off)
  cd = cohensD(subset(intralab1, treatment == intra_t_test[i] & lab == 'UU')$dO.off)
  ttest_intralab_SIRFER$mean[i] = tt$estimate
  ttest_intralab_SIRFER$t[i] = round(tt$statistic, 2)
  ttest_intralab_SIRFER$pvalue[i] = round(tt$p.value, 3)
  ttest_intralab_SIRFER$cohens[i] = round(cd, 1)
}

# Intralab ttests, carbon -------------------------------------------------

intra_t_test <- sort(unique(intralab1$treatment))
ttest_intralab_DPAA_C = data.frame(intra_t_test)
datacols = c("C")
for(i in 1:5){
  tt = t.test(subset(intralab1, treatment == paste(intra_t_test[i]) & lab == 'DPAA')$dC.off)
  cd = cohensD(subset(intralab1, treatment == intra_t_test[i] & lab == 'DPAA')$dC.off)
  ttest_intralab_DPAA_C$mean[i] = tt$estimate
  ttest_intralab_DPAA_C$t[i] = round(tt$statistic, 2)
  ttest_intralab_DPAA_C$pvalue[i] = round(tt$p.value, 3)
  ttest_intralab_DPAA_C$cohens[i] = round(cd, 1)
}

intra_t_test <- sort(unique(subset(intralab1, lab == 'UU')$treatment))
ttest_intralab_SIRFER_C = data.frame(intra_t_test)
datacols = c("C")
for(i in 1:3){
  tt = t.test(subset(intralab1, treatment == paste(intra_t_test[i]) & lab == 'UU')$dC.off)
  cd = cohensD(subset(intralab1, treatment == intra_t_test[i] & lab == 'UU')$dC.off)
  ttest_intralab_SIRFER_C$mean[i] = tt$estimate
  ttest_intralab_SIRFER_C$t[i] = round(tt$statistic, 2)
  ttest_intralab_SIRFER_C$pvalue[i] = round(tt$p.value, 3)
  ttest_intralab_SIRFER_C$cohens[i] = round(cd, 1)
}


# Intralab linear relationships -------------------------------------------

## Carbon -------------------------------------------
intra_linear <- sort(unique(intralab2$treatment))
linear_intralab_DPAA_C = data.frame(intra_linear)
datacols = c("C")
for(i in 1:5){
  linear = lm(subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'DPAA')$d13C ~
            subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'DPAA')$d13Ccompare)
  linear_intralab_DPAA_C$intercept[i] = linear[["coefficients"]][["(Intercept)"]]
  linear_intralab_DPAA_C$slope[i] = linear[["coefficients"]][['subset(intralab2, treatment == paste(intra_linear[i]) & lab == "DPAA")$d13Ccompare']]
}
linear_intralab_DPAA_C$lab = "DPAA"

intra_linear <- sort(unique(subset(intralab2, lab == 'UU')$treatment))
linear_intralab_UU_C = data.frame(intra_linear)
datacols = c("C")
for(i in 1:5){
  linear = lm(subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'UU')$d13C ~
                subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'UU')$d13Ccompare)
  linear_intralab_UU_C$intercept[i] = linear[["coefficients"]][["(Intercept)"]]
  linear_intralab_UU_C$slope[i] = linear[["coefficients"]][['subset(intralab2, treatment == paste(intra_linear[i]) & lab == "UU")$d13Ccompare']]
}
linear_intralab_UU_C$lab = "SIRFER"

linear_intralab_C <- rbind(linear_intralab_DPAA_C, linear_intralab_UU_C)
rm(linear_intralab_UU_C, linear_intralab_DPAA_C)

## Oxygen ------------------------------------------------------------------
intra_linear <- sort(unique(intralab2$treatment))
linear_intralab_DPAA_O = data.frame(intra_linear)
datacols = c("C")
for(i in 1:5){
  linear = lm(subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'DPAA')$d18O ~
                subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'DPAA')$d18Ocompare)
  linear_intralab_DPAA_O$intercept[i] = linear[["coefficients"]][["(Intercept)"]]
  linear_intralab_DPAA_O$slope[i] = linear[["coefficients"]][['subset(intralab2, treatment == paste(intra_linear[i]) & lab == "DPAA")$d18Ocompare']]
}
linear_intralab_DPAA_O$lab = "DPAA"

intra_linear <- sort(unique(subset(intralab2, lab == 'UU')$treatment))
linear_intralab_UU_O = data.frame(intra_linear)
datacols = c("O")
for(i in 1:5){
  linear = lm(subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'UU')$d18O ~
                subset(intralab2, treatment == paste(intra_linear[i]) & lab == 'UU')$d18Ocompare)
  linear_intralab_UU_O$intercept[i] = linear[["coefficients"]][["(Intercept)"]]
  linear_intralab_UU_O$slope[i] = linear[["coefficients"]][['subset(intralab2, treatment == paste(intra_linear[i]) & lab == "UU")$d18Ocompare']]
}
linear_intralab_UU_O$lab = "SIRFER"

linear_intralab_O <- rbind(linear_intralab_DPAA_O, linear_intralab_UU_O)
rm(linear_intralab_UU_O, linear_intralab_DPAA_O)

# Re-calculating RID ------------------------------------------------------
## RID for each group comparison -------------------------------------------
# did you know I can do this whole stupid thing in one line of dplyr. See summInterlab

treats = sort(unique(delta$type))
RIDO <- data.frame(treats)
for(i in 1:6){
  RIDO$RID[i] = round((mean(subset(delta, iso == "O" & type == paste(treats[i]))$value) + 2*sd(subset(delta, iso == "O" & type == paste(treats[i]))$value)), 1)
  RIDO$mean[i] = round(mean(subset(delta, iso == "O" & type == paste(treats))$value), 2)
  RIDO$sd[i] = round(sd(subset(delta, iso == "O" & type == paste(treats[i]))$value), 2)
}
RIDC <- data.frame(treats)
for(i in 1:6){
  RIDC$RID[i] = round((mean(subset(delta, iso == "C" & type == paste(treats[i]))$value) + 2*sd(subset(delta, iso == "C" & type == paste(treats[i]))$value)), 1)
  RIDC$mean[i] = round(mean(subset(delta, iso == "C" & type == paste(treats[i]))$value), 2)
  RIDC$sd[i] = round(sd(subset(delta, iso == "C" & type == paste(treats[i]))$value), 2)
  }


# Baking Comparison -------------------------------------------------------
t.test(d18O~treatment, 
       data = sv %>% filter(lab == 'DPAA' & treatment == 'treated_baked_30' |  
                                            lab == 'DPAA' & treatment == "treated_30"),
       paired = T)

t.test(d18O~treatment,
       data = sv %>% filter(lab == 'DPAA' & treatment == 'untreated_baked_30' |  
                              lab == 'DPAA' & treatment == "untreated_30"),
       paired = T)

t.test(d18O~treated,
       data = dpaa,
       paired = T)
t.test(d18O~baked,
       data = dpaa,
       paired = T)

# can't compare baking at SIRFER...

# ANOVA??? ---------------------------------------------------------------
kruskal.test(data = subset(sv), d18O ~ lab * treated * temp * baked)

anova(lm(data = subset(sv), d18O ~ lab *treated * temp * baked))
anova(lm(data = subset(sv), d13C ~ lab *treated * temp * baked))

anova(lm(data = subset(sv, lab == 'UU'), d18O ~ treated * temp * baked))

   