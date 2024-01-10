# Setup -------------------------------------------------------------------

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
  summarize(mean = round(mean(value), 2), 
            sd = round(sd(value), 2), 
            min = min(value),
            max = max(value), 
            srd = (sd(value)*100)/mean(value)
            )
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

t.test(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)

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

# Re-calculating RID ------------------------------------------------------

samples = sort(unique(sv$sample))
SDOuu <- data.frame(samples)
SDOdpaa <- data.frame(samples)
for(i in 1:10){
  SDOuu$sd[i] = sd(subset(sv, sample == paste(samples[i]) & lab == 'UU')$d18O)
  SDOuu$lab = 'UU'
}
for(i in 1:10){
  SDOdpaa$sd[i] = sd(subset(sv, sample == paste(samples[i]) & lab == 'DPAA')$d18O)
  SDOdpaa$lab = 'DPAA'
}

SDO <- rbind(SDOdpaa, SDOuu)
rm(SDOdpaa, SDOuu)
RIDO <- round((mean(subset(delta, iso == "O")$value) + 4*mean(SDO$sd))/2, 1)

SDCuu <- data.frame(samples)
SDCdpaa <- data.frame(samples)
for(i in 1:10){
  SDCuu$sd[i] = sd(subset(sv, sample == paste(samples[i]) & lab == 'UU')$d13C)
  SDCuu$lab = 'UU'
}
for(i in 1:10){
  SDCdpaa$sd[i] = sd(subset(sv, sample == paste(samples[i]) & lab == 'DPAA')$d13C)
  SDCdpaa$lab = 'DPAA'
}

SDC <- rbind(SDCdpaa, SDCuu)
rm(SDCdpaa, SDCuu)
RIDC <- round((mean(subset(delta, iso == "C")$value) + 4*mean(SDC$sd))/2, 1)

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

   