# Putting statistical tests that aren't currently in the paper, but may be useful/of interest. 

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


# DataVis Graveyard too ---------------------------------------------------

# Not needed for now ------------------------------------------------------

## Interscatter 1:1
ggplot() + 
  geom_point(data = interlab1, aes(x = d13Cuu, y = d13Cdpaa, fill = treatment,
                                   shape = treatment, color = treatment), size = 3) + 
  geom_abline(slope=1, intercept = 0) +
  scale_fill_manual(values = cols4, 
                    name = "Treatment") +
  scale_shape_manual(values = shps, 
                     name = "Treatment") + 
  scale_color_manual(values = cols6, 
                     name = 'Treatment') + 
  theme_classic() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12)) + 
  labs(x = expression(paste('SIRFER ', delta^13, 'C', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^13, 'C', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2)) + 
  scale_x_continuous(breaks = c(-17, -15, -13, -11, -9)) + 
  scale_y_continuous(breaks = c(-17, -15, -13, -11, -9)) 
ggsave("figures/interscatterC1.png", units = c("in"), width = 7, height = 5)

## Intralab, one by one. 
ggplot() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(data = intralab2, aes(y = d13C, x = d13Ccompare, color = treatment), size = 3) + 
  theme_classic()

intraO1 <- ggplot() + 
  geom_point(data = subset(intralab2, treatment == "Treated, Baked, 30 Rxn Temp"), 
             aes(y = d18O, x = d18Ocompare, shape = lab), size = 3, color = "#60CEACFF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    legend.position = 'none',
    axis.text= element_text(size = 10),
    axis.title = element_text(size = 8), 
  ) + 
  labs(y = "", x = "") + 
  scale_y_continuous(breaks = seq(-9, 1, by = 2)) + 
  scale_x_continuous(breaks = seq(-9, 1, by = 2), 
                     labels = function(d18O) str_wrap(d18O, width = 10))

intraO2 <- ggplot() + 
  geom_point(data = subset(intralab2, treatment == "Treated, Unbaked, 50 Rxn Temp"), 
             aes(y = d18O, x = d18Ocompare, shape = lab), size = 3, color = "#3497A9FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    legend.position = 'none',
    axis.text= element_text(size = 10),
    axis.title = element_text(size = 8), 
  ) + 
  labs(y = "", x = "") +   
  scale_y_continuous(breaks = seq(-9, 1, by = 2)) + 
  scale_x_continuous(breaks = seq(-9, 1, by = 2), 
                     labels = function(d18O) str_wrap(d18O, width = 10))

intraO3 <- ggplot() + 
  geom_point(data = subset(intralab2, treatment == "Untreated, Unbaked, 50 Rxn Temp"), 
             aes(y = d18O, x = d18Ocompare, shape = lab), size = 3, color = "#382A54FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme(
    legend.position = 'none',
    axis.text= element_text(size = 10),
    axis.title = element_text(size = 8), 
  ) + 
  labs(y = "", x = "") + 
  scale_y_continuous(breaks = seq(-9, 1, by = 2)) + 
  scale_x_continuous(breaks = seq(-9, 1, by = 2), 
                     labels = function(d18O) str_wrap(d18O, width = 10))

ggarrange(intraO1, intraO2, intraO3, nrow = 2, ncol = 2, labels = "AUTO")
ggsave("figures/intrascatterOArranged.png", units = c("in"), width = 7, height = 5)

# Intralab boxplots ----------------------------------------------------

intralabO <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(intralab1, treatment != "Treated, Unbaked, 30 Rxn Temp" & treatment != "Untreated, Unbaked, 30 Rxn Temp"), 
               aes(x = treatment, y = dO.off, fill = lab)) + 
  theme_classic() +
  scale_fill_manual(values = cols, 
                    name = "Lab" 
  ) + 
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), ) + 
  scale_x_discrete(labels = function(treatment) str_wrap(treatment, width = 10)) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^18, 'O', " (\u2030)")))
#ggsave("figures/intralabO.png", units = c("in"), width = 7, height = 4)

intralabC <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(intralab1, treatment != "Treated, Unbaked, 30 Rxn Temp" & treatment != "Untreated, Unbaked, 30 Rxn Temp"), 
               aes(x = treatment, y = dC.off, fill = lab)) + 
  theme_classic() +
  scale_fill_manual(values = cols, 
                    name = "Lab" 
  ) + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)) + 
  scale_x_discrete(labels = function(treatment) str_wrap(treatment, width = 10)) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^13, 'C', " (\u2030)")))
#ggsave("figures/intralabC.png", units = c("in"), width = 7, height = 4)

ggarrange(intralabO, intralabC, nrow = 1, common.legend = T, legend = 'bottom')
ggsave("figures/intralab.png", units = c("in"), width = 8, height = 4)

