# Setup -------------------------------------------------------------------

library(dplyr); library(tidyr);library(ggplot2); library(stringr); library(ggpubr)

delta <- read.csv('data/delta.csv')
sv <- read.csv('data/singlevalues.csv')
intralab1 <- read.csv('data/intralab.csv') %>% 
  filter(treatment != 'Untreated, Baked, 30 Rxn Temp')
interlab1 <- read.csv('data/interlab.csv')
intralab2 <- read.csv('data/intralab2.csv')
cols <- c("DPAA" = "#97c2f7",  "UU" = "#2a3f70")
cols2 <- c("O" = "#8f3858", "C" = "#6670d9")
cols3 <- c("Treated, Baked, 30 Rxn Temp" = "#60CEACFF", 
           "Treated, Unbaked, 50 Rxn Temp" = "#3497A9FF", 
           "Untreated, Baked, 30 Rxn Temp" = "#395D9CFF",
           "Untreated, Unbaked, 50 Rxn Temp" = "#382A54FF")

# Single Value Data -------------------------------------------------------
#how do the samples look, regardless of treatment? 
summ <- sv %>% 
  group_by(lab, sample) %>% 
  summarize(C_mean = round(mean(d13C), 2), 
            C_sd = round(sd(d13C), 2), 
            C_min = min(d13C),
            C_max = max(d13C), 
            O_mean = round(mean(d18O), 2), 
            O_sd = round(sd(d18O), 2), 
            O_min = min(d18O),
            O_max = max(d18O)) # huh some samples seem to just have higher SD 

# Comparing Delta Values --------------------------------------------------

summDelta <- delta %>% 
  group_by(iso, type) %>% 
  summarize(mean = round(mean(value), 2), 
            sd = round(sd(value), 2), 
            min = min(value),
            max = max(value), 
            srd = (sd(value)*100)/mean(value)
            )
# Treated v Untreated by Lab ----------------------------------------------
t.test(subset(delta, type == 'Treated' & iso == 'C')$value)
cohensD(subset(delta, type == 'Treated' & iso == 'C')$value)

t.test(subset(delta, type == 'Treated' & iso == 'O')$value)
cohensD(subset(delta, type == 'Treated' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated' & iso == 'O')$value)


ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(delta, type == 'Treated' | type == 'Untreated, Baked, 30 Rxn Temp'), 
               aes(x = type, y = value, fill = iso)) + 
  theme_classic() +
  scale_fill_manual(values = cols2, 
                    name = "Isotope", 
                    labels = c(expression(paste(delta^13, 'C')), 
                               expression(paste(delta^18, 'O')))) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) + 
  labs(x = "Treatment", 
       y = expression(paste(Delta, "isotope value", " (\u2030)")))
ggsave("figures/Figure1.png", units = c("in"), width = 7, height = 4)

# Baking and Reaction Temp -----------------------------------------------------------

t.test(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)
cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)
cohensD(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(delta, type != 'Treated'), 
               aes(x = type, y = value, fill = iso)) + 
  theme_classic() +
  scale_fill_manual(values = cols2, 
                    name = "Isotope", 
                    labels = c(expression(paste(delta^13, 'C')), 
                               expression(paste(delta^18, 'O')))) + 
  scale_x_discrete(labels = c('Own Lab Protocols', '50°C Reaction', 'Baked, 30°C Reaction')) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) + 
  labs(x = "Treatment", 
       y = expression(paste(Delta, "isotope value", " (\u2030)")))
ggsave("figures/Figure2.png", units = c("in"), width = 7, height = 4)

# Interlab scatter plots 
# Let's make 1:1 lines to explore changes in values as we change treatments. 

ggplot() + 
  geom_point(data = il1, aes(x = d13Cuu, y = d13Cdpaa, fill = treatment, shape = treatment), size = 3) + 
  geom_abline(slope=1, intercept = 0) +
  scale_fill_manual(values = cols3, 
                    name = "Treatment") +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     name = "Treatment") + 
  theme_classic() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12)) + 
  labs(x = expression(paste('UU ', delta^13, 'C', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^13, 'C', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2)) + 
  scale_x_continuous(breaks = c(-17, -15, -13, -11, -9)) + 
  scale_y_continuous(breaks = c(-17, -15, -13, -11, -9)) 
ggsave("figures/interscatterC.png", units = c("in"), width = 7, height = 5)

ggplot() + 
  geom_point(data = il1, aes(x = d18Ouu, y = d18Odpaa, fill = treatment, shape = treatment), size = 3) + 
  geom_abline(slope=1, intercept = 0) +
  scale_fill_manual(values = cols3, 
                    name = "Treatment") +
  scale_shape_manual(values = c(21, 22, 23, 24), 
                     name = "Treatment") + 
  theme_classic() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12)) + 
  labs(x = expression(paste('UU ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2)) + 
  xlim(-9, 3) + 
  ylim(-9, 3)
ggsave("figures/interscatterO.png", units = c("in"), width = 7, height = 5)

O1 <- ggplot() + 
  geom_point(data = subset(il1, treatment == "Treated, Baked, 30 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#60CEACFF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('UU ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
O2 <- ggplot() + 
  geom_point(data = subset(il1, treatment == "Treated, Unbaked, 50 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#3497A9FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('UU ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
O3 <- ggplot() + 
  geom_point(data = subset(il1, treatment == "Untreated, Baked, 30 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#395D9CFF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('UU ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
O4 <- ggplot() + 
  geom_point(data = subset(il1, treatment == "Untreated, Unbaked, 50 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#382A54FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('UU ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
ggarrange(O1, O2, O3, O4, nrow = 2, ncol = 2, labels = "AUTO")
ggsave("figures/interscatterOArranged.png", units = c("in"), width = 7, height = 5)


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
summIntra <- intralab %>% 
  group_by(treatment, lab) %>% 
  summarize(
    Omean = round(mean(dO.off), 2),
    Osd = round(sd(dO.off), 2), 
    #Omin = min(dO.off),
    #Omax = max(dO.off), 
    Orange = max(dO.off) - min(dO.off),
    #Osrd = (sd(dO.off)*100)/mean(dO.off),
    Cmean = round(mean(dC.off), 2),
    Csd = round(sd(dC.off), 2), 
    #Cmin = min(dC.off),
    #Cmax = max(dC.off), 
    Crange = max(dO.off) - min(dO.off),
    #Csrd = (sd(dC.off)*100)/mean(dC.off)
  )

intralabO <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(il, treatment != "Treated, Unbaked, 30 Rxn Temp" & treatment != "Untreated, Unbaked, 30 Rxn Temp"), 
               aes(x = treatment, y = dO.off, fill = lab)) + 
  theme_classic() +
  scale_fill_manual(values = cols, 
                    name = "Lab" 
                    ) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) + 
  scale_x_discrete(labels = function(treatment) str_wrap(treatment, width = 10)) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^18, 'O', " (\u2030)")))
#ggsave("figures/intralabO.png", units = c("in"), width = 7, height = 4)

intralabC <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(il, treatment != "Treated, Unbaked, 30 Rxn Temp" & treatment != "Untreated, Unbaked, 30 Rxn Temp"), 
               aes(x = treatment, y = dC.off, fill = lab)) + 
  theme_classic() +
  scale_fill_manual(values = cols, 
                    name = "Lab" 
  ) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)) + 
  scale_x_discrete(labels = function(treatment) str_wrap(treatment, width = 10)) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^13, 'C', " (\u2030)")))
#ggsave("figures/intralabC.png", units = c("in"), width = 7, height = 4)

ggarrange(intralabO, intralabC, nrow = 1, labels = "AUTO")
ggsave("figures/intralab.png", units = c("in"), width = 8, height = 4)

# What if we want 1:1 plots? Well, time to go wide
ggplot() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(data = il2, aes(x = d13C, y = d13Ccompare, color = treatment), size = 3) + 
  theme_classic()

intraO1 <- ggplot() + 
  geom_point(data = subset(il2, treatment == "Treated, Baked, 30 Rxn Temp"), 
             aes(x = d18O, y = d18Ocompare, shape = lab), size = 3, color = "#60CEACFF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    legend.position = 'none',
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('Treated, Baked, 30 Rxn Temp ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('Unreated, Unbaked, 30 Rxn Temp ', delta^18, 'O', " (\u2030)"))) + 
  xlim(-9, 3) + 
  ylim(-9, 3)

intraO2 <- ggplot() + 
  geom_point(data = subset(il2, treatment == "Treated, Unbaked, 50 Rxn Temp"), 
             aes(x = d18O, y = d18Ocompare, shape = lab), size = 3, color = "#3497A9FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    legend.position = 'none',
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('Treated, Baked, 50 Rxn Temp ', delta^18, 'O', " (\u2030)")), 
       y = "") + 
  xlim(-9, 3) + 
  ylim(-9, 3)

intraO3 <- ggplot() + 
  geom_point(data = subset(il2, treatment == "Untreated, Unbaked, 50 Rxn Temp"), 
             aes(x = d18O, y = d18Ocompare, shape = lab), size = 3, color = "#382A54FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme(
    legend.position = c(0.9, 0.1),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('Untreated, Unbaked, 50 Rxn Temp ', delta^18, 'O', " (\u2030)")), 
       y = "", 
       legend = 'Lab') + 
  xlim(-9, 3) + 
  ylim(-9, 3)
ggarrange(intraO1, intraO2, intraO3, nrow = 1, ncol = 3, labels = "AUTO")
ggsave("figures/interscatterOArranged.png", units = c("in"), width = 7, height = 5)


# Graveyard for now -------------------------------------------------------

# Interlab stats
summDelta <- subset(delta, lab == 'interlab') %>% 
  group_by(iso, type) %>% 
  summarize(mean = round(mean(value), 2), 
            sd = round(sd(value), 2), 
            min = min(value),
            max = max(value))

shapiro.test(subset(delta, iso == 'O')$value)
shapiro.test(subset(delta, iso == 'C')$value)
ggplot(subset(delta, iso == 'O'), aes(value)) + geom_density()
ggplot(subset(delta, iso == 'C'), aes(value)) + geom_density()

treats = unique(subset(delta, lab == 'interlab')$type)
shapTable = data.frame(treats, "C" = rep(0), "O" = rep(0))
datacols = c("C", "O")

for(i in seq_along(treats)){
  for(j in 1:2){
    shapTable[i, j+1] = shapiro.test(subset(delta, lab == 'interlab' & type == treats[i] & iso == datacols[j])$value)$p.value
  }
}

# One-sided t.tests for interlab comparisons
diffTable = tTable = pTable = data.frame(treats, "dC" = rep(0), "dO" = rep(0))

for(i in seq_along(treats)){
  for(j in 1:2){
    test = t.test(subset(delta, lab == 'interlab' & type == treats[i] & iso == datacols[j])$value)
    #delta[delta$type == treats[i], datacols[j]])
    diffTable[i, j+1] = round(test$estimate, 2)
    tTable[i, j+1] = round(test$statistic, 2)
    pTable[i, j+1] = round(test$p.value, 3)
  }
}
rm(datacols, treats, i, j, test)

# Variance tests (because I hate myself) ----------------------------------

var.test(subset(sv, lab == 'UU' & treatment == 'untreated_50')$d13C, 
         subset(sv, lab == 'DPAA' & treatment == 'untreated_30')$d13C)
var.test(subset(sv, lab == 'UU' & treatment == 'treated_50')$d13C, 
         subset(sv, lab == 'DPAA' & treatment == 'treated_30')$d13C)

var.test(subset(sv, lab == 'UU' & treatment == 'untreated_50')$d18O, 
         subset(sv, lab == 'DPAA' & treatment == 'untreated_30')$d18O)
var.test(subset(sv, lab == 'UU' & treatment == 'treated_50')$d18O, 
         subset(sv, lab == 'DPAA' & treatment == 'treated_30')$d18O)

