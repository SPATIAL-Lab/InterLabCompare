# Setup -------------------------------------------------------------------

library(dplyr); library(tidyr);library(ggplot2); library(stringr); 
library(ggpubr); library(lsr)

delta <- read.csv('data/delta.csv')
sv <- read.csv('data/singlevalues.csv')
intralab1 <- read.csv('data/intralab.csv') %>% 
  filter(treatment != 'Untreated, Baked, 30 Rxn Temp')
interlab1 <- read.csv('data/interlab.csv')
intralab2 <- read.csv('data/intralab2.csv')
cols <- c("DPAA" = "#97c2f7",  "UU" = "#2a3f70")
cols2 <- c("O" = "#8f3858", "C" = "#6670d9")
cols3 <- c("Treated, Baked, 30 Rxn Temp" = "#ADE3C0FF", 
           "Treated, Unbaked, 50 Rxn Temp" = "#43BBADFF",
           "Treated, Unbaked, 30 Rxn Temp" = "#3487A6FF",
           "Untreated, Unbaked, 50 Rxn Temp" = "#3D5296FF", 
           "Untreated, Unbaked, 30 Rxn Temp" = "#35264CFF")
cols4 <- c("Treated, Baked, 30 Rxn Temp" = "#43BBADFF", 
           "Treated, Unbaked, 50 Rxn Temp" = "#43BBADFF",
           "Treated, Unbaked, 30 Rxn Temp" = "#43BBADFF",
           "Untreated, Unbaked, 50 Rxn Temp" = "#35264CFF", 
           "Untreated, Baked, 30 Rxn Temp" = "#35264CFF")
shps <- c("Treated, Baked, 30 Rxn Temp" = 21, 
          "Treated, Unbaked, 50 Rxn Temp" = 21,
          "Treated, Unbaked, 30 Rxn Temp" = 21,
          "Untreated, Unbaked, 50 Rxn Temp" = 22, 
          "Untreated, Baked, 30 Rxn Temp" = 22)
cols5 <- c("Treated, Baked, 30 Rxn Temp" = "#8f3858", 
          "Treated, Unbaked, 50 Rxn Temp" = "#6670d9",
          "Treated, Unbaked, 30 Rxn Temp" = "#8f3858",
          "Untreated, Unbaked, 50 Rxn Temp" = "#6670d9", 
          "Untreated, Baked, 30 Rxn Temp" = "#8f3858")
cols6 <- c("Treated, Baked, 30 Rxn Temp" = "#33386C", 
           "Treated, Unbaked, 50 Rxn Temp" = "#8f3858",
           "Treated, Unbaked, 30 Rxn Temp" = "#6670d9",
           "Untreated, Unbaked, 50 Rxn Temp" = "#8f3858", 
           "Untreated, Baked, 30 Rxn Temp" = "#33386C")
#cc7b81 if we had a baked 50

# Figure 1 ----------------------------------------------------------------

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(delta, type == 'Treated, Unbaked, Own Rxn Temp' | type == 'Untreated, Baked, 30 Rxn Temp'), 
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
       y = expression(paste(Delta, "isotope value", " (\u2030)"))) + 
  scale_x_discrete(labels = function(treatment) str_wrap(treatment, width = 10))
ggsave("figures/Figure1.png", units = c("in"), width = 7, height = 4)


# Figure 2 ----------------------------------------------------------------

delta %>% filter(str_detect(type, "Untreated")) %>% 
  ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(aes(x = type, y = value, fill = iso)) + 
  theme_classic() +
  scale_fill_manual(values = cols2, 
                    name = "Isotope", 
                    labels = c(expression(paste(delta^13, 'C')), 
                               expression(paste(delta^18, 'O')))) + 
  #scale_x_discrete(labels = c('Own Lab Protocols', '50°C Reaction', 'Baked, 30°C Reaction')) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) + 
  labs(x = "Treatment", 
       y = expression(paste(Delta, "isotope value", " (\u2030)"))) + 
  scale_x_discrete(labels = function(treatment) str_wrap(treatment, width = 10))
ggsave("figures/Figure2.png", units = c("in"), width = 7, height = 4)


# Interlab scatter plots --------------------------------------------------
# Let's make 1:1 lines to explore changes in values as we change treatments. 

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

interscatterC <- ggplot() + 
  geom_point(data = interlab1, aes(x = d13Cuu, y = d13Cdpaa, fill = treatment,
                                   shape = treatment), size = 3) + 
  geom_abline(slope=1, intercept = 0) +
  scale_fill_manual(values = cols6, 
                    name = "Treatment") +
  scale_shape_manual(values = shps, 
                     name = "Treatment") + 
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
#ggsave("figures/interscatterC2.png", units = c("in"), width = 7, height = 5)


interscatterO <- ggplot() + 
  geom_point(data = interlab1, aes(x = d18Ouu, y = d18Odpaa, fill = treatment, shape = treatment), size = 3) + 
  geom_abline(slope=1, intercept = 0) +
  scale_fill_manual(values = cols6, 
                    name = "Treatment") +
  scale_shape_manual(values = shps, 
                     name = "Treatment") + 
  theme_classic() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        legend.text = element_text(size = 12)) + 
  labs(x = expression(paste('SIRFER ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2)) + 
  xlim(-9, 3) + 
  ylim(-9, 3)
#ggsave("figures/interscatterO.png", units = c("in"), width = 7, height = 5)

ggarrange(interscatterO, interscatterC, nrow = 1, ncol = 2, common.legend = T, legend="bottom")
ggsave("figures/interscatter.png", units = c("in"), width = 7, height = 5)

O1 <- ggplot() + 
  geom_point(data = subset(interlab1, treatment == "Treated, Baked, 30 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#60CEACFF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('SIRFER ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
O2 <- ggplot() + 
  geom_point(data = subset(interlab1, treatment == "Treated, Unbaked, 50 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#3497A9FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('SIRFER ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
O3 <- ggplot() + 
  geom_point(data = subset(interlab1, treatment == "Untreated, Baked, 30 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#395D9CFF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('SIRFER ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
O4 <- ggplot() + 
  geom_point(data = subset(interlab1, treatment == "Untreated, Unbaked, 50 Rxn Temp"), 
             aes(x = d18Ouu, y = d18Odpaa), size = 3, color = "#382A54FF") + 
  geom_abline(slope=1, intercept = 0) +
  theme_classic() +
  theme( 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14), 
  ) + 
  labs(x = expression(paste('SIRFER ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste('DPAA ', delta^18, 'O', " (\u2030)")))
ggarrange(O1, O2, O3, O4, nrow = 2, ncol = 2, labels = "AUTO")
ggsave("figures/interscatterOArranged.png", units = c("in"), width = 7, height = 5)

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

# What if we want 1:1 plots? Well, time to go wide
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

#okay but what if we did all the DPAA options, and then all the SIRFER options available? 

intraO_DPAA <- ggplot() + 
  geom_point(data = subset(intralab2, lab == 'DPAA'),
             aes(y = d18O,
                 x = d18Ocompare,
                 fill = treatment,
                 shape = treatment),
             size = 3) + 
  geom_abline(slope = 1, intercept = 0) +
  scale_fill_manual(values = cols3,
                     name = 'Treatment') +
  scale_shape_manual(values = shps3,
                     name = "Treatment") + 
  theme_classic() +
  theme() + 
  labs(title = "DPAA", 
       x = expression(paste('Untreated, Baked, 30 Rxn Temp ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste(delta^18, 'O', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2)) + 
  scale_x_continuous(limits = c(-10, 3)) + 
  scale_y_continuous(limits = c(-10, 3))

intraO_UU <- ggplot() + 
  geom_point(data = subset(intralab2, lab == 'UU'),
             aes(y = d18O,
                 x = d18Ocompare,
                 fill = treatment,
                 shape = treatment),
             size = 3) + 
  geom_abline(slope = 1, intercept = 0) +
  scale_fill_manual(values = cols3,
                    name = 'Treatment') +
  scale_shape_manual(values = shps3,
                     name = "Treatment") + 
  theme_classic() +
  theme() + 
  labs(title = "SIRFER", 
       x = expression(paste('Untreated, Baked, 30 Rxn Temp ', delta^18, 'O', " (\u2030)")), 
       y = expression(paste(delta^18, 'O', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2)) + 
  scale_x_continuous(limits = c(-10, 3)) + 
  scale_y_continuous(limits = c(-10, 3))

ggarrange(intraO_DPAA, intraO_UU, nrow = 1, ncol = 2, common.legend = T, legend = 'bottom')
ggsave("figures/intraO11.png", units = c("mm"), width = 200, height = 120)

intraC_DPAA <- ggplot() + 
  geom_point(data = subset(intralab2, lab == 'DPAA'),
             aes(y = d13C,
                 x = d13Ccompare,
                 fill = treatment,
                 shape = treatment),
             size = 3) + 
  geom_abline(slope = 1, intercept = 0) +
  scale_fill_manual(values = cols3,
                    name = 'Treatment') +
  scale_shape_manual(values = shps3, 
                     name = "Treatment") + 
  theme_classic() +
  theme() + 
  labs(title = "DPAA", 
       x = expression(paste('Untreated, Baked, 30 Rxn Temp ', delta^13, 'C', " (\u2030)")), 
       y = expression(paste(delta^13, 'C', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2)) + 
  scale_x_continuous(limits = c(-17, -7)) + 
  scale_y_continuous(limits = c(-17, -7))

intraC_UU <- ggplot() + 
  geom_point(data = subset(intralab2, lab == 'UU'),
             aes(y = d13C,
                 x = d13Ccompare,
                 fill = treatment,
                 shape = treatment),
             size = 3) + 
  geom_abline(slope = 1, intercept = 0) +
  scale_fill_manual(values = cols3,
                    name = 'Treatment') +
  scale_shape_manual(values = shps3,
                     name = "Treatment") + 
  theme_classic() +
  theme() + 
  labs(title = "SIRFER", 
       x = expression(paste('Untreated, Baked, 30 Rxn Temp ', delta^13, 'C', " (\u2030)")), 
       y = expression(paste(delta^13, 'C', " (\u2030)"))) + 
  guides(fill = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2)) + 
  scale_x_continuous(limits = c(-17, -7)) + 
  scale_y_continuous(limits = c(-17, -7))

ggarrange(intraC_DPAA, intraC_UU, nrow = 1, ncol = 2, common.legend = T, legend="bottom")
ggsave("figures/intraC11.png", units = c("mm"), width = 200, height = 120)
