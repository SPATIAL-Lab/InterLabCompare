library(dplyr); library(tidyr);library(ggplot2); library(stringr)

delta <- read.csv('delta.csv')
sv <- read.csv('singlevalues.csv')
il <- read.csv('intralab.csv') %>% 
  filter(treatment != 'untreated')
cols <- c("DPAA" = "#97c2f7",  "interlab" = "#5474C0", "UU" = "#2a3f70")
cols2 <- c("O" = "#131F71", "C" = "#9CA4f8")
#how do the samples look, regardless of treatment? 
summ <- sv %>% 
  group_by(iso, lab, sample) %>% 
  summarize(mean = mean(value), 
            sd = sd(value), 
            min = min(value),
            max = max(value)) # huh some samples seem to just have higher SD 

ggplot() + 
  geom_point(data = summ, aes(x = sd, y = mean, color = sample, shape = iso))

ggplot() + 
  geom_col(data = summ, aes(y = sd, x = sample, color = iso, fill = lab), position = "dodge") +
  scale_fill_manual(values = cols) + 
  theme_classic()

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = delta, aes(x = type, y = value, fill = iso, color = iso)) + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Treatment", 
       y = expression(paste(Delta, "isotope value", " (\u2030)")))
# what do we only have one isotope set for still? 

cols <- c("DPAA" = "#97c2f7",  "interlab" = "#5474C0", "UU" = "#2a3f70")
cols2 <- c("O" = "#131F71", "C" = "#9CA4f8")

#exploring interlab variability
ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(delta, lab == 'interlab'), 
               aes(x = type, y = value, fill = iso)) + 
  theme_classic() +
  scale_fill_manual(values = cols2, 
                    name = "Isotope", 
                    labels = c(expression(paste(delta^13, 'C')), 
                               expression(paste(delta^18, 'O')))) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) + 
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 10) 

                   ) +
  labs(x = "Protocol", 
       y = expression(paste(Delta, " isotope value ", "(\u2030)")))

# exploring DPAA variability
ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(delta, lab != 'interlab' & iso == 'C'), aes(x = type, y = value, fill = lab)) + 

  theme_classic() +
  scale_fill_manual(values = cols2, 
                    name = "Isotope", 
                    labels = c(expression(paste(delta^13, 'C')), 
                               expression(paste(delta^18, 'O')))) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +   
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "Protocol", 
       y = expression(paste(Delta, "isotope value", " (\u2030)")))


# crazy test
ggplot() + 
  geom_boxplot(data = subset(sv, iso == 'C'), aes(x = type, y = value, fill = iso)) + 
  theme_classic() +
  scale_fill_manual(values = cols2, 
                    name = "Isotope", 
                    labels = c(expression(paste(delta^13, 'C')), 
                               expression(paste(delta^18, 'O')))) + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +   
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "Protocol", 
       y = expression(paste(Delta, "isotope value", " (\u2030)")))

# MMD and RID -------------------------------------------------------------
#according to Pestle, MMD is mean pairwise difference + 4(avg standard deviations of each laboratory)

#calculating mean pairwise difference using delta df
mpdO <- round(mean(subset(delta, lab == 'interlab' & iso == 'O')$value), 2)
mpdC <- round(mean(subset(delta, lab == 'interlab' & iso == 'C')$value), 2)
sdUUO <- round(sd(subset(sv, lab == 'UU' & iso =='O')$value), 2)
sdUUC <- round(sd(subset(sv, lab == 'UU' & iso =='C')$value), 2)
sdDPAAO <- round(sd(subset(sv, lab == 'DPAA' & iso =='O')$value), 2)
sdDPAAC <- round(sd(subset(sv, lab == 'DPAA' & iso =='C')$value), 2)


# Descriptive Stats -------------------------------------------------------

#are the data normally distributed? 
names <- unique(sv$sample)
for (name in names) {
  print(shapiro.test(subset(sv, sample == name)$d18O))
}
for (name in names) {
  print(shapiro.test(subset(sv, sample == name)$d13C))
}#nothing is significant in these outputs
rm(name, names)
#mean and sd of Delta values by protocol
delta %>% 
  filter(lab == 'interlab') %>% 
  group_by(type,iso) %>% 
  summarize(mean = mean(value), 
                 sd = sd(value))

# Interlab stats ----------------------------------------------------------
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

