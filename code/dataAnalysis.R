library(dplyr); library(tidyr);library(ggplot2); library(stringr)

delta <- read.csv('data/delta.csv')
sv <- read.csv('data/singlevalues.csv')
il <- read.csv('intralab.csv') %>% 
  filter(treatment != 'untreated')
cols <- c("DPAA" = "#97c2f7",  "interlab" = "#5474C0", "UU" = "#2a3f70")
cols2 <- c("O" = "#8f3858", "C" = "#6670d9")
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
t.test(subset(delta, type == 'Treated' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated' & iso == 'C')$value)
t.test(subset(delta, type == 'Untreated' & iso == 'O')$value)

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(delta, type == 'Untreated' | type == 'Treated'), 
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

# Baking and Reaction Temp -----------------------------------------------------------

t.test(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'C')$value)
t.test(subset(delta, type == 'Untreated, 50 Rxn Temp' & iso == 'O')$value)

t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'C')$value)
t.test(subset(delta, type == 'Untreated, Baked, 30 Rxn Temp' & iso == 'O')$value)

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


# Absolute Values??? ------------------------------------------------------
delta$value <- abs(delta$value)
#nope this was terrible. A terrible plan. 
summDelta <- delta %>% 
  group_by(iso, type) %>% 
  summarize(mean = round(mean(value), 2), 
            sd = round(sd(value), 2), 
            min = min(value),
            max = max(value), 
            srd = (sd(value)/mean(value))*100)


# Variance tests (because I hate myself) ----------------------------------

var.test(subset(sv, lab == 'UU' & treatment == 'untreated_50')$d13C, 
         subset(sv, lab == 'DPAA' & treatment == 'untreated_30')$d13C)
var.test(subset(sv, lab == 'UU' & treatment == 'treated_50')$d13C, 
         subset(sv, lab == 'DPAA' & treatment == 'treated_30')$d13C)

var.test(subset(sv, lab == 'UU' & treatment == 'untreated_50')$d18O, 
         subset(sv, lab == 'DPAA' & treatment == 'untreated_30')$d18O)
var.test(subset(sv, lab == 'UU' & treatment == 'treated_50')$d18O, 
         subset(sv, lab == 'DPAA' & treatment == 'treated_30')$d18O)
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
deltaSumm <- delta %>% 
  group_by(type, iso) %>% 
  summarize(mean = round(mean(value), 2),
            sd = round(sd(value), 2), 
            range = round(max(value) - min(value), 2)
            )

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

