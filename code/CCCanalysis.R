# let's see how epiR and CCC works for looking at differences between analyses. 
# we'll be calculating Lin's (1989, 2000) concordance correlation coefficient for agreement on a continuous measure.

library(tidyr); library(dplyr); library(ggplot2); library(epiR)

sv <- read.csv("data/singlevalues.csv")
# notes on interpreting CCC https://real-statistics.com/reliability/interrater-reliability/lins-concordance-correlation-coefficient/

# Business as usual
BAU_O <- epi.ccc(subset(sv, treatment == "treated_unbaked_30" & lab == "DPAA")$d18O, subset(sv, treatment == "treated_unbaked_50" & lab == "UU")$d18O)
BAU_C <- epi.ccc(subset(sv, treatment == "treated_unbaked_30" & lab == "DPAA")$d13C, subset(sv, treatment == "treated_unbaked_50" & lab == "UU")$d13C)

#BAU but no chemical pretreatment
untreat_O <- epi.ccc(subset(sv, treatment == "untreated_unbaked_30" & lab == "DPAA")$d18O, subset(sv, treatment == "untreated_unbaked_50" & lab == "UU")$d18O)
untreat_C <- epi.ccc(subset(sv, treatment == "untreated_unbaked_30" & lab == "DPAA")$d13C, subset(sv, treatment == "untreated_unbaked_50" & lab == "UU")$d13C)

untreat_C$rho.c$est # get CCC

# Examine acid rxn temp
temp30_O <- epi.ccc(subset(sv, treatment == "untreated_unbaked_30" & lab == "DPAA")$d18O, subset(sv, treatment == "untreated_unbaked_30" & lab == "UU")$d18O)
temp30_C <- epi.ccc(subset(sv, treatment == "untreated_unbaked_30" & lab == "DPAA")$d13C, subset(sv, treatment == "untreated_unbaked_30" & lab == "UU")$d13C)

temp50_O <- epi.ccc(subset(sv, treatment == "untreated_unbaked_50" & lab == "DPAA")$d18O, subset(sv, treatment == "untreated_unbaked_50" & lab == "UU")$d18O)
temp50_C <- epi.ccc(subset(sv, treatment == "untreated_unbaked_50" & lab == "DPAA")$d13C, subset(sv, treatment == "untreated_unbaked_50" & lab == "UU")$d13C)


# Baking!
baking_O <- epi.ccc(subset(sv, treatment == "untreated_baked_30" & lab == "DPAA")$d18O, subset(sv, treatment == "untreated_baked_30" & lab == "UU")$d18O)
baking_C <- epi.ccc(subset(sv, treatment == "untreated_baked_30" & lab == "DPAA")$d13C, subset(sv, treatment == "untreated_baked_30" & lab == "UU")$d13C)

# tabulate CCC output
d = data.frame(matrix(ncol = 3, nrow = 10))
colnames(d) <- c("treat", "rho", "deltasd")

oxy <- c("BAU_O", "untreat_O", "temp50_O", "temp30_O", "baking_O")

for (i in oxy){
  d$rho = paste0("",i)[rho.cest
}
