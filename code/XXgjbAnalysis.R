sv <- read.csv("data/singlevalues.csv")

# Expectations ----
## Unbiased results 
### Test: are mean difference between labs different from zero?

## Lab metrics accurately reflect uncertainty seen in between-lab comparisons
### Test: is the variance of differences between labs different from that
### expected from propigation of reported external uncertainties in the 2 labs?
euc_C = sqrt(0.13 ^ 2 + 0.18 ^ 2)
euc_O = sqrt(0.31 ^ 2 + 0.23 ^ 2)

## Greater standardization improves comparability between labs
### Test: is mean difference (bias) reduced w/ increased standardization?
### Test: is variance of differences reduced w/ increased standardization?

## Protocol changes systematically affect reported values across labs
### Test: does imposing a given change produce the same effect (sign, size)
### in both labs?


# BAU - business as usual base case ----

uu = sv[sv$treatment == "untreated_unbaked_50" & sv$lab == "UU",]
dp = sv[sv$treatment == "treated_unbaked_30" & sv$lab == "DPAA",]

DC = dp$d13C - uu$d13C[match(dp$sample, uu$sample)]
DO = dp$d18O - uu$d18O[match(dp$sample, uu$sample)]

### Significant d13C bias
t.test(DC)
### Between-lab variability is within expectation
euc_C
sd(DC)

plot(uu$d13C[match(dp$sample, uu$sample)], dp$d13C)
abline(0, 1)

### Significant d18O bias
t.test(DO)
### Between-lab variability is slightly higher than
euc_O
sd(DO)

plot(uu$d13C[match(dp$sample, uu$sample)], dp$d13C)
abline(0, 1)

## Summary results:
## BAU gives bias between labs for both elements

## Lab-reported uncertainty accurately reflects (or overestimates) variance  
## in between-lab differences for C

## Lab-reported uncertainty slightly underestimates the variance of between-lab
## differences for O

# Reaction T ----
## 50 ----
## 50 degrees

uu.50 = sv[sv$treatment == "untreated_unbaked_50" & sv$lab == "UU",]
dp.50 = sv[sv$treatment == "treated_unbaked_50" & sv$lab == "DPAA",]

DC.50 = dp.50$d13C - uu.50$d13C[match(dp.50$sample, uu.50$sample)]
DO.50 = dp.50$d18O - uu.50$d18O[match(dp.50$sample, uu.50$sample)]

### Significant d13C bias, similar magnitude to BAU
t.test(DC)
t.test(DC.50)
### Between-lab variability similar to BAU and within expectation
euc_C
sd(DC)
sd(DC.50)
### Increasing reaction T at DPAA gives lower d13C values
DC.dp.50_BAU = (dp.50$d13C - dp$d13C[match(dp$sample, dp.50$sample)])
t.test(DC.dp.50_BAU)

### Significant d18O bias, larger than BAU
t.test(DO)
t.test(DO.50)
### Betwen-lab variability larger than BAU and expectation
euc_O
sd(DO)
sd(DO.50)
### Increasing reaction T at DPAA gives higher d18O values
DO.dp_50_BAU = (dp.50$d18O - dp$d18O[match(dp$sample, dp.50$sample)])
t.test(DO.dp_50_BAU)

## 30 ----
## 30 degrees

uu.30 = sv[sv$treatment == "untreated_unbaked_30" & sv$lab == "UU",]
dp.30 = sv[sv$treatment == "treated_unbaked_30" & sv$lab == "DPAA",]

DC.30 = dp.30$d13C - uu.30$d13C[match(dp.30$sample, uu.30$sample)]
DO.30 = dp.30$d18O - uu.30$d18O[match(dp.30$sample, uu.30$sample)]

### Significant d13C bias, slightly larger magnitude to BAU
t.test(DC)
t.test(DC.30)
### Between-lab variability larger than BAU, similar to expectation 
euc_C
sd(DC)
sd(DC.30)
### Decreasing reaction T at UU gives no change in d13C values
DC.uu.30_BAU = (uu.30$d13C - uu$d13C[match(uu$sample, uu.30$sample)])
t.test(DC.uu.30_BAU)

### Significant d18O bias, larger than BAU
t.test(DO)
t.test(DO.30)
### Betwen-lab variability slightly larger than BAU and expectation
euc_O
sd(DO)
sd(DO.30)
### Decreasing reaction T at UU gives no change in d18O values
DO.uu_30_BAU = (uu.30$d18O - uu$d18O[match(uu$sample, uu.30$sample)])
t.test(DO.uu_30_BAU)

## Summary results:
## Standardizing rxn temperature along does not eliminate bias and in some
## cases increases it.

## When labs deviate from their BAU rxn temp the between-lab differences
## often become noisier (higher variance) and in some cases exceed those 
## expected based on the external uncertainties from the labs (i.e., the
## reported metrics don't accurately capture the true uncertainties).

## These effects are larger for O than C.

## Effects of changing rxn temp are inconsistent between labs, with small
## but consistent shifts seen for both elements at DPAA and no change at UU.

# Treatment ----
# What if we add standardization of treatment? 

## t.50 ----
## With treatment, 50 degree rxn temp

uu.50.t = sv[sv$treatment == "treated_unbaked_50" & sv$lab == "UU",]
dp.50.t = sv[sv$treatment == "treated_unbaked_50" & sv$lab == "DPAA",]

DC.50.t = dp.50.t$d13C - uu.50.t$d13C[match(dp.50.t$sample, uu.50.t$sample)]
DO.50.t = dp.50.t$d18O - uu.50.t$d18O[match(dp.50.t$sample, uu.50.t$sample)]

### Significant d13C bias, larger magnitude than w/o standardizing treatment
t.test(DC.50)
t.test(DC.50.t)
### Between-lab variability larger than w/o standardizing treatment,
### similar to expectation
euc_C
sd(DC.50)
sd(DC.50.t)
### Adding treatment at UU gives lower d13C values
DC.uu_50.t_50 = (uu.50.t$d13C - uu.50$d13C[match(uu.50$sample, uu.50.t$sample)])
t.test(DC.uu_50.t_50)

### Significant d18O bias, slightly lower than w/o standardizing treatment
t.test(DO.50)
t.test(DO.50.t)
### Betwen-lab variability larger than w/o standardizing treatment,
### much larger than expectation
euc_O
sd(DO.50)
sd(DO.50.t)
### Adding treatment at UU slightly increases d18O values
DO.uu_50.t_50 = (uu.50.t$d18O - uu.50$d18O[match(uu.50$sample, uu.50.t$sample)])
t.test(DO.uu_50.t_50)

## ut.50 ----
## Without treatment, 50 degree rxn temp

uu.50.ut = sv[sv$treatment == "untreated_unbaked_50" & sv$lab == "UU",]
dp.50.ut = sv[sv$treatment == "untreated_unbaked_50" & sv$lab == "DPAA",]

DC.50.ut = dp.50.ut$d13C - uu.50.ut$d13C[match(dp.50.ut$sample, uu.50.ut$sample)]
DO.50.ut = dp.50.ut$d18O - uu.50.ut$d18O[match(dp.50.ut$sample, uu.50.ut$sample)]

### Significant d13C bias, similar magnitude to w/o standardizing treatment
t.test(DC.50)
t.test(DC.50.ut)
### Between-lab variability slightly larger than w/o standardizing treatment,
### similar to expectation
euc_C
sd(DC.50)
sd(DC.50.ut)
### Removing treatment at DPAA gives higher d13C values
DC.dp_50.ut_50 = (dp.50.ut$d13C - dp.50$d13C[match(dp.50$sample, dp.50.ut$sample)])
t.test(DC.dp_50.ut_50)

### No d18O bias
t.test(DO.50.ut)
### Betwen-lab variability same as w/o standardizing treatment,
### larger than expectation
euc_O
sd(DO.50)
sd(DO.50.ut)
### Removing treatment at DPAA dramatically decreases d18O values
DO.dp_50.ut_50 = (dp.50.ut$d18O - dp.50$d18O[match(dp.50$sample, dp.50.ut$sample)])
t.test(DO.dp_50.ut_50)

## ut.30 ----
## Without treatment, 30 degree rxn temp

uu.30.ut = sv[sv$treatment == "untreated_unbaked_30" & sv$lab == "UU",]
dp.30.ut = sv[sv$treatment == "untreated_unbaked_30" & sv$lab == "DPAA",]

DC.30.ut = dp.30.ut$d13C - uu.30.ut$d13C[match(dp.30.ut$sample, uu.30.ut$sample)]
DO.30.ut = dp.30.ut$d18O - uu.30.ut$d18O[match(dp.30.ut$sample, uu.30.ut$sample)]

### No d13C bias
t.test(DC.30.ut)
### Between-lab variability slightly larger than w/o standardizing treatment,
### similar to expectation
euc_C
sd(DC.30)
sd(DC.30.ut)
### Removing treatment at DPAA gives lower d13C values
DC.dp_30.ut_30 = (dp.30.ut$d13C - dp.30$d13C[match(dp.30$sample, dp.30.ut$sample)])
t.test(DC.dp_30.ut_30)

### Significant d18O bias but much smaller than w/o standardizing treatment
t.test(DO.30)
t.test(DO.30.ut)
### Betwen-lab variability slightly higher than  w/o standardizing treatment,
### larger than expectation
euc_O
sd(DO.30)
sd(DO.30.ut)
### Removing treatment at DPAA decreases d18O values
DO.dp_30.ut_30 = (dp.30.ut$d18O - dp.30$d18O[match(dp.30$sample, dp.30.ut$sample)])
t.test(DO.dp_30.ut_30)

## Summary results:
## Standardizing treatment in addition to rxn temperature has inconsistent
## effects.

## Using treatment in both labs does not reduce bias and increases the 
## variance of between-lab differences, which exceed those expected from 
## reported external uncertainties.

## Eliminating treatment in both labs, regardless of rxn temp, appears to 
## reduce d18O bias between labs but has limited impact on variance
## of between-lab differences, which exceed expectations. Effect on d13C
## less clear, but standardizing w/ no treatment and 30 degree rxn temp
## eliminates bias and gives variance consistent with expectations.

## Effects of treatment on d13C values are inconsistent, with small increases
## and decreases seen.

## Effects of treatment on d18O values are consistent across all trials,
## with increases in d18O seen when treatment is used. This effect is larger
## at DPAA than UU (but the magnitude is also pretty variable across the 2
## comparisons at DPAA). This effect is consistent w/ previous reports of 
## increasing d18O values after acetic acid treatment.

# Baking ----
# What if we add baking to our 'best' standardized treatment? 

## Without treatment, 30 degree rxn temp, baking

uu.30.ut.b = sv[sv$treatment == "untreated_baked_30" & sv$lab == "UU",]
dp.30.ut.b = sv[sv$treatment == "untreated_baked_30" & sv$lab == "DPAA",]

DC.30.ut.b = dp.30.ut.b$d13C - 
  uu.30.ut.b$d13C[match(dp.30.ut.b$sample, uu.30.ut.b$sample)]
DO.30.ut.b = dp.30.ut.b$d18O - 
  uu.30.ut.b$d18O[match(dp.30.ut.b$sample, uu.30.ut.b$sample)]

### Significant but small d13C bias, (marginally) smaller than BAU
t.test(DC)
t.test(DC.30.ut)
t.test(DC.30.ut.b)
### Between-lab variability lower than w/o baking, similar to BAU,
### lower than expectation
euc_C
sd(DC)
sd(DC.30.ut)
sd(DC.30.ut.b)
### Baking gives a tiny bit higher d13C values at DPAA, no difference at UU
DC.dp_30.ut.b_30.ut = 
  (dp.30.ut.b$d13C - dp.30.ut$d13C[match(dp.30.ut$sample, dp.30.ut.b$sample)])
t.test(DC.dp_30.ut.b_30.ut)
DC.uu_30.ut.b_30.ut = 
  (uu.30.ut.b$d13C - uu.30.ut$d13C[match(uu.30.ut$sample, uu.30.ut.b$sample)])
t.test(DC.uu_30.ut.b_30.ut)

### Significant d18O bias but much smaller than w/o baking, much much smaller
### than BAU
t.test(DO)
t.test(DO.30.ut)
t.test(DO.30.ut.b)
### Betwen-lab variability lower than w/o baking, similar to BAU,
### slightly larger than expectation
euc_O
sd(DO)
sd(DO.30.ut)
sd(DO.30.ut.b)
### Baking gives lower d18O values at DPAA, no difference at UU
DO.dp_30.ut.b_30.ut = 
  (dp.30.ut.b$d18O - dp.30.ut$d18O[match(dp.30.ut$sample, dp.30.ut.b$sample)])
t.test(DO.dp_30.ut.b_30.ut)
DO.uu_30.ut.b_30.ut = 
  (uu.30.ut.b$d18O - uu.30.ut$d18O[match(uu.30.ut$sample, uu.30.ut.b$sample)])
t.test(DO.uu_30.ut.b_30.ut)

## Summary results:
## Adding baking reduces but does not eliminate bias for both elements and 
## reduces the variance of between-lab differences.

## Variance in d18O differences remains somewhat higher than expected
## based on lab-reported external uncertainty.

## Effect of baking negligable for d13C and only apparant for d18O at DPAA,
## consistent with expetation for removal of adhered water in high-humidity
## environment.

## Standardizing rxn temp, eliminating treatment, and baking reduces bias 
## relative to BAU and other examples tested here. Also performs better than
## other standardized treatments, and simlar to BAU, wrt the 'noisiness' of 
## the data (variance of between-lab differences). This is our recommendation.

## That said, we still see biases in d18O between labs and the comparisons
## are noisier than we would expect based on reported uncertainties from the
## labs. This suggests that there are additional complexities/factors 
## affecting d18O analysis of enamel which remain to be identified and could
## further improve the comparability of these analyses.
