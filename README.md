# Improving Inter-laboratory Comparability of Tooth Enamel Carbonate Stable Isotope Analysis (δ18O, δ13C)

This is the data and code used in a systematic comparison of isotope delta (𝛿) values for 10 “modern” faunal teeth 
(obtained from field recoveries) measured in two different laboratories. Our tests included comparisons of enamel powder subsamples 
that were chemically pretreated using commonly adopted protocols and subsamples that received no pretreatment. We also evaluated 𝛿 values 
generated with and without (1) standardizing the reaction temperature used for sample acidification and (2) baking the samples and vials to remove moisture before analysis.

## Data
The datasets consist of the δ13C and δ18O values of ten samples that were treated in a variety of different protocols at two labs (DPAA and SIRFER). The file `baseInterlabData` contains the stable isotope values (columns `d13C` and `d18O`) by sample and treatment. 
Reaction time has its own column, `rxn_time`. Other datasets in this repo include those created by the R code `01setup`. The dataset `UU_0424` was added midway through the experiments, and so R code `00newUUdata` lightly cleans and incorporates that data. 

__Data Contact__ 
For questions about the code, data, or paper, contact Chris Stantis (stantis@siu.edu, [stantis](https://github.com/stantis) on GitHub). 

## Authors
Chris Stantis started this repo and associated code. 
Gabe Bowen ([bumbanian](https://github.com/bumbanian))contributed significantly to this repo, especially the parts that worked and don't have left-arrow operators. See associated paper for other contributions. 
