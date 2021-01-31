library('afex')
library('BayesFactor')

exp1stats <- function() {
  
  exp1data <- read.csv('data/experiment_1.csv')
  
  exp1_aov <- afex::aov_ez(id='participant', dv='percept', within='path_length', data=exp1data, type=3)
  cat('F-test (one-way ANOVA) to see if the distance the frame travels can\nexplain the perceived distance between the flashed edges of the frame:\n\n')
  print(exp1_aov)
  cat('\n\nIt can not, so we want a Bayes Factor of the effect of path length on perceived distance between edges:\n\n')
  print(BayesFactor::oneWayAOV.Fstat(F=exp1_aov$anova_table$F, N=8, J=5, simple=T))
  cat('\nIt remains undecided.\n')
  
}


