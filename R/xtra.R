
library('afex')
library('BayesFactor')

lab_NwayAOV <- function(file='data/experiment2_frame_movement.csv', id='participant', dv='percept', within=c('path_length', 'condition'), type=3, printAOV=FALSE) {
  
  df <- read.csv(file)
  
  df[[id]]     <- as.factor(df[[id]])
  for (n in c(1:length(within))) {
    df[[within[n]]] <- as.factor(df[[within[n]]])
  }
  
  aov_model <- afex::aov_ez(id=id, dv=dv, within=within, data=df, type=type)
  if (printAOV) {
    print(aov_model)
  }
  
  cat('\n\nA Bayes Factor version for the ANOVA:\n\n')
  
  form <- paste0(dv, " ~ ")
  
  for (n in c(1:length(within))) {
    if (n == 1) {
      form <- paste0(form, within[n])
    } else {
      form <- paste0(form, " + ", within[n])
    }
  }
  form <- paste0(form, " + ", id)
  
  anovaBF(as.formula(form), data=df, whichRandom=id, whichModel='all')
  
}

