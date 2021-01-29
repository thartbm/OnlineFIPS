
library('lme4')
library('lmerTest')
library('BayesFactor')

source('R/select.R')

# this function runs a linear mixed effects model on the online FIPS data:

FIPS_LME <- function() {
  
  FIPS <- getFIPSconditions()
  
  names(FIPS) <- c("framedist","participant","percept","cond")
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  FIPS_LME  <- lmerTest::lmer(percept ~ framedist * cond - (1|participant),
                              na.action = na.exclude,
                              data = FIPS,
                              REML = TRUE,
                              control = lme4::lmerControl(optimizer ="Nelder_Mead")
  )
  
  cat('\nLINEAR MIXED EFFECTS MODEL:\n\n')
  
  print(summary(FIPS_LME))
  
  options(default.contrasts)
  
}


getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    
    return(quantile(BS, probs = c(lo,hi)))
    
  }
  
}

DVAregression <- function() {
  
  # get data:
  dva<- getDVAdata()
  one_dva <- .245 / (dva$one_dva)
  slope <- dva$slope
  
  BayesFactor::regressionBF(slope ~ one_dva, data=data.frame(one_dva, slope))
  
}