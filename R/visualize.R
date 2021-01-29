
source('R/select.R')
source('R/statistics.R')

# a color 

getColors <- function() {
  
  solidcolors =  c(rgb(229, 22,  54,  255, max = 255), 
                   rgb(136, 153, 255, 255, max = 255),
                   rgb(127, 0,   216, 255, max = 255),
                   rgb(255, 147, 41,  255, max = 255))
  
  transcolors =  c(rgb(229, 22,  54,  47,  max = 255), 
                   rgb(136, 153, 255, 47,  max = 255),
                   rgb(127, 0,   216, 47,  max = 255),
                   rgb(255, 147, 41,  47,  max = 255))
  
  colors <- list()
  
  colors[['red']]    <- list('s'=solidcolors[1], 't'=transcolors[1])
  colors[['blue']]   <- list('s'=solidcolors[2], 't'=transcolors[2])
  colors[['purple']] <- list('s'=solidcolors[3], 't'=transcolors[3])
  colors[['orange']] <- list('s'=solidcolors[3], 't'=transcolors[3])
  
  return(colors)
  
}

# this plots the main data:

plotFIPSpercepts <- function(target='inline') {
  
  data <- getFIPSconditions()
  colors <- getColors()
  N <- length(unique(data$participant))
  
  
  if (target == 'pdf') {
    pdf(file='doc/main_effect.pdf', width=5, height=5)
  }
  
  plot(-1000, -1000, 
       main='', xlab='frame movement [prop frame size]', ylab='percieved distance [prop frame size]', 
       xlim=c(0.3,0.8), ylim=c(0.3,0.8), 
       asp=1, 
       ax=F, bty='n')
  
  lines(c(0.35, 0.8), c(0.35, 0.8), col='gray', lty=2)
  
  constVel <- data[which(data$condition == 'constant_velocity'),]
  constPer <- data[which(data$condition == 'constant_period'),]
  
  # amp     = constant velocity
  # amp+vel = constant period

  constVel.avg    <- aggregate(norm_percept ~ framedistance_rel, data=constVel, FUN=mean)
  constPer.avg    <- aggregate(norm_percept ~ framedistance_rel, data=constPer, FUN=mean)
  
  constVel.CI     <- aggregate(norm_percept ~ framedistance_rel, data=constVel, FUN=getConfidenceInterval, method='b')
  constPer.CI     <- aggregate(norm_percept ~ framedistance_rel, data=constPer, FUN=getConfidenceInterval, method='b')
  
  Yvel <- c(constVel.CI[,2][,1], rev(constVel.CI[,2][,2]))
  Yper <- c(constPer.CI[,2][,1], rev(constPer.CI[,2][,2]))
  
  x <- sort(unique(constVel.avg$framedistance_rel))
  
  X <- c(x, rev(x))
  polygon(X, Yvel, border=NA, col=colors$red$t)
  polygon(X, Yper, border=NA, col=colors$blue$t)
  
  lines(x=constVel.avg$framedistance_rel, y=constVel.avg$norm_percept, col=colors$red$s)
  lines(x=constPer.avg$framedistance_rel, y=constPer.avg$norm_percept, col=colors$blue$s)
  
  legend(.3,.8, legend=c('constant velocity', 'constant period'), col=c(colors$red$s, colors$blue$s), lw=1, lty=1, bty='n')
  text(.65,.35,sprintf('N=%d',N))
  
  axis(side=1, at=c(.4,.5,.6,.7,.8))
  axis(side=2, at=c(.4,.5,.6,.7,.8))
  
  if (target %in% c('pdf')) {
    dev.off()
  }
  
}



plotDVAeffects <- function(target='inline') {
  
  # get color palette:
  colors <- getColors()
  
  # get data:
  dva<- getDVAdata()
  one_dva <- .245 / (dva$one_dva)
  slope <- dva$slope
  
  # set up pdf for pdf output
  if (target == 'pdf') {
    pdf(file='doc/screen_size_effect.pdf', width=5, height=5)
  }
  
  # start figure:
  plot(-1000, -1000, 
       main='', xlab='frame size [dva]', ylab='percieved distance slope', 
       xlim=c(0,20), ylim=c(-.2,3), 
       ax=F, bty='n')
  
  polygon(x=c(7.5,9,9,7.5),y=c(0,0,3,3),col='#EEEEEE',border=NA)
  
  lines(c(1,19),c(1,1),col='#999999',lty=2)
  
  points(one_dva, slope, col=colors$red$s)
  
  D2S <- lm(slope ~ one_dva)
  #print(summary(D2S))
  
  coef <- D2S$coefficients
  at <- c(1.8, 18.6)
  lines(at, coef[1]+(at*coef[2]), col=colors$red$s)
  
  ci <- predict( D2S,
                 newdata=data.frame(one_dva=seq(1.8,18.6,.1)),
                 interval = "confidence")
  
  #X <- c(seq(.013,.136,.001),rev(seq(.013,.136,.001)))
  X <- c(seq(1.8,18.6,.1),rev(seq(1.8,18.6,.1)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=colors$red$t,border=NA)
  
  text(15,3,sprintf('N=%d',dim(dva)[1]))
  
  axis(side=1, at=c(0,5,10,15,20))
  axis(side=2, at=c(0,1,2,3))
  
  if (target %in% c('pdf')) {
    dev.off()
  }
  
}
