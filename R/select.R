
source('R/participants.R')

# this selects participants based on their performance and returns one kind of data:
# 'FIPS': the main 12 trials
# 'wallach': the data from the 6 Wallach style trials
# 'catch': the 4 catch trial
# 'dva': the calibration data to calculate degrees visual angle
# set the data argument to a vector with one or more of these strings

# to get data from 'all' participants, or only those with 'selected' performance
# set the sample argument to 'selected' or to 'all'
# (or 'strict' to remove participants with 1 or 2 zero responses)

getFIPSdata <- function(data=c('FIPS'), set='selected') {
  
  if (file.exists('data/participants.csv')) {
    participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  } else {
    participants <- getParticipants()
  }
  wallach   <- NA
  catch     <- NA
  DVAinfo   <- NA
  allFIPS   <- NA
  
  for (ppno in c(1:dim(participants)[1])) {
    
    # read participant's data:
    pp <- participants$participant[ppno]
    ts <- participants$timestamp[ppno]
    filename <- sprintf('data/Pavlovia/%s_FrameDots_%s.csv', pp, ts)
    ppFIPS <- read.csv(filename, stringsAsFactors = F)
    
    ppFIPS$trialtype[which(ppFIPS$trialtype == 'base')]   <- 'reference'
    ppFIPS$trialtype[which(ppFIPS$trialtype == 'amp+vel')] <- 'constant_period'
    ppFIPS$trialtype[which(ppFIPS$trialtype == 'amp')] <- 'constant_speed'
    
    # get calibration info:
    dva <- ppFIPS[1,c('cc_width', 'cc_height', 'eye_distance_cm', 'one_dva_width', 'one_dva_height')]
    
    # correct for xfactor:
    ppFIPS$norm_percept <- ppFIPS$distancepercept_rel * ppFIPS$xfactor
    
    # calculate speed from movement amplitude and duration of a full cycle:
    ppFIPS$velocity <- ppFIPS$framedistance_rel / (ppFIPS$period_s / 2)
    
    frame_size <- 0.245
    # outer frame: 0.245 nsu
    # inner frame: 0.225 nsu
    # maybe the middle of these is better? 0.235?
    # dot width & height: 0.025 nsu
    
    # normalize percept and frame movement to a proportion of frame size:
    ppFIPS$norm_percept      <- ppFIPS$norm_percept / frame_size
    ppFIPS$framedistance_rel <- ppFIPS$framedistance_rel / frame_size
    
    pp.12FIPS  <- ppFIPS[which(ppFIPS$trialtype %in% c('reference', 'constant_speed', 'constant_period')), c('trialtype', 'norm_percept', 'framedistance_rel', 'period_s', 'velocity')]
    pp.wallach <- aggregate(norm_percept ~ framedistance_rel + period_s, data=ppFIPS[which(ppFIPS$trialtype == 'Wallach'),], FUN=mean)
    pp.catch   <- ppFIPS[which(ppFIPS$trialtype == 'catch'),c('horizontal_offset', 'distancepercept_rel', 'participant')]
    
    # # # # # # # 
    # HERE WE START SELECTING BASED ON PERFORMANCE:
    
    below <- length(which(pp.12FIPS$norm_percept <=  0            ))
    above <- length(which(pp.12FIPS$norm_percept  > (0.54 / .245) ))
    
    idx <- which(pp.12FIPS$norm_percept > 0 & !is.nan(pp.12FIPS$norm_percept))
    percepts <- as.numeric(pp.12FIPS$norm_percept[idx])
    
    select <- TRUE
    zeroes <- FALSE
    
    if (above & below) {
      # remove participant if both above and below limits
      select <- FALSE
    } else if (above) {
      # also remove participant if any trials above limits
      select <- FALSE
    } else if (below) {
      # if participant below limits: remove....
      select <- FALSE
      if (length(percepts) >= 10) {
        # ... EXCEPT when only 2 trials are below limits
        select <- TRUE
        zeroes <- TRUE
      }
    }
    
    # SELECTION DONE
    # # # # #
    
    # add participant info to data frames to return:
    
    pp.12FIPS$participant <- pp
    pp.12FIPS$zeroes <- zeroes
    pp.12FIPS$selected <- select
    
    dva$participant <- pp
    dva$zeroes <- zeroes
    dva$selected <- select
    
    pp.wallach$participant <- pp
    pp.wallach$zeroes <-  zeroes
    pp.wallach$selected <- select
    
    pp.catch$participant <- pp
    pp.catch$zeroes <- zeroes
    pp.catch$selected <- select
    
    
    if (is.data.frame(allFIPS)) {
      allFIPS <- rbind(allFIPS, pp.12FIPS)
    } else {
      allFIPS <- pp.12FIPS
    }
    

    if (is.data.frame(catch)) {
      # amp     <- rbind(amp, pp.amp)
      # ampvel  <- rbind(ampvel, pp.ampvel)
      DVAinfo <- rbind(DVAinfo, dva)
      catch   <- rbind(catch, pp.catch)
    } else {
      # amp     <- pp.amp
      # ampvel  <- pp.ampvel
      DVAinfo <- dva
      catch   <- pp.catch
    }
    
    # does this make sense for Wallach trials?
    if (any(pp.wallach$norm_percept == 0 | pp.wallach$norm_percept > (0.54/frame_size))) {
      # do we need to do anything here?
    } else {
      
      if (is.data.frame(wallach)) {
        wallach <- rbind(wallach, pp.wallach)
      } else {
        wallach <- pp.wallach
      }
      
    }

  }
  
  if (set %in% c('selected','strict')) {
    allFIPS <- allFIPS[which(allFIPS$selected == TRUE),]
    wallach <- wallach[which(wallach$selected == TRUE),]
    catch   <-   catch[which(  catch$selected == TRUE),]
    dva     <-     dva[which(    dva$selected == TRUE),]
  }
  if (set == 'strict') {
    allFIPS <- allFIPS[which(allFIPS$zeroes == FALSE),]
    wallach <- wallach[which(wallach$zeroes == FALSE),]
    catch   <-   catch[which(  catch$zeroes == FALSE),]
    dva     <-     dva[which(    dva$zeroes == FALSE),]
  }
  
  
  # all data collected, decide whether or not to return it:
  
  output <- list()
  
  if ('FIPS' %in% data) {
    output[['FIPS']] <- allFIPS
  }
  if ('wallach' %in% data) {
    output[['wallach']] <- wallach
  }
  if ('catch' %in% data) {
    output[['catch']] <- catch
  }
  if ('dva' %in% data) {
    output[['dva']] <- DVAinfo
  }
  
  # all done:
  
  return(output)
  
}


getFIPSconditions <- function() {
  
  FIPSdata <- getFIPSdata()[['FIPS']]
  
  FIPSdata <- FIPSdata[c(which(FIPSdata$zeroes == FALSE),which(FIPSdata$zeroes == TRUE)),]
  
  speed   <- aggregate(norm_percept ~ framedistance_rel + participant, data=FIPSdata[which(FIPSdata$trialtype %in% c('reference', 'constant_speed')),], FUN=mean)
  period  <- aggregate(norm_percept ~ framedistance_rel + participant, data=FIPSdata[which(FIPSdata$trialtype %in% c('reference', 'constant_period')),], FUN=mean)
  
  speed$condition   <- 'constant_velocity'
  period$condition  <- 'constant_period'
  
  FIPS              <- rbind(speed, period)
  FIPS$condition    <- as.factor(FIPS$condition)
  
  return(FIPS)
  
}


getDVAdata <- function() {
  
  # get calibration data:
  dva <- getFIPSdata(data=c('dva'))[['dva']]
  
  # get participants with reliable calibration:
  participants <- getParticipants()
  calibrated <- participants$participant[ which( participants$card_used == "Yes" & 
                                                   participants$distance_method == "measured") ]
  
  # select reliable calibration data:
  dva <- dva[which(dva$participant %in% calibrated),]
  
  # average the dva for height and width:
  dva$one_dva <- (dva$one_dva_height + dva$one_dva_width) / 2
  
  # remove participants with negative values:
  dva <- dva[which(dva$one_dva > 0),]
  
  # get percept data
  FIPS <- getFIPSconditions()
  
  # we only want to have participants that actually exist in FIPS
  dva <- dva[which(dva$participant %in% FIPS$participant),]
  
  # select FIPS data from participants with reliable calibration:
  FIPS <- FIPS[which(FIPS$participant %in% dva$participant),]
  
  FIPS$norm_percept      <- FIPS$norm_percept * .245
  FIPS$framedistance_rel <- FIPS$framedistance_rel * .245
  
  # we'll get the slope of a linear fit of percept over frame movement amplitude
  # averaged over both conditions (constant speed and velocity)
  dva$slope <- NA
  
  for (ppno in c(1:length(dva$participant))) {
    
    pp <- dva$participant[ppno]
    one_dva <- dva$one_dva[ppno]
    
    idx <- which(FIPS$participant == pp)
    
    linmod <- lm(norm_percept ~ framedistance_rel, data=aggregate(norm_percept ~ framedistance_rel, data=FIPS[idx,], FUN=mean))
    dva$slope[ppno] <- linmod[['coefficients']][['framedistance_rel']]
    
  }
  
  return(dva)
  
}