

# this returns a data frame describing all participants
# with info from 22 trials:

getParticipantsFromData <- function() {
  
  if (!file.exists('data/Pavlovia/')) {
    getRawData()
  }
  
  # get all .csv files from data folder:
  # csv_files <- Sys.glob(paths=c('data/*.csv'))
  csv_files <- list.files('data/Pavlovia/', pattern='*.csv')
  
  # will collect participants and timestamps in these lists:
  participant <- c()
  timestamp <- c()
  OS <- as.character(c())
  frameRate <- c()
  ttotal <- c()
  
  for (csv_file in csv_files) {
    
    filename <- sprintf('data/Pavlovia/%s', csv_file)
    
    # check how many trials in file? remove if not 22
    csv_lines <- readLines(filename)
    if (length(csv_lines) < 23) {
      next
    }
    
    # read the data to take a better look:
    ppFIPS <- read.csv(sprintf('data/Pavlovia/%s', csv_file), stringsAsFactors = F)
    
    # analyse string to get participant ID and timestamp
    FDpos <- gregexpr(pattern='_FrameDots_', csv_file)[[1]][1]
    pp <- substr(csv_file, 1, FDpos-1)
    ts <- substr(csv_file, FDpos+11, nchar(csv_file)-4)
    
    participant <- c(participant, pp)
    timestamp <- c(timestamp, ts)
    
    ppOS <- as.character(ppFIPS$OS[1])
    OS <- c(OS, ppFIPS$OS[1])
    
    frameRate <- c(frameRate, ppFIPS$frameRate[1])
    ttotal <- c(ttotal, ppFIPS$cumulativetime[22])
    
  }
  
  # return the data frame:
  return(data.frame(participant, timestamp, OS, frameRate, ttotal, stringsAsFactors=F))
  
}


getParticipants <- function() {
  
  # get participant descriptions from both sources:
  data_pp <- getParticipantsFromData()
  data_qq <- read.csv('data/Qualtrics_cleaned.csv', stringsAsFactors=F)

  # get those that match:
  pp <- unique(intersect(data_pp$participant, data_qq$participant))
  data_pp <- data_pp[which(data_pp$participant %in% pp),]
  data_qq <- data_qq[which(data_qq$participant %in% pp),]
  
  # add the pavlovia info to the qualtrics data frame, first make empty columns:
  data_qq$timestamp <- NA
  data_qq$OS <- NA
  data_qq$frameRate <- NA
  data_qq$completiontime <- NA 
  
  # now loop through the qualtrics info and get the corresponding pavlovia info:
  for (rowno in c(1:dim(data_qq)[1])) {
    
    ppid <- data_qq$participant[rowno]
    prows <- which(data_pp == ppid)
    if (length(prows) == 1) {
      
      data_qq$timestamp[rowno] <- as.character(data_pp$timestamp)[prows]
      data_qq$OS[rowno] <- data_pp$OS[prows]
      data_qq$frameRate[rowno] <- data_pp$frameRate[prows]
      data_qq$completiontime[rowno] <- data_pp$ttotal[prows]
      
    } else {
      # if there are multiple instances in the task data, we take the last one
      # assuming that practice makes perfect
      # (at this point, there should not be 0 matches)
      idx <- which(data_pp$participant == ppid)
      idx <- idx[order(data_pp$timestamp[idx])[length(idx)]]
      data_qq$timestamp[rowno] <- as.character(data_pp$timestamp[idx])
      data_qq$OS[rowno] <- data_pp$OS[idx]
      data_qq$frameRate[rowno] <- data_pp$frameRate[idx]
      data_qq$completiontime[rowno] <- data_pp$ttotal[idx]
      
    }
    
  }
  
  write.csv(data_qq, 'data/participants.csv', row.names=F, quote=F)
  
  return(data_qq)
  
}


getRawData <- function(check=TRUE) {
  
  folderfilename <- 'data.zip'
  
  if (!check | !file.exists(folderfilename)) {
    
    url = as.character('https://osf.io/utyeh/download')
    
    cat(sprintf("Downloading: 'data.zip' from '%s'\n", url))
    
    download.file(url = url, 
                  destfile = folderfilename, 
                  method = 'auto', 
                  quiet = FALSE, 
                  mode = "wb")
    
  } else {
    
    cat('"data.zip" already present.\n')
    
  }
  
  unzip(zipfile = folderfilename, exdir = '.')
  
}