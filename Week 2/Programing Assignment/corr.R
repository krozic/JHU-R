corr <- function(directory, threshold = 0) {
  
        threshdata <- complete(directory)
        threshindex <- which(threshdata[2] >= threshold)
        
        cordata <- numeric()
        
        fileslist <- list.files(path = directory, full.names = TRUE)
        
        for(i in threshindex) {
                datax <- read.csv(fileslist[i])
                dataindex <- which(!is.na(datax[2] == datax[3]))
                nscor <- cor(datax[dataindex, 2], datax[dataindex, 3])
                if(!is.na(nscor)) {
                        cordata <- c(cordata, nscor)
                }
        }
        cordata
}