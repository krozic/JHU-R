pollutantmean <- function(directory, pollutant, id = 1:332) {
        fileslist <- list.files(path = directory, full.names = TRUE)
        total <- numeric()
        for(i in id) {
                      datax <- read.csv(fileslist[i])
                      total <- c(total, datax[[pollutant]]) 
        }
        mean(total, na.rm = TRUE)
}