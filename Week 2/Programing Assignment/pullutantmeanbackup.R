pollutantmean <- function(directory, pollutant, id = 1:332) {
        means <- numeric(length(id)) #empty vector
        m <- id[1] - 1
        for(i in id) {
                if(i < 10) {
                      datax <- read.csv(paste(directory, "/", "00", i, ".csv", sep = ""))
                      y <- datax[[pollutant]] 
                      means[(i - m)] <- mean(y, na.rm = TRUE)
                } else if(i < 100) {
                      datax <- read.csv(paste(directory, "/", "0", i, ".csv", sep = ""))
                      y <- datax[[pollutant]] 
                      means[(i - m)] <- mean(y, na.rm = TRUE)
                } else {
                      datax <- read.csv(paste(directory, "/", i, ".csv", sep = ""))
                      y <- datax[[pollutant]] 
                      means[(i - m)] <- mean(y, na.rm = TRUE)
                }
        }
        mean(means)
}

#This method does not work, because it weights each file equally
#However, you can have 50 points from one file and 3 from the other
#This should not be weighted equally in an average calculation
#The correct way is to compile all the data WITH the NA's
#Then perform the mean on that final batch of data only