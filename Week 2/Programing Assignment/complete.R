complete <- function(directory, id = 1:332) {
        fileslist <- list.files(path = directory, full.names = TRUE)
        numcomp <- data.frame(matrix(ncol = 2, nrow = 0))
        for (i in id) {
                datax <- read.csv(fileslist[i])
                comp <- sum(!is.na(datax[2] == datax[3]))
                numcomp <- rbind(numcomp, c(i, comp))
        }
        colnames(numcomp) <- c("id", "nobs")
        numcomp
}