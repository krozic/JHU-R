add2 <- function(x, y) {
        x + y
}

above10 <- function(x) {
        use <- x > 10 #creates a logical vector
        x[use]
}

above <- function(x, n = 10) { #default n is 10
        use <- x > n
        x[use]
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc) #empty vector
        for(i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
}