rankhospital <- function(state, outcome, num) {
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        col_vals <- c(11, 17, 23)
        names(col_vals) <- outcomes
        
        col_num = as.numeric(col_vals[outcome])
        
        if (!(state %in% outcome_data$State)) {
                stop("invalid state")
        }
        if (!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }
        
        state_data <- as.data.frame(split(outcome_data, outcome_data$State)[state])
        ordered_data <- state_data[order(as.numeric(state_data[, col_num]), state_data[, 2], na.last = NA),]
        
        max_rank <- length(ordered_data[, col_num])
        
        
        if (num == "best") {
                hospital <- ordered_data[1, 2]
        } else if (num == "worst") {
                hospital <- ordered_data[max_rank, 2]
        } else if (num > max_rank) {
                hospital <- NA
        } else {
                hospital <- ordered_data[num, 2]
        }
        
        hospital
}