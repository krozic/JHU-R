best <- function(state, outcome) {
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
        
        bestperstate <- tapply(as.numeric(outcome_data[, col_num]), outcome_data$State, min, na.rm = TRUE)
        #Chooses the lowest value per state and makes a new labelled vector with the lowest value per state
        
        state_data <- as.data.frame(split(outcome_data, outcome_data$State)[state])
        #Splits the entire dataset into a dataset per state, then stores the data for the state in question
        
        hosp_ind <- which(as.numeric(state_data[, col_num]) == bestperstate[state]) 
        #Gets the indice of the lowest value for state specific data set
              
        hospital <- state_data[hosp_ind, 2]
        #Gets the hospital name for the indice with the lowest value
        
        hospital
}