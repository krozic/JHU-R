best <- function(state, outcome) {
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 ##lowest 30 day mortality
 ##do this for each state
 ##give back in format 
        
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
        
        
        # col_num = numeric()
        # if (outcome == "heart attack") {
        #   col_num = 11
        # }
        # 
        # if (outcome == "heart failure") {
        #   col_num = 17
        # }
        #     
        # if (outcome == "pneumonia") {
        #   col_num = 23
        # }
        
        bestperstate <- tapply(as.numeric(outcome_data[, col_num]), outcome_data$State, min, na.rm = TRUE)
        #This chooses the lowest value per state and makes a new labelled vector with the lowest value per state
        
        #perstate <- split(outcome_data, outcome_data$State)
        #This splits the entire dataset into a dataset per state
        state_data <- as.data.frame(split(outcome_data, outcome_data$State)[state])
        
        hosp_ind <- which(as.numeric(state_data[, col_num]) == bestperstate[state]) 
        #hosp_ind <- which(as.numeric(perstate$state[, col_num]) == bestperstate[state]) 
        #This gets the indice of the lowest value for that state
              
        hospital <- state_data[hosp_ind, 2]
        #hospital <- perstate$state[hosp_ind, 2]
        #This gets the hospital name for the indice with the lowest value
        
        
        hospital
        #(state_data$state[, 11])
        #split(outcome_data, outcome_data$State)[state]
        #dim(as.data.frame(state_data))
      
        
        
        #which(min(as.numeric(outcome[, 11]), na.rm = TRUE) == as.numeric(outcome[, 11]))
        #This grabs the indice# of the lowest value in the outcome column
}