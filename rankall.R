rankall <- function(outcome, num = "best"){
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  valid_state <- unique(outcome_data$State)
  valid_state <- valid_state[order(valid_state)]
  
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% valid_outcome)){
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  
  if (outcome == "heart attack"){
    condition <- 11
  } else if(outcome == "heart failure"){
    condition <- 17
  } else if(outcome == "pneumonia"){
    condition <- 23
  }
  
  dataframe <- na.omit(outcome_data[,c(7, 2, condition)])
  names(dataframe) <- c("State", "hospital", "outcome")
  sorted <- dataframe[order(dataframe$State, dataframe$outcome, dataframe$hospital),]
  hospital_results <- vector(mode = "character")
  sought_hospital <- data.frame()
  hospital_state <- vector(mode = "character")
  results <- data.frame()
  
  for (i in seq_along(valid_state)){
    state_subset <- subset(sorted, State == valid_state[i], drop = FALSE)
    
    if(!is.integer(num) & num != "best")
    num <- nrow(state_subset)
    if(num == "best")
      num <- 1
    else
      num <- as.numeric(num)
    
    sought_hospital <- state_subset[num, 2]
    
    hospital_results <- c(hospital_results, sought_hospital)
    hospital_state <- c(hospital_state, valid_state[i])
  }
  
  results <- cbind(hospital_results, hospital_state)
  results <- as.data.frame(results)
  names(results) <- c("hospital", "state")
  
  return(results)
  
}