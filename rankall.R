rankall <- function(outcome, num = "best"){
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  valid_state <- unique(outcome_data$State)
  
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
  results <- data.frame()
  sought_hospital <- data.frame()
  
  for (i in seq_along(valid_state)){
    state_subset <- subset(sorted, State == valid_state[i], drop = FALSE)
    
    if(num == "best"){
    num <- 1
    }else if(num == "worst"){
      num <- nrow(state_subset)
    }else{
      num <- as.numeric(num)
    }
    sought_hospital <- state_subset[num, 2:1]
    
    results <- rbind(results, sought_hospital)
  }
  
  return(results)
  
}