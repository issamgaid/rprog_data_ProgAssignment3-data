rankhospital <- function(state, outcome, num = "best") 
  {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
  ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"   
  outcomes[,11] <- as.numeric(outcomes[,11])
  outcomes[,17] <- as.numeric(outcomes[,17])
  outcomes[,23] <- as.numeric(outcomes[,23])
  
  ## Check that state and outcome are valid
  if (outcome!="heart attack" && outcome!= "heart failure" && outcome !="pneumonia")
    stop("invalid outcome")
  
  checkCity<- state %in% na.omit(outcomes[, 7])
  if (!checkCity)
    stop ("invalid state")
  
  ##Hospitals that do not have data on a particular
  ##outcome should be excluded from the set of hospitals 
  ##when deciding the rankings.
  outcomes<-na.omit(outcomes)
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  hospitals <- split(outcomes, outcomes$State)[[state]]
  
  if(outcome == "heart attack") {
    hospitals <- hospitals[order(
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      hospitals$Hospital.Name, na.last = NA), ]
    if(num == "best") {
      return(hospitals[1, 2])
    } else if(num == "worst") {
      return(hospitals[nrow(hospitals), 2])
    } else if(is.numeric(num)) {
      if(num > nrow(hospitals)) {
        return(NA)
      } else {
        return(hospitals[num, 2])
      }
    } else {
      stop("invalid num")
    }
  } else if(outcome == "heart failure") {
    hospitals <- hospitals[order(
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      hospitals$Hospital.Name, na.last = NA), ]
    if(num == "best") {
      return(hospitals[1, 2])
    } else if(num == "worst") {
      return(hospitals[nrow(hospitals), 2])
    } else if(is.numeric(num)) {
      if(num > nrow(hospitals)) {
        return(NA)
      } else {
        return(hospitals[num, 2])
      }
    } else {
      stop("invalid num")
    }
  } else if(outcome == "pneumonia") {
    hospitals <- hospitals[order(
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
      hospitals$Hospital.Name, na.last = NA), ]
    if(num == "best") {
      return(hospitals[1, 2])
    } else if(num == "worst") {
      return(hospitals[nrow(hospitals), 2])
    } else if(is.numeric(num)) {
      if(num > nrow(hospitals)) {
        return(NA)
      } else {
        return(hospitals[num, 2])
      }
    } else {
      stop("invalid num")
    }
  }
}