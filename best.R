
best <- function(state, outcome) 
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
    
    ## Return hospital name in that state with lowest 30-day death
      hospitals <- split(outcomes, outcomes$State)[[state]]
      
      if(outcome == "heart attack") {
        hospitals <- hospitals[
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ==
            min(hospitals[, 11], na.rm = TRUE), 2]
        hospitals <- sort(hospitals)
        return(hospitals[1])
      } else if(outcome == "heart failure") {
        hospitals <- hospitals[
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ==
            min(hospitals[, 17], na.rm = TRUE), 2]
        hospitals <- sort(hospitals)
        return(hospitals[1])
      } else if(outcome == "pneumonia") {
        hospitals <- hospitals[
          hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ==
            min(hospitals[, 23], na.rm = TRUE), 2]
        hospitals <- sort(hospitals)
        return(hospitals[1])
      }
  ## rate
}