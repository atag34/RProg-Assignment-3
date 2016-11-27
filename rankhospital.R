library(dplyr)
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                       na.strings="Not Available");
  columns <- c("Hospital.Name", "State", 
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  data <- subset.data.frame(outcomes,select = columns)
  colnames(data) = c("Hospital","State","heart attack","heart failure","pneumonia");
  ## Check that state and outcome are valid
  if(!state %in% data[,2]){ 
    stop("invalid state")
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){ 
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  data <- filter(data, State == state)
  cols <-  grep(outcome, colnames(data), value = TRUE)
  cols <- c("Hospital", "State",cols)
  datase <- subset(data, select = cols)
  datase <- na.omit(datase)
  datase[,3] <- suppressWarnings(as.numeric(datase[,3]))
  datase <- datase[order(datase[,3], datase[,1]), ]
  if (num == "best"){
    print (datase[1,1])
  }else if (num == "worst"){
    print(tail(datase[,1],1))
  }
  else print(datase[num,1])
}