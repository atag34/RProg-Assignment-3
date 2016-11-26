library(dplyr)
best <- function(state, outcome) {
  ## Read outcome data
outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                     na.strings="Not Available");
columns <- c("Hospital.Name", "State", 
             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
data <- subset.data.frame(outcomes,select = columns)
colnames(data) = c("Hospital","State","HeartAttack","HeartFailure","Pneumonia");
  ## Check that state and outcome are valid
if(!state %in% data[,2]){ 
    stop("state invalid")
} else if (!outcome %in% c("HeartAttack", "HeartFailure", "Pneumonia")){ 
    stop("outcome invalid")
}
  ## Return hospital name in that state with lowest 30-day death
  else {
  ds <- subset(data, State == state);
  ds <- subset(ds, select = c("Hospital",outcome));
  ds <- as.numeric(ds[,outcome])
  bad <- is.na(ds)
  ds <- ds[!bad, ]
  ds <- arrange_(ds,outcome, "Hospital")
  }
  ## rate
print(ds[1,1])
tail(ds)
}
