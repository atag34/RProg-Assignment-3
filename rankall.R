library(dplyr)
rankall <- function(outcome, num = "best") {
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
   if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){ 
    stop("invalid outcome")
  }
allstates <- unique(data$State)
mylist <- list()
for (i in allstates){
  statedata <- filter(data, State == i)
  cols <-  grep(outcome, colnames(data), value = TRUE)
  cols <- c("Hospital", "State",cols)
  datase <- subset(statedata, select = cols)
  datase <- na.omit(datase)
  datase[,3] <- suppressWarnings(as.numeric(datase[,3]))
  datase <- datase[order(datase[,3], datase[,1]), ]
  if (num == "best"){
datase <-  datase[1,c(1,2)]
mylist[[i]] <- datase
  }else if (num == "worst"){
datase <- tail(datase[,c(1,2)],1)
mylist[[i]] <- datase
  }
  else 
datase <- datase[num,c(1,2)]
mylist[[i]] <- datase
}
df <- do.call("rbind",mylist)
print(df)
}