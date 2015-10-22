best <- function (state, outcome) {
    dt <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    if(length(dt[dt[, "State"] == state,"State"]) == 0) stop("invalid state")
    if(!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) stop("invalid outcome")
    
    if(outcome == "heart attack") {
        dt[,11] <- as.numeric(dt[,11])
        dt <- dt[dt[, "State"] == state, c(2, 7, 11)]
    } else if (outcome == "heart failure") {
        dt[,17] <- as.numeric(dt[,17])
        dt <- dt[dt[, "State"] == state, c(2, 7, 17)]
    } else {
        dt[,23] <- as.numeric(dt[,23])
        dt <- dt[dt[, "State"] == state, c(2, 7, 23)]
    }
    
    ok <- complete.cases(dt[,3])
    dt <- dt[ok,]
    hospName <- dt[dt[,3] == min(dt[,3]), 1]
    
    hospName <- hospName[order(hospName)[1]]
    hospName
}