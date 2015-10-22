rankall <- function (outcome, num = "best") {
    dt <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    if(!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) stop("invalid outcome")
    
    if(outcome == "heart attack") {
        dt[,11] <- as.numeric(dt[,11])
        dt <- dt[, c(2, 7, 11)]
    } else if (outcome == "heart failure") {
        dt[,17] <- as.numeric(dt[,17])
        dt <- dt[, c(2, 7, 17)]
    } else {
        dt[,23] <- as.numeric(dt[,23])
        dt <- dt[, c(2, 7, 23)]
    }
    
    orderOutcome <- function (x, num) {
        x <- x[order(x[,3], x[,1]),]
        
        if(num == "best") {
            return(x[1, 1:2])
        } else if(num == "worst") {
            if(is.na(x[dim(x)[1],2])) x[dim(x)[1],2] <- x[1,2]
            return(x[dim(x)[1], 1:2])
        } else {
            if(is.na(x[num,2])) x[num,2] <- x[1,2]
            return(x[num, 1:2])
        }
    }
    
    getName <- function(x) {
        return(x[1,1])
    }
    
    getState <- function(x) {
        return(x[1,2])
    }
    
    ok <- complete.cases(dt[,3])
    dt <- dt[ok,]
    splitted <- split(dt, dt[,2])
    dtf <- lapply(splitted, orderOutcome, num=num)
    hospnames <- sapply(dtf, getName)
    statenames <- sapply(dtf, getState)
    
    dtf <- data.frame(hospital = hospnames, state = statenames)
    dtf
}