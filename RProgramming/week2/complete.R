complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ids <- vector("numeric", length = length(id))
    nobses <- vector("numeric", length = length(id))
    
    ci <- 0
    for(i in id) {
        ci <- ci + 1
        if(i < 10) {
            fstring <- sprintf("%s/00%.0f.csv", directory, i)
        } else if (i < 100) {
            fstring <- sprintf("%s/0%.0f.csv", directory, i)
        } else {
            fstring <- sprintf("%s/%.0f.csv", directory, i)
        }
        
        dt <- read.csv(fstring)
        
        ids[ci] <- i
        nobses[ci] <- sum(complete.cases(dt))
    }
    
    dtf <- data.frame(id=ids, nobs=nobses)
    
    dtf
}