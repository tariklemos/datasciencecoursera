corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    res <- vector("numeric")
    
    ci <- 0
    for(i in 1:332) {
        if(i < 10) {
            fstring <- sprintf("%s/00%.0f.csv", directory, i)
        } else if (i < 100) {
            fstring <- sprintf("%s/0%.0f.csv", directory, i)
        } else {
            fstring <- sprintf("%s/%.0f.csv", directory, i)
        }
        
        dt <- read.csv(fstring)
        
        ncc <- sum(complete.cases(dt))
        if(ncc > threshold) {
            sulf <- dt[complete.cases(dt),"sulfate"]
            nitr <- dt[complete.cases(dt),"nitrate"]
            
            res <- c(res, cor(sulf, nitr))
        }
    }
    
    return(res)
}