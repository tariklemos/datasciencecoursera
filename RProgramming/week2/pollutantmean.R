pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    mus <- vector("numeric", length = length(id)) 
    allPolu <- vector("numeric")
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
        
        polu <- dt[!is.na(dt[,pollutant]),pollutant]
        allPolu <- c(allPolu, polu)
    }
    
    mu <- mean(allPolu)
    
    return(mu)
}