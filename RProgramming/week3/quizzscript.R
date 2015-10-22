absDif <- function(a1, a2) {
    return(abs(a1-a2))
}

avMPGperCyl <- tapply(mtcars$mpg, mtcars$cyl, mean)

avHPperCyl <- tapply(mtcars$hp, mtcars$cyl, mean)

absDif(avHPperCyl[1], avHPperCyl[3])