pureDigit <- function(X){ as.numeric(gsub('[^[:digit:]]','', as.character(X))) } #convert string to valid numeric type
