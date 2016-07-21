##' This function removes yield that bears the value zero and replace with
##' missing value.
##'
##' By definition, yield can not be zero. When both area harvested and
##' production are zero, it is a missing value.
##'
##' @param data The dataset
##' @param yieldValue The column corresponding to the value of yield
##' @param yieldObsFlag The column corresponding to the observation status flag
##'     of yield
##' @param yieldMethodFlag The column corresponding to the method flag of yield
##' @param getSummary logic It is a logic parameter, if it is set to TRUE you can
##'  display on the console messages about how many items have been modified
##' @return A data table where all entries with zero yield are replaced with NA.
##' @export

removeZeroYield = function(data,
                           yieldValue,
                           yieldObsFlag,
                           yieldMethodFlag,
                           getSummary=FALSE){
    dataCopy = copy(data)
    if(all(c(yieldValue, yieldObsFlag, yieldMethodFlag) %in% colnames(dataCopy))){

      dimYieldZero=dim(dataCopy[yieldValue==0,])


        dataCopy[get(yieldValue) == 0,
                 `:=`(c(yieldValue, yieldObsFlag, yieldMethodFlag),
                      list(NA, "M", "u"))]

    if(getSummary){
      message("Number of zero yield:   ", dimYieldZero[1])


    }

    } else {
        warning("Yield column is not present")
    }
    dataCopy
}

