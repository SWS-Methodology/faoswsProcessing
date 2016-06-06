##' This function removes previous calculated values
##'
##' @details The observation flag is not required, as previous calculated values
##'     can contain any observation flag due to flag aggregation.
##'
##' @param data The data.table object containing the values.
##' @param valueVar The column name of data which contains the values for the
##'     variable to be modified.
##' @param observationFlagVar The column name of the observation flag
##'     corresponding to value.
##' @param methodFlagVar The column name of the method flag corresponding to
##'     value.
##' @param missingObservationFlag The character value which is used to represent
##'     missing values in the observation flag. This value will be put in any
##'     column containing imputation data that is removed.
##' @param missingMethodFlag The character value which is used to represent
##'     missing values in the method flag. This value will be put in any column
##'     containing imputation data that is removed.
##' @param calculatedMethodFlag This character value specifies which method flag
##'     correspond to previous calculated values in the data.
##'
##' @return No value is returned. However, the object "data" which was passed to
##'     this function is modified.
##'
##' @export

removeCalculated = function(data,
                            valueVar,
                            observationFlagVar,
                            methodFlagVar,
                            missingObservationFlag = "M",
                            missingMethodFlag = "u",
                            calculatedMethodFlag = "i"){
    dataCopy = copy(data)

    ## Data Quality Checks
    stopifnot(is(dataCopy, "data.table"))

    if(all(c(valueVar, observationFlagVar, methodFlagVar) %in% colnames(dataCopy))){
        calculatedIndex =
            dataCopy[[methodFlagVar]] == calculatedMethodFlag
        dataCopy[calculatedIndex,
                 `:=`(c(valueVar, observationFlagVar, methodFlagVar),
                      list(NA, missingObservationFlag, missingMethodFlag))]
    } else {
        stop("Selected columns are not present")
    }
    dataCopy
}

