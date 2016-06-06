##' Function to remove prior imputation.
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
##' @param imputationEstimationObservationFlag This character value specifies
##'     which observation flags correspond to imputed/estimated values in the
##'     data.
##' @param imputationEstimationMethodFlag This character value specifies which
##'     method flags correspond to imputed/estimated values in the data.
##'
##' @details Observations which have observationFlagVar == imputedFlag will be
##'     modified to missing. Thus, the value will be updated to NA, the
##'     observation flag will be updated to missingObservationFlag, and the
##'     method flag will be updated to missingMethodFlag.
##'
##' @return No value is returned. However, the object "data" which was passed to
##'     this function is modified.
##'
##' @export
##'

removeImputationEstimation = function(data,
                                      valueVar,
                                      observationFlagVar,
                                      methodFlagVar,
                                      missingObservationFlag = "M",
                                      missingMethodFlag = "u",
                                      imputationEstimationObservationFlag = "I",
                                      imputationEstimationMethodFlag = "e"){
    dataCopy = copy(data)

    ## Data Quality Checks
    stopifnot(is(dataCopy, "data.table"))
    if(length(imputationEstimationObservationFlag) != 1 |
       length(imputationEstimationMethodFlag) != 1)
        stop("The observation and the flag method should be a single character, ",
             "they determine a single unique combination")

    if(all(c(valueVar, observationFlagVar, methodFlagVar) %in% colnames(dataCopy))){
        imputedIndex =
            dataCopy[[observationFlagVar]] == imputationEstimationObservationFlag &
            dataCopy[[methodFlagVar]] == imputationEstimationMethodFlag
        dataCopy[imputedIndex, `:=`(c(valueVar, observationFlagVar, methodFlagVar),
                                list(NA, missingObservationFlag,
                                     missingMethodFlag))]
    } else {
        stop("Selected columns are not present")
    }
    dataCopy
}
