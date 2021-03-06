##' This function replaces non-existing records with missing records.
##'
##' Occastionally the database may return records where all three columns
##' (value, observation flag, method flag) are all NA and is a non-existing
##' record in the database; this can also be created when denormalising data. We
##' will replace is with a missing record representation where (Value = NA,
##' flagObservationStatus = "M" and flagMethod = "u").
##'
##' @param data The dataset
##' @param areaVar The column name corresponding to the geographic area.
##' @param itemVar The column name corresponding to the commodity item.
##' @param elementVar The column name corresponds to the measured element.
##' @param yearVar The column name corresponds to the time dimension.
##' @param valueVar The regular expression to capture the value column.
##' @param flagObsVar The regular expression to capture the observation
##'     status flag column.
##' @param flagMethodVar The regular expression to capture the method flag
##'     column.
##'
##' @return dataset with identical dimension but creates missing record in place
##'     of non-existing record.
##'
##' @export
##' @import faoswsUtil
##'

fillRecord = function(data,
                      areaVar = "geographicAreaM49",
                      itemVar = "measuredItemCPC",
                      elementVar = "measuredElement",
                      yearVar = "timePointYears",
                      valueVar = "Value",
                      flagObsVar = "flagObservationStatus",
                      flagMethodVar = "flagMethod"){
    dataCopy = copy(data)
    normalised = all(c(areaVar, itemVar, elementVar, yearVar) %in%
                     colnames(dataCopy))
    if(normalised){
        dataCopy[is.na(dataCopy[[valueVar]]) &
                 is.na(dataCopy[[flagObsVar]]) &
                 is.na(dataCopy[[flagMethodVar]]),
                 `:=`(c(valueVar, flagObsVar, flagMethodVar),
                      list(NA, "M", "u"))]
    } else {
        elementCodes = gsub("[^0-9]", "",
                            grep("[0-9]{4}", colnames(dataCopy), value = TRUE))
        valueVars = grep(valueVar, colnames(dataCopy), value = TRUE)
        flagObsVars = grep(flagObsVar, colnames(dataCopy), value = TRUE)
        flagMethodVars = grep(flagMethodVar, colnames(dataCopy), value = TRUE)

        for(elementCode in elementCodes){
            currentValueVar = grep(paste0(elementCode, "$"),
                                   valueVars, value = TRUE)
            currentFlagObsVar = grep(paste0(elementCode, "$"),
                                     flagObsVars, value = TRUE)
            currentFlagMethodVar = grep(paste0(elementCode, "$"),
                                        flagMethodVars, value = TRUE)
            dataCopy[is.na(dataCopy[[currentValueVar]]) &
                     is.na(dataCopy[[currentFlagObsVar]]) &
                     is.na(dataCopy[[currentFlagMethodVar]]),
                     `:=`(c(currentValueVar, currentFlagObsVar,
                            currentFlagMethodVar),
                          list(NA, "M", "u"))]
        }
    }
    dataCopy
}
