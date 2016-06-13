##' This function performs manipulation of the data that are standard
##' after the data is retrieved from the data base.
##'
##' @param data The data.table object
##' @param normalised logical, whether the data is normalised
##' @param dropNonExistingRecord logical, whether non existing records should be
##'     droped. Non-existing records are those having flagObservationStatus and
##'     flagMethod have the value NA. This can be generated from denormalising
##'     the data or occassionaly returned by the GetData function.
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##'
##' @return A data.table with standard pre processing steps
##'     performed.
##'
##' @export
##'

preProcessing = function(data,
                         normalised = TRUE,
                         dropNonExistingRecord = TRUE,
                         denormalisedKey = "measuredElement"){

    dataCopy = copy(data)

    if(!normalised){
        dataCopy = normalise(dataCopy)
    }

    if(!all(c("timePointYears", "Value", "flagObservationStatus") %in%
            colnames(dataCopy)))
        stop("Required column not in data, this function assumes the data is ",
             "normalised")

    ## Converting year to numeric for modelling
    dataCopy[, `:=`(c("timePointYears"), as.numeric(.SD[["timePointYears"]]))]

    dataCopy =
        remove0M(dataCopy, valueVars = "Value", flagVars = "flagObservationStatus")

    if(dropNonExistingRecord){
        dataCopy =
            dataCopy[!is.na(dataCopy[["flagObservationStatus"]]), ]
    }


    if(any(is.na(dataCopy[["flagObservationStatus"]]))){
        stop("There are non-existing records, please fill them with the ",
             "fillRecord() function or remove them from the data")
    }

    if(!normalised){
        dataCopy = denormalise(dataCopy, denormalisedKey)
    }
    dataCopy
}
