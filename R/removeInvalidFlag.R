##' This function has been built in order to transform all the values corresponding to ivalid flag combinations
##' into NA M u and activate the imputation process.
##'
##' @param data The data.table object to be manipulate
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
##' @param normalised logical, whether the data is normalised
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##' @param flagValidTable Table containing the Valid/Invalid and Protected/Non
##' protected flag combinations
##'
##' @details flagValidTable Table containing valid and protected flag combination.
##'  This table is generally recorded into the Flag package.
##'
##' @export


removeInvalidFlag = function(data,
                                  valueVar= "Value",
                                  observationFlagVar="flagObservationStatus",
                                  methodFlagVar="flagMethod",
                                  missingObservationFlag = "M",
                                  missingMethodFlag = "u",
                                  normalised= TRUE,
                                  denormalisedKey = "measuredElement",
                                  flagValidTable= NULL ){

  if(is.null(flagValidTable)){
    flagValidTable <- faoswsFlag::flagValidTable
  }
  dataCopy = copy(data)

  if(!normalised){
    dataCopy = normalise(dataCopy)

  }

  if(all(c(valueVar, observationFlagVar, methodFlagVar) %in% colnames(dataCopy))){


    validFlag = flagValidTable[Valid == TRUE,]
    validFlag[, combination := paste(flagObservationStatus, flagMethod, sep = ";")]



    dataCopy = dataCopy[,flagCombination:=paste(get(observationFlagVar), get(methodFlagVar), sep = ";")]
    validFlagCombinations=validFlag[,combination]

    imputedIndex=(!dataCopy$flagCombination %in% validFlagCombinations)

    dataCopy[imputedIndex, `:=`(c(valueVar, observationFlagVar, methodFlagVar),
                                list(NA_real_, missingObservationFlag,
                                     missingMethodFlag))]


    if(!normalised){
      dataCopy = denormalise(dataCopy, denormalisedKey)
    }

    return(dataCopy)



  }

  else {
    stop("Selected columns are not present")
  }



}


