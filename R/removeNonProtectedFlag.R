##' This function has been built in order to transform all the values corresponding to non-protected in to NA
##' with ObservationFlag=M and methodFlag=u
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
##'
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##'
##' @details flagValidTable Table containing valid and protected flag combination.
##'  This table is generally recorded into the Flag package.
##'


removeNonProtectedFlag = function(data,
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


    NonprotectedFlag = flagValidTable[Protected == FALSE,]
    NonprotectedFlag[, combination := paste(flagObservationStatus, flagMethod, sep = ";")]



    dataCopy = dataCopy[,flagCombination:=paste(observationFlagVar, methodFlagVar, sep = ";")]
    NonprotectedFlagCombinations=NonprotectedFlag[,combination]

    imputedIndex=dataCopy$flagCombination %in% NonprotectedFlagCombinations

    dataCopy[imputedIndex, `:=`(c(valueVar, observationFlagVar, methodFlagVar),
                                list(NA_real_, missingObservationFlag,
                                     missingMethodFlag))]






    if(!normalised){
      dataCopy = denormalise(dataCopy, denormalisedKey)
    }

    dataCopy



  }

  else {
    stop("Selected columns are not present")
  }



}


