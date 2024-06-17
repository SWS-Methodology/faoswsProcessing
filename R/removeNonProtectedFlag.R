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
##' @param keepDataUntil numeric It is possible to delete non-protected flag starting from a
##'         specific year. This feature is important when we need to impute missing values
##'         keeping data (protected or not) up to a specific year. This parameter allows specifying
##'         the year from which we want to start deleting non-protected figures.
##'
##' @details flagValidTable Table containing valid and protected flag combination.
##'  The default is an outdated table temporarily kept for compatibility reasons.
##'
##' @export


removeNonProtectedFlag = function(data,
                                  valueVar= "Value",
                                  observationFlagVar="flagObservationStatus",
                                  methodFlagVar="flagMethod",
                                  yearVar="timePointYears",
                                  missingObservationFlag = "M",
                                  missingMethodFlag = "u",
                                  normalised= TRUE,
                                  denormalisedKey = "measuredElement",
                                  flagValidTable= ReadDatatable("valid_flags"),
                                  keepDataUntil=NULL){

  if(is.null(flagValidTable)){
    flagValidTable <- ReadDatatable("valid_flags")
  }
  dataCopy = copy(data)

  if(!normalised){
    dataCopy = normalise(dataCopy)

  }

  if(all(c(valueVar, observationFlagVar, methodFlagVar) %in% colnames(dataCopy))){


    NonprotectedFlag = flagValidTable[Protected == FALSE,]
    NonprotectedFlag[, combination := paste(flagObservationStatus, flagMethod, sep = ";")]


    dataCopy = dataCopy[,flagCombination:=paste(get(observationFlagVar), get(methodFlagVar), sep = ";")]
    NonprotectedFlagCombinations=NonprotectedFlag[,combination]


    if(!is.null(keepDataUntil)){
    ##Check if the year specified inthe keepDataUntil parametre is within the time range of the dataset
      maxYear=max(dataCopy[,get(yearVar)])
      minYear=min(dataCopy[,get(yearVar)])

      if(!(keepDataUntil>=minYear & keepDataUntil<maxYear)){
        stop("The parameter keepDataUntil is not feasible")
      }else{


        imputedIndex=dataCopy$flagCombination %in% NonprotectedFlagCombinations & dataCopy[,get(yearVar)>=keepDataUntil]

        dataCopy[imputedIndex, `:=`(c(valueVar, observationFlagVar, methodFlagVar),
                                    list(NA_real_, missingObservationFlag,
                                         missingMethodFlag))]

        dataCopy[, flagCombination:=NULL]




        if(!normalised){
          dataCopy = denormalise(dataCopy, denormalisedKey)
        }

        dataCopy





      }


    }else{

    imputedIndex=dataCopy$flagCombination %in% NonprotectedFlagCombinations

    dataCopy[imputedIndex, `:=`(c(valueVar, observationFlagVar, methodFlagVar),
                                list(NA_real_, missingObservationFlag,
                                     missingMethodFlag))]

    dataCopy[, flagCombination:=NULL]




    if(!normalised){
      dataCopy = denormalise(dataCopy, denormalisedKey)
    }

    dataCopy

  }

  }

  else {
    stop("Selected columns are not present")
  }



}


