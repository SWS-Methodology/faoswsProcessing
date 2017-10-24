##' This function expands the range of the year variable.
##'
##' This is required due the sparse representation of databases. When a new
##' round of processing commencing in 2015, since there are no data input nor
##' previous imputation, the record does not exist and thus imputation or any
##' processing has no record to replace. We need to expand the year variable in
##' order to perform imputation.
##'
##' New records created are not filled with flags, both observation status and
##' method flag remains NA. The reason behind this is so that after the
##' processing, if the record can not be populated with imputation then it will
##' be dropped and not saved back to the database.
##'
##' @param data The dataset
##' @param areaVar The column name corresponding to the geographic area.
##' @param itemVar The column name corresponding to the commodity item.
##' @param elementVar The column name corresponds to the measured element.
##' @param yearVar The column name corresponds to the time dimension.
##' @param valueVar The regular expression to capture the value column.
##'
##' @return A new dataset with the year variable expanded
##'
##' @export
##'
##'


expandYear = function(data,
                      areaVar = "geographicAreaM49",
                      elementVar = "measuredElement",
                      itemVar = "measuredItemCPC",
                      yearVar = "timePointYears",
                      valueVar = "Value",
                      obsflagVar="flagObservationStatus",
                      methFlagVar="flagMethod",
                      newYears=NULL){
    key = c(elementVar, areaVar,  itemVar)
    keyDataFrame = data[, key, with = FALSE]

    keyDataFrame=keyDataFrame[with(keyDataFrame, order(get(key)))]
    keyDataFrame=keyDataFrame[!duplicated(keyDataFrame)]

    yearDataFrame = unique(data[,get(yearVar)])
    if(!is.null(newYears)){

      yearDataFrame=unique(c(yearDataFrame, newYears, newYears-1, newYears-2))

    }

    yearDataFrame=data.table(yearVar=yearDataFrame)
    colnames(yearDataFrame) = yearVar

    completeBasis =
        data.table(merge.data.frame(keyDataFrame, yearDataFrame))
    expandedData = merge(completeBasis, data, by = colnames(completeBasis), all.x = TRUE)
    expandedData = fillRecord(expandedData)

##------------------------------------------------------------------------------------------------------------------
    ## control closed series: if in the data pulled from the SWS, the last protected value is flagged as (M,-).
    ## In this situation we do not have to expand the session with (M, u), but with (M, -) in order to
    ## avoid that the series is imputed for the new year

    ## 1. add a column containing the last year for which it is available a PROTECTED value
    seriesToBlock=expandedData[(get(methFlagVar)!="u"),]
    #seriesToBlock[,lastYearAvailable:=max(timePointYears), by=c( "geographicAreaM49","measuredElement","measuredItemCPC")]
    seriesToBlock[,lastYearAvailable:=max(get(yearVar)), by=key]
    ## 2. build the portion of data that has to be overwritten

    seriesToBlock[,flagComb:=paste(get(obsflagVar),get(methFlagVar), sep = ";")]
    seriesToBlock=seriesToBlock[get(yearVar)==lastYearAvailable & flagComb=="M;-"]


    ##I have to expand the portion to include all the yers up to the last year
  if(nrow(seriesToBlock)>0){
    seriesToBlock=seriesToBlock[, {max_year = max(as.integer(.SD[,timePointYears]))
    data.table(timePointYears = seq.int(max_year + 1, newYears),
               Value = NA_real_,
               flagObservationStatus = "M",
               flagMethod = "-")[max_year < newYears]},  by = key]

    ##I have to expand the portion to include all the yers up to the last year
    expandedData=
      merge(expandedData, seriesToBlock,
            by=c( "geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears"),
            all.x=TRUE, suffixes = c("","_MDash"))

    expandedData[!is.na(flagMethod_MDash),flagMethod:=flagMethod_MDash]
    expandedData=expandedData[,colnames(data),with=FALSE]
  }


  expandedData
}
