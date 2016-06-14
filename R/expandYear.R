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
                      valueVar = "Value"){
    key = c(areaVar, elementVar, itemVar)
    keyDataFrame = data[, key, with = FALSE]
    yearDataFrame = data.frame(unique(data[[yearVar]]))
    colnames(yearDataFrame) = yearVar

    completeBasis =
        data.table(merge.data.frame(keyDataFrame, yearDataFrame))
    expandedData =
        merge(completeBasis, data, by = colnames(completeBasis), all.x = TRUE)
    expandedData[, `:=`("emptyTimeSeries", sum(is.na(.SD[[valueVar]])) == .N),
                 by = key]
    reducedData = expandedData[!expandedData$emptyTimeSeries, ]
    reducedData[, `:=`("emptyTimeSeries", NULL)]
    reducedData
}
