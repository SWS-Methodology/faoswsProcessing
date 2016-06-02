##' This function creates the triplet element for an item if the full triplet is
##' missing.
##'
##' The element triplet includes the input, productivity and output. However,
##' for certain commodities certain element was never collected and under the
##' sparse data representation the element is not availble.
##'
##' This function creates the temporary element that is required for processing.
##'
##' @param data The data, must be denormalised
##' @param formula The formula table, see \code{getYieldFormula}.
##'
##' @return The original data if the triplet is present, otherwise a new
##'     data.table with the missing elements created.
##'
##' @export


createTriplet = function(data, formula){
    dataCopy = copy(data)

    allElements = with(formula, c(input, productivity, output))
    dataElements = getDenormalisedElementCodes(dataCopy)
    missingElements = setdiff(allElements, dataElements)
    if(length(missingElements) > 0){
        elementMeasuredDim =
            grep(dataElements[1], colnames(dataCopy), value = TRUE)
        for(missingElement in missingElements){
            newElementDim =
                gsub(dataElements[1], missingElement, elementMeasuredDim)
            dataCopy[, `:=`(c(newElementDim), list(NA, "M", "u"))]
        }
    }
    dataCopy
}

