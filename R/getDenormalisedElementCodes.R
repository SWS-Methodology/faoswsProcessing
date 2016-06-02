##' Function to get all the elements of a denormalised dataset
##'
##' @param denormalisedData A data.table with the elements denormalised.
##'
##' @return The element codes in the data
##'
##' @export

getDenormalisedElementCodes = function(denormalisedData){
    ## We assume the element code is always the last four digit of the name.
    elementColName =
        colnames(denormalisedData)[which(grepl("_[0-9]{4}$",
                                               colnames(denormalisedData)))]
    elements =
        unique(sapply(strsplit(elementColName, "_"),
                      function(x) x[[length(x)]]))
    elements
}
