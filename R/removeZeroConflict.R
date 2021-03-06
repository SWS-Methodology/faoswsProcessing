##' Remove Zero Conflicts
##'
##' The function examines two variables of a data.table object.  If one
##' variable is 0 and another is not, then both variables are marked as
##' missing.  This is useful for variables which should always be zero
##' concurrently, such as area harvested and production.
##'
##' @param data The data table object.
##' @param value1 The column name of data corresponding to the first variable.
##' @param value2 The column name of data corresponding to the second variable.
##' @param observationFlag1 The column name of data containing the observation
##' flag for the first variable.
##' @param observationFlag2 The column name of data containing the observation
##' flag for the second variable.
##' @param methodFlag1 The column name of data containing the method flag for
##' the first variable.
##' @param methodFlag2 The column name of data containing the method flag for
##' the second variable.
##' @param missingObservationFlag The flag (character value) which should be
##' placed in the observation flag columns to signify a missing value.
##' @param missingMethodFlag The flag (character value) which should be placed
##' in the method flag columns to signify a missing value.
##' @param getSummary logic It is a logic parameter, if it is set to TRUE you can
##'  display on the console messages about how many items have been modified
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified (some values are marked as missing if the have
##' conflicting zeroes).
##'
##' @export
##'

removeZeroConflict = function(data, value1, value2, observationFlag1,
                              observationFlag2, methodFlag1, methodFlag2,
                              missingObservationFlag = "M",
                              missingMethodFlag = "u",
                              getSummary=FALSE){
    dataCopy = copy(data)

    ### Data Quality Checks
    stopifnot(is(dataCopy, "data.table"))
    cnames = c(value1, value2, observationFlag1, observationFlag2,
               methodFlag1, methodFlag2)
    stopifnot(is(cnames, "character"))
    if(length(cnames) < 6){
        stop("One of the column names supplied (value1/2, observationFlag1/2 ",
             ", methodFlag1/2 is NULL!")
    }
    if(all(cnames %in% colnames(dataCopy))){
        ## Identify points where area = 0 and production != 0 (or vice versa)
        filter1 = dataCopy[, get(value1) == 0 & get(value2) != 0]
        filter2 = dataCopy[, get(value1) != 0 & get(value2) == 0]


        dimFilter1= dim(dataCopy[filter1 ,])
        dimFilter2= dim(dataCopy[filter2 ,])

        message("Number of item with value1=0 and value2 !=0   ", dimFilter1[1])

        message("Number of item with value1!=0 and value2 =0   ", dimFilter2[1])

        ## For problematic observations, set the zero value to missing.
        dataCopy[filter1 , `:=`(c(value1, observationFlag1, methodFlag1),
                                list(NA_real_, missingObservationFlag,
                                          missingMethodFlag))]
        dataCopy[filter2 , `:=`(c(value2, observationFlag2, methodFlag2),
                                list(NA_real_, missingObservationFlag,
                                          missingMethodFlag))]

        if(getSummary){
          message("Number of item with value1=0 and value2 !=0:   ", dimFilter1[1])

          message("Number of item with value1!=0 and value2 =0:  ", dimFilter2[1])


        }

    } else {
        warning("Selected columns are not present, no processing is performed")
    }
    dataCopy
}
