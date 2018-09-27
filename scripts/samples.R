# ---------- ---------- ----------
#      Snippets and Samples
# ---------- ---------- ----------
# 
# This document provides samples of
# R functions and concepts that are
# tricky or are not totally obvious.
# 
# NAME: 
# DESC: 
# KEYS: 
# 
# /// --- SECTION TITLE --- ///
#
# ideas - subsetting, transforms, order, APIs
# reading from a connection
# ggplot2 for sure
#
# help(package = name)
# chaining
# cut()
#
# /// DEV COMP /// #



# NAME: useColClasses
# DESC: How to easily set up the colClasses parameter when using read.table
# KEYS: 
useColClasses <- function(file) {
  initial <- read.table(file, nrows = 10)
  classes <- sapply(initial, class)
  full <- read.table(file, colClasses = classes)
}



# NAME: bracketSubsetting
# DESC: example of subsetting a dataframe with single brackets
#       in this example the user provides parameters to first filter the dataframe by a value of one of the colunms
#       the function will then sort the filtered dataframe by another user-defined column and return the rows containing the minimum values
#       the user can also define a subset of columns to return
# PARAMS: all required, can be numbers or column names
# KEYS: subset, sort, filter

bracketSubsetting <- function(df,  filterVal, filterCol, sortedCol, returnCols) {
    filteredSelector <- df[,filterCol] == filterVal  # get rows containing the filter column's  filter value
    filteredOutcome <- df[filteredSelector, ]  # reduce the dataframe using the filteredSelector
    complete <- complete.cases(filteredOutcome[, sortedCol])  # check for complete cases in the sorting column
    filteredComplete <- filteredOutcome[complete,]  # reduce to complete cases only
    sortedOutcome <- sort(filteredComplete[,sortedCol])  # sort on the sortedCol parameter
    minValues <- filteredComplete[, sortedCol] == sortedOutcome[1]  # only keep rows containing the minimum value
    minValuesDataFrame <- filteredComplete[minValues, returnCols]  # return the rows containing the minimum value and the columns specified
    minValuesDataFrame
}


# /// IN DEV /// #



# course_week: 2.4
# description: read file, filter by variable parameter, split, take the second-variable place in each partition, return df of results
# make generic: yes
rankall <- function(outcome, num = "best") {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClass = "character")
    switch(outcome, 
           "heart attack" = outcomeCol <- 11,
           "heart failure" = outcomeCol <- 17,
           "pneumonia" = outcomeCol <- 23, stop("invalid outcome"))
    if (num == "worst") {worst <- TRUE}
    else {worst <- FALSE}
    colData <- as.numeric(outcomeData[, outcomeCol])
    colDataComplete <- complete.cases(colData)
    outcomeDataComplete <- outcomeData[colDataComplete, ]
    outcomeDataComplete[, outcomeCol] <- as.numeric(outcomeDataComplete[, outcomeCol])
    # browser()
    states <- split(outcomeDataComplete, outcomeDataComplete$State)
    for (i in states) {
        orderedOutcome <- i[order(i[outcomeCol], i[2]),]
        if (num == "best") {num <- 1}
        if (worst)  {num <- nrow(orderedOutcome)}
        binder <- data.frame("hospital" = orderedOutcome[num, 2], "state" = orderedOutcome[1, 7])
        hospitalNames <- rbind(hospitalNames, binder)
    }
    hospitalNames
}

# NAME: makeCacheMatrix
# DESC: example of a function constructor to define get and set operations
# KEYS: constructor, cache, matrix

# the makeCacheMatrix function creates a list object of functions.
# the get functions can store values for their input parameters

makeCacheMatrix <- function(x) {
    inv <- NULL
    set <- function(y) {
        # the <<- function assigns a value to a variable. 
        # if the variable is not found at the current stack level, 
        # the funciton will travel up the stack until it finds the variable.
        x <<- y  # set the cached x value
        inv <<- NULL # clear the cached inverse value
    }
    get <- function() x  # simply return x, the matrix that this get function was initialized with.
    setinv <- function(inverse) inv <<-- inverse  # set the inv with a given inverse value (without validating the input parameter...)
    getinv <- function() inv  # return the inverse value, even if NULL.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# NAME: cacheSolve
# DESC: example of how to utilize construted get/set functions (from makeCacheMatrix)
# KEYS: constructor, cache, matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) 
    x$setinv(inv)
    inv
}

# NAME: combineFiles
# DESC: open a set of files from a directory, numbered (001, 002, ...), check for complete cases, and combine into a singel data frame.
# KEYS: read, files, combine
combineFiles <- function(directory, id = 1:999, column) {
    full_df = data.frame()
    for (i in id){
        padded_id <- formatC(i, width = 3, flag = 0) # format the number to be at least three characters in length
        path <- paste(directory, "/", padded_id, ".csv", sep = "") # this assumes the files are simply named as 3-character numbers
        df <- read.csv(path)
        remove_na <- complete.cases(df[[column]]) # check for complete cases of a given column
        cleaned_df <- df[remove_na,]
        full_df <- rbind(full_df, cleaned_df) # bind the complete rows into a single dataframe
    }
    full_df
}
