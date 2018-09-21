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
#
# help(package = name)
# chaining
# cut()
#
# NAME: useColClasses
# DESC: How to easily set up the colClasses parameter when using read.table
# KEYS: 
useColClasses <- function(file) {
  initial <- read.table(file, nrows = 100)
  classes <- sapply(initial, class)
  full <- read.table(file, colClasses = classes)
}


# /// STARTING TO JUST GATHER FUNCTIONS FROM VARIOUS COURSE WEEKS /// #

# NAME: best
# DESC: read file, filter by variable parameter, sort by second parameter, make df of best of sorted variable
# KEYS: 
# make generic: yes
best <- function(state, outcome){
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClass = "character") # set all of the columns as character vectors
    switch(outcome, 
           "heart attack" = outcomeCol <- 11,
           "heart failure" = outcomeCol <- 17,
           "pneumonia" = outcomeCol <- 23, stop("invalid outcome"))
    stateSelector <- outcomeData$State == state # take the state column
    outcomeState <- outcomeData[stateSelector, ]
    outcomeState[, outcomeCol] <- as.numeric(outcomeState[, outcomeCol]) # convert to numeric
    complete <- complete.cases(outcomeState[, outcomeCol]) 
    outcomeStateComplete <- outcomeState[complete, ]
    sortedOutcome <- sort(outcomeStateComplete[, outcomeCol]) # sort by the outcome column
    minHospitals <- outcomeStateComplete[, outcomeCol] == sortedOutcome[1] # take the minimum value of the sorted column
    hospitalNames <- outcomeStateComplete[minHospitals, 2] # make a data frame of the best row(s)
    hospitalNames
}


# course_week: 2.4
# description: read file, filter by variable parameter, order by second variable, return name of row in third-variable place
# make generic: yes
rankhospital <- function(state, outcome, num) {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClass = "character")
    switch(outcome, 
           "heart attack" = outcomeCol <- 11,
           "heart failure" = outcomeCol <- 17,
           "pneumonia" = outcomeCol <- 23, stop("invalid outcome"))
    stateSelector <- outcomeData$State == state  # logical vector by state
    outcomeState <- outcomeData[stateSelector, ]
    outcomeState[, outcomeCol] <- as.numeric(outcomeState[, outcomeCol]) # coerce outcome column to numeric
    complete <- complete.cases(outcomeState[, outcomeCol])
    outcomeStateComplete <- outcomeState[complete, ]
    orderedOutcome <- outcomeStateComplete[order(outcomeStateComplete[,outcomeCol], outcomeStateComplete[, 2]),]
    if (num == "best") {num <- 1}
    if (num == "worst")  {num <- nrow(orderedOutcome)}
    hospitalName <- orderedOutcome[num, 2]
    hospitalName
}

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

# course_week: 2.3
# description: Use a function constructor to define get and set operations
# make generic: no
makeCacheMatrix <- function (x) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function () x
    setinv <- function(inverse) inv <<-- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function (x, ...) {
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

# course_week: 2.2 
# description: Loop through a set of files, combine all complete cases to a new data frame
# make generic: yes
pollutantmean <- function(directory, pollutant, id = 1:332) {
    full_df = data.frame()
    for (i in id){
        padded_id <- formatC(i, width = 3, flag = 0)
        path <- paste(directory, "/", padded_id, ".csv", sep = "")
        df <- read.csv(path)
        remove_na <- complete.cases(df[[pollutant]])
        cleaned_df <- df[remove_na,]
        full_df <- rbind(full_df, cleaned_df)
    }
    mean(full_df[, pollutant])
}

# course_week: 10
# description: given a document term matrix, plot either 
## a) a histogram of the top n most common words, or 
## b) an analysis showing the cumulative frequency of terms
# make generic: maybe

build.pareto <- function(dtm) {
    library(ggplot2)
    freq <- colSums(as.matrix(dtm))
    ord <- order(freq, decreasing = TRUE)
    ordered <- freq[ord]
    running.total <- sapply(seq_along(ordered), function(x){sum(ordered[1:x])})
    p <- qplot(seq_along(running.total), running.total/running.total[length(running.total)], xlab = "Top Grams", ylab = "Instances (%)")
    p
}
