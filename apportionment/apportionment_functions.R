# NEXT STEP: Build out the table with various derived columns for things like comparative influence, etc.
#
# API Key: 9d57165f0c02abba2b8838cc2deedc830e271035
# API Documentation: https://www.census.gov/data/developers/geography.html <<< INACCURATE
# API Example: 
# http://api.census.gov/data/2010/dec/sf1?key=9d57165f0c02abba2b8838cc2deedc830e271035&get=P001001&for=state:06
#
# ---------- ---------- ----------
#             Title
# ---------- ---------- ----------
#
#
# description
#
# ********************************
#
# /// --- buildDataframe --- ///
# 
# Notes:
#   parameters:
#       - seats_capped: set to TRUE to use the 435 seat limit, FALSE to use the uncapped apportionment formula
#   calls:
#       - getCensusData
#       - uncappedApportionment
#       - recalculateMultiplier
#
buildDataframe <- function(seats_capped = TRUE) {
    
    df <- getCensusData()
    df <- as.data.frame(df)
    
    # move variable names into colnames
    colnames(df) <- df[1,]
    df <- df[-1,]
    df <- df[,-4]
    
    # remove District of Columbia
    df <- df[!df$STATE == 11,]
    
    # set data types
    df$P001001 <- as.numeric(df$P001001)
    
    # set seat limit
    seat_limit <- 435
    if (!seats_capped) {
        total_population <- sum(df$P001001)
        seat_limit <- uncappedApportionment(total_population, 30000, 100)    
    }
    
    # instantiate seats
    df <- cbind(df, seats = 1)
    
    # instantiate multiplier
    priority_values <- recalculatePriorityValues(df)
    df <- cbind(df, priority_values = priority_values)
    
    # loop to allocate seats
    while (sum(df$seats) < seat_limit) {
        priority_values <- recalculatePriorityValues(df)
        df$priority_values = priority_values
        df$seats[which.max(df$priority_values)] = df$seats[which.max(df$priority_values)] + 1
    }
    
    # order by state
    df <- df[order(df$STATE),]
    
    df
}

# /// --- recalculatePriorityValues --- ///
# 
# Notes:
#   Derived from the Method of Equal Proportions: https://census.gov/population/apportionment/about/computing.html
#   parameters:
#       - df: dataframe from buildDataframe
#   returns: priority_value, as described by the Method of Equal Proportions 
#
recalculatePriorityValues <- function(df) {
    multiplier <- (1/(sqrt((df$seats+1)*df$seats))) # geometric mean, as described by the Method of Equal Proportions
    priority_values <- df$P001001 * multiplier
    priority_values
}

# /// --- getCensusData --- ///
# 
# Notes:
#   dependencies: httr, jsonlite
#
getCensusData <- function() {
  library('httr')
  library('jsonlite')
    
  url <- "https://api.census.gov/data/2010/dec/sf1"
  key <- "9d57165f0c02abba2b8838cc2deedc830e271035"
  
  # build a state string for the querystring parameter, in the format of "states:01,02,03...55,56"
  states_keys <- formatC(1:56, width = 2, flag = "0") # 56 states, territories and D.C. will be filtered along the way.
  states_string <- "state:"
  for (i in states_keys) {
    states_string <- paste0(states_string, i)
    if (i < length(states_keys)) {
        states_string <- paste0(states_string, ",")    
    }
  }
  
  # GET request
  req_get <- httr::GET(url, query = list(key = "9d57165f0c02abba2b8838cc2deedc830e271035", get = "STATE,LSAD_NAME,P001001", "for" = states_string))
  req_content <- httr::content(req_get, as = "text")
  raw_json <- jsonlite::fromJSON(req_content)
  raw_json
}

# /// --- uncappedApprotionment --- ///
# 
# Notes:
#   _description_
#   parameters:
#       - population: total population as a summary of represented states with populations of districts and territories excluded
#       - init: initial divisor, number of citizens represented by one seat, (default: 30,000)
#       - stepval: step value, number of apportioned seats needed to increment the init value (default: 100)
#   returns: priority_value, as described by the Method of Equal Proportions 
#
uncappedApportionment <- function(population, init = 30000, stepval = 100){
    seat_limit <- 0
      while (TRUE) {
        if ((population/(init))>=stepval) {
          seat_limit <- seat_limit+stepval
          population <- population-(init*stepval)
        } else {
            seat_limit <- seat_limit + ceiling(population/(10000*init))
            break
          }
        init <- init+1
      }
    seat_limit
}