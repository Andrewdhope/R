#
# API Key: 9d57165f0c02abba2b8838cc2deedc830e271035
# API Documentation: https://www.census.gov/data/developers/geography.html <<< INACCURATE
# API Example: 
# http://api.census.gov/data/2010/dec/sf1?key=9d57165f0c02abba2b8838cc2deedc830e271035&get=P001001&for=state:06
# Method of Equal Proportions: https://census.gov/population/apportionment/about/computing.html
#
# ---------- ---------- ----------
#             Title
# ---------- ---------- ----------
# 
# description 
#
buildDataframe <- function(use_limit = FALSE) {
    
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
    if (use_limit) {
        total_population <- sum(df$P001001)
        seat_limit <- proposedCount(total_population, 30000, 100)    
    }
    
    # instantiate seats
    df <- cbind(df, seats = 1)
    
    # instantiate multiplier
    multiplier <- recalculateMultiplier(df)
    df <- cbind(df, multiplier = multiplier)
    
    # loop to allocate seats
    while (sum(df$seats) < seat_limit) {
        multiplier <- recalculateMultiplier(df)
        df$multiplier = multiplier
        df$seats[which.max(df$multiplier)] = df$seats[which.max(df$multiplier)] + 1
    }
    
    # order by state
    df <- df[order(df$STATE),]
    
    df
}

recalculateMultiplier <- function(df) {
    geometric_mean <- (1/(sqrt((df$seats+1)*df$seats)))
    multiplier <- df$P001001 * geometric_mean
    multiplier
}

# /// --- getCensusData --- ///
# 
# Notes:
#   Use httr and jsonlite
# 
# Next step:
#   Read through getCensus and decide whether to us it or to use httr and jsonlite
#
getCensusData <- function() {
  library('httr')
  library('jsonlite')
  # getreq <- censusapi::getCensus("dec/sf1", vintage = 2010, key, vars = c("P001001"), region = "state:06")
  url <- "https://api.census.gov/data/2010/dec/sf1"
  key <- "9d57165f0c02abba2b8838cc2deedc830e271035"
  # build state:01,02,03...56
  states_keys <- formatC(1:56, width = 2, flag = "0") # 56 states, need a reference table for state names
  states_string <- "state:"
  for (i in states_keys) {
    states_string <- paste0(states_string, i)
    if (i < length(states_keys)) {
        states_string <- paste0(states_string, ",")    
    }
  }
  req_httr <- httr::GET(url, query = list(key = "9d57165f0c02abba2b8838cc2deedc830e271035", get = "STATE,LSAD_NAME,P001001", "for" = states_string))
  req_content <- httr::content(req_httr, as = "text")
  raw_json <- jsonlite::fromJSON(req_content)
  raw_json
}

# /// --- proposedCount --- ///
# p - total population
# i - initial multiplier, multiplied by 10,000 (typically 3 for a ration of 1 per 30,000) 
# x - step value, number of representatives to increment to the next step of the function
#
proposedCount <- function(population, init = 30000, stepval = 100){
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