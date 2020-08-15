# NEXT STEPS:
#   - consolidate what i have in a presentable draft, with plans to expand
#   - research/draft arguments, write them out manually, keep them general for now
#   - doing this for: portfolio, practice, edification, publication
#   - start with content, then format to feature it
#   - plot variance in average seat size
#   - use state abbreviations to flip back to the x-axis
#   - get historical data: population per state, seats per state
#       + check the derived values: 1863, 1920
#
# LAYOUT:
#   - Title: an artificial shortage of congressional representation
#   - Intro, brief w/ sources
#   - Problem summary and key statistics
#   - Proposal
#       + justification extrapolated Article I algorithm
#       + 3 plots
#   - Conclusion
#   - References
#   - Data
#
# API Key: 9d57165f0c02abba2b8838cc2deedc830e271035
# API Documentation: https://www.census.gov/data/developers/geography.html <<< INACCURATE
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
#   calls:
#       - getCensusData
#        - seatsDataframe
#
buildDataframe <- function() {
    df <- getCensusData()
    df <- as.data.frame(df)
    
    #efficiency gain: memory
    df_limit <- seatsDataframe(df, TRUE)
    df_unlimit <- seatsDataframe(df, FALSE)
    
    # merge limit and unlimit tables
    df <- cbind(df_limit, df_unlimit$seats)
    df <- df[,-5]
    
    # influence columns - limit, unlim, delta
    df <- cbind(df, 100*round((df[4]/sum(df[4])),5))
    df <- cbind(df, 100*round((df[5]/sum(df[5])),5))
    df <- cbind(df, df[7]-df[6])
    
    # avg_seat_size_limit, avg_seat_size_unlim
    df <- cbind(df, round(df[3]/df[4],0))
    df <- cbind(df, round(df[3]/df[5],0))
    
    # order, rank, re-order
    # population_rank
    df <- cbind(df, rank(-df[3], ties.method = "first"))

    colnames(df)[1] <- "state_id"
    colnames(df)[2] <- "state_name"
    colnames(df)[3] <- "state_pop"
    colnames(df)[4] <- "seats_lim"
    colnames(df)[5] <- "seats_unlim"
    colnames(df)[6] <- "inf_lim(%)"
    colnames(df)[7] <- "inf_unlim(%)"
    colnames(df)[8] <- "inf_delta(%)"
    colnames(df)[9] <- "avg_seat_size_lim"
    colnames(df)[10] <- "avg_seat_size_unlim"
    colnames(df)[11] <- "pop_rank"

    # re-order alphabetically
    df <- df[order(df$state_name),]
    
    df
}

# /// --- historicalDataPrep --- ///
#
# Notes:
#   parameters:
#
historicalDataPrep <- function() {
    library(dplyr)
    library(tidyr)
    library(stringr)
    
    # next steps: check the math, especially 1863, 1911-1943 (AZ, NM admissions in 1912?).
    
    # read dataframes
    tb_app <- read.csv("../data/Historical_Apportionment.csv")
    tb_ens <- read.csv("../data/Historical_Population-Enslaved.csv")
    tb_tot <- read.csv("../data/Historical_Population-Total.csv")
    
    # melt into tibbles
    tb_app <- pivot_longer(tb_app, -c(1:2), names_to = "years_string", values_to = "seats")
    tb_ens <- pivot_longer(tb_ens, -name, names_to = "decade", values_to = "count.ens")
    tb_tot <- pivot_longer(tb_tot, -c(1:2), names_to = "decade", values_to = "count.tot")
    
    # account for enslaved population
    tb_join <- left_join(tb_tot, tb_ens, by = c("name" = "name", "decade" = "decade"))
    tb_join <- bind_cols(tb_join, count.comp = tb_join$count.tot - round(0.4*tb_join$count.ens)) # subtract 40% enslaved from total population
    tb_join[is.na(tb_join$count.comp),6] = tb_join[is.na(tb_join$count.comp),4] # if sum is NA, replace with total column
    
    # split apportionment column into apportionment year and associated decade    
    tb_app <- mutate(tb_app, s = str_split_fixed(tb_app$years_string, "[[:punct:]]", n = 2)) %>% rowwise() # rowwise needed for tibbles
    tb_app <- mutate(tb_app, decade = unlist(s)[1], apportionment = unlist(s)[2])
    tb_app <- select(tb_app, -c("s","years_string"))
    tb_app <- tb_app[,c(1,2,4,5,3)]
    
    # join apportionment data into population data
    tb_join <- right_join(tb_join, tb_app, by = c("name" = "name", "decade" = "decade"))
    
    # additional calculated columns
    tb_join <- mutate(tb_join, seatsize = round(tb_join$count.comp/tb_join$seats))
    tb_join <- tb_join[!tb_join$decade == "X1789",] # remove 1789 rows, no prior census
    tb_join$decade = as.integer(str_sub(tb_join$decade, start = 2L, end = -1L)) # trim leading X and make numeric
    tb_join$apportionment = as.integer(tb_join$apportionment)
    
    # calculate mean seatsize for each decade
    tb_join <- group_by(tb_join, apportionment) 
    tb_join <- mutate(tb_join, mean.seatsize = round(mean(seatsize, na.rm = TRUE)))
    tb_join <- ungroup(tb_join)
    
    tb_join <- mutate(tb_join, diff.seatsize = seatsize-mean.seatsize)
    
    tb_join
    
}

# /// --- historicalPlot --- ///
#
# Notes:
#   parameters:
#
historicalPlot <- function() {
    library(dplyr)
    library(ggplot2)
    
    tb_summ <- historicalDataPrep()
    tb_summ <- summarise(group_by(tb_join, apportionment), mean.seatsize = mean(mean.seatsize), max.diff = max(diff.seatsize, na.rm = TRUE), min.diff = min(diff.seatsize, na.rm = TRUE))
    
    g <- ggplot(data = tb_summ)
    g <- g + geom_line(mapping = aes(x = apportionment, y = mean.seatsize))
    g <- g + geom_ribbon(aes(x = apportionment, y = mean.seatsize, ymin = mean.seatsize+min.diff, ymax = mean.seatsize+max.diff, alpha = 0.1))

    g
}
# /// --- generatePlot --- ///
#
# Notes:
#   parameters:
#       - df: dataframe
#       - y_col: the name of the column in df that will be used for the plot's y-axis
#       - order_col: the name of the column in df that will be used for the order of the x-axis
#
generatePlot <- function(df, y_col, order_col) {
    library(ggplot2)
    
    # get column name from dataframe
    y_col_name <- names(df[y_col])
    order_col_name <- names(df[order_col])
    
    g <- ggplot(data = df)
    # !!ensym() needed to format the string properly within the arguments list (string w/o quotes)
    g <- g + geom_col(mapping = aes(x = reorder(state_name, !!ensym(order_col_name)), y = !!ensym(y_col_name), fill = "#6C5B7B")) 
    g <- g + xlab("50 States")
    g <- g + coord_flip()
    
    g
}

# /// --- seatsDataframe --- ///
# 
# Notes:
#   parameters:
#       - seats_capped: set to TRUE to use the 435 seat limit, FALSE to use the uncapped apportionment formula
#   calls:
#       - getCensusData
#       - uncappedApportionment
#       - recalculateMultiplier
#
seatsDataframe <- function(df, seats_capped = TRUE) {

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
        if ((population/init)>=stepval) {
          seat_limit <- seat_limit+stepval
          population <- population-(init*stepval)
        } else {
            seat_limit <- seat_limit + ceiling(population/init)
            break
          }
        init <- init+10000
      }
    seat_limit
}