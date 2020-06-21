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
# description 
#
# /// --- getCensusDataframe --- ///
# 
# Notes:
#   Use httr and jsonlite
# 
# Next step:
#   Read through getCensus and decide whether to us it or to use httr and jsonlite
#
getCensusDataframe <- function() {
  library('httr')
  library('jsonlite')
  library('censusapi')
  url <- "https://api.census.gov/data/2010/dec/sf1"
  key <- "9d57165f0c02abba2b8838cc2deedc830e271035"
  req_httr <- httr::GET(url, query = list(key = "9d57165f0c02abba2b8838cc2deedc830e271035", get = "P001001", "for" = "state:06"))
  req_content <- httr::content(req_httr, as = "text")
  raw_json <- jsonlite::fromJSON(req_content)
  print(raw_json)
  getreq <- getCensus("dec/sf1", vintage = 2010, key, vars = c("P001001"), region = "state:06")
  print(getreq)
}
# /// --- proposedCount --- ///
# p - total population
# i - initial multiplier, multiplied by 10,000 (typically 3 for a ration of 1 per 30,000) 
# x - step value, number of representatives to increment to the next step of the function
#
proposedCount <- function(population,init,stepval){
  reps <- 0
  while (TRUE) {
    if ((population/(10000*init))>=stepval) {
      reps <- reps+stepval
      population <- population-(10000*init*stepval)
    } else {
        reps <- reps + ceiling(population/(10000*init))
        break
      }
    init <- init+1
  }
  reps
}