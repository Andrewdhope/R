# https://developer.twitter.com/en/docs/api-reference-index

printTweets <- function(tweets) {
  for (i in seq(along = tweets)) {
    print(tweets[[i]]$text)
  } 
}

getUserTweets <- function(screen_name){
  library(httr)
  
  # instantiate my app with references to my registered keys
  myapp <- oauth_app("twitter", key = "sZd2IQv1SO68cP4ECNX9mTnTO", secret = "5aFakrIFpoI33X5Tr1PDTIawtMpm2EKsKDEf3HjbxG7V8b63hM")
  
  # get OAuth credentials
  twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)
  gtoken <- config(token = twitter_token)
  
  url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
  
  req <- GET(url, gtoken, screen_name = "AndresEsperanza")
  stop_for_status(req)
  content(req)
}

getHomeTimeline <- function() {
  library(httr)
  
  # instantiate my app with references to my registered keys
  myapp <- oauth_app("twitter", key = "sZd2IQv1SO68cP4ECNX9mTnTO", secret = "5aFakrIFpoI33X5Tr1PDTIawtMpm2EKsKDEf3HjbxG7V8b63hM")
  
  # get OAuth credentials
  twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)
  gtoken <- config(token = twitter_token)
  
  url <- "https://api.twitter.com/1.1/statuses/home_timeline.json"
  
  req <- GET(url, gtoken)
  stop_for_status(req)
  content(req)
}

