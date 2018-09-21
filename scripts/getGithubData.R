# https://developer.github.com/v3/
  
getRepos <- function(user = "andrewdhope") {
  library(httr)
  
  # instantiate my app with references to my registered keys
  myapp <- oauth_app("github", key = "3ba49c05c52a96264bc9", secret = "b8263c838fc23ba083b19a80eeb4d5730778a47d")
  
  # get OAuth credentials
  github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
  gtoken <- config(token = github_token)
  
  url <- paste("https://api.github.com/users/", user, "/repos", sep = "")
  
  req <- GET(url, gtoken)
  stop_for_status(req)
  content(req)
}

printNames <- function(repo) {
 for (i in seq(along = repo)) {
   print(repo[[i]]$name)
 } 
}
