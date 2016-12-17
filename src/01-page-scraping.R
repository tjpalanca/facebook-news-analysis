# Facebook Page Scraping
# mail@tjpalanca.com
# 17 Dec 2016

# We scrape the Facebook Graph API in order to collect the data required
# for the analysis of fake account behaviors

# Libraries ---------------------------------------------------------------
library(httr)     # Making API calls
library(jsonlite) # JSON parsing

library(magrittr) # Advanced piping
library(purrr)    # Functional programming

library(stringr)  # String manipulation
library(dplyr)    # Data frame manipulation
library(tibble)   # Better data frames

# Scraping Functions ------------------------------------------------------

getAppAccessToken <- function(fb_app_id, fb_app_secret) {
  # Get app access token 
  #
  # Args:
  #   fb_app_id:      character scalar containing the Facebook App ID
  #   fb_app_secret:  character scalar containing the Fabebook App Secret
  #
  # Returns:
  #   App Access Token (do not reveal)
  
  GET(
    url   = "https://graph.facebook.com/oauth/access_token",
    query = list(
      client_id     = fb_app_id,
      client_secret = fb_app_secret,
      grant_type    = "client_credentials"
    )
  ) %>% 
    content() %>% 
    str_replace("^access_token=", "")
}

makeFBGraphAPICall <- function(node, query = NULL, version = "v2.8", 
                               access_token = fb.tkn) {
  # Function to generate a generic Facebook Graph API Call.
  #
  # Args:
  #   node:         the node to be pinged
  #   query:        list of queries
  #   access_token: the access token (not needed if fb.tkn is present)
  # 
  # Returns:
  #   Unparsed resuts from the FB API call. 
  
  GET(
    url   = paste0("https://graph.facebook.com/", version, "/"),
    path  = node,
    query = append(list(access_token = access_token), query)
  )
  
}

getFBPagePosts <- function(page_name, n_posts, access_token = fb.tkn) {
  
  # Initializing call
  
  # Grab new content
  makeFBGraphAPICall(
    node         = paste0(page_name, "/posts"),
    query        = list(fields = "message,created_time,id,story,attachments"),
    access_token = access_token
  ) %>% 
    content(as = "text") %>% 
    fromJSON(flatten = TRUE) ->
    fbpage.cnt
  
  cat(paste0("Grabbed ", nrow(fbpage.cnt$data), " posts... "))
  
  posts.dt <- fbpage.cnt$data %>% remove_rownames()
  
  cat(paste0(nrow(posts.dt), " posts now collected. \n"))
  
  # Repeat until n_posts has been reached and next page still exists
  while (nrow(posts.dt) < n_posts & !is.null(fbpage.cnt$paging$`next`)) {
    
    # Grab new content
    GET(fbpage.cnt$paging$`next`) %>% 
      content(as = "text") %>% 
      fromJSON(flatten = TRUE) ->
      fbpage.cnt
    
    cat(paste0("Grabbed ", nrow(fbpage.cnt$data), " posts... "))
    
    # Append posts
    posts.dt %>% rbind(fbpage.cnt$data %>% remove_rownames()) -> posts.dt
    
    cat(paste0(nrow(posts.dt), " posts now collected.\n"))
    
  }
  
  return(posts.dt %>% slice(1:n_posts))
  
}

# Scraping ----------------------------------------------------------------

# Get app access token

# NOTE: Ensure you have two character scalars fb_app_id and fb_app_secret
# in this rda file. You need to create your own credentials
load("bin/fb_auth.rda") 

fb.tkn <- getAppAccessToken(fb_app_id, fb_app_secret)

getFBPagePosts(
  page_name = "rapplerdotcom",
  n_posts = 10000
) -> test
