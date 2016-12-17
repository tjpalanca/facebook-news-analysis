# Facebook Page Scraping
# mail@tjpalanca.com
# 17 Dec 2016

# We scrape the Facebook Graph API in order to collect the data required
# for the analysis of fake account behaviors

# Libraries ---------------------------------------------------------------
library(httr)      # Making API calls
library(jsonlite)  # JSON parsing

library(magrittr)  # Advanced piping
library(purrr)     # Functional programming

library(stringr)   # String manipulation
library(dplyr)     # Data frame manipulation
library(tibble)    # Better data frames
library(tidyr)     # Frame shaping
library(lubridate) # Timestamp manipulation

# Scraping Functions ------------------------------------------------------

convertTimeStamp <- function(ts) {
  # Converts string timestamp ts into POSIXct format
  as.POSIXct(strptime(ts, "%Y-%m-%dT%H:%M:%S%z"))
}

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


getFBPagePosts <- function(page_name, n_posts = Inf, date_limit = -Inf,
                           access_token = fb.tkn) {
  # Scrapes posts from a Facebook Page, subject to a limit on either/both
  # number of posts and or the date (in local system time).
  #
  # Args:
  #   page_name:   the username of the page to be scraped
  #   n_posts:     limit on the number of posts to be taken
  #   date_limit:  limit on the minimum date to be scraped
  #   access_token: access_token (ignore if fb.tkn is already initialized)
  #
  # Returns:
  #   data frame containing information on the posts from the Facebook 
  #   page that satisfy the limits
  
  # Raise exception if no limit is specified
  if (is.infinite(n_posts) & is.infinite(date_limit)) {
    stop("No post or date limit specified!")
  }
  
  # Function: Collection Status
  showCollectionStatus <- function(dt) {
    # Shows the collection status given a dataframe with scrape
    cat(
      paste0(
        nrow(dt), 
        " posts now collected from ",
        format(
          convertTimeStamp(min(dt$created_time)), 
          format = "%Y-%m-%d %H:%M:%S"
        ), 
        "\n"
      )
    )
  }
  
  # Initialize content
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
  showCollectionStatus(posts.dt)
    
  # Repeat until n_posts has been reached and next page still exists
  while (nrow(posts.dt) < n_posts & 
         min(convertTimeStamp(posts.dt$created_time)) > date_limit &
         !is.null(fbpage.cnt$paging$`next`)) {
    
    # Grab new content
    GET(fbpage.cnt$paging$`next`) %>% 
      content(as = "text") %>% 
      fromJSON(flatten = TRUE) ->
      fbpage.cnt
    cat(paste0("Grabbed ", nrow(fbpage.cnt$data), " posts... "))
    
    # Append posts
    posts.dt %>% bind_rows(fbpage.cnt$data %>% remove_rownames()) -> posts.dt
    showCollectionStatus(posts.dt)
  }
  
  # Limit to n_posts and date_limit if applicable
  if (!is.infinite(n_posts)) {
    posts.dt %<>% slice(1:n_posts) 
  }
  # Limit to date limit
  if (!is.infinite(date_limit)) {
    posts.dt %<>% 
      slice(which(convertTimeStamp(posts.dt$created_time) > date_limit))
  }
  
  # Flatten out attachments data
  posts.dt$attachments.data %>% 
    map_df(
      function(x) {
        if (length(x) == 0) {
          return(data_frame(description = NA))
        } else {
          return(x[1,])
        }
      }
    ) -> posts_attachments.dt
  
  # Return clean dataset with renamed columns
  return(
    cbind(
      posts.dt %>% select(-attachments.data),
      posts_attachments.dt
    ) %>% 
      mutate(
        article_url = coalesce(url, target.url),
        created_time = convertTimeStamp(created_time)
      ) %>% 
      select(
        post_id = id,
        post_timestamp_utc = created_time,
        post_message = message,
        post_story = story,
        article_id = target.id,
        article_title = title,
        article_description = description,
        article_type = type,
        article_url,
        article_media_height = media.image.height,
        article_media_width = media.image.width,
        article_media_url = media.image.src
      ) 
  )
}

# Scraping ----------------------------------------------------------------

# Get app access token

# NOTE: Ensure you have two character scalars fb_app_id and fb_app_secret
# in this rda file. You need to create your own credentials
load("bin/fb_auth.rda") 

fb.tkn <- getAppAccessToken(fb_app_id, fb_app_secret)

getFBPagePosts(
  page_name = "rapplerdotcom",
  date_limit = as.POSIXct("2016-12-17")
) -> posts.dt

