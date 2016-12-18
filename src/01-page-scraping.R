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

makeFBGraphAPICall <- function(node = "", query = NULL, version = "v2.8", 
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
    query        = 
      list(fields = "message,created_time,id,story,attachments", limit = 100),
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
        created_time = convertTimeStamp(created_time),
        page_name = page_name
      ) %>% 
      select(
        page_name,
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

getFBComments <- function(object_ids, access_token = fb.tkn) {
  # For a vector of Facebook Objcet IDs object_ids, get all the comments
  # associated with these objects and return a clean dataframe of 
  # results.
  #
  # Args:
  #   object_ids: vector of Facebook Object IDs
  #   access_token: access token (not needed if fb.tkn is initialized)
  
  map_df(
    # For each object_id
    object_ids,
    function(object_id) {
      # Get comments associated with object
      makeFBGraphAPICall(
        node = paste0(object_id, "/comments"),
        query = list(limit = 10000)
      ) %>% 
        content(as = "text") %>% 
        fromJSON(flatten = TRUE) -> fbobject.cnt
      fbobject.cnt$data -> object_comments.dt
      cat(nrow(object_comments.dt), " comments retrieved.\n")
      
      # Return NULL if no comments
      if (length(object_comments.dt) == 0) return(NULL)
      
      # Grab until no more next page
      while (!is.null(fbobject.cnt$paging$`next`)) {
        GET(fbobject.cnt$paging$`next`) %>% 
          content(as = "text") %>% 
          fromJSON(flatten = TRUE) ->
          fbobject.cnt
        object_comments.dt %<>% rbind(fbobject.cnt$data)
        cat(nrow(object_comments.dt), " comments retrieved.\n")
      }
      
      # Add column for object id and return
      object_comments.dt$object_id <- object_id
      object_comments.dt 
    }
  ) %>% 
    mutate(
      comment_timestamp_utc = convertTimeStamp(created_time)
    ) %>% 
    select(
      object_id,
      comment_id = id,
      comment_timestamp_utc,
      comment_message = message,
      commenter_name = from.name,
      commenter_id = from.id
    )
}

getFBReactions <- function(object_ids, access_token = fb.tkn) {
  # For a vector of Facebook Object IDs object_ids, get all the reactions
  # associated with these objects and return a clean dataframe of 
  # results.
  #
  # Args:
  #   object_ids: vector of Facebook Object IDs
  #   access_token: access token (not needed if fb.tkn is initialized)
  
  map_df(
    # For each object_id
    object_ids,
    function(object_id) {
      # Get reactions associated with object
      makeFBGraphAPICall(
        node = paste0(object_id, "/reactions"),
        query = list(limit = 10000)
      ) %>% 
        content(as = "text") %>% 
        fromJSON(flatten = TRUE) -> fbobject.cnt
      fbobject.cnt$data -> object_comments.dt
      cat(nrow(object_comments.dt), " reactions retrieved.\n")
      
      # Return NULL if no comments
      if (length(object_comments.dt) == 0) return(NULL)
      
      # Grab until no more next page
      while (!is.null(fbobject.cnt$paging$`next`)) {
        GET(fbobject.cnt$paging$`next`) %>% 
          content(as = "text") %>% 
          fromJSON(flatten = TRUE) ->
          fbobject.cnt
        object_comments.dt %<>% rbind(fbobject.cnt$data)
        cat(nrow(object_comments.dt), " reactions retrieved.\n")
      }
      
      # Add column for object id and return
      object_comments.dt$object_id <- object_id
      object_comments.dt 
    }
  ) 
}

getFBPage <- function(page_names, n_posts, date_limit, access_token = fb.tkn) {
  # Get relevant posts, comments, reactions, comment replies, and 
  # comment reactions associated with Facebook page(s).
  #
  # Args:
  #   page_names:   vector of Facebook page name(s) and/or ID(s)
  #   n_posts:      limit to number of posts to scrape
  #   date_limit:   limit to earliest date to scrape 
  #   access_token: App access token (no need if fb.tkn is initialized)
  # 
  # Returns:
  #   A list per page containing the page posts, comments, reactions,
  #   comment replies, and comment reactions in that order.
  
  map(
    page_names,
    function(page_name) {
      getFBPagePosts(
        page_name = page_name,
        n_posts = n_posts,
        date_limit = date_limit
      ) -> posts.dt
      
      getFBComments(
        object_ids = posts.dt$post_id
      ) -> posts_comments.dt
      
      getFBReactions(
        object_ids = posts.dt$post_id
      ) -> posts_reactions.dt
      
      getFBComments(
        object_ids = posts_comments.dt$comment_id
      ) -> posts_comments_replies.dt
      
      getFBReactions(
        object_ids = posts_comments.dt$comment_id[1:5]
      ) -> comments_reactions.dt
      
      list(
        posts                    = posts.dt,
        posts_comments           = posts_comments.dt,
        posts_reactions          = posts_reactions.dt,
        posts_comments_replies   = posts_comments_replies.dt,
        posts_comments_reactions = posts_comments_reactions.dt
      )
    }
  )
}

# Scraping ----------------------------------------------------------------

# Get app access token

# NOTE: Ensure you have two character scalars fb_app_id and fb_app_secret
# in this rda file. You need to create your own credentials
load("bin/fb_auth.rda") 

fb.tkn <- getAppAccessToken(fb_app_id, fb_app_secret)

