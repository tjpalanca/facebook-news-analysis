# Facebook Page Scraping Functions
# mail@tjpalanca.com | 1 Jan 2017

# This section contains functions to be used in scraping Facebook page posts,
# likes, comments, and the likes and comments of such comments.

# Pushover functions
load("../bin/pushover_auth.rda")

pushNotification <- function(text = "Execution Completed") {
  pushover(
    title = text,
    url = "http://adhoc01.tjpalanca.com",
    message = "adhoc01.tjpalanca.com",
    user = pushover_user_key,
    app = pushover_app_key
  )
}

getNAforNULL <- function(x) {
  # returns NA when object is null
  if (is.null(x)) {
    return(NA)
  } else{
    return(x)
  }
}

convertTimeStamp <- function(ts) {
  # Converts string timestamp ts into POSIXct format
  as.POSIXct(strptime(ts, "%Y-%m-%dT%H:%M:%S%z"))
}

getAppAccessToken <- function(fb_app_id, fb_app_secret) {
  # Get app access token 
  #
  # Args:
  #   fb_app_id:      character scalar containing the Facebook App ID
  #   fb_app_secret:  character scalar containing the Facebook App Secret
  #
  # Returns:
  #   App Access Token in fb.tkn (do not assign)
  
  GET(
    url   = "https://graph.facebook.com/oauth/access_token",
    query = list(
      client_id     = fb_app_id,
      client_secret = fb_app_secret,
      grant_type    = "client_credentials"
    )
  ) %>% 
    content(as = "text") %>% 
    str_replace("^access_token=", "") ->> fb.tkn 
}

callFBGraphAPI <- function(node = "", query = NULL, url = "", 
                           version = "v2.8", access_token = fb.tkn) {
  # Function to generate a generic Facebook Graph API Call and parse results. 
  # Uses API v2.8 by default.
  #
  # Args:
  #   node:         the node to be pinged
  #   query:        list of queries
  #   url:          if url is already provided (as in pagination), 
  #                   url will be fetched directly
  #   access_token: the access token (not needed if fb.tkn is present)
  # 
  # Returns:
  #   Parsed resuts from the FB API call.
  
  # Query string out until satisfactory result or error
  if (url != "") {
    result <- GET(url)
    
    url %>% 
      URLdecode() %>% 
      str_extract("\\.limit\\([0-9]+\\)") %>%
      str_replace_all("\\.limit\\(|\\)", "") %>% 
      as.integer() -> 
      original_number
    
    dropdown_ratio <- 1
    
    # Drop down query string by 50% as long as API returns 500 error
    while (result$status_code == 500) {
      dropdown_ratio * 0.5 -> dropdown_ratio 
      
      ceiling(original_number * dropdown_ratio) -> new_number
      
      message("Dropping down request to ", new_number, " due to API error 500...")
      
      url %>% 
        URLdecode() %>% 
        str_replace(
          "\\.limit\\([0-9]+\\)",
          paste0(".limit(", new_number, ")")
        ) %>% 
        URLencode() ->
        new_url
      
      GET(new_url) -> result
      
    }
  } else {
    GET(
      url   = paste0("https://graph.facebook.com/", version, "/"),
      path  = node,
      query = append(list(access_token = access_token), query)
    ) -> result
  }
  
  result %T>% {
    # Check for API call errors
    if (!(.$status_code %in% c(200))) {
      
      # Check for allowable errors for error 400
      if (.$status_code == 400) {
        # Get error code
        code <- content(., as = "text") %>% fromJSON() %$% error %$% code
        # Code 100: Design enforced comments limit
        if (code == 100) return(NULL)
      } else {
        stop (paste0("API Call Error: ", .$status_code))
      }
    } else {
      
      # Parse content
      content(., as = "text") %>% 
        fromJSON() ->
        result
      
      # Restore original next link
      if (url != "" & !is.null(result$paging$`next`)) {
        result$paging$`next` %>%
          URLdecode() %>% 
          str_replace(
            "\\.limit\\([0-9]+\\)",
            paste0(".limit(", original_number, ")")
          ) %>% 
          URLencode() ->
          result$paging$`next`
      }
      
      return(result)
    }
  }
}

expandPaging <- function(object_ids, object_data, object_next) {
  # If API results are incomplete due to paging, completes the data
  # and cleans it up into a tidy data frame
  #
  # Args:
  #   object_ids:  list of IDs to be assigned to each batch
  #   object_data: list of data frames that comprise initial data
  #   object_next: list of links returned as the next API call
  # 
  # Returns:
  #   list of bindable dataframes with complete data for all pages 
  
  # Return NULL if no reasonable input
  if (is.null(object_ids) | is.null(object_data)) return(NULL)
  
  # Replace next with NA if no next
  if (is.null(object_next)) object_next = rep(NA, length(object_ids))
  
  pmap(
    list(
      object_id   = object_ids,
      object_data = object_data,
      object_next = object_next
    ),
    function(object_id, object_data, object_next) {
      
      message(".", appendLF = FALSE)
      
      data <- object_data
      
      if (ifelse(is.null(object_next), FALSE, 
                 ifelse(is.na(object_next), FALSE, TRUE))) {
        
        object_next %>% 
          URLdecode() %>% 
          str_extract("comments\\.limit\\([0-9]+\\)") %>%
          str_replace_all("comments\\.limit\\(|\\)", "") %>% 
          as.integer() -> 
          original_number
        
      }
      
      while(ifelse(is.null(object_next), FALSE, 
                   ifelse(is.na(object_next), FALSE, TRUE))) {
        message(".", appendLF = FALSE)
        
        object_next %>% 
          URLdecode() %>% 
          str_replace(
            "comments\\.limit\\([0-9]+\\)", 
            paste0("comments.limit(", original_number * 2, ")")
          ) %>% 
          URLencode() ->
          object_next 
        
        object_next ->> a
        
        callFBGraphAPI(url = object_next) -> content
        
        if(length(content$data) > 0) {
          rbind.pages(list(data, content$data)) -> data
        }
        
        object_next <- content$paging$`next`
        
      }
      
      data$object_id <- object_id
      data
    }
  )
  
}

queryFBPosts <- function(
  num_comments, num_attachments, num_reactions, 
  num_comments_likes, num_comments_comments, num_comments_comments_likes
) {
  # Generates Graph API query statement based on number of entities
  # 
  # Args:
  #   num_*: number of that entity to retrieve for the first page.
  #   
  # Returns:
  #   Facebook Graph API Query String that can be passed on to the API call.
  paste0(
    "
    id, created_time, message, story, 
    attachments{
    title, description, type, target{id, url}, media{image{src}}
    },
    comments.limit(", num_comments,"){
    id, created_time, message, from{id, name},
    comments.limit(", num_comments_comments,"){
    id, created_time, message, from{id, name},
    likes.limit(", num_comments_comments_likes,")
    },
    likes.limit(", num_comments_likes,"){
    id, name
    },
    attachments.limit(", num_attachments,"){
    title, description, type, target{id, url}, media{image{src}}
    }
    },
    reactions.limit(", num_reactions,"){id, name, type}
    "
  )
}

getFBPage <- function(page_name = NULL, next_link = NULL, limit_posts = Inf, 
                      limit_timestamp = -Inf, posts_per_page = 5, 
                      num_comments = 50, num_attachments = 50, num_reactions = 500,  
                      num_comments_likes = 100, num_comments_comments = 10,
                      num_comments_comments_likes = 100,
                      access_token = fb.tkn) {
  # Take complete posts data from a Facebook page
  #
  # Args:
  #   page_name:       username or page_id of the page to be scraped
  #   next_link:       if continuing from a previous query, the next link to be called
  #   limit_posts:     maximum number of posts to be scraped
  #   limit_timestamp: earliest timestamp of posts to be scraped
  #   posts_per_page:  number of posts per call (reduce if failing)
  #   num_comments, num_attachments, num_reactions: Number of entities to include in each call
  #     reduce the number if you are getting 500 errors
  #   access_token:    app access token (not needed if fb.tkn initialized)
  #
  # Returns:
  #   A list of post data, and the next link, bound in a list
  
  # Raise exception if no limit is specified
  if (is.infinite(limit_posts) & is.infinite(limit_timestamp)) {
    stop("No post or date limit specified!")
  }
  
  # Raise exception if no page_name specified
  if (is.null(page_name)) stop("No page name specified!")
  
  # Make initial API call
  if (is.na(next_link)) {
    callFBGraphAPI(
      node = paste0(page_name, "/posts"),
      query = list(
        fields = queryFBPosts(
          num_comments, num_attachments, num_reactions, 
          num_comments_likes, num_comments_comments, num_comments_comments_likes
        ),
        limit = posts_per_page
      )
    ) -> fbpage.cnt
  } else if (!is.na(next_link)) {
    callFBGraphAPI(url = next_link) -> fbpage.cnt
  }
  
  # Extract data and limit comparison data
  fbposts.ls  <- fbpage.cnt$data
  num_posts   <- nrow(fbposts.ls)
  min_created <- min(convertTimeStamp(fbposts.ls$created_time))
  message(num_posts, " posts - ", format(min_created, "%Y-%m-%d %H:%M:%S %z"))
  
  # Repeat while limit_posts or limit_timestamp has not been reached and 
  # next page still exists
  while (num_posts   < limit_posts & 
         min_created > limit_timestamp &
         !is.null(fbpage.cnt$paging$`next`)) {
    
    # Grab new content
    callFBGraphAPI(url = fbpage.cnt$paging$`next`) -> fbpage.cnt
    
    # Bind new content
    rbind.pages(pages = list(fbposts.ls, fbpage.cnt$data)) -> fbposts.ls
    
    # Recompute limits and report
    num_posts   <- nrow(fbposts.ls)
    min_created <- min(convertTimeStamp(fbposts.ls$created_time))
    message(num_posts, " posts - ", format(min_created, "%Y-%m-%d %H:%M:%S %z"))
    
  }
  
  # Assign page name and return
  fbposts.ls$page_name <- rep(page_name, length(fbposts.ls$id))
  
  # Return with next link (in case of further querying)
  list(
    posts = fbposts.ls,
    next_link = fbpage.cnt$paging$`next`
  )
  
}

tidyFBPageData <- function(posts.ls, access_token = fb.tkn) {
  # Take a list of post data and translate into tidy data frames
  # 
  # Args:
  #   posts:        list of post data generated from `getFBPage`
  #   access_token: Facebook app access token (not needed if fb.tkn exists)
  #
  # Returns:
  #   List of 7 dataframes:
  #     posts:             posts data
  #     posts_reactions:   reactions to top-level posts
  #     posts_attachments: data of articles/links/media attached to post
  #     posts_comments:    comments on posts
  #     posts_comments_likes:    likes on comments on posts
  #     posts_comments_comments: comment replies
  #     posts_comments_comments_likes: likes on comment replies
  
  # Get post data
  message("Grabbing post data...", appendLF = FALSE)
  posts.ls %>%
    select(-matches("reactions|attachments|comments")) %>% 
    mutate(
      post_timestamp_utc = convertTimeStamp(created_time)
    ) %>% {
      data_frame(
        page_name    = .$page_name,
        post_id      = .$id, 
        post_message = .$message %>% getNAforNULL(), 
        post_timestamp_utc = .$post_timestamp_utc,
        post_story   = .$story %>% getNAforNULL()
      )
    } -> posts.dt
  message(" Done!")
  
  # Get post attachments data 
  message("Grabbing post attachments data...", appendLF = FALSE)
  if (is.null(posts.ls$attachments)) {
    posts_attachments.dt <- NA
    message(" Does not exist. Skipped!")
  } else {
    posts.ls %>% 
      select(id, attachments) %>% {
        expandPaging(
          object_ids  = .$id,
          object_data = .$attachments$data,
          object_next = .$attachments$paging$`next`
        ) 
      } %>% 
      map_df(
        function(attachments) {
          data_frame(
            object_id              = attachments$object_id,
            attachment_title       = attachments$title      %>% getNAforNULL(),
            attachment_type        = attachments$type       %>% getNAforNULL(),
            attachment_target_url  = attachments$target$url %>% getNAforNULL(),
            attachment_media_url   = attachments$media$url  %>% getNAforNULL()
          )
        }
      )->
      posts_attachments.dt
    message(" Done!")
  }
  
  # Get post reactions data
  message("Grabbing post reactions data...", appendLF = FALSE)
  if (is.null(posts.ls$reactions)) {
    posts_reactions.dt <- NA
    message(" Does not exist. Skipped!")
  } else {
    posts.ls %>% 
      select(id, reactions) %>% {
        if (is.null(.$id)) return(NULL)
        expandPaging(
          object_ids  = .$id,
          object_data = .$reactions$data,
          object_next = .$reactions$paging$`next`
        ) %>% 
          bind_rows() %>% 
          select(
            object_id     = object_id,
            reactor_id    = id,
            reactor_name  = name,
            reaction_type = type
          )
      } ->
      posts_reactions.dt
    message(" Done!")
  }
  
  # Get post comments raw data
  message("Grabbing post comments raw data...", appendLF = FALSE)
  if (is.null(posts.ls$comments)) {
    posts_comments.dt <- NA
    posts_comments_likes.dt <- NA
    posts_comments_comments.dt <- NA 
    posts_comments_comments_likes.dt <- NA
    message(" Does not exist. Skipped!")
  } else {
    posts.ls %>% 
      select(id, comments) %>% {
        expandPaging(
          object_ids  = .$id,
          object_data = .$comments$data,
          object_next = .$comments$paging$`next`
        ) 
      } ->
      posts_comments.ls
    message(" Done!")
    
    # Get post comments data
    message("Grabbing post comments processed data...", appendLF = FALSE)
    posts_comments.ls %>% 
      map_df(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          data_frame(
            object_id             = comments$object_id,
            comment_id            = comments$id,
            comment_timestamp_utc = convertTimeStamp(comments$created_time),
            comment_message       = comments$message,
            commenter_name        = comments$from$name,
            commenter_id          = comments$from$id
          ) 
        }
      ) -> posts_comments.dt
    message(" Done!")
    
    # Get post comments likes data
    message("Grabbing post comments reactions data...", appendLF = FALSE)
    posts_comments.ls %>% 
      map(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          list(
            id    = comments$id,
            likes = comments$likes
          )
        }
      ) %>% 
      map(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          expandPaging(
            object_ids  = comments$id,
            object_data = comments$likes$data,
            object_next = comments$likes$paging$`next`
          ) 
        }
      ) %>% 
      flatten() %>% 
      bind_rows() %>% {
        if (is.null(.) | nrow(.) == 0) {
          return(NA)
        } else {
          return(
            select(
              .,
              object_id   = object_id,
              liker_id    = id,
              liker_name  = name
            )
          )
        }
      } ->
      posts_comments_likes.dt
    message(" Done!")
    
    # Get post comment replies raw data
    message("Grabbing post comments replies raw data...", appendLF = FALSE)
    posts_comments.ls %>% 
      map(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          list(
            id       = comments$id,
            comments = comments$comments
          )
        }
      ) %>% 
      map(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          expandPaging(
            object_ids  = comments$id,
            object_data = comments$comments$data,
            object_next = comments$comments$paging$`next`
          ) 
        }
      ) %>% 
      flatten() ->
      posts_comments_comments.ls
    message(" Done!")
    
    # Get post comment replies data
    message("Grabbing post comments replies processed data...", appendLF = FALSE)
    posts_comments_comments.ls %>% 
      map_df(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          data_frame(
            object_id             = comments$object_id,
            comment_id            = comments$id,
            comment_timestamp_utc = convertTimeStamp(comments$created_time),
            comment_message       = comments$message,
            commenter_name        = comments$from$name,
            commenter_id          = comments$from$id
          ) 
        }
      ) %>% {
        if (is.null(.) | nrow(.) == 0) {
          return(NA)
        } else {
          return(.)
        }
      } -> posts_comments_comments.dt
    message(" Done!")
    
    # Get post comment replies reactions data
    message("Grabbing post comments replies reactions data...", appendLF = FALSE)
    posts_comments_comments.ls %>% 
      map(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          list(
            id    = comments$id,
            likes = comments$likes
          )
        }
      ) %>% 
      map(
        function(comments) {
          if (is.null(comments$id)) return(NULL)
          expandPaging(
            object_ids  = comments$id,
            object_data = comments$likes$data,
            object_next = comments$likes$paging$`next`
          ) 
        }
      ) %>% 
      flatten() %>% 
      bind_rows() %>% {
        if (is.null(.) | nrow(.) == 0) {
          return(NA)
        } else {
          return(
            select(
              .,
              object_id   = object_id,
              liker_id    = id,
              liker_name  = name
            )
          )
        }
      } ->
      posts_comments_comments_likes.dt
    message(" Done!")
    
  }
  
  message("Returning data...")
  list(
    posts                         = posts.dt,
    posts_attachments             = posts_attachments.dt,
    posts_reactions               = posts_reactions.dt,
    posts_comments                = posts_comments.dt,
    posts_comments_likes          = posts_comments_likes.dt,
    posts_comments_comments       = posts_comments_comments.dt,
    posts_comments_comments_likes = posts_comments_comments_likes.dt
  )
  
}

cachedGetFBPage <- function(page_name, limit_timestamp, 
                            timezone, cache_interval, ...) {
  # Grabs complete data from a Facebook Page as specified by a date limit. 
  # Unlike getFBPage, this method caches every cache_interval to file ond disk
  # ../cache/FBpage.rds so that the process can resume in cases of failure.
  # 
  # Args:
  #   page_name: Facebook Page ID of the page
  #   limit_timestamp: string representing the timestamp limit (earliest) in POSIX format
  #   timezone: timezone for the limit_timestamp (e.g. Asia/Manila)
  #   cache_interval: in words, the interval at which it will cache (e.g. 1 day)
  #   ...: Other arguments to be passed to getFBpage
  #   
  # Returns:
  #   List of list by cache_interval, each sublist contains posts in downloaded format,
  #   the posts in expanded and cleaned format, and the next_link for the next set of 
  #   posts.
  
  # Set up initial cache intervals
  message("Setting up cache intervals...")
  cache_intervals.ls <- 
    seq(
      as.POSIXct(limit_timestamp, tz = timezone), 
      Sys.time(),
      cache_interval
    ) %>%
    rev()
  
  # Gather all cache file paths
  fbpage_caches.ls <- 
    list.files("../cache", full.names = TRUE)[
      str_detect(list.files("../cache"), "FBPage_.+.rds")]
  
  # Check if cached object exists and initialize variables accordingly
  if (length(fbpage_caches.ls) > 0) {
    message("Cache exists: Checking if page name match")
    # Record cache
    cache <- readRDS(sort(fbpage_caches.ls)[1])
    # Check if page_name is equal
    if (unique(cache$posts$page_name) == page_name) {
      message("Page name matches (", page_name, "): Using cache.")
      # Update cache_intervals.ls
      cache_intervals.ls <- 
        cache_intervals.ls[cache_intervals.ls < 
                             min(convertTimeStamp(cache$posts$created_time))]
      # Restore cache
      posts.ls <- cache
    } else {
      message("Page name does not match (", page_name, "): Deleting cache.")
      # Get Expanded caches as well
      fbpage_caches_expanded.ls <-
        list.files("../cache", full.names = TRUE)[
          str_detect(list.files("../cache"), "FBPageExpanded_.+.rds")]
      # Clear the cache
      file.remove(fbpage_caches.ls)
      file.remove(fbpage_caches_expanded.ls)
      # Initialize posts.ls
      posts.ls <- NULL
    }
  } else {
    message("Cache does not exist: Initializing cache")
    # If cached object does not exist, initialize posts.ls
    posts.ls <- NULL
  }
  
  # Get information and cache
  if (length(cache_intervals.ls) > 0) {
    for (interval in cache_intervals.ls) {
      interval_text <- format(as.POSIXct(interval, origin = "1970-01-01", tz = timezone), "%Y-%m-%d %z")
      message("Scraping up to ", interval_text, " then caching...")
      # Get information
      posts.ls <-
        getFBPage(
          page_name       = page_name,
          next_link       = ifelse(
            length(posts.ls) == 0, NA, 
            posts.ls$next_link
          ),
          limit_timestamp = as.POSIXct(interval, origin = "1970-01-01"),
          ...
        )
      # Cache
      saveRDS(posts.ls, paste0("../cache/FBPage_", interval_text,".rds"))
      message("Scraped up to ", interval_text, " and cached.")
    }
  }
  
  # Expand paginated content
  message("Page downloading is complete: now expanding pagination and tidying data.")
  
  # Refresh cache file paths
  fbpage_caches.ls <- 
    list.files("../cache", full.names = TRUE)[
      str_detect(list.files("../cache"), "FBPage_.+.rds")]
  
  for (index in 1:length(fbpage_caches.ls)) {
    message("Expanding pages... ", round(index/length(fbpage_caches.ls) * 100), "% complete.")
    # If posts_expanded does not exist, add it
    if (!file.exists(str_replace(fbpage_caches.ls[index], "FBPage", "FBPageExpanded"))) {
      posts_expanded.ls <-
        readRDS(fbpage_caches.ls[index]) %$% posts %>% tidyFBPageData()
      # Cache
      saveRDS(posts_expanded.ls, str_replace(fbpage_caches.ls[index], "FBPage", "FBPageExpanded"))
    }
  }
  
  # Get all expanded caches
  fbpage_caches_expanded.ls <-
    list.files("../cache", full.names = TRUE)[
      str_detect(list.files("../cache"), "FBPageExpanded_.+.rds")]
  
  # Collect all caches together and return
  fbpage.ls <- list()
  
  for (expanded_cache in fbpage_caches_expanded.ls) {
    
    message(expanded_cache)
    
    new_fbpage.ls <- readRDS(expanded_cache)
    
    fbpage.ls %>% 
      append(list(new_fbpage.ls)) ->
      fbpage.ls
    
    rm(new_fbpage.ls)
    
  }
  
  # Cleaning
  file.remove(fbpage_caches.ls)
  file.remove(fbpage_caches_expanded.ls)
  
  # Return output
  fbpage.ls
}
