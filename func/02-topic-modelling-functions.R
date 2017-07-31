# Topic Modelling Functions
# mail@tjpalanca.com
# 15 Feb 2017

# This section contains functions that assist with topic modelling.

stemWords <- function(df, patterns, lang) {
  # Applies a simple, conservative stemming algorithm
  #
  # Args:
  #   df:       data frame containing words (`word`)
  #   patterns: list of regex expressions containing the prefixes, suffixes, and mid-fixes
  #   lang:     en_US or tl_PH, depending on the language in question
  # 
  # Returns:
  #   A unique mapping between the identified words and the suggested stem
  
  df %>% 
    # Get all distinct words
    distinct(word) %>%
    # Filter to language
    filter(hunspell_check(word, lang)) %>% 
    # Remove words too short for any modification
    filter(str_length(word) > 3) %>% 
    # Generate pattern matches
    mutate(
      pattern = 
        map(
          word,
          function(word, patterns) patterns[map_lgl(patterns, ~str_detect(word, .))],
          patterns = patterns
        )
    ) %>% 
    unnest(pattern) %>% 
    # Remove pattern from word
    mutate(stem_word = str_replace(word, pattern, "")) %>%
    # Add pattern exceptions if no words pass spell check
    mutate(
      stem_word = 
        map(
          stem_word,
          function(stem_word) {
            append(
              stem_word,
              c(
                # ENGLISH
                ifelse(
                  lang == "en_US",
                  paste0(stem_word, "e"), NA
                ),
                ifelse(
                  lang == "en_US" & 
                    str_detect(stem_word, "i$"), 
                  stem_word %>% str_replace("i$", "y"), NA
                ),
                ifelse(
                  lang == "en_US" &
                    str_sub(stem_word, -1, -1) == str_sub(stem_word, -2, -2),
                  str_sub(stem_word, 1, -2), NA
                ),
                # TAGALOG
                ifelse(
                  lang == "tl_PH",
                  str_replace_all(stem_word, "u", "o"), NA
                ),
                ifelse(
                  lang == "tl_PH",
                  str_replace(stem_word, "r$", "d$"), NA
                )
              ) %>% na.omit()
            )
          }
        )
    ) %>% 
    # Get only words that pass spell check
    mutate(stem_word = map(stem_word, ~.[hunspell_check(., lang)])) %>%
    # Get only words that are long enough
    mutate(stem_word = map(stem_word, ~.[str_length(.) >= 3])) %>%
    # Remove stem_word_pass
    # Combine up the canididate stem words per word
    group_by(word) %>%
    summarise(stem_words = list(unique(unlist(stem_word)))) %>%
    # Get final stem word by minimum Levenshtein distance
    mutate(
      stem_word =
        map2_chr(
          stem_words,
          word,
          function(stem_words, word) {
            if (length(stem_words) == 0) return(NA)
            # Rank by Levenshtein distance
            distances  <- stringdist(stem_words, word)
            stem_words <- stem_words[distances == min(distances, na.rm = TRUE)]
            stem_words[[1]]
          }
        )
    ) %>% 
    select(-stem_words) %>% 
    filter(!is.na(stem_word)) %>% 
    # Manual adjustments to Tagalog words %>% 
    mutate(
      stem_word = map_chr(
        stem_word,
        function(stem_word) {
          ifelse(
            lang != "tl_PH",
            stem_word,
            ifelse(
              str_sub(stem_word, 1, 1) == str_sub(stem_word, 2, 2),
              str_sub(stem_word, 2, -1),
              ifelse(
                str_sub(stem_word, 1, 2) == str_sub(stem_word, 3, 4),
                str_sub(stem_word, 3, -1), stem_word
              )
            )
          )
        }
      )
      
    )
  
}

crossValidatePerplexity <- function(dtm, n_folds, k_candidates, method) {
  # Performs `n_folds`-fold cross validation on the LDA perplexity for different k_candidates
  # 
  # Args:
  #   dtm:          The document term matrix on which LDA is to be performaed
  #   n_folds:      Number of folds to be split per k candidate
  #   k_candidates: The candidate k values on which perplexity will be computed
  #   method:       Use either Variational EM "VEM" or Gibbs Sampling "Gibbs"
  #
  # Returns:
  #   data_frame containing the cross validation results, including original training and 
  #   testing DTMs, and the perplexity on training and testing.
  set.seed(7292)
  
  # Get DTM splits
  dtm_split <- sample(1:n_folds, dtm$nrow, replace = TRUE) 
  
  # Register parallel backend
  registerDoMC(cores = detectCores() - 1)
  
  results <- 
    foreach(k_index = 1:length(k_candidates), .combine = 'rbind') %dopar% {
      # Set k parameter
      k.prm <- k_candidates[k_index]
      cat("K =", k.prm, ":")
      map_df(
        1:n_folds,
        function(folds_index) {
          cat("", folds_index, "")
          # Split dataset
          training.dtm <- dtm[dtm_split != folds_index, ]
          testing.dtm  <- dtm[dtm_split == folds_index, ]
          
          # Train LDA on training set
          training.lda <- LDA(
            x       = training.dtm,
            k       = k.prm,
            method  = method,
            control = list(seed = 7292)
          )
          
          # Compute perplexity on test set and return results
          data_frame(
            k            = k.prm,
            fold         = folds_index,
            training_dtm = list(training.dtm),
            testing_dtm  = list(testing.dtm),
            training_lda = list(training.lda),
            training_perplexity = perplexity(training.lda, training.dtm),
            testing_perplexity  = perplexity(training.lda, testing.dtm)
          )
        }
      ) %T>% {
        cat("\n")
      }
    }
  
  # Remove parallel facilities
  registerDoSEQ()
  
  # Return results
  return(results)
}

plotCrossValidatedPerplexity <- function(df) {
  # Given the dataframe of cross validation results, plot the rate of 
  # perplexity change to be used in determining the ideal number of topics.
  #
  # Args:
  #   df:     data frame produced by crossValidatePreplexity
  # 
  # Returns:
  #   ggplot of the RPC change
  df %>% 
    gather(type, perplexity, -k, -fold, -training_dtm, -testing_dtm, -training_lda) %>% {
      bind_rows(
        group_by(., fold, type) %>% 
          arrange(fold, type, k) %>% 
          mutate(
            perplexity_change = perplexity - lag(perplexity),
            topic_number_change = k - lag(k),
            rate_perplexity_change = abs(perplexity_change/topic_number_change)
          ) %>% 
          ungroup(),
        group_by(., k, type) %>% 
          summarise(perplexity = mean(perplexity)) %>% 
          mutate(
            fold = 0,
            training_dtm = NA,
            testing_dtm = NA,
            training_lda = NA
          ) %>% 
          ungroup() %>% 
          group_by(type) %>% 
          arrange(type, k) %>% 
          mutate(
            perplexity_change = perplexity - lag(perplexity),
            topic_number_change = k - lag(k),
            rate_perplexity_change = abs(perplexity_change/topic_number_change)
          ) %>% 
          ungroup()
      )
    } %>% 
    filter(type == "testing_perplexity", !is.na(perplexity_change)) %>% {
      suppressWarnings({
        ggplot(data = ., aes(x = k, y = rate_perplexity_change)) +
          geom_point(data = filter(., fold != 0), color = "darkgray") +
          geom_path(data = filter(., fold == 0), color = "forestgreen", lwd = 1) 
      })
    }
}

computeJensenShannonPCA <- function(wtp) {
  # Adapted from https://github.com/cpsievert/LDAvis/blob/master/R/createJSON.R
  # 
  # Returns coordinates for plotting topics with Jensen Shannon distance as 
  # the distance measure, projected down to 2 dimensions using principal 
  # coordinates analysis.
  #
  # Args:    wtp:  word-topic probability matrix
  # Returns: data frame containing the x-y location of each topic 
  
  jensenShannon <- function(x, y) {
    m <- 0.5 * (x + y)
    0.5 * sum(x * log(x / m)) + 0.5 * sum(y * log(y / m))
  }
  
  wtp %>% 
    spread(term, beta) %>% 
    { .[, 2:ncol(.)] } %>%
    dist(x = ., method = jensenShannon) %>% 
    cmdscale(k = 2) %>% {
      data.frame(
        topic = 1:n_distinct(wtp$topic),
        x = .[, 1],
        y = .[, 2]
      )
    }
}

computeJensenShannonPCA2 <- function(dtp) {
  # Adapted from https://github.com/cpsievert/LDAvis/blob/master/R/createJSON.R
  # 
  # Returns coordinates for plotting topics with Jensen Shannon distance as 
  # the distance measure, projected down to 2 dimensions using principal 
  # coordinates analysis.
  #
  # Args:    dtp:  document-topic probability matrix
  # Returns: data frame containing the x-y location of each topic 
  
  jensenShannon <- function(x, y) {
    m <- 0.5 * (x + y)
    0.5 * sum(x * log(x / m)) + 0.5 * sum(y * log(y / m))
  }
  
  dtp %>% 
    { .[, 2:ncol(.)] } %>%
    dist(x = ., method = jensenShannon) %>% 
    cmdscale(k = 2) %>% {
      data.frame(
        document = unique(dtp$document),
        x = .[, 1],
        y = .[, 2]
      )
    }
}