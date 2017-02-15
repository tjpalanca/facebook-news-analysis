# Topic Modelling Functions
# mail@tjpalanca.com
# 15 Feb 2017

# This section contains functions that assist with topic modelling.

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
    filter(type == "testing_perplexity") %>% {
      suppressWarnings({
        ggplot(data = ., aes(x = k, y = rate_perplexity_change)) +
          geom_point(data = filter(., fold != 0), color = "darkgray") +
          geom_path(data = filter(., fold == 0), color = "forestgreen", lwd = 1) 
      })
    }
}