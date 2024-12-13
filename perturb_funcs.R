# Functions to obtain perturbations for STM and LDA
library(topicmodels)
library(lsa)

# STM perturbation
run_stm_perturb <- function(words_to_remove, data){
  processed_data <- lapply(data, 
                           function(txt) textProcessor(txt, 
                                                       metadata=data.frame(txt,dat$y)))
  data_output <- lapply(processed_data, 
                        function(step1) prepDocuments(step1$documents, 
                                                      step1$vocab, 
                                                      step1$meta))
  model_stm <- lapply(data_output, 
                      function(prepped) stm(prepped$documents, prepped$vocab, 
                                            K = 100, data = prepped$meta,
                                            init.type = "Spectral"))
  model_stm_theta <- lapply(model_stm, function(x) x$theta)
  # Extract term probabilities by topic
  # This is the beta in stm terms
  model_stm_post_words <- lapply(model_stm, function(stm_obj) stm_obj$beta[[1]][[1]])
  # Add corresponding term labels 
  for (idx in 1:length(model_stm_post_words)){
    colnames(model_stm_post_words[[idx]]) <- model_stm[[idx]]$vocab
  }
  # Align by vocabulary
  for (i in 1:100){
    for (j in 1:100){
      if (i!=j){
        model_stm_post_words[[i]] <- model_stm_post_words[[i]][,colnames(model_stm_post_words[[i]]) %in% colnames(model_stm_post_words[[j]])]
      }
    }
  }
  # Reordered by highly frequent words
  for (stm_lda_idx in 2:length(model_stm_theta)){
    new_top_ordering <- rep(-1,ncol(model_stm_theta[[1]]))
    max_cos_vals <- rep(-1,ncol(model_stm_theta[[1]]))
    for (top_idx in 1:ncol(model_stm_theta[[1]])){
      for (top_idx2 in 1:ncol(model_stm_theta[[1]])){
        # Compare each topic from stm_lda to true topics and find the most similar
        new_max <- cosine(model_stm_post_words[[stm_lda_idx]][,top_idx],model_stm_post_words[[1]][,top_idx2])
        corresponding_idx <- top_idx2
        if ((new_max > max_cos_vals[top_idx])&&!(corresponding_idx %in% new_top_ordering)){
          max_cos_vals[top_idx] <- new_max
          new_top_ordering[top_idx] <- corresponding_idx
        }
      }
    }
    model_stm_theta[[stm_lda_idx]] <- model_stm_theta[[stm_lda_idx]][,new_top_ordering]
    model_stm_post_words[[stm_lda_idx]] <- model_stm_post_words[[stm_lda_idx]][new_top_ordering,]
  }
  
  return(list(tm_reliab(model_stm_theta,model_stm_post_words),model_stm))
}

# LDA perturbation
run_lda_perturb <- function(words_to_remove, data){
  prepr_data <- lapply(data,
                       function(dat_txt) DocumentTermMatrix(VCorpus(VectorSource(dat_txt))))
  # Remove words that make the documents empty strings
  # We use just the 1st random seed as we are now varying the corpus
  model_lda <- lapply(prepr_data,
                      function(proc_txt) LDA(proc_txt, k=100, 
                                             control = list(seed = lda_seeds[1])))
  model_lda_theta <- lapply(model_lda, function(x) x@gamma)
  # Extract term probabilities by topic
  model_lda_post_words <- lapply(model_lda, function(x) posterior(x)$terms)
  # Align matrices to include only the same posterior words for comparison purposes
  for (i in 1:100){
    for (j in 1:100){
      if (i!=j){
        model_lda_post_words[[i]] <- model_lda_post_words[[i]][,colnames(model_lda_post_words[[i]]) %in% colnames(model_lda_post_words[[j]])]
      }
    }
  }
  
  # Reordered by highly frequent words
  for (lda_idx in 2:length(model_lda_theta)){
    new_top_ordering <- rep(-1,ncol(model_lda_theta[[1]]))
    max_cos_vals <- rep(-1,ncol(model_lda_theta[[1]]))
    for (top_idx in 1:ncol(model_lda_theta[[1]])){
      for (top_idx2 in 1:ncol(model_lda_theta[[1]])){
        # Compare each topic from LDA to true topics and find the most similar
        new_max <- cosine(model_lda_post_words[[lda_idx]][,top_idx],model_lda_post_words[[1]][,top_idx2])
        corresponding_idx <- top_idx2
        if ((new_max > max_cos_vals[top_idx])&&!(corresponding_idx %in% new_top_ordering)){
          max_cos_vals[top_idx] <- new_max
          new_top_ordering[top_idx] <- corresponding_idx
        }
      }
    }
    model_lda_theta[[lda_idx]] <- model_lda_theta[[lda_idx]][,new_top_ordering]
    model_lda_post_words[[lda_idx]] <- model_lda_post_words[[lda_idx]][new_top_ordering,]
  }
  
  return(list(tm_reliab(model_lda_theta,model_lda_post_words),model_lda))
}


