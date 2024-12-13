# Application of reliability methods to CFPB dataset
library(topicmodels)
library(tm)
library(ltm)
library(lsa)
library(caret)

source('tm_reliab.R')

set.seed(3)

cfpb_dat <- read.csv('data/CFPBdataset_virtualcurrency.csv')
cfpb_txt <- cfpb_dat$Consumer.complaint.narrative
cfpb_dat <- cfpb_dat[cfpb_txt!='',]
cfpb_txt <- cfpb_txt[cfpb_txt!='']


# Randomly sample seeds
cfpb_lda_seeds <- sample(1:length(cfpb_txt),100, replace=F) 
## Preprocessing
cfpb_txt <- tolower(cfpb_txt)
cfpb_corpus <- Corpus(DataframeSource(df_title <- data.frame(doc_id=1:length(cfpb_txt),
                                                             text=cfpb_txt)))
# Remove punctuation, numbers, and stopwords
cfpb_corpus <- tm_map(cfpb_corpus, removePunctuation)
cfpb_corpus <- tm_map(cfpb_corpus, removeNumbers)
cfpb_corpus <- tm_map(cfpb_corpus, removeWords, stopwords('en'))
# Stem 
cfpb_corpus <- tm_map(cfpb_corpus, stemDocument)
# Remove excess whitespace
cfpb_corpus <- tm_map(cfpb_corpus, stripWhitespace)
cfpb_prepr_lda_processed <- DocumentTermMatrix(cfpb_corpus)

## Model: 100 topics
cfpb100_lda <- lapply(cfpb_lda_seeds, 
                      function(seed_i) LDA(cfpb_prepr_lda_processed, 
                                           k=100, 
                                           control = list(seed = seed_i)))
# Extract thetas (called gamma here referring to the variational parameter)
cfpb100_lda_theta <- lapply(1:length(cfpb100_lda), function(seed_i) cfpb100_lda[[seed_i]]@gamma)
# Extract posterior term probabilities for use in Maier method
cfpb100_lda_post_words <- lapply(cfpb100_lda, function(x) posterior(x)$terms)

# Align matrices to include only the same posterior words for comparison purposes
for (i in 1:100){
  for (j in 1:100){
    if (i!=j){
      cfpb100_lda_post_words[[i]] <- cfpb100_lda_post_words[[i]][,colnames(cfpb100_lda_post_words[[i]]) %in% colnames(cfpb100_lda_post_words[[j]])]
    }
  }
}

# Reordered by highly frequent words
for (cfpb_idx in 2:length(cfpb100_lda_theta)){
  new_top_ordering <- rep(-1,ncol(cfpb100_lda_theta[[1]]))
  max_cos_vals <- rep(-1,ncol(cfpb100_lda_theta[[1]]))
  for (top_idx in 1:ncol(cfpb100_lda_theta[[1]])){
    for (top_idx2 in 1:ncol(cfpb100_lda_theta[[1]])){
      # Compare each topic from LDA to true topics and find the most similar
      new_max <- lsa::cosine(cfpb100_lda_post_words[[cfpb_idx]][,top_idx],cfpb100_lda_post_words[[1]][,top_idx2])
      corresponding_idx <- top_idx2
      if ((new_max > max_cos_vals[top_idx])&&!(corresponding_idx %in% new_top_ordering)){
        max_cos_vals[top_idx] <- new_max
        new_top_ordering[top_idx] <- corresponding_idx
      }
    }
  }
  cfpb100_lda_theta[[cfpb_idx]] <- cfpb100_lda_theta[[cfpb_idx]][,new_top_ordering]
  cfpb100_lda_post_words[[cfpb_idx]] <- cfpb100_lda_post_words[[cfpb_idx]][new_top_ordering,]
}

cfpb100_reliab <- tm_reliab(cfpb100_lda_theta,cfpb100_lda_post_words)
strat_alph_cfpb100 <- cfpb100_reliab[[1]]
omega_cfpb100 <- cfpb100_reliab[[2]]
max_reliab_cfpb100 <- cfpb100_reliab[[3]]
comparison_cfpb100 <- cfpb100_reliab[[4]]
# Standard error
se_cfpb100 <- cfpb100_reliab$OmegaSE


## Investigation into topics across replications
# Top topics across first document
cfpb100_top_topics_doc1 <- lapply(1:length(cfpb100_lda_theta), 
                                  function(x) head(order(cfpb100_lda_theta[[x]][1,],decreasing = T),3))
# Top words across top topics for the first document
cfpb100_top_words_doc1 <- lapply(1:length(cfpb100_lda_theta), 
                                 function(x) terms(cfpb100_lda[[x]],3)[,cfpb100_top_topics_doc1[[x]]])


###########################################################################################
## Model: 50 topics
cfpb50_lda <- lapply(cfpb_lda_seeds, 
                     function(seed_i) LDA(cfpb_prepr_lda_processed, 
                                          k=50, 
                                          control = list(seed = seed_i)))
# Extract thetas (called gamma here referring to the variational parameter)
cfpb50_lda_theta <- lapply(1:length(cfpb50_lda), function(seed_i) cfpb50_lda[[seed_i]]@gamma)
# Extract posterior term probabilities for use in Maier method
cfpb50_lda_post_words <- lapply(cfpb50_lda, function(x) posterior(x)$terms)

# Align matrices to include only the same posterior words for comparison purposes
for (i in 1:100){
  for (j in 1:100){
    if (i!=j){
      cfpb50_lda_post_words[[i]] <- cfpb50_lda_post_words[[i]][,colnames(cfpb50_lda_post_words[[i]]) %in% colnames(cfpb50_lda_post_words[[j]])]
    }
  }
}

# Reordered by highly frequent words
for (cfpb_idx in 2:length(cfpb50_lda_theta)){
  new_top_ordering <- rep(-1,ncol(cfpb50_lda_theta[[1]]))
  max_cos_vals <- rep(-1,ncol(cfpb50_lda_theta[[1]]))
  for (top_idx in 1:ncol(cfpb50_lda_theta[[1]])){
    for (top_idx2 in 1:ncol(cfpb50_lda_theta[[1]])){
      # Compare each topic from LDA to true topics and find the most similar
      new_max <- lsa::cosine(cfpb50_lda_post_words[[cfpb_idx]][,top_idx],cfpb50_lda_post_words[[1]][,top_idx2])
      corresponding_idx <- top_idx2
      if ((new_max > max_cos_vals[top_idx])&&!(corresponding_idx %in% new_top_ordering)){
        max_cos_vals[top_idx] <- new_max
        new_top_ordering[top_idx] <- corresponding_idx
      }
    }
  }
  cfpb50_lda_theta[[cfpb_idx]] <- cfpb50_lda_theta[[cfpb_idx]][,new_top_ordering]
  cfpb50_lda_post_words[[cfpb_idx]] <- cfpb50_lda_post_words[[cfpb_idx]][new_top_ordering,]
}

cfpb50_reliab <- tm_reliab(cfpb50_lda_theta,cfpb50_lda_post_words)
strat_alph_cfpb50 <- cfpb50_reliab[[1]]
omega_cfpb50 <- cfpb50_reliab[[2]]
max_reliab_cfpb50 <- cfpb50_reliab[[3]]
comparison_cfpb50 <- cfpb50_reliab[[4]]
# Standard error
se_cfpb50 <- cfpb50_reliab$OmegaSE


## Investigation into topics across replications
# Top topics across first document
cfpb50_top_topics_doc1 <- lapply(1:length(cfpb50_lda_theta), 
                                 function(x) head(order(cfpb50_lda_theta[[x]][1,],decreasing = T),3))
# Top words across top topics for the first document
cfpb50_top_words_doc1 <- lapply(1:length(cfpb50_lda_theta), 
                                function(x) terms(cfpb50_lda[[x]],3)[,cfpb50_top_topics_doc1[[x]]])


###########################################################################################
## Model: 20 topics
cfpb20_lda <- lapply(cfpb_lda_seeds, 
                     function(seed_i) LDA(cfpb_prepr_lda_processed, 
                                          k=20, 
                                          control = list(seed = seed_i)))
# Extract thetas (called gamma here referring to the variational parameter)
cfpb20_lda_theta <- lapply(1:length(cfpb20_lda), function(seed_i) cfpb20_lda[[seed_i]]@gamma)
# Extract posterior term probabilities for use in Maier method
cfpb20_lda_post_words <- lapply(cfpb20_lda, function(x) posterior(x)$terms)
# Align matrices to include only the same posterior words for comparison purposes
for (i in 1:100){
  for (j in 1:100){
    if (i!=j){
      cfpb20_lda_post_words[[i]] <- cfpb20_lda_post_words[[i]][,colnames(cfpb20_lda_post_words[[i]]) %in% colnames(cfpb20_lda_post_words[[j]])]
    }
  }
}


# Reordered by highly frequent words
for (cfpb_idx in 2:length(cfpb20_lda_theta)){
  new_top_ordering <- rep(-1,ncol(cfpb20_lda_theta[[1]]))
  max_cos_vals <- rep(-1,ncol(cfpb20_lda_theta[[1]]))
  for (top_idx in 1:ncol(cfpb20_lda_theta[[1]])){
    for (top_idx2 in 1:ncol(cfpb20_lda_theta[[1]])){
      # Compare each topic from LDA to true topics and find the most similar
      new_max <- lsa::cosine(cfpb20_lda_post_words[[cfpb_idx]][,top_idx],cfpb20_lda_post_words[[1]][,top_idx2])
      corresponding_idx <- top_idx2
      if ((new_max > max_cos_vals[top_idx])&&!(corresponding_idx %in% new_top_ordering)){
        max_cos_vals[top_idx] <- new_max
        new_top_ordering[top_idx] <- corresponding_idx
      }
    }
  }
  cfpb20_lda_theta[[cfpb_idx]] <- cfpb20_lda_theta[[cfpb_idx]][,new_top_ordering]
  cfpb20_lda_post_words[[cfpb_idx]] <- cfpb20_lda_post_words[[cfpb_idx]][new_top_ordering,]
}

cfpb20_reliab <- tm_reliab(cfpb20_lda_theta,cfpb20_lda_post_words)
strat_alph_cfpb20 <- cfpb20_reliab[[1]]
omega_cfpb20 <- cfpb20_reliab[[2]]
max_reliab_cfpb20 <- cfpb20_reliab[[3]]
comparison_cfpb20 <- cfpb20_reliab[[4]]
# Standard error
se_cfpb20 <- cfpb20_reliab$OmegaSE

## Case study investigation into topics across replications
# Top topics across first document
cfpb20_top_topics_doc1 <- lapply(1:length(cfpb20_lda_theta), 
                                 function(x) head(order(cfpb20_lda_theta[[x]][1,],decreasing = T),3))
# Top words across top topics for the first document
cfpb20_top_words_doc1 <- lapply(1:length(cfpb50_lda_theta), 
                                function(x) terms(cfpb20_lda[[x]],3)[,cfpb20_top_topics_doc1[[x]]])



######################################################################################################
## Cosine similarity of top words in the full and matched cases

# 20 topics
cfpb20_cos_match_max <- lapply(1:length(cfpb20_lda_post_words), function(a) lapply(1:length(cfpb20_lda_post_words),
                        function(b) if (a!=b){max(unlist(lapply(1:n_n100,function(y) lapply(1:n_n100,
                        function(x) lsa::cosine(cfpb20_lda_post_words[[a]][y,],cfpb20_lda_post_words[[b]][x,])))))} else{NA}))
cfpb20_cos_match_max_aligned <- lapply(1:length(cfpb20_lda_post_words), function(a) lapply(1:length(cfpb20_lda_post_words),
                                function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                function(y) lsa::cosine(cfpb20_lda_post_words[[a]][y,],cfpb20_lda_post_words[[b]][y,]))))} else{NA}))
# 50 topics 
cfpb50_cos_match_max <- lapply(1:length(cfpb50_lda_post_words),function(a) lapply(1:length(cfpb50_lda_post_words),
                        function(b) if (a!=b){max(unlist(lapply(1:n_n100,function(y) lapply(1:n_n100,
                        function(x) lsa::cosine(cfpb50_lda_post_words[[a]][y,],cfpb50_lda_post_words[[b]][x,])))))} else{NA}))

cfpb50_cos_match_max_aligned <- lapply(1:length(cfpb50_lda_post_words), function(a) lapply(1:length(cfpb50_lda_post_words),
                                function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                function(y) lsa::cosine(cfpb50_lda_post_words[[a]][y,], cfpb50_lda_post_words[[b]][y,]))))} else{NA}))
# 100 topics 
cfpb100_cos_match_max <- lapply(1:length(cfpb100_lda_post_words),function(a) lapply(1:length(cfpb100_lda_post_words),
                                                                                    function(b) if (a!=b){max(unlist(lapply(1:n_n100,function(y) lapply(1:n_n100,
                                                                                                                                                        function(x) lsa::cosine(cfpb100_lda_post_words[[a]][y,],cfpb100_lda_post_words[[b]][x,])))))} else{NA}))

cfpb100_cos_match_max_aligned <- lapply(1:length(cfpb100_lda_post_words), function(a) lapply(1:length(cfpb100_lda_post_words),
                                                                                             function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                                                                                                                     function(y) lsa::cosine(cfpb100_lda_post_words[[a]][y,], cfpb100_lda_post_words[[b]][y,]))))} else{NA}))
## Cosine similarity of theta in the full and matched cases
# 20 topics
cfpb20_cos_match_theta <- lapply(1:length(cfpb20_lda_theta), function(a) lapply(1:length(cfpb20_lda_theta),
                          function(b) if (a!=b){max(unlist(lapply(1:n_n100, function(y) lapply(1:n_n100,
                          function(x) lsa::cosine(cfpb20_lda_theta[[a]][y,], cfpb20_lda_theta[[b]][x,])))))} else{NA}))
cfpb20_cos_match_theta_aligned <- lapply(1:length(cfpb20_lda_theta), function(a) lapply(1:length(cfpb20_lda_theta),
                                  function(b) if (a!=b){max(unlist(lapply(1:n_n100,
                                  function(y) lsa::cosine(cfpb20_lda_theta[[a]][y,],cfpb20_lda_theta[[b]][y,]))))} else{NA}))
# 50 topics 
cfpb50_cos_match_theta <- lapply(1:length(cfpb50_lda_theta), function(a) lapply(1:length(cfpb50_lda_theta),
                          function(b) if (a!=b){max(unlist(lapply(1:n_n100, function(y) lapply(1:n_n100,
                          function(x) lsa::cosine(cfpb50_lda_theta[[a]][y,], cfpb50_lda_theta[[b]][x,])))))} else{NA}))
cfpb50_cos_match_theta_aligned <- lapply(1:length(cfpb50_lda_theta),function(a) lapply(1:length(cfpb50_lda_theta),
                                  function(b) if (a!=b){max(unlist(lapply(1:n_n100,
                                  function(y) lsa::cosine(cfpb50_lda_theta[[a]][y,],cfpb50_lda_theta[[b]][y,]))))} else{NA}))
# 100 topics 
cfpb100_cos_match_theta <- lapply(1:length(cfpb100_lda_theta), function(a) lapply(1:length(cfpb100_lda_theta),
                                                                                  function(b) if (a!=b){max(unlist(lapply(1:n_n100, function(y) lapply(1:n_n100,
                                                                                                                                                       function(x) lsa::cosine(cfpb100_lda_theta[[a]][y,], cfpb100_lda_theta[[b]][x,])))))} else{NA}))
cfpb100_cos_match_theta_aligned <- lapply(1:length(cfpb100_lda_theta),function(a) lapply(1:length(cfpb100_lda_theta),
                                                                                         function(b) if (a!=b){max(unlist(lapply(1:n_n100,
                                                                                                                                 function(y) lsa::cosine(cfpb100_lda_theta[[a]][y,],cfpb100_lda_theta[[b]][y,]))))} else{NA}))

par(mfrow = c(2,2))
## Plots comparing across differing topic numbers
boxplot(cbind(unlist(cfpb20_cos_match_theta),unlist(cfpb50_cos_match_theta),unlist(cfpb100_cos_match_theta)),
        beside=T,col='lightblue', names=c('20','50'),
        main = 'Theta')
boxplot(cbind(unlist(cfpb20_cos_match_theta_aligned),unlist(cfpb50_cos_match_theta_aligned),
              unlist(cfpb100_cos_match_theta_aligned)),
        beside=T,col='lightblue', names=c('20','50'),
        main = 'Aligned Theta')
boxplot(cbind(unlist(cfpb20_cos_match_max),unlist(cfpb50_cos_match_max),unlist(cfpb100_cos_match_max)),
        beside=T,col='lightblue', names=c('20','50'),
        main = 'Top Words')
boxplot(cbind(unlist(cfpb20_cos_match_max_aligned),unlist(cfpb50_cos_match_max_aligned),
              unlist(cfpb100_cos_match_max_aligned)),
        beside=T,col='lightblue', names=c('20','50'),
        main = 'Aligned Top Words')


par(mfrow = c(1,1))
## Best topic match across replications
boxplot(cbind(unlist(cfpb20_cos_match_max_aligned),unlist(cfpb50_cos_match_max_aligned),
              unlist(cfpb100_cos_match_max_aligned)),
        beside=T,col='lightblue', names=c('20 Topics','50 Topics'),
        main = 'Matched Topic Vocabulary Cosine Similarity')

##############################################################################################
# Prediction of timely response using all topics
# 20 Topics
cfpb_upsamp20_dat <- lapply(1:length(cfpb20_lda_theta),
                     function(rep) caret::upSample(cfpb20_lda_theta[[rep]],
                                                   as.factor(cfpb_dat$Timely_response_numeric)))
mod_cfpb20 <- lapply(1:length(cfpb20_lda), 
                     function(rep) glm(Class ~ ., data = cfpb_upsamp20_dat[[rep]], family = "binomial"))
# Accuracy
mod_accuracy_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),
                                     function(rep) sum((as.numeric(cfpb_upsamp20_dat[[rep]]$Class)-1) - round(mod_cfpb20[[rep]]$fitted.values) == 0)/length(mod_cfpb20[[rep]]$fitted.values)))
# Word weightings by topic
# (Topic over vocabulary values times respective coefficients)
mod_weightings_cfpb20 <- lapply(1:length(cfpb20_lda), 
                                function(rep) t(cfpb20_lda_post_words[[rep]])[,-nrow(cfpb20_lda_post_words[[rep]])]%*%mod_cfpb20[[rep]]$coefficients[!(names(mod_cfpb20[[rep]]$coefficients)%in%c('(Intercept)','V20'))])
mod_word_weights_cfpb20 <- lapply(1:length(cfpb20_lda), 
                           function(rep) data.frame('Word'=rownames(data.frame(mod_weightings_cfpb20[[rep]]))[order(mod_weightings_cfpb20[[rep]],decreasing=T)],'Weight'=sort(mod_weightings_cfpb20[[rep]],decreasing=T)))
# Individual word impact investigation: Coinbase
mod_coinbase_weight_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),function(rep)mod_word_weights_cfpb20[[rep]][mod_word_weights_cfpb20[[rep]]$Word=='coinbas','Weight']))
# Total amount of word contribution for each replication
mod_tot_weight_cfpb20 <- unlist(lapply(1:length(mod_word_weights_cfpb20),function(rep) sum(mod_word_weights_cfpb20[[rep]]$Weight)))

# 50 Topics
cfpb_upsamp50_dat <- lapply(1:length(cfpb50_lda_theta),
                            function(rep) caret::upSample(cfpb50_lda_theta[[rep]],
                                                          as.factor(cfpb_dat$Timely_response_numeric)))
mod_cfpb50 <- lapply(1:length(cfpb50_lda), 
                     function(rep) glm(Class ~ ., data = cfpb_upsamp50_dat[[rep]], family = "binomial"))
# Accuracy
mod_accuracy_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),
                       function(rep) sum((as.numeric(cfpb_upsamp50_dat[[rep]]$Class)-1) - round(mod_cfpb50[[rep]]$fitted.values) == 0)/length(mod_cfpb50[[rep]]$fitted.values)))
# Word weightings by topic
# (Topic over vocabulary values times respective coefficients)
mod_weightings_cfpb50 <- lapply(1:length(cfpb50_lda), 
                                function(rep) t(cfpb50_lda_post_words[[rep]])[,-nrow(cfpb50_lda_post_words[[rep]])]%*%mod_cfpb50[[rep]]$coefficients[!(names(mod_cfpb50[[rep]]$coefficients)%in%c('(Intercept)','V50'))])
mod_word_weights_cfpb50 <- lapply(1:length(cfpb50_lda), 
                           function(rep) data.frame('Word'=rownames(data.frame(mod_weightings_cfpb50[[rep]]))[order(mod_weightings_cfpb50[[rep]],decreasing=T)],'Weight'=sort(mod_weightings_cfpb50[[rep]],decreasing=T)))
# Individual word impact investigation: Coinbase
mod_coinbase_weight_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),function(rep)mod_word_weights_cfpb50[[rep]][mod_word_weights_cfpb50[[rep]]$Word=='coinbas','Weight']))
# Total amount of word contribution for each replication
mod_tot_weight_cfpb50 <- unlist(lapply(1:length(mod_word_weights_cfpb50),function(rep) sum(mod_word_weights_cfpb50[[rep]]$Weight)))

# 100 Topics
cfpb_upsamp100_dat <- lapply(1:length(cfpb100_lda_theta),
                             function(rep) caret::upSample(cfpb100_lda_theta[[rep]],
                                                           as.factor(cfpb_dat$Timely_response_numeric)))
mod_cfpb100 <- lapply(1:length(cfpb100_lda), 
                      function(rep) glm(Class ~ ., data = cfpb_upsamp100_dat[[rep]], family = "binomial"))
# Accuracy
mod_accuracy_cfpb100 <- unlist(lapply(1:length(cfpb100_lda),
                                      function(rep) sum((as.numeric(cfpb_upsamp100_dat[[rep]]$Class)-1) - round(mod_cfpb100[[rep]]$fitted.values) == 0)/length(mod_cfpb100[[rep]]$fitted.values)))
# Word weightings by topic
# (Topic over vocabulary values times respective coefficients)
mod_weightings_cfpb100 <- lapply(1:length(cfpb100_lda), 
                                 function(rep) t(cfpb100_lda_post_words[[rep]])[,-nrow(cfpb100_lda_post_words[[rep]])]%*%mod_cfpb100[[rep]]$coefficients[!(names(mod_cfpb100[[rep]]$coefficients)%in%c('(Intercept)','V100'))])
mod_word_weights_cfpb100 <- lapply(1:length(cfpb100_lda), 
                                   function(rep) data.frame('Word'=rownames(data.frame(mod_weightings_cfpb100[[rep]]))[order(mod_weightings_cfpb100[[rep]],decreasing=T)],'Weight'=sort(mod_weightings_cfpb100[[rep]],decreasing=T)))
# Individual word impact investigation: Coinbase
mod_coinbase_weight_cfpb100 <- unlist(lapply(1:length(cfpb100_lda),function(rep)mod_word_weights_cfpb100[[rep]][mod_word_weights_cfpb100[[rep]]$Word=='coinbas','Weight']))
# Total amount of word contribution for each replication
mod_tot_weight_cfpb100 <- unlist(lapply(1:length(mod_word_weights_cfpb100),function(rep) sum(mod_word_weights_cfpb100[[rep]]$Weight)))


## Accuracy visualization
boxplot(cbind(mod_accuracy_cfpb20,mod_accuracy_cfpb50,mod_accuracy_cfpb100),
        beside=T,col='lightblue', names=c('20','50','100'),
        ylab = "Accuracy", xlab = "Topics", main = 'Prediction Accuracy Across Replications')

# Table
mod_cfpb_5numsum_df <- t(data.frame(cbind(round(fivenum(mod_accuracy_cfpb20),3),
                                    round(fivenum(mod_accuracy_cfpb50),3),
                                    round(fivenum(mod_accuracy_cfpb100),3))))
colnames(mod_cfpb_5numsum_df) <- c('Min', 'Q1', 'Median','Q3','Max')
rownames(mod_cfpb_5numsum_df) <- c('20', '50', '100')
knitr::kable(mod_cfpb_5numsum_df)

## Individual word impact investigation: Coinbase weightings by topic across replications visualization
boxplot(cbind(mod_coinbase_weight_cfpb20,mod_coinbase_weight_cfpb50,mod_coinbase_weight_cfpb100),
        beside=T,col='lightblue', names=c('20 Topics','50 Topics','100 Topics'),
        main = 'Coinbase Weighting Across Replications')

## Individual word impact investigation
# 20 Topics
## Individual word impact investigation
coinbase_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),function(rep)mod_word_weights_cfpb20[[rep]][mod_word_weights_cfpb20[[rep]]$Word=='coinbas','Weight']))
transfer_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),function(rep)mod_word_weights_cfpb20[[rep]][mod_word_weights_cfpb20[[rep]]$Word=='transfer','Weight']))
case_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),function(rep)mod_word_weights_cfpb20[[rep]][mod_word_weights_cfpb20[[rep]]$Word=='case','Weight']))

cfpb20_5numsum_df <- data.frame(rbind(round(fivenum(transfer_cfpb20),3),
                                      round(fivenum(case_cfpb20),3),
                                      round(fivenum(coinbase_cfpb20),3)))
colnames(cfpb20_5numsum_df) <- c('Min', 'Q1', 'Median','Q3','Max')
rownames(cfpb20_5numsum_df) <- c('transfer', 'case', 'coinbas')
knitr::kable(cfpb20_5numsum_df)

# 50 Topics
coinbase_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),function(rep)mod_word_weights_cfpb50[[rep]][mod_word_weights_cfpb50[[rep]]$Word=='coinbas','Weight']))
transfer_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),function(rep)mod_word_weights_cfpb50[[rep]][mod_word_weights_cfpb50[[rep]]$Word=='transfer','Weight']))
case_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),function(rep)mod_word_weights_cfpb50[[rep]][mod_word_weights_cfpb50[[rep]]$Word=='case','Weight']))

cfpb50_5numsum_df <- data.frame(rbind(round(fivenum(transfer_cfpb50),3),
                                      round(fivenum(case_cfpb50),3),
                                      round(fivenum(coinbase_cfpb50),3)))
colnames(cfpb50_5numsum_df) <- c('Min', 'Q1', 'Median','Q3','Max')
rownames(cfpb50_5numsum_df) <- c('transfer', 'case', 'coinbas')
knitr::kable(cfpb50_5numsum_df)

# 100 Topics
coinbase_cfpb100 <- unlist(lapply(1:length(cfpb100_lda),function(rep)mod_word_weights_cfpb100[[rep]][mod_word_weights_cfpb100[[rep]]$Word=='coinbas','Weight']))
transfer_cfpb100 <- unlist(lapply(1:length(cfpb100_lda),function(rep)mod_word_weights_cfpb100[[rep]][mod_word_weights_cfpb100[[rep]]$Word=='transfer','Weight']))
case_cfpb100 <- unlist(lapply(1:length(cfpb100_lda),function(rep)mod_word_weights_cfpb100[[rep]][mod_word_weights_cfpb100[[rep]]$Word=='case','Weight']))

cfpb100_5numsum_df <- data.frame(rbind(round(fivenum(transfer_cfpb100),3),
                                       round(fivenum(case_cfpb100),3),
                                       round(fivenum(coinbase_cfpb100),3)))
colnames(cfpb100_5numsum_df) <- c('Min', 'Q1', 'Median','Q3','Max')
rownames(cfpb100_5numsum_df) <- c('transfer', 'case', 'coinbas')
knitr::kable(cfpb100_5numsum_df)


## Total amount of word contribution across replications visualization
boxplot(cbind(mod_tot_weight_cfpb20,mod_tot_weight_cfpb50,mod_tot_weight_cfpb100),
        beside=T,col='lightblue', names=c('20 Topics','50 Topics','100 Topics'),
        main = 'Total Word Weighting Across Replications')


##############################################################################################
# # Prediction of timely response using the topic with the highest proportion of Coinbase 
# # Proportion of Coinbase determined using posterior vocabulary distribution
# 
# ## The 10 topics with the highest Coinbase proportions for each replication
# # 20 topics
# coinbas_top_cfpb20 <- lapply(1:length(cfpb20_lda), function(rep) head(order(unlist(lapply(1:20,  
#                       function(top) if(length(which(terms(cfpb20_lda[[rep]],3000)[,top] == 'coinbas'))!=0){ 
#                         which(terms(cfpb20_lda[[rep]],3000)[,top] == 'coinbas')}else{NA})),decreasing=T),10))
# # 50 topics
# coinbas_top_cfpb50 <- lapply(1:length(cfpb50_lda), function(rep) head(order(unlist(lapply(1:50,  
#                       function(top) if(length(which(terms(cfpb50_lda[[rep]],3000)[,top] == 'coinbas'))!=0){
#                         which(terms(cfpb50_lda[[rep]],3000)[,top] == 'coinbas')}else{NA})),decreasing=T),10))
# 
# ## Fit prediction models using a logistic regression
# # Convert timely response to a boolean for use in a logistic regression model
# cfpb_dat$Timely_response_numeric <- as.numeric(cfpb_dat$Timely.response. =='Yes')
# # Upsample the "No" responses as they are very infrequent
# set.seed(3)
# cfpb_dat_upsamp20 <- lapply(1:length(cfpb20_lda_theta),
#                      function(rep) caret::upSample(cfpb20_lda_theta[[rep]][,coinbas_top_cfpb20[[rep]]],
#                                                    as.factor(cfpb_dat$Timely_response_numeric)))
# cfpb_dat_upsamp50 <- lapply(1:length(cfpb50_lda_theta),
#                      function(rep) caret::upSample(cfpb50_lda_theta[[rep]][,coinbas_top_cfpb50[[rep]]],
#                                                    as.factor(cfpb_dat$Timely_response_numeric)))
# 
# # 20 topics
# coinbas_mod_cfpb20 <- lapply(1:length(cfpb20_lda), 
#                       function(rep) glm(Class ~ ., data = cfpb_dat_upsamp20[[rep]], family = "binomial"))
# # McFadden's Pseudo R-squared
# coinbas_pseudo_r2_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),
#                                           function(rep) 1-(coinbas_mod_cfpb20[[rep]]$deviance)/(coinbas_mod_cfpb20[[rep]]$null.deviance)))
# # Accuracy
# coinbas_mod_accuracy_cfpb20 <- unlist(lapply(1:length(cfpb20_lda),
#                                function(rep) sum((as.numeric(cfpb_dat_upsamp20[[rep]]$Class)-1) - round(coinbas_mod_cfpb20[[rep]]$fitted.values) == 0)/length(coinbas_mod_cfpb20[[rep]]$fitted.values)))
# 
# # 50 topics 
# coinbas_mod_cfpb50 <- lapply(1:length(cfpb50_lda), 
#                       function(rep) glm(Class ~ ., data = cfpb_dat_upsamp50[[rep]], family = "binomial"))
# # McFadden's Pseudo R-squared
# coinbas_pseudo_r2_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),
#                             function(rep) 1-(coinbas_mod_cfpb50[[rep]]$deviance)/(coinbas_mod_cfpb50[[rep]]$null.deviance)))
# # Accuracy
# coinbas_mod_accuracy_cfpb50 <- unlist(lapply(1:length(cfpb50_lda),
#                                function(rep) sum((as.numeric(cfpb_dat_upsamp50[[rep]]$Class)-1) - round(coinbas_mod_cfpb50[[rep]]$fitted.values) == 0)/length(coinbas_mod_cfpb50[[rep]]$fitted.values)))
# 
# ## Accuracy visualization
# boxplot(cbind(coinbas_mod_accuracy_cfpb20,coinbas_mod_accuracy_cfpb50),
#         beside=T,col='lightblue', names=c('20 Topics','50 Topics'),
#         main = 'Prediction Accuracy Across Replications')

