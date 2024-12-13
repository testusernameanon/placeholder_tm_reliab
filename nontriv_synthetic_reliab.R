# Topic Reliability Method Comparison for Nontrivial Synthetic Data
# 100 topics from data with 200 true underlying topics
library(topicmodels)
library(tm)
library(ltm)
library(lsa)

source('tm_reliab.R')

set.seed(1)

dat <- read.csv("data/lda.1-1seed.128len.0.1-0.2delta.0.6-0.5tau.0.csv")
txt <- dat$text

topic_num_n100 <- 100

lda_seeds_n100 <- sample(1:10000,10, replace=F) 

# Preprocessing: Create DTM
prepr_lda_processed <- DocumentTermMatrix(VCorpus(VectorSource(txt)))
# Model
lda_full_n100 <- lapply(lda_seeds_n100, function(seed_i) LDA(prepr_lda_processed, k=topic_num_n100,
                                                             control = list(seed = seed_i)))
# Extract thetas (called gamma here referring to the variational parameter)
lda_theta_n100 <- lapply(1:length(lda_full_n100), function(seed_i) lda_full_n100[[seed_i]]@gamma)
# Extract term probabilities by topic
lda_post_words_n100 <- lapply(lda_full_n100, function(x) posterior(x)$terms)

for (lda_idx in 2:length(lda_theta_n100)){
  new_top_ordering <- rep(-1,topic_num_n100)
  max_cos_vals <- rep(-1,topic_num_n100)
  for (top_idx in 1:topic_num_n100){
    for (top_idx2 in 1:topic_num_n100){
      # Compare each topic from LDA to true topics and find the most similar
      new_max <- lsa::cosine(lda_post_words_n100[[lda_idx]][,top_idx],lda_post_words_n100[[1]][,top_idx2])
      corresponding_idx <- top_idx2
      if ((new_max > max_cos_vals[top_idx])&&!(corresponding_idx %in% new_top_ordering)){
        max_cos_vals[top_idx] <- new_max
        new_top_ordering[top_idx] <- corresponding_idx
      }
    }
  }
  lda_theta_n100[[lda_idx]] <- lda_theta_n100[[lda_idx]][,new_top_ordering]
  lda_post_words_n100[[lda_idx]] <- lda_post_words_n100[[lda_idx]][new_top_ordering,]
}

lda_n100_reliab <- tm_reliab(lda_theta_n100,lda_post_words_n100)
strat_alph_n100 <- lda_n100_reliab[[1]]
lda_omega_n100 <- lda_n100_reliab[[2]]
max_reliab_n100 <- lda_n100_reliab[[3]]
lda_cos_sim_n100 <- lda_n100_reliab[[4]]
# Standard error
se_strat_alph_n100 <- lda_n100_reliab$StratAlphSE
se_omega_n100 <- lda_n100_reliab$OmegaSE


########################################################################################################
# Difference of replication 1 from other replications
lda_diff_bytopic_n100 <- lapply(2:length(lda_theta_n100), 
                                function(x) colMeans(abs(lda_theta_n100[[1]]-lda_theta_n100[[x]])))
lda_diff_combined_n100 <- lapply(1:length(lda_diff_bytopic_n100), 
                                 function(x) mean(lda_diff_bytopic_n100[[x]]))

########################################################################################################
# Detriment of the hard cutoff

n100_cos_match_max <- lapply(1:length(lda_post_words_n100), 
                             function(a) lapply(1:length(lda_post_words_n100), 
                                                function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                                                                        function(y) lapply(1:n_n100,
                                                                                                           function(x) lsa::cosine(lda_post_words_n100[[a]][y,],
                                                                                                                                   lda_post_words_n100[[b]][x,])))))} else{NA}))

n100_cos_match_max_aligned <- lapply(1:length(lda_post_words_n100), 
                                     function(a) lapply(1:length(lda_post_words_n100), 
                                                        function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                                                                                function(y) lsa::cosine(lda_post_words_n100[[a]][y,],
                                                                                                                        lda_post_words_n100[[b]][y,]))))} else{NA}))

# Plot the distribution of matches with aligned matches for posterior words
boxplot(cbind(unlist(n100_cos_match_max),unlist(n100_cos_match_max_aligned)),
        beside=T,col='lightblue', names=c('Full Similarity','Matched Similarity'),
        main = 'Top Words Cosine Similarity')
abline(h=0.7,lty=1,col='red')


## Visualization including theta distribution's implications as well
n100_cos_theta_max <- lapply(1:length(lda_theta_n100), 
                             function(a) lapply(1:length(lda_theta_n100), 
                                                function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                                                                        function(y) lapply(1:n_n100,
                                                                                                           function(x) lsa::cosine(lda_theta_n100[[a]][,y],
                                                                                                                                   lda_theta_n100[[b]][,x])))))} else{NA}))

n100_cos_theta_max_aligned <- lapply(1:length(lda_theta_n100), 
                                     function(a) lapply(1:length(lda_theta_n100), 
                                                        function(b) if (a!=b){max(unlist(lapply(1:n_n100, 
                                                                                                function(y) lsa::cosine(lda_theta_n100[[a]][,y],
                                                                                                                        lda_theta_n100[[b]][,y]))))} else{NA}))


library(ggplot2)
n100_visualization <- data.frame('Method' = c(rep(c(rep('Full',100),rep('Matched',100)),2)), 
                                 'type' = c(rep('Top Words',200),rep('Docs over Topics',200)),
                                 'Similarity' = c(unlist(n100_cos_match_max),
                                                  unlist(n100_cos_match_max_aligned),
                                                  unlist(n100_cos_theta_max),
                                                  unlist(n100_cos_theta_max_aligned)))
# png('new_method_comparison.png')
ggplot(n100_visualization, aes(x = Method, y = Similarity)) +
  geom_boxplot(fill='lightblue') +
  facet_wrap(~ type, scales = "free") + #"free_x") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),panel.background=element_rect(colour="black"),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold")) +
  ggtitle("Cosine Similarity Comparison")  +
  geom_hline(yintercept=0.7, linetype="dashed", color = "red")
# dev.off()


