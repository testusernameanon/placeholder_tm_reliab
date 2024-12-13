# Trivial Synthetic Data Reliability Method Comparison
# 2 true underlying topics

library(topicmodels)
library(tm)
library(ltm)
library(lsa) 
library(psych)

source('tm_reliab.R')

set.seed(1)

# Trivial Synthetic Data
triv_dat <- read.csv("data/trivial.1-1seed.16len.0.0-0.7delta.0.0-0.84tau.0.csv")
triv_txt <- triv_dat$text

# Run LDA and STM for the true number of topics (K=2)
# LDA (using topicmodels package since this is most widely used)
# (fast version with collapsed Gibbs sampler is an option)
# Randomly sample seeds
triv_lda_seeds <- sample(1:10000,10, replace=F) 
# Preprocessing: Create DTM
triv_prepr_lda_processed <- DocumentTermMatrix(VCorpus(VectorSource(triv_txt)))
# Model
triv_lda <- lapply(triv_lda_seeds, 
                   function(seed_i) LDA(triv_prepr_lda_processed, k=2, control = list(seed = seed_i)))

# Extract thetas (called gamma here referring to the variational parameter)
triv_lda_theta <- lapply(1:length(triv_lda_seeds), function(seed_i) triv_lda[[seed_i]]@gamma)
# Extract posterior term probabilities for use in Maier method
triv_lda_post_words <- lapply(triv_lda, function(x) posterior(x)$terms)

# Columns reordered to align topics
triv_lda_theta_diffs <- lapply(2:length(triv_lda_seeds), 
                               function(x) colMeans(abs(triv_lda_theta[[1]] - triv_lda_theta[[x]][,1])))
triv_lda_theta[2:10] <- lapply(2:length(triv_lda_seeds),
                               function(x) if (which.min(triv_lda_theta_diffs[[x-1]])!=1){triv_lda_theta[[x]][,c(2,1)]}
                               else{triv_lda_theta[[x]]}) 
triv_lda_post_words[2:10] <- lapply(2:length(triv_lda_seeds),
                                    function(x) if (which.min(triv_lda_theta_diffs[[x-1]])!=1){triv_lda_post_words[[x]][c(2,1),]}
                                    else{triv_lda_post_words[[x]]}) 

# Difference in thetas
# Compare just mean, not column means, since each column has the same information with only two columns
# Compare recovery of true underlying distribution
true_triv <- round(triv_lda_theta[[1]]) # Ground truth 
triv_lda_true_error_full <- lapply(1:length(triv_lda_theta), function(x) mean(abs(true_triv-triv_lda_theta[[x]])))
triv_lda_true_error <- unlist(triv_lda_true_error_full)

# LDA comparison
# Compare just 1 topic from each since we know that the other topic has the same information
triv_lda_theta <- lapply(triv_lda_theta, function(x) x[,c(T,F)])
triv_lda_post_words_alt <- lapply(triv_lda_post_words, function(x) x[c(F,T),])
triv_lda_post_words <- lapply(triv_lda_post_words, function(x) x[c(T,F),])


# Compute standard practice reliability method from Maier et al., 2018 
# Matched pair: 2 topics with cosine similarity of their top-word probabilities at a maximum & above 0.7
# Proportion of topic matches from models i and j over all K topics was is a reliability score 
triv_lda_cos_sim_sum <- 0
triv_lda_cos_sim_tot <- 0
for (i in 1:length(triv_lda_post_words)){
  for (j in 1:length(triv_lda_post_words)){
    if (i!=j){
      triv_lda_cos_sim_tot = triv_lda_cos_sim_tot + 1
      if (lsa::cosine(triv_lda_post_words[[i]],triv_lda_post_words[[j]])>=0.7){
        triv_lda_cos_sim_sum = triv_lda_cos_sim_sum + 1
      }
    }
  }
}
triv_lda_cos_sim <- triv_lda_cos_sim_sum/triv_lda_cos_sim_tot

# Cronbach's Alpha 
triv_lda_cron_alph1 <- cronbach.alpha(data.frame(do.call("cbind", 
                                                         triv_lda_theta)),CI=T)
triv_lda_cron_alph2 <- cronbach.alpha(data.frame(do.call("cbind", 
                                                         triv_lda_post_words)),CI=T)
triv_lda_alph_cron_alph1 <- triv_lda_cron_alph1$alpha
triv_lda_alph_cron_alph2 <- triv_lda_cron_alph2$alpha
triv_lda_cron_alph <- (triv_lda_alph_cron_alph1+triv_lda_alph_cron_alph2)/2
# Standard error
triv_lda_sd_cron_alph <- mean(c(as.vector(triv_lda_cron_alph1$ci[2] - 
                                            triv_lda_alph_cron_alph1)/1.96,
                                as.vector(triv_lda_cron_alph2$ci[2] - 
                                            triv_lda_alph_cron_alph2)/1.96))
triv_lda_se_cron_alph <- triv_lda_sd_cron_alph*sqrt(1/length(triv_lda_theta) + 
                                                      1/length(triv_lda_post_words))

# Mcdonald's Omega
triv_omega1 <- xpectr::suppress_mw(omega(data.frame(do.call("cbind", triv_lda_theta)),
                                         nfactors=1))
triv_omega2 <- xpectr::suppress_mw(omega(data.frame(do.call("cbind", triv_lda_post_words)),
                                         nfactors=1))
triv_omega <- (triv_omega1$omega.tot+triv_omega2$omega.tot)/2
# Standard error
triv_omega_se <- mean(c(triv_omega1$stats$sd, 
                        triv_omega2$stats$sd))*sqrt(1/length(triv_lda_theta) + 
                                                      1/length(triv_lda_post_words))

# Unidimensional Maximal Reliability (Spearman-Brown Reliability)
triv_max_reliability_r <- lapply(1:length(triv_lda_theta),
                                 function(x) lapply(1:length(triv_lda_theta),
                                                    function(y) if (x!=y){
                                                      lsa::cosine(triv_lda_theta[[x]], triv_lda_theta[[y]])}))
triv_max_reliability_r <- mean(unlist(triv_max_reliability_r))
triv_max_reliability_r2 <- lapply(1:length(triv_lda_theta), 
                                  function(x) lapply(1:length(triv_lda_theta),
                                                     function(y) if (x!=y){
                                                       lsa::cosine(triv_lda_post_words[[x]], 
                                                                   triv_lda_post_words[[y]])}))
triv_max_reliability_r2 <- mean(unlist(triv_max_reliability_r2))
triv_max_reliability_r_full <- rowMeans(cbind(triv_max_reliability_r,triv_max_reliability_r2))
triv_n <- length(triv_lda_theta)
triv_max_reliability <- (triv_n*triv_max_reliability_r_full)/(1+(triv_n-1)*triv_max_reliability_r_full)
# Standard error
triv_max_reliab_se <- (triv_n*(1-triv_max_reliability_r_full^2))/((1+(triv_n-1)*triv_max_reliability_r_full)^2)
triv_max_reliab_se <- triv_max_reliab_se*sqrt(1/length(triv_lda_theta) + 
                                                1/length(triv_lda_post_words))

#########################Issue with Standard Practice #####################################################
# Maier et al., 2018 does not pick up on issue in reliability from replication 6

# Maier et al., 2018 reliability
# Topic 1
triv_lda_cos_sim_sum <- 0
triv_lda_cos_sim_tot <- 0
for (i in 5:6){
  for (j in 5:6){
    
    if (i!=j){
      triv_lda_cos_sim_tot = triv_lda_cos_sim_tot + 1
      if (cosine(triv_lda_post_words[[i]],triv_lda_post_words[[j]])>=0.7){
        triv_lda_cos_sim_sum = triv_lda_cos_sim_sum + 1
      }
    }
  }
}
triv_lda_cos_sim <- triv_lda_cos_sim_sum/triv_lda_cos_sim_tot

# Topic 2
triv_lda_cos_sim_sum <- 0
triv_lda_cos_sim_tot <- 0
for (i in 5:6){
  for (j in 5:6){
    
    if (i!=j){
      triv_lda_cos_sim_tot = triv_lda_cos_sim_tot + 1
      if (cosine(triv_lda_post_words_alt[[i]],triv_lda_post_words_alt[[j]])>=0.7){
        triv_lda_cos_sim_sum = triv_lda_cos_sim_sum + 1
      }
    }
  }
}
triv_lda_cos_sim <- triv_lda_cos_sim_sum/triv_lda_cos_sim_tot

# Cronbach's alpha 
triv_lda_cron_alph1 <- cronbach.alpha(data.frame(do.call("cbind", 
                                                         triv_lda_theta[5:6])),CI=T)
triv_lda_cron_alph2 <- cronbach.alpha(data.frame(do.call("cbind", 
                                                         triv_lda_post_words[5:6])),CI=T)
triv_lda_alph_cron_alph1 <- triv_lda_cron_alph1$alpha
triv_lda_alph_cron_alph2 <- triv_lda_cron_alph2$alpha
triv_lda_cron_alph <- (triv_lda_alph_cron_alph1+triv_lda_alph_cron_alph2)/2
# Standard error
triv_lda_sd_cron_alph <- mean(c(as.vector(triv_lda_cron_alph1$ci[2] - 
                                            triv_lda_alph_cron_alph1)/1.96,
                                as.vector(triv_lda_cron_alph2$ci[2] - 
                                            triv_lda_alph_cron_alph2)/1.96))
triv_lda_se_cron_alph <- triv_lda_sd_cron_alph*sqrt(1/length(triv_lda_theta) + 
                                                      1/length(triv_lda_post_words))


# McDonald's Omega
triv_omega1 <- xpectr::suppress_mw(omega(data.frame(do.call("cbind", triv_lda_theta[5:6])),
                                         nfactors=1))
triv_omega2 <- xpectr::suppress_mw(omega(data.frame(do.call("cbind", triv_lda_post_words[5:6])),
                                         nfactors=1))
triv_omega <- (triv_omega1$omega.tot+triv_omega2$omega.tot)/2
# Standard error
triv_omega_se <- mean(c(triv_omega1$stats$sd, 
                        triv_omega2$stats$sd))*sqrt(1/length(triv_lda_theta) + 
                                                      1/length(triv_lda_post_words))

# Unidimensional maximal reliability (item reliabilty)
triv_max_reliability_r <- lapply(5:6, 
                                 function(x) lapply(5:6,
                                                    function(y) if (x!=y){
                                                      lsa::cosine(triv_lda_theta[[x]], triv_lda_theta[[y]])}))
triv_max_reliability_r <- mean(unlist(triv_max_reliability_r))
triv_max_reliability_r2 <- lapply(5:6, 
                                  function(x) lapply(5:6,
                                                     function(y) if (x!=y){
                                                       lsa::cosine(triv_lda_post_words[[x]], 
                                                                   triv_lda_post_words[[y]])}))
triv_max_reliability_r2 <- mean(unlist(triv_max_reliability_r2))
triv_max_reliability_r_full <- rowMeans(cbind(triv_max_reliability_r,triv_max_reliability_r2))
triv_n <- length(triv_lda_theta[5:6])
triv_max_reliability <- (triv_n*triv_max_reliability_r_full)/(1+(triv_n-1)*triv_max_reliability_r_full)
# Standard error
triv_max_reliab_se <- (triv_n*(1-triv_max_reliability_r_full^2))/((1+(triv_n-1)*triv_max_reliability_r_full)^2)
triv_max_reliab_se <- triv_max_reliab_se*sqrt(1/length(triv_lda_theta) + 
                                                1/length(triv_lda_post_words))

#############################################

# Trivial data quality matches: comparing replication 6 to other replications (which are all identical)

# Top words
triv_rep6_post_words_top1 <- triv_lda_post_words[[6]]
triv_rep6_post_words_top2 <- posterior(triv_lda[[6]])$terms[1,]
triv_rep5_post_words_top1 <- triv_lda_post_words[[5]]
triv_rep5_post_words_top2 <- posterior(triv_lda[[5]])$terms[2,]

triv_rep6_top1_selection <- triv_rep6_post_words_top1 > triv_rep6_post_words_top2
triv_rep5_top1_selection <- triv_rep5_post_words_top1 > triv_rep5_post_words_top2
sum(triv_rep5_top1_selection != triv_rep6_top1_selection)

# Documents over topics
triv_rep6_theta_top1 <- triv_lda_theta[[6]] > 0.5
triv_rep5_theta_top1 <- triv_lda_theta[[5]] > 0.5
sum(triv_rep6_theta_top1 != triv_rep5_theta_top1)

