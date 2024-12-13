# Multidimensional reliability for topic models
# Returns values for stratified alpha, multidimensional omega, maximal reliability, and the standard practice
library(ltm)
library(lsa)
library(psych)

tm_reliab <- function(theta, post_words){
  
  ## Cronbach's alpha stratified coefficient
  # Average inter-item covariance among the items
  by_col <- lapply(1:(ncol(theta[[1]])-1), 
                   function(y) lapply(1:length(theta), 
                                      function(x) theta[[x]][,y]))
  post_words_by_col <- lapply(1:(ncol(theta[[1]])-1), 
                              function(y) lapply(1:length(post_words),
                                                 function(x) post_words[[x]][y,]))
  i1_sig <- lapply(1:length(by_col),
                   function(x) cov(as.data.frame(by_col[[x]])))
  i1_sig <- lapply(1:length(i1_sig),
                   function(x) i1_sig[[x]][row(i1_sig[[x]]) - col(i1_sig[[x]]) <= - 1])
  i1_sig <- unlist(lapply(1:length(i1_sig), function(x) mean(i1_sig[[x]])^2))
  i2_sig <- lapply(1:length(post_words_by_col),
                   function(x) cov(as.data.frame(post_words_by_col[[x]])))
  i2_sig <- lapply(1:length(i2_sig),
                   function(x) i2_sig[[x]][row(i2_sig[[x]]) - col(i2_sig[[x]]) <= - 1])
  i2_sig <- unlist(lapply(1:length(i2_sig), function(x) mean(i2_sig[[x]])^2))
  i_sig <- (i1_sig+i2_sig)/2

  tot_sig <- mean(i_sig)
  # cron_alph1 <- lapply(1:length(by_col),
  #                      function(x) cronbach.alpha(data.frame(do.call("cbind",
  #                                                                    by_col[[x]])))$alpha)
  # cron_alph1 <- unlist(cron_alph1)
  # cron_alph2 <- lapply(1:length(post_words_by_col),
  #                      function(x) cronbach.alpha(data.frame(do.call("cbind",
  #                                                                    post_words_by_col[[x]])))$alpha)
  # cron_alph2 <- unlist(cron_alph2)
  # i_alph <- (cron_alph1 + cron_alph2)/2
  # strat_alph_val <- 1 - sum((i_sig)*(1-i_alph))/(tot_sig)
  cron_alph1_vals <- lapply(1:length(by_col),
                       function(x) cronbach.alpha(data.frame(do.call("cbind",
                                                                     by_col[[x]])),
                                                  CI=T))
  cron_alph1 <- unlist(lapply(1:length(by_col), function(x) cron_alph1_vals[[x]]$alpha))
  cron_alph2_vals <- lapply(1:length(post_words_by_col),
                            function(x) cronbach.alpha(data.frame(do.call("cbind",
                                                                          post_words_by_col[[x]])),
                                                       CI=T))
  cron_alph2 <- unlist(lapply(1:length(post_words_by_col),
                              function(x) cron_alph2_vals[[x]]$alpha))
  i_alph <- (cron_alph1 + cron_alph2)/2
  strat_alph_val <- 1 - sum((i_sig)*(1-i_alph))/(tot_sig)
  # Standard error
  strat_alph_sd1 <- lapply(1:length(by_col), 
                           function(x) as.vector(cron_alph1_vals[[x]]$ci[2]-cron_alph1_vals[[x]]$alpha)/1.96)
  strat_alph_sd2 <- lapply(1:length(post_words_by_col), 
                           function(x) as.vector(cron_alph2_vals[[x]]$ci[2]-cron_alph2_vals[[x]]$alpha)/1.96)
  strat_alph_se <- mean(c(mean(unlist(strat_alph_sd1)), 
                          mean(unlist(strat_alph_sd2))))*sqrt(1/length(by_col) + 
                                                                1/length(post_words_by_col))
  
  
  ## Multidimensional Omega
  # omega1_vals <- lapply(1:length(by_col), 
  #                       function(x) 
  #                         suppressMessages(suppressWarnings(omega(data.frame(do.call("cbind",
  #                                                                                    by_col[[x]])),
  #                                                                 nfactors=1)$omega.tot)))
  # omega1 <- mean(unlist(omega1_vals))
  # omega2_vals <- lapply(1:length(post_words_by_col), 
  #                       function(x) 
  #                         suppressMessages(suppressWarnings(omega(data.frame(do.call("cbind",
  #                                                                                    post_words_by_col[[x]])),
  #                                                                 nfactors=1)$omega.tot)))
  # omega2 <- mean(unlist(omega2_vals))
  
  omega1_vals <- lapply(1:length(by_col), 
                        function(x) 
                          suppressMessages(suppressWarnings(omega(data.frame(do.call("cbind",
                                                                                     by_col[[x]])),
                                                                  nfactors=1))))
  omega1 <- mean(unlist(lapply(1:length(by_col),
                               function(x) omega1_vals[[x]]$omega.tot)))
  omega2_vals <- lapply(1:length(post_words_by_col), 
                        function(x) 
                          suppressMessages(suppressWarnings(omega(data.frame(do.call("cbind",
                                                                                     post_words_by_col[[x]])),
                                                                  nfactors=1))))
  omega2 <- mean(unlist(lapply(1:length(post_words_by_col),
                               function(x) omega2_vals[[x]]$omega.tot)))
  
  # omega1 <- omega(data.frame(do.call("cbind", theta)),
  #                 nfactors=ncol(theta[[1]]))$omega.tot
  # omega2 <- omega(t(data.frame(do.call("rbind", post_words))),
  #                 nfactors=ncol(theta[[1]]))$omega.tot
  omega_val <- (omega1+omega2)/2
  
  omega_se <- mean(c(mean(unlist(lapply(1:length(by_col),
                                        function(x) omega1_vals[[x]]$stats$sd))), 
                     mean(unlist(lapply(1:length(post_words_by_col),
                                        function(x) omega2_vals[[x]]$stats$sd)))))*sqrt(1/length(by_col) + 
                                                                                          1/length(post_words_by_col))

  ## Maximal Reliability  (Li, Resenthal, and Rubin (1996))
  # First we obtain item reliability via Spearman-Brown reliability
  # Correlation between the same columns in different replications
  i_cor <- lapply(1:length(by_col),
                  function(z) lapply(1:length(by_col[[z]]),
                                     function(x) lapply(1:length(by_col[[z]]),
                                                        function(y) if (x!=y){
                                                          cosine(by_col[[z]][[x]],
                                                                 by_col[[z]][[y]])})))
  i_cor <- unlist(lapply(1:length(i_cor), function(x) mean(unlist(i_cor[[x]]))))
  i_cor2 <- lapply(1:(ncol(theta[[1]])-1),
                   function(top_k) lapply(1:length(post_words),
                                          function(j) lapply(1:length(post_words),
                                                             function(i) if (i!=j){
                                                               lsa::cosine(post_words[[i]][top_k,],
                                                                           post_words[[j]][top_k,])})))
  i_cor2 <- unlist(lapply(1:length(i_cor2), function(x) mean(unlist(i_cor2[[x]]))))
  i_cor_full <- rowMeans(cbind(i_cor,i_cor2))
  reliab_item <- (length(theta)*i_cor_full)/(1+(length(theta)-1)*i_cor_full)
  # Obtain maximal reliability
  rho_val <- mean(i_cor_full)
  num_mr <- sum((length(theta)*reliab_item)/(1-reliab_item))
  denom_mr <- (ncol(theta[[1]])-1)/(1+((ncol(theta[[1]])-1)-1)*rho_val) + num_mr
  max_reliab_val <- num_mr/denom_mr

  ## Comparison: similarity measure
  # Compare existing reliability method from Maier et al., 2018
  sum_cos_sim <- 0
  tot_cos_sim <- 0
  for (i in 1:length(post_words)){
    for (j in 1:length(post_words)){
      for (top_k in 1:(ncol(theta[[1]])-1)){
        if (i!=j){
          tot_cos_sim = tot_cos_sim + 1
          if (cosine(post_words[[i]][top_k,],post_words[[j]][top_k,])>=0.7){
            sum_cos_sim = sum_cos_sim + 1
          }
        }
      }
    }
  }
  comparison_val <- sum_cos_sim/tot_cos_sim
  
  return(list(strat_alph_val,omega_val,max_reliab_val,comparison_val,
              'StratAlphSE' = strat_alph_se, 'OmegaSE' = omega_se))
}








