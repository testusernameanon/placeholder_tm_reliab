# Perturbation visualizations
# To be run after `perturbations.R`

## Plotting results
df_strat_alph <- data.frame('STM'=c(stm_strat_alph_perturb_1wrd,
                                    stm_strat_alph_perturb_2wrd,
                                    stm_strat_alph_perturb_3wrd,
                                    stm_strat_alph_perturb_5wrd,
                                    stm_strat_alph_perturb_10wrd,
                                    stm_strat_alph_perturb_20wrd,
                                    stm_strat_alph_perturb_50wrd,
                                    stm_strat_alph_perturb_100wrd),
                            'LDA'=c(strat_alph_perturb_1wrd,
                                    strat_alph_perturb_2wrd,
                                    strat_alph_perturb_3wrd,
                                    strat_alph_perturb_5wrd,
                                    strat_alph_perturb_10wrd,
                                    strat_alph_perturb_20wrd,
                                    strat_alph_perturb_50wrd,
                                    strat_alph_perturb_100wrd),
                            'WordsRemoved'=c(1:3,5,10,20,50,100))
df_strat_alph$Label <- rep('Stratified Alpha',nrow(df_strat_alph))
df_mult_omega <- data.frame('STM'=c(stm_lda_perturb_omega_1wrd,
                                    stm_lda_perturb_omega_2wrd,
                                    stm_lda_perturb_omega_3wrd,
                                    stm_lda_perturb_omega_5wrd,
                                    stm_lda_perturb_omega_10wrd,
                                    stm_lda_perturb_omega_20wrd,
                                    stm_lda_perturb_omega_50wrd,
                                    stm_lda_perturb_omega_100wrd),
                            'LDA'=c(lda_perturb_omega_1wrd,
                                    lda_perturb_omega_2wrd,
                                    lda_perturb_omega_3wrd,
                                    lda_perturb_omega_5wrd,
                                    lda_perturb_omega_10wrd,
                                    lda_perturb_omega_20wrd,
                                    lda_perturb_omega_50wrd,
                                    lda_perturb_omega_100wrd),
                            'WordsRemoved'=c(1:3,5,10,20,50,100))
df_mult_omega$Label <- rep('Multivariate Omega',nrow(df_mult_omega))
df_max_reliab <- data.frame('STM'=c(stm_max_reliab_perturb_1wrd,
                                    stm_max_reliab_perturb_2wrd,
                                    stm_max_reliab_perturb_3wrd,
                                    stm_max_reliab_perturb_5wrd,
                                    stm_max_reliab_perturb_10wrd,
                                    stm_max_reliab_perturb_20wrd,
                                    stm_max_reliab_perturb_50wrd,
                                    stm_max_reliab_perturb_100wrd),
                            'LDA'=c(max_reliab_perturb_1wrd,
                                    max_reliab_perturb_2wrd,
                                    max_reliab_perturb_3wrd,
                                    max_reliab_perturb_5wrd,
                                    max_reliab_perturb_10wrd,
                                    max_reliab_perturb_20wrd,
                                    max_reliab_perturb_50wrd,
                                    max_reliab_perturb_100wrd),
                            'WordsRemoved'=c(1:3,5,10,20,50,100))
df_max_reliab$Label <- rep('Maximal Reliability',nrow(df_max_reliab))
df_std_practice <- data.frame('STM'=c(stm_lda_perturb_cos_sim_1wrd,
                                      stm_lda_perturb_cos_sim_2wrd,
                                      stm_lda_perturb_cos_sim_3wrd,
                                      stm_lda_perturb_cos_sim_5wrd,
                                      stm_lda_perturb_cos_sim_10wrd,
                                      stm_lda_perturb_cos_sim_20wrd,
                                      stm_lda_perturb_cos_sim_50wrd,
                                      stm_lda_perturb_cos_sim_100wrd),
                              'LDA'=c(lda_perturb_cos_sim_1wrd,
                                      lda_perturb_cos_sim_2wrd,
                                      lda_perturb_cos_sim_3wrd,
                                      lda_perturb_cos_sim_5wrd,
                                      lda_perturb_cos_sim_10wrd,
                                      lda_perturb_cos_sim_20wrd,
                                      lda_perturb_cos_sim_50wrd,
                                      lda_perturb_cos_sim_100wrd),
                              'WordsRemoved'=c(1:3,5,10,20,50,100))
df_std_practice$Label <- rep('Standard Practice',nrow(df_std_practice))

##########################################################################
# Result plots 

# Plotting results excluding stratified alpha
library(ggplot2)

df_full_plot <- rbind(df_mult_omega,df_max_reliab,df_std_practice)
df_full_plot$`Words Removed` <- as.factor(df_full_plot$WordsRemoved)

ggplot(data=df_full_plot,mapping=aes(x=LDA,y=STM,
                                     group=Label,
                                     linetype=Label)) +
  geom_path() +
  geom_point(aes(color= `Words Removed`)) +
  geom_abline(slope=1,color='grey') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Reliability Method Comparison')


## Separated plots
df_facet_plot <- rbind(df_strat_alph,df_mult_omega,
                       df_max_reliab,df_std_practice)
df_facet_plot$`Words Removed` <- as.factor(df_facet_plot$WordsRemoved)

ggplot(df_facet_plot, mapping=aes(x=LDA,y=STM)) + 
  geom_path() +
  geom_point(aes(color= `Words Removed`)) +
  facet_wrap(~ Label,scales='free') +
  theme_bw() +
  theme(panel.grid = element_blank(),axis.text.x = element_text(angle = 90))


# Numeric performance comparison for description in paper
set.seed(1) 
rand_selected_topic <- sample(1:ncol(stm_lda_perturb_theta[[1]]),1) # 68

table(table(unlist(lapply(1:length(lda_perturb_theta),
                    function(x) head(order(lda_perturb_theta[[x]][,rand_selected_topic],decreasing = T),10)))))
table(table(unlist(lapply(1:length(stm_lda_perturb_theta),
                    function(x) head(order(stm_lda_perturb_theta[[x]][,rand_selected_topic],decreasing = T),10)))))

# Top topics across first document
lda_synthetic_top_topics_doc1 <- lapply(1:length(lda_perturb_theta),
                                        function(x) head(order(lda_perturb_theta[[x]][1,],decreasing = T),3))
# Top words across top topics for the first document
lda_synthetic_top_words_doc1 <- lapply(1:length(lda_perturb_theta), 
                                function(x) terms(lda_perturb_1wrd[[x]],3)[,lda_synthetic_top_topics_doc1[[x]]])


#########################################################################

# Comparison of 3 most common words from 3 most common topics

# STM
labelTopics(stm_lda_perturb_1wrd[[10]],
            topics=order(colSums(stm_lda_perturb_1wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_2wrd[[10]],
            topics=order(colSums(stm_lda_perturb_2wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_3wrd[[10]],
            topics=order(colSums(stm_lda_perturb_3wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_5wrd[[10]],
            topics=order(colSums(stm_lda_perturb_5wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_10wrd[[10]],
            topics=order(colSums(stm_lda_perturb_10wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_20wrd[[10]],
            topics=order(colSums(stm_lda_perturb_20wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_50wrd[[10]],
            topics=order(colSums(stm_lda_perturb_50wrd[[10]]$theta),decreasing=T)[1],n=5)
labelTopics(stm_lda_perturb_100wrd[[10]],
            topics=order(colSums(stm_lda_perturb_100wrd[[10]]$theta),decreasing=T)[1],n=5)




