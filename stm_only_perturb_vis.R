# Plotting results excluding stratified alpha
library(ggplot2)

# Add jitter for plot
df_max_reliab_stm <- df_max_reliab
df_max_reliab_stm['STM'] <- df_max_reliab_stm['STM']-0.003
df_full_plot_stm <- rbind(df_mult_omega,df_max_reliab_stm,df_std_practice)
df_full_plot_stm$`Words Removed` <- as.factor(df_full_plot_stm$WordsRemoved)
df_full_plot_stm$Method <- df_full_plot_stm$Label

ggplot(data=df_full_plot_stm,mapping=aes(x=WordsRemoved,y=STM,
                                     group=Method,
                                     color=Method)) +
  geom_path() +
  geom_point() +
  # geom_abline(slope=-1,color='grey') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Reliability Method Comparison')


## Separated plots
df_facet_plot <- rbind(df_strat_alph,df_mult_omega,
                       df_max_reliab,df_std_practice)
df_facet_plot$`Words Removed` <- as.factor(df_facet_plot$WordsRemoved)

ggplot(df_facet_plot, mapping=aes(x=WordsRemoved,y=STM)) + 
  geom_path() +
  geom_point() +
  facet_wrap(~ Label,scales='free') +
  theme_bw() +
  theme(panel.grid = element_blank(),axis.text.x = element_text(angle = 90))


############################################################################
## Investigation into topics across replications
# Top 5 FREX words across top topic for all replications
# 1 word removed
library(stm)
lapply(1:length(stm_lda_perturb_1wrd), 
       function(x) labelTopics(stm_lda_perturb_1wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_1wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 2 words removed
lapply(1:length(stm_lda_perturb_2wrd), 
       function(x) labelTopics(stm_lda_perturb_2wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_2wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 3 words removed
lapply(1:length(stm_lda_perturb_3wrd), 
       function(x) labelTopics(stm_lda_perturb_3wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_3wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 5 words removed
lapply(1:length(stm_lda_perturb_5wrd), 
       function(x) labelTopics(stm_lda_perturb_5wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_5wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 10 words removed
lapply(1:length(stm_lda_perturb_10wrd), 
       function(x) labelTopics(stm_lda_perturb_10wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_10wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 20 words removed
lapply(1:length(stm_lda_perturb_20wrd), 
       function(x) labelTopics(stm_lda_perturb_20wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_20wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 50 words removed
lapply(1:length(stm_lda_perturb_50wrd), 
       function(x) labelTopics(stm_lda_perturb_50wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_50wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])
# 100 words removed
lapply(1:length(stm_lda_perturb_100wrd), 
       function(x) labelTopics(stm_lda_perturb_100wrd[[x]],
                               topics=order(colSums(stm_lda_perturb_100wrd[[x]]$theta),
                                            decreasing=T)[1],n=5)$frex[1,])



