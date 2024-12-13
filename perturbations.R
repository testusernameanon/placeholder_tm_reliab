# Comparison of reliability of STM and LDA with perturbations
# STM is deterministic, so this is our baseline for change
# Perturbations of only non-trivial LDA data 
  # Trivial vocabulary is too small and perturbations would have a massive impact
# Sequentially: after trivial and nontrivial synthetic data scripts

library(stm)
library(stringr)
library(lsa)
library(tm)

source('tm_reliab.R')
source('perturb_funcs.R')

set.seed(2)

# Randomly removing one word - this is done for all 100 replications
words_to_remove_1wrd <- lapply(1:100,
                               function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,1))
dat_perturbations_1wrd <- lapply(words_to_remove_1wrd, 
                                 function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_1wrd <- run_stm_perturb(words_to_remove_1wrd,dat_perturbations_1wrd)
stm_strat_alph_perturb_1wrd <- stm_reliab_perturb_1wrd[[1]][[1]]
stm_lda_perturb_omega_1wrd <- stm_reliab_perturb_1wrd[[1]][[2]]
stm_max_reliab_perturb_1wrd <- stm_reliab_perturb_1wrd[[1]][[3]]
stm_lda_perturb_cos_sim_1wrd <- stm_reliab_perturb_1wrd[[1]][[4]]
# rm(stm_reliab_perturb_1wrd)
# LDA perturbations
lda_reliab_perturb_1wrd <- run_lda_perturb(words_to_remove_1wrd,dat_perturbations_1wrd)
strat_alph_perturb_1wrd <- lda_reliab_perturb_1wrd[[1]][[1]]
lda_perturb_omega_1wrd <- lda_reliab_perturb_1wrd[[1]][[2]]
max_reliab_perturb_1wrd <- lda_reliab_perturb_1wrd[[1]][[3]]
lda_perturb_cos_sim_1wrd <- lda_reliab_perturb_1wrd[[1]][[4]]
# rm(lda_reliab_perturb_1wrd)

# Randomly removing two words - this is done for all 100 replications
words_to_remove_2wrd <- lapply(1:100,
                               function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,2))
dat_perturbations_2wrd <- lapply(words_to_remove_2wrd, 
                                 function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_2wrd <- run_stm_perturb(words_to_remove_2wrd,dat_perturbations_2wrd)
stm_strat_alph_perturb_2wrd <- stm_reliab_perturb_2wrd[[1]][[1]]
stm_lda_perturb_omega_2wrd <- stm_reliab_perturb_2wrd[[1]][[2]]
stm_max_reliab_perturb_2wrd <- stm_reliab_perturb_2wrd[[1]][[3]]
stm_lda_perturb_cos_sim_2wrd <- stm_reliab_perturb_2wrd[[1]][[4]]
# rm(stm_reliab_perturb_2wrd)
# LDA perturbations
lda_reliab_perturb_2wrd <- run_lda_perturb(words_to_remove_2wrd,dat_perturbations_2wrd)
strat_alph_perturb_2wrd <- lda_reliab_perturb_2wrd[[1]][[1]]
lda_perturb_omega_2wrd <- lda_reliab_perturb_2wrd[[1]][[2]]
max_reliab_perturb_2wrd <- lda_reliab_perturb_2wrd[[1]][[3]]
lda_perturb_cos_sim_2wrd <- lda_reliab_perturb_2wrd[[1]][[4]]
rm(lda_reliab_perturb_2wrd)

# Randomly removing three words - this is done for all 100 replications
words_to_remove_3wrd <- lapply(1:100,
                               function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,3))
dat_perturbations_3wrd <- lapply(words_to_remove_3wrd, 
                                 function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_3wrd <- run_stm_perturb(words_to_remove_3wrd,dat_perturbations_3wrd)
stm_strat_alph_perturb_3wrd <- stm_reliab_perturb_3wrd[[1]][[1]]
stm_lda_perturb_omega_3wrd <- stm_reliab_perturb_3wrd[[1]][[2]]
stm_max_reliab_perturb_3wrd <- stm_reliab_perturb_3wrd[[1]][[3]]
stm_lda_perturb_cos_sim_3wrd <- stm_reliab_perturb_3wrd[[1]][[4]]
rm(stm_reliab_perturb_3wrd)
# LDA perturbations
lda_reliab_perturb_3wrd <- run_lda_perturb(words_to_remove_3wrd,dat_perturbations_3wrd)
strat_alph_perturb_3wrd <- lda_reliab_perturb_3wrd[[1]][[1]]
lda_perturb_omega_3wrd <- lda_reliab_perturb_3wrd[[1]][[2]]
max_reliab_perturb_3wrd <- lda_reliab_perturb_3wrd[[1]][[3]]
lda_perturb_cos_sim_3wrd <- lda_reliab_perturb_3wrd[[1]][[4]]
rm(lda_reliab_perturb_3wrd)

# Randomly removing five words - this is done for all 100 replications
words_to_remove_5wrd <- lapply(1:100,
                               function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,5))
dat_perturbations_5wrd <- lapply(words_to_remove_5wrd, 
                                 function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_5wrd <- run_stm_perturb(words_to_remove_5wrd,dat_perturbations_5wrd)
stm_strat_alph_perturb_5wrd <- stm_reliab_perturb_5wrd[[1]][[1]]
stm_lda_perturb_omega_5wrd <- stm_reliab_perturb_5wrd[[1]][[2]]
stm_max_reliab_perturb_5wrd <- stm_reliab_perturb_5wrd[[1]][[3]]
stm_lda_perturb_cos_sim_5wrd <- stm_reliab_perturb_5wrd[[1]][[4]]
rm(stm_reliab_perturb_5wrd)
# LDA perturbations
lda_reliab_perturb_5wrd <- run_lda_perturb(words_to_remove_5wrd,dat_perturbations_5wrd)
strat_alph_perturb_5wrd <- lda_reliab_perturb_5wrd[[1]][[1]]
lda_perturb_omega_5wrd <- lda_reliab_perturb_5wrd[[1]][[2]]
max_reliab_perturb_5wrd <- lda_reliab_perturb_5wrd[[1]][[3]]
lda_perturb_cos_sim_5wrd <- lda_reliab_perturb_5wrd[[1]][[4]]
rm(lda_reliab_perturb_5wrd)

# Randomly removing ten words - this is done for all 100 replications
words_to_remove_10wrd <- lapply(1:100,
                                function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,10))
dat_perturbations_10wrd <- lapply(words_to_remove_10wrd, 
                                  function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_10wrd <- run_stm_perturb(words_to_remove_10wrd,dat_perturbations_10wrd)
stm_strat_alph_perturb_10wrd <- stm_reliab_perturb_10wrd[[1]][[1]]
stm_lda_perturb_omega_10wrd <- stm_reliab_perturb_10wrd[[1]][[2]]
stm_max_reliab_perturb_10wrd <- stm_reliab_perturb_10wrd[[1]][[3]]
stm_lda_perturb_cos_sim_10wrd <- stm_reliab_perturb_10wrd[[1]][[4]]
rm(stm_reliab_perturb_10wrd)
# LDA perturbations
lda_reliab_perturb_10wrd <- run_lda_perturb(words_to_remove_10wrd,dat_perturbations_10wrd)
strat_alph_perturb_10wrd <- lda_reliab_perturb_10wrd[[1]][[1]]
lda_perturb_omega_10wrd <- lda_reliab_perturb_10wrd[[1]][[2]]
max_reliab_perturb_10wrd <- lda_reliab_perturb_10wrd[[1]][[3]]
lda_perturb_cos_sim_10wrd <- lda_reliab_perturb_10wrd[[1]][[4]]
rm(lda_reliab_perturb_10wrd)

# Randomly removing 20 words - this is done for all 100 replications
words_to_remove_20wrd <- lapply(1:100,
                                function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,20))
dat_perturbations_20wrd <- lapply(words_to_remove_20wrd, 
                                  function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_20wrd <- run_stm_perturb(words_to_remove_20wrd,dat_perturbations_20wrd)
stm_strat_alph_perturb_20wrd <- stm_reliab_perturb_20wrd[[1]][[1]]
stm_lda_perturb_omega_20wrd <- stm_reliab_perturb_20wrd[[1]][[2]]
stm_max_reliab_perturb_20wrd <- stm_reliab_perturb_20wrd[[1]][[3]]
stm_lda_perturb_cos_sim_20wrd <- stm_reliab_perturb_20wrd[[1]][[4]]
rm(stm_reliab_perturb_20wrd)
# LDA perturbations
lda_reliab_perturb_20wrd <- run_lda_perturb(words_to_remove_20wrd,dat_perturbations_20wrd)
strat_alph_perturb_20wrd <- lda_reliab_perturb_20wrd[[1]][[1]]
lda_perturb_omega_20wrd <- lda_reliab_perturb_20wrd[[1]][[2]]
max_reliab_perturb_20wrd <- lda_reliab_perturb_20wrd[[1]][[3]]
lda_perturb_cos_sim_20wrd <- lda_reliab_perturb_20wrd[[1]][[4]]
rm(lda_reliab_perturb_20wrd)

# Randomly removing 50 words - this is done for all 100 replications
words_to_remove_50wrd <- lapply(1:100,
                                function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,50))
dat_perturbations_50wrd <- lapply(words_to_remove_50wrd, 
                                  function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_50wrd <- run_stm_perturb(words_to_remove_50wrd,dat_perturbations_50wrd)
stm_strat_alph_perturb_50wrd <- stm_reliab_perturb_50wrd[[1]][[1]]
stm_lda_perturb_omega_50wrd <- stm_reliab_perturb_50wrd[[1]][[2]]
stm_max_reliab_perturb_50wrd <- stm_reliab_perturb_50wrd[[1]][[3]]
stm_lda_perturb_cos_sim_50wrd <- stm_reliab_perturb_50wrd[[1]][[4]]
rm(stm_reliab_perturb_50wrd)
# LDA perturbations
lda_reliab_perturb_50wrd <- run_lda_perturb(words_to_remove_50wrd,dat_perturbations_50wrd)
strat_alph_perturb_50wrd <- lda_reliab_perturb_50wrd[[1]][[1]]
lda_perturb_omega_50wrd <- lda_reliab_perturb_50wrd[[1]][[2]]
max_reliab_perturb_50wrd <- lda_reliab_perturb_50wrd[[1]][[3]]
lda_perturb_cos_sim_50wrd <- lda_reliab_perturb_50wrd[[1]][[4]]
rm(lda_reliab_perturb_50wrd)

# Randomly removing 100 words - this is done for all 100 replications
words_to_remove_100wrd <- lapply(1:100,
                                 function(x) sample(TermDocumentMatrix(VCorpus(VectorSource(dat$text)))$dimnames$Terms,100))
cleaned_words_to_remove_100wrd <- lapply(words_to_remove_100wrd, 
                                         function(set) unlist(lapply(set, 
                                                                     function(string) gsub("[[:punct:]]", "", string))))
dat_perturbations_100wrd <- lapply(cleaned_words_to_remove_100wrd, 
                                   function(words) gsub(str_replace_all(str_c(words,collapse ="|"),'\\.',''),'',dat$text))
# STM perturbations
stm_reliab_perturb_100wrd <- run_stm_perturb(words_to_remove_100wrd,dat_perturbations_100wrd)
stm_strat_alph_perturb_100wrd <- stm_reliab_perturb_100wrd[[1]][[1]]
stm_lda_perturb_omega_100wrd <- stm_reliab_perturb_100wrd[[1]][[2]]
stm_max_reliab_perturb_100wrd <- stm_reliab_perturb_100wrd[[1]][[3]]
stm_lda_perturb_cos_sim_100wrd <- stm_reliab_perturb_100wrd[[1]][[4]]
rm(stm_reliab_perturb_100wrd)
# LDA perturbations
lda_reliab_perturb_100wrd <- run_lda_perturb(words_to_remove_100wrd,dat_perturbations_100wrd)
strat_alph_perturb_100wrd <- lda_reliab_perturb_100wrd[[1]][[1]]
lda_perturb_omega_100wrd <- lda_reliab_perturb_100wrd[[1]][[2]]
max_reliab_perturb_100wrd <- lda_reliab_perturb_100wrd[[1]][[3]]
lda_perturb_cos_sim_100wrd <- lda_reliab_perturb_100wrd[[1]][[4]]
rm(lda_reliab_perturb_100wrd)

