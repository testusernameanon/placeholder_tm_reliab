# Reliability of Topic Modeling
### Introduction
The work in this repository supports the paper 'Reliability of Topic Modeling' which can be accessed here.

The contributions of this research are as follows:

We highlight the shortcomings of the existing standard practice for measuring topic model reliability, which primarily relies on ad-hoc methods and fails to capture essential aspects of model variation.
We propose three new reliability metrics, namely Stratified Alpha, Multivariate Omega, and Maximal Reliability, which are grounded in statistical theory and address the limitations of the standard practice.
We conduct extensive empirical evaluations on both synthetic and real-world datasets to assess the performance of the proposed metrics. We demonstrate that Multivariate Omega consistently outperforms other metrics in capturing the true reliability of topic models.
Our work emphasizes the importance of considering reliability in topic modeling research. By using the proposed reliability metrics, researchers can assess the robustness of their findings and avoid drawing misleading conclusions based on unreliable topic models.
This work contributes to the methodological advancements in topic modeling by providing a rigorous framework for evaluating the reliability of these models. This can lead to more reliable and trustworthy insights derived from text data analysis.

#### Repo Setup
This repo includes corresponding data and scripts for both synthetic data results and application (CFPB data) results. Additional scripts are included to allow for replication of all figures and tables within the paper. The structure of the repo is as follows:

#### Synthetic Data Experiments
The trivial and non-trivial synthetic datasets used in our experiments are the respective files beginning with `trivial.1-1seed` and `lda.1-1seed`, located within the `data/` folder. The function of the files for the synthetic data experiments are as follows:

* `tm_reliab.R`: Contains a function to calculate the reliability of a topic model for all proposed reliability metrics as well as the current standard practice.
* `triv_synthetic_reliab.R`: Encomapsses all the functionality needed to get any and all trivial results discussed in the paper (Table 2). Two topics are utilized for this analysis.
* `nontriv_synthetic_reliab.R`: Contains all necessary components for calculating the reliability of LDA topic models for non-trivial synthetic data (Table 4 and Figure 3). The default number of topics utilized in this analysis is 100.
* `perturb_funcs.R`: Helper functions utilized to prepare and run perturbations for stm and LDA models given the randomly sampled words to be removed. A default of 100 topics is utilized.
* `perturbations.R`: Contains perturbation experiments of LDA and stm as utilized within the paper for selection of best methodology.
* `perturb_visualizations.R` and `stm_only_perturb_vis.R`: Allows for replication of Figures 2 and 4 as well as Table 7.

#### Application Experiments
The application dataset used in our experiments is within the `data/` folder in the file `CFPBdataset_virtualcurrency.csv`. The entirety of application experiments are encompassed within the `cfpb_reliability.R` file, using the helper functions within `tm_reliab.R`.
