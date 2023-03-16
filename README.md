# Bayesian
I build a Bayesian hierarchical model that is fitted to students’ course data from
Maastricht College, a Liberal Arts college. This model is compared to an independent
model fitted to each course and a pooled model using the mean squared error from a test
set. The models are all fitted with a Gibbs sampler, a popular Markov Chain Monte Carlo
algorithm used in Bayesian models. There are certain semi conjugate priors and
conditional distributions used to build the models. A Gibbs sampler is used due to its
simplicity when building a hierarchical model and its feasibility to sample from the
distributions. This study will explore how well the hierarchical model can help predict
student course grades using a training and testing set. There will be a prediction for each
course that will be used when calculating the mean squared error.
