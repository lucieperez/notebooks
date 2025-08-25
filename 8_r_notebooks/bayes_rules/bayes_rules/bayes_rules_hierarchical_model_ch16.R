# Load packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

# Load data
data(spotify)

spotify <- spotify %>% 
  select(artist, title, popularity) %>% 
  mutate(artist = fct_reorder(artist, popularity, .fun = 'mean'))

# First few rows
head(spotify, 3)

# Number of songs
nrow(spotify)

# Number of artists
nlevels(spotify$artist)


artist_means <- spotify %>% 
  group_by(artist) %>% 
  summarize(count = n(), popularity = mean(popularity))

artist_means %>%
  slice(1:2, 43:44)

head(artist_means, 2)

artist_means %>% 
  summarize(min(count), max(count))

ggplot(spotify, aes(x = popularity)) + 
  geom_density()

spotify_complete_pooled <- stan_glm(
  popularity ~ 1, 
  data = spotify, family = gaussian, 
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

# Get prior specifications
prior_summary(spotify_complete_pooled)

complete_summary <- tidy(spotify_complete_pooled, 
                         effects = c("fixed", "aux"), 
                         conf.int = TRUE, conf.level = 0.80)
complete_summary

set.seed(84735)
predictions_complete <- posterior_predict(spotify_complete_pooled,
                                          newdata = artist_means)

ppc_intervals(artist_means$popularity, yrep = predictions_complete,
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist,
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1)


ggplot(spotify, aes(x = popularity, group = artist)) + 
  geom_density()

spotify_no_pooled <- stan_glm(
  popularity ~ artist - 1, 
  data = spotify, family = gaussian, 
  prior = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

# Simulate the posterior predictive models
set.seed(84735)
predictions_no <- posterior_predict(
  spotify_no_pooled, newdata = artist_means)

# Plot the posterior predictive intervals
ppc_intervals(artist_means$popularity, yrep = predictions_no, 
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist, 
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1)

ggplot(artist_means, aes(x = popularity)) + 
  geom_density()

spotify_hierarchical <- stan_glmer(
  popularity ~ (1 | artist), 
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735)

# Confirm the prior tunings
prior_summary(spotify_hierarchical)

mcmc_trace(spotify_hierarchical)
mcmc_dens_overlay(spotify_hierarchical)
mcmc_acf(spotify_hierarchical)
neff_ratio(spotify_hierarchical)
rhat(spotify_hierarchical)

pp_check(spotify_hierarchical) + 
  xlab("popularity")

# Store the simulation in a data frame
spotify_hierarchical_df <- as.data.frame(spotify_hierarchical)


# Check out the first 3 and last 3 parameter labels
spotify_hierarchical_df %>% 
  colnames() %>% 
  as.data.frame() %>% 
  slice(1:3, 45:47)

tidy(spotify_hierarchical, effects = "fixed", 
     conf.int = TRUE, conf.level = 0.80)

tidy(spotify_hierarchical, effects = "ran_pars")

artist_summary <- tidy(spotify_hierarchical, effects = "ran_vals", 
                       conf.int = TRUE, conf.level = 0.80)


# Check out the results for the first & last 2 artists
artist_summary %>% 
  select(level, conf.low, conf.high) %>% 
  slice(1:2, 43:44)


# Get MCMC chains for each mu_j
artist_chains <- spotify_hierarchical %>%
  spread_draws(`(Intercept)`, b[,artist]) %>% 
  mutate(mu_j = `(Intercept)` + b)

# Check it out
artist_chains %>% 
  select(artist, `(Intercept)`, b, mu_j) %>% 
  head(4)

# Get posterior summaries for mu_j
artist_summary_scaled <- artist_chains %>% 
  select(-`(Intercept)`, -b) %>% 
  mean_qi(.width = 0.80) %>% 
  mutate(artist = fct_reorder(artist, mu_j))

# Check out the results
artist_summary_scaled %>% 
  select(artist, mu_j, .lower, .upper) %>% 
  head(4)

ggplot(artist_summary_scaled, 
       aes(x = artist, y = mu_j, ymin = .lower, ymax = .upper)) +
  geom_pointrange() +
  xaxis_text(angle = 90, hjust = 1)

artist_means %>% 
  filter(artist %in% c("Frank Ocean", "Lil Skies"))

# Simulate Ocean's posterior predictive model
set.seed(84735)
ocean_chains <- spotify_hierarchical_df %>%
  rename(b = `b[(Intercept) artist:Frank_Ocean]`) %>% 
  select(`(Intercept)`, b, sigma) %>% 
  mutate(mu_ocean = `(Intercept)` + b,
         y_ocean = rnorm(20000, mean = mu_ocean, sd = sigma))

# Check it out
head(ocean_chains, 3)

# Posterior summary of Y_new,j
ocean_chains %>% 
  mean_qi(y_ocean, .width = 0.80) 

# Posterior summary of mu_j
artist_summary_scaled %>% 
  filter(artist == "artist:Frank_Ocean")

# Shrinkage and the bias-variance trade-off

set.seed(84735)
predictions_hierarchical <- posterior_predict(spotify_hierarchical, 
                                              newdata = artist_means)

# Posterior predictive plots
ppc_intervals(artist_means$popularity, yrep = predictions_hierarchical, 
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist, 
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1) + 
  geom_hline(yintercept = 58.4, linetype = "dashed")
