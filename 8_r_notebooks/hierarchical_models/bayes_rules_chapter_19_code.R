# Load packages
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(janitor)
library(tidybayes)
library(broom.mixed)

data(airbnb)

# Number of listing
nrow(airbnb)

# Number of neighborhoods and other summaries

airbnb %>%
  summarize(nlevels(neighborhood), min(price), max(price))

ggplot(airbnb, aes(x = price)) +
  geom_histogram(color = "white", breaks = seq(0, 500, by = 20))
ggplot(airbnb, aes(x = log(price))) +
  geom_histogram(color = "white", binwidth = 0.5)

# Hierarchical model

airbnb_model_1 <- stan_glmer(
  log(price) ~ bedrooms + rating + room_type + (1 | neighborhood),
      data = airbnb, family = gaussian,
      prior_intercept = normal(4.6, 2.5, autoscale = TRUE),
      prior = normal(0, 2.5, autoscale = TRUE),
      prior_aux = exponential(1, autoscale = TRUE),
      prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
      chains = 4, iter = 5000*2, seed = 84735
)

prior_summary(airbnb_model_1)

pp_check(airbnb_model_1) +
  labs(title = "airbnb_model_1 of log (price)") +
  xlab("log(price)")
