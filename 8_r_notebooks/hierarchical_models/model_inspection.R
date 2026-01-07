library(brms)
library(loo)
library(bayesplot)

# MODELS
motion_verb_1 <- readRDS("models/motion_verb_1.rds")
motion_verb_2 <- readRDS("models/motion_verb_2_nested.rds")
motion_verb_3 <- readRDS("models/motion_verb_3.rds")
motion_verb_4 <- readRDS("models/motion_verb_4_nested.rds")


# Prior summary
prior_summary(motion_verb_1)

# Summary
# how to see information about the base levels?
summary(motion_verb_1)

#  extract the results of the fixed effects (grouping variables) and the credible intervals 
coef <-fixef(motion_verb_1, summary = TRUE)
coef

pp_check(motion_verb_1, ndraws=NULL)

# Conditional effects
conditional_effects(motion_verb_1, categorical = TRUE, surface = TRUE)

# MODEL 2

motion_verb_2 <- readRDS("models/motion_verb_2_nested.rds")


# Prior summary
prior_summary(motion_verb_2)

# Summary
summary(motion_verb_2)

#  extract the results of the fixed effects (grouping variables) and the credible intervals 
coef <-fixef(motion_verb_2, summary = TRUE)
coef

pp_check(motion_verb_2)

# Model diagnostic with plot

par(ask = FALSE)

# MODEL 1
color_scheme_set("red")
plot(motion_verb_1)
title("Model 1: Normal Priors - Crossed")
dev.off()

# MODEL 2
pdf("plots/motion_verb_2_diagnostics.pdf")
color_scheme_set("blue")
plot(motion_verb_2)
title("Model 2: Normal Priors - Nested")

# MODEL 3
pdf("plots/motion_verb_3_diagnostics.pdf")
color_scheme_set("orange")
plot(motion_verb_3)
title("Model 3: Student-T Priors - Crossed")

# MODEL 4
pdf("plots/motion_verb_4_diagnostics.pdf")
color_scheme_set("green")
plot(motion_verb_4)
title("Model 4: Student-T Priors - Nested")

pp_check(motion_verb_2)

# Conditional effects
conditional_effects(motion_verb_2, categorical = TRUE, surface = TRUE)


## COMPARING MODELS WITH LOO

options(future.globals.maxSize = 4 * 1024^3)

loo1 <- loo(motion_verb_1)
loo2 <- loo(motion_verb_2)
loo3 <- loo(motion_verb_3, reloo = TRUE)
loo4 <- loo(motion_verb_4, reloo = TRUE)

loo1
loo2
loo3
loo4

# Direct comparison:
loo_compare(loo1, loo2, loo3, loo4)

## Bayes factor

