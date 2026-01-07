# Settings
mu    <- 0      # mean on logit scale
sigma <- 1.5    # sd on logit scale
df    <- 5      # degrees of freedom for Student-t
tau   <- 2.5    # scale for Student-t
n     <- 50000  # number of draws

# Two rows: Normal (top), Student-t (bottom)
par(mfrow = c(2, 2))

# Normal prior (logit scale)
logit_norm <- rnorm(n, mean = mu, sd = sigma)
prob_norm  <- plogis(logit_norm)

plot(
  density(logit_norm),
  main = sprintf("Normal(μ=%.2f, σ=%.2f) on Logit Scale", mu, sigma),
  xlab = "logit",
  ylab = "density"
)

plot(
  density(prob_norm),
  main = sprintf("Normal(μ=%.2f, σ=%.2f) → Probability", mu, sigma),
  xlab = "probability",
  ylab = "density"
)

# Student-t prior (logit scale)
logit_t <- rt(n, df = df) * tau + mu
prob_t  <- plogis(logit_t)

plot(
  density(logit_t),
  main = sprintf("Student-t(df=%d, scale=%.2f, μ=%.2f) on Logit Scale", df, tau, mu),
  xlab = "logit",
  ylab = "density"
)

plot(
  density(prob_t),
  main = sprintf("Student-t(df=%d, scale=%.2f, μ=%.2f) → Probability", df, tau, mu),
  xlab = "probability",
  ylab = "density"
)

par(mfrow = c(1, 1))