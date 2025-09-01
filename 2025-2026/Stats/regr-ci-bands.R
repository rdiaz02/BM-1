

N <- 20

sim_fit <- function(N = 20, a = 3, b = 5, sd = 10, plot = TRUE) {
  x <- runif(N, min = 10, max = 30)
  y <- a + b * x + rnorm(N, sd = sd)
  dummy_data <- data.frame(y = y, x = x)
  rm(y, x)
  lm_cib_2 <- lm(y ~ x, data = dummy_data)
  ## the next does not work reliably
  if (plot) ci.plot(lm_cib_2)
  return(lm_cib_2$coefficients)
}


fits <- replicate(1000, sim_fit())
fits <- t(fits)
sum(fits[, 1] )
