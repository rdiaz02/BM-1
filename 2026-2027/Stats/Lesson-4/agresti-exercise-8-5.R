p <- seq(0, 1, by = 0.0001)
pval <- sapply(p, function(p0) binom.test(3, 15, p = p0)$p.value)

plot(p, pval, type = "l", xlab = "p under H0", ylab = "two-sided p-value")
abline(h = 0.05, lty = 2)

## CI that binom.test reports (Clopper-Pearson)
(ci <- binom.test(3, 15)$conf.int)
abline(v = ci, col = "red", lty = 3)

## "CI" by inverting the two-sided test: p0 values not rejected
range(p[pval >= 0.05])
ci

## The 2-sided p-value is by summing the probs. of results with prob. <=
## observed prob.

## The CI inverts two one-sided tests
