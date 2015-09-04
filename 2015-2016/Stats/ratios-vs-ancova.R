## Classical ancova: common slope and intercept differences
##  The ratio misses the point completely

z <- runif(6)
t <- rep(c(1, 2), 3)
y <- z + t + rnorm(6, 0, 0.01)
tf <- factor(t)
plot(y ~ z, col = c("red", "blue")[tf])

summary(lm(y ~ z + tf))

yratio <- y/z
summary(lm(yratio ~ tf))

plot(yratio ~ z, col = c("red", "blue")[tf])


#########################
## Ratio differences


## first, create the noise
noise <- rnorm(6, 0, 0.01)

## Suppose the ratios are given by t. Then, ratio in group 1 is 1 and in
## group 2 is 2. So y = ratio * z

y1 <- t * z + noise
y1

## That sounds confusing but is the same as saying:
y1b <- rep(0, 6)
y1b[tf == 1] <- z[tf ==1]
y1b[tf == 2] <- 2 * z[tf == 2]
y1b <- y1b + noise
y1b == y1

## Now form the ratio
yr <- y1/z
## As it should
plot(yr ~ z, col = c("red", "blue")[tf])

## Fit the models.

## I really have a hard time interpreting this
summary(lm(yr ~ z + tf))

## This is what we are after
summary(lm(yr ~ tf))


## This seems to kind of work but it is not really correct. Here we are
## modling a common slope, which is not what "differences in ratio" means,
## and we are making different assumptions about noise (this is a minor
## technical point here).

summary(lm(y1 ~ z + tf))


## And this is really what we are after. Slopes differ by group, which is
## the same as "ratios differ by group"

summary(lm(y1 ~ z * tf))

## or

anova(lm(y1 ~ z * tf))

## note the very highly significant interaction



## NOTE: of course, diagnostics might/ought to help us choose the model.
