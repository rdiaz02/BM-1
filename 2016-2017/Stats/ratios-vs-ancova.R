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



## case 1:
## parallel slopes, different intercept, same range of z

## case 2:
## parallel slopes, different intercept, different range of z

## case 3:
## parallel slopes, same intercept, different range of z



## Case 4
## pencil of lines with intercept of 0


## Case 5

## Actual differences in ratios might not be detected taking the ratio if
## regression does not go through the origin
## use just two groups, for simplicity
n <- 20
sd <- 0.05
z2 <- seq(from = 1, to = 3, length.out = n)
ya <- z2 + rnorm(n, 0, sd)
## yb <- 2 + rnorm(n, 0, sd)
yb <- 0.5 * z2 + 1 + rnorm(n, 0, sd)
y <- c(ya, yb)
tf <- factor(rep(c("g1", "g2"), rep(n, 2)))
z <- rep(z2, 2)
yr <- y/z
plot(yr ~ tf)
summary(lm(yr ~ tf))


plot(y ~ z, col = c("red", "blue")[tf])
abline(lm(ya ~ z2), col = "red")
abline(lm(yb ~ z2), col = "blue")
## abline(lm(yc ~ z2), col = "orange")
## no difference
summary(lm(yr ~ tf))

## Of course, differences are detected
summary(lm(y ~ z * tf))
