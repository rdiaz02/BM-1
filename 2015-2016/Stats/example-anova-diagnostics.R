## Diagnostics suggest missing interaction
set.seed(1)
sex <- factor(rep(c("Male", "Female"), c(20, 20)))
drug <- factor(rep(rep(c("A", "B"), c(10, 10)), 2))
y <- rep(c(8, 16, 10, 12), rep(10, 4))
y <- y + rnorm(length(y), sd = 1.5)
y.data <- data.frame(y, sex, drug)
rm(y, sex, drug)
with(y.data, tapply(y, list(sex, drug), mean))

## Fit the model
myAdditive <- lm(y ~ sex + drug, data = y.data)
myInteract <- lm(y ~ sex * drug, data = y.data)
summary(myAdditive)
summary(myInteract)

dev.off()
par(mfrow = c(2, 3))
plot(myAdditive, which = c(1:5)) ## look at first plot

dev.off()
par(mfrow = c(2, 3))
plot(myInteract, which = c(1:5))



############

## Large cook's in anova



set.seed(1)
sex <- factor(rep(c("Male", "Female"), c(20, 20)))
drug <- factor(rep(rep(c("A", "B"), c(10, 10)), 2))
y <- rep(c(8, 12, 11, 15), rep(10, 4))
y <- y + rnorm(length(y), sd = 1.5)
y.data <- data.frame(y, sex, drug)
rm(y, sex, drug)
## create a large outlier

y.data[1, 1] <- 30
with(y.data, tapply(y, list(sex, drug), mean))

## Fit the model
myAdditive <- lm(y ~ sex + drug, data = y.data)
myInteract <- lm(y ~ sex * drug, data = y.data)
summary(myAdditive)
summary(myInteract)

## diagnostics, all of them except 6th
par(mfrow = c(2, 3))
plot(myAdditive, which = 1:5)
dev.off()

## Now, create unbalance
y.data <- y.data[-c(35, 40), ]
par(mfrow = c(2, 3))
myAdditive <- lm(y ~ sex + drug, data = y.data)
plot(myAdditive, which = 1:5) ## see Cook's
