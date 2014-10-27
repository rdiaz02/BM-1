library(HH)
library(car)
library(effects)

set.seed(1)
sex <- factor(rep(c("Male", "Female"), c(20, 20)))
drug <- factor(rep(rep(c("A", "B"), c(10, 10)), 2))
chol <- rep(c(10, 13, 12, 16), rep(10, 4))
chol <- chol + rnorm(length(chol), sd = 1.5)
chol.data <- data.frame(chol, sex, drug)
with(chol.data, tapply(chol, list(sex, drug), function(x) sum(!is.na(x))))

with(chol.data, tapply(chol, list(sex, drug), mean))

summary(lm(chol ~ sex * drug, data = chol.data))

m1 <- lm(chol ~ sex + drug, data = chol.data)
m2 <- lm(chol ~ drug + sex, data = chol.data)
msex <- lm(chol ~ sex, data = chol.data)
mdrug <- lm(chol ~ drug, data = chol.data)


summary(m1)
summary(m2)
summary(msex)
summary(mdrug)


anova(m1)
anova(m2)

anova(msex)
anova(mdrug)


##############



set.seed(1)
sex <- factor(rep(c("Male", "Female"), c(30, 30)))
drug <- factor(rep(rep(c("A", "B"), c(10, 20)), 2))
chol <- rep(c(10, 13, 12, 16), c(10, 10, 20, 20))
chol <- chol + rnorm(length(chol), sd = 1.5)
chol.data <- data.frame(chol, sex, drug)
with(chol.data, tapply(chol, list(sex, drug), function(x) sum(!is.na(x))))
m1 <- lm(chol ~ sex + drug, data = chol.data)
m2 <- lm(chol ~ drug + sex, data = chol.data)
msex <- lm(chol ~ sex, data = chol.data)
mdrug <- lm(chol ~ drug, data = chol.data)


summary(m1)
summary(m2)
summary(msex)
summary(mdrug)


anova(m1)
anova(m2)

anova(msex)
anova(mdrug)


