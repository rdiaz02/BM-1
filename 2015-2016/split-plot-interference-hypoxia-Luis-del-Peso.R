## A more complicated split-plot based upon a design from Luis del Peso.


## I: interfernce

## H, hypoxia

## R1: cultivo

## R2: each pair of two samples from the same cultivo with the same
## interference; for each value of R2 we have two (sub)samples, and one of
## them is under hypoxia (H = h) and the other normoxia (H = o)

## In other words, we take one cultivo, split it into two, one of the two
## gets one level of I and the other the other level. Once grown, we split
## each of them into two again, and to each one we give on level of H.


## some unbalance

set.seed(123)
(df <- data.frame(
    R1 = factor(rep(c(1, 2, 3), c(4, 4, 8))),
    R2 = factor(rep(letters[1:6], c(rep(2, 4), 4, 4))),
    I = factor(rep(rep(c("i", "n"), c(2, 2)), 4)),
    H = factor(rep(c("h", "o"), 8)),
    Y = rnorm(16)
    )
 )

## convenient
df$R3 <- paste0(df$R1, ":", df$R2)

## balanced
df1 <- df[-(11:14) , ]
rownames(df1) <- 1:12
df1


#########################################################################
#########################################################################

######### Only model factor I

## The error terms expand to R1 and R1:I, and we get a three-stratum table
summary(aov(Y ~ I + Error(R1/I), data = df1))
## This is identical, but singularity comes from R2 same as R1:I
summary(aov(Y ~ I + Error(R1/R2), data = df1))

#### Check with lme, and it does since, when 1 df in numerator, F = t^2
library(lme4)
## the next two are identical, as they should be
summary(lmer(Y ~ I + (1|R1) + (1|R1:I), data = df1))
summary(lmer(Y ~ I + (1|R1) + (1|R2), data = df1))


#########################################################################
## When there is balance, the above are exactly identical to 
## a paired t-test averaging over the replicates

## Aggregate data by R1 and I and do t-test
(dfa <- aggregate(Y ~ R1 + I, data = df1, FUN = mean))
t.test(dfa$Y[dfa$I == "i"], dfa$Y[dfa$I == "n"], paired = TRUE)

## Of course, this is the same of a paired t-test
anova(lm(Y ~ I + R1, data = dfa))



## BUT this ain't correct (both are doing the same thing), as we are
## saying that I and R1 are crossed, which is not true.  Actually, we can
## see the problem is the denominator term and the dfs, because the Sum sq
## are OK. Both are same problem. Here I am saying I have four independent
## measures of R1= 1, two assigned to i and two to n. That is wrong in
## this case.

summary(aov(Y ~ I + R1, data = df1))
summary(aov(Y ~ I + Error(R1), data = df1))



#########################################################################
#########################################################################

############ Both factors, I and H, no interaction

## Each main effect in a different stratum and main effect of I same as if
## we had not modeled H, as it should be

summary(aov(Y ~ I + H + Error(R1/I), data = df1))


## The test for H is, of course, the same as a paired t-test done correctly:
t.test(df1$Y[df1$H == "h"], df1$Y[df1$H == "o"], paired = TRUE)
## which is the same as
t.test(df1$Y[df1$H == "h"] - df1$Y[df1$H == "o"])
## (the above works because the h and o levels within a unit alternate and
## come one after the other)

## Equivalence with lmer not as good anymore, which is not unexpected, and
## testing with lmer is a delicate thing, etc.

lm1 <- lmer(Y ~ I + H + (1|R1) + (1|R2), data = df1)
lm2 <- lmer(Y ~ I + (1|R1) + (1|R2), data = df1)
anova(lm1, lm2)




#########################################################################
#########################################################################

############ Both factors, I and H, interaction

## Each main effect in a different stratum, and the interaction only in
## the lower stratum. As we have modeled interaction, looking at main
## effect of H is questionable.

summary(aov(Y ~ I * H + Error(R1/I), data = df1))

## Next two are the same, but I think the one above is preferable.
summary(aov(Y ~ I/H + H + Error(R1/I), data = df1))
summary(aov(Y ~ I/H + H + Error(R1/I/H), data = df1))


## (Trying to do paired t-tests logic here is much harder, or impossible)


#########################################################################
#########################################################################

############ Both factors, I and H, interaction, unbalance


summary(aov(Y ~ I * H + Error(R1/I), data = df))

## Equivalence with lmer not good, but then all terms are very
## non-significant
m3 <- lmer(Y ~ I * H + (1|R1) + (1|R2), data = df)
m4 <- lmer(Y ~ I +  H + (1|R1) + (1|R2), data = df)
anova(m3, m4)
