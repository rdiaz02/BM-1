## Plots, with dagitty
library(dagitty)
library(car) ## For S function

## library(rethinking) ## for drawdag
## Installing rethinking can be complicated just for a few graphs
## So have a fallback if rethinking not available
if(!suppressWarnings(require("rethinking", quietly = TRUE))) {
    drawdag <- plot
} 


mediator <- dagitty("dag {
Exercise -> Food
Food -> Cholesterol
Exercise -> Cholesterol
}")

drawdag(mediator)


N <- 1e4 ## huge N

mediator <- data.frame(Exercise = runif(N, 1, 100))
mediator$Food <- mediator$Exercise * 3 + rnorm(N)
## Cholesterol decreases with Exercise (-1) but increases with Food
mediator$Cholesterol <- mediator$Food * 2 - mediator$Exercise + rnorm(N)


m_mediator_adjust <- lm(Cholesterol ~ Exercise + Food, data = mediator)
m_mediator_no_adjust <- lm(Cholesterol ~ Exercise, data = mediator)

S(m_mediator_adjust)
S(m_mediator_no_adjust)
## The estimate from the last model is what we would expect for the total effects:
## 3 * 2 - 1, where the 3 * 2 is the usual rule of product along paths.
