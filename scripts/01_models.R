

# libraries --------------------------------------------------------------------
library(tidyverse)


# model 1 ----------------------------------------------------------------------
mod_1 <- glm(
  dissection ~
    residency +
    years +
    posttraining +
    frequency,
  "binomial",
  hgns
)
summary(mod_1)


# model 2 ----------------------------------------------------------------------
mod_2 <- glm(
  time ~
    as.factor(dissection) +
    resolved +
    residency +
    years +
    posttraining +
    frequency,
  "binomial",
  hgns
)
summary(mod_2)

