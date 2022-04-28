# MITCH WENTZEL
# STAT 461 - Group Project

rm(list = ls())

library(kableExtra)
library(parameters)
library(effectsize)
library(tidyverse)
library(hasseDiagram)
library(knitr)
library(car)
library(psych)
library(dplyr)
library(DescTools)

options(knitr.kable.NA = "")
options(contrasts = c("contr.sum", "contr.poly"))

source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

data <- read.csv("RandomizedFinalSD.csv", header = TRUE)

data$Year <- as.factor(data$Year)
data$College <- as.factor(data$College)
data$index <- 1:nrow(data)

model <- aov(
  formula = Value ~ College + Year,
  data = data
)

# NONPARAMETRIC SHORTCUT

# BASE R
dataModelBase <- kruskal.test(
  formula = Value ~ College,
  data = data,
  na.action = "na.omit"
)

dataModelBase

round(dataModelBase$statistic, digits = 2)
dataModelBase$parameter
round(dataModelBase$p.value, digits = 4)

# COIN PACKAGE
dataModelCoin <- coin::kruskal_test(
  formula = Value ~ College,
  data = data,
  ties.method = "mid-ranks"
)

dataModelCoin

round(coin::statistic(dataModelCoin), digits = 2)
dataModelCoin@statistic@df
round(coin::pvalue(dataModelCoin), digits = 4)

# EFFECT SIZE
EffectSize <- rcompanion::epsilonSquared(
  x = data$Value,
  g = data$College,
  digits = 4
)

EffectSize

# BLOCK EFFICIENCY
block.RelEff(
  aov.obj = model,
  blockName = "Year",
  trtName = "College"
)

# POINT ESTIMATES
pEst <- dummy.coef(model)
pEst <- unlist(pEst)
names(pEst) <- c(
  "Grand Mean",
  levels(data$Year),
  levels(data$College)
)
data.frame("Estimate" = pEst) %>%
  knitr::kable(
    digits = 3,
    caption = "Point Estimates from NFL Rookie Salary Study",
    booktabs = TRUE,
    align = "c"
  ) %>%
  kableExtra::kable_styling(
    font_size = 12,
    latex_options = c("HOLD_position")
  )




