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

data <- read.csv("RandomizedFinalSD.csv", header = TRUE)

data$Year <- as.factor(data$Year)
data$College <- as.factor(data$College)
data$index <- 1:nrow(data)

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






