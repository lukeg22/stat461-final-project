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

data %>%
  count(Round)

hist(data$Value)

data$Year <- as.factor(data$Year)
data$College <- as.factor(data$College)
data$index <- 1:nrow(data)

model <- aov(
  formula = Value ~ College + Year,
  data = data
)

# NONPARAMETRIC -------------------------------------

# CAUCHY PLOT
car::qqPlot(
  x = model$residuals,
  distribution = "cauchy",
  envelope = 0.9,
  id = FALSE,
  pch = 20,
  ylab = "Residuals"
)

# INDEPENDENCE
ggplot(
  data = data,
  mapping = aes(
    x = index,
    y = Value,
    color = Year,
    linetype = Year
  )
) +
  geom_point(size = 2) +
  geom_path(
    mapping = aes(group = Year)
  ) +
  ggplot2::theme_bw() +
  xlab("Draft Selection Order") +
  ylab("Salary Value") + 
  ggtitle("Independence of Observations / Blocked by Year")

# ----------------------------------------------------------

# PARAMETRIC ----------------------------------------
# QQ PLOT
car::qqPlot(
  x = model$residuals,
  distribution = "norm",
  envelope = 0.9,
  id = FALSE,
  pch = 20,
  ylab = "Residuals"
)

# HOMOSCADESTICITY
ggplot(
  data = data.frame(
    residuals = model$residuals,
    fitted = model$fitted.values
  ),
  mapping = aes(x = fitted, y = residuals)
) +
  geom_point(size = 2) +
  theme_bw() +
  xlab("Fitted values (USD)") +
  ylab("Residuals (USD)")

# INDEX PLOT - NOT BLOCKED BY YEAR
ggplot(
  data = data.frame(
    residuals = model$residuals,
    index = 1:length(model$residuals)
  ),
  mapping = aes(x = index, y = residuals)
) +
  geom_point(size = 1.5) +
  geom_line() +
  theme_bw() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  xlab("Measurement order") +
  ylab("Residuals")











