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

# POST HOC NONPARAMETRIC

# DUNNS TEST - BONFERRONI
dunn <- purrr::quietly(dunn.test::dunn.test)(
  x = data$Value,
  g = data$College,
  method = "bonferroni",
  alpha = 0.15,
  kw = FALSE,
  table = FALSE,
  list = FALSE
)$result

knitr::kable(
  x = data.frame(
    comparison = dunn$comparisons,
    pvalues = dunn$P.adjusted
  ),digits = 4,
  caption = "Post Hoc Dunn's Test--Bonferroni Adjustment", # Fill this in
  col.names = c("Comparison", "Adj. p-Value"),
  align = 'lc'
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12,
    latex_options = "HOLD_position"
  )

# DSCF TEST
dscf <- dscfTest(
  response = data$Value,
  factor = data$College
)

knitr::kable(
  x = dscf,
  digits = 3,
  col.names = c("Comparison", "Observed W", "Adj. p-value"),
  caption = paste("Post Hoc-Dwass-Steel-Critchlow-Fligner Tests"),
  align = 'lcc',
  booktabs = TRUE) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed"),
    font_size = 12,
    latex_options = "HOLD_position"
  )

# EFFECT SIZES
kw.PostHoc(
  response = data$Value,
  treatments = data$College
) %>%
  knitr::kable(
    digits = 3,
    caption = "Post Hoc Comparison Effect Sizes",
    col.names = c("Pairwise Comparison","Hodges Lehmann Estimate",
                  "Prob. Superiority"),
    align = 'lcc',
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("condensed", "boardered"),
    font_size = 12,
    latex_options = "HOLD_position"
  )


