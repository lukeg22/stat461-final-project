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
library(rstatix)
library(emmeans)

options(knitr.kable.NA = "")
options(contrasts = c("contr.sum", "contr.poly"))

source("https://raw.github.com/neilhatfield/STAT461/master/rScripts/ANOVATools.R")

data <- read.csv("RandomizedFinalSD.csv", header = TRUE)

data$Year <- as.factor(data$Year)
data$College <- as.factor(data$College)
data$index <- 1:nrow(data)

data$Pick <- log(data$Pick, base = 10)

model <- aov(
  formula = Value ~ College + Year + Pick,
  data = data
)

interactionCheck <- aov(
  formula = Value ~ College * Pick,
  data = data
)

ggplot(
  data = data,
  mapping = aes(
    y = Value,
    x = Pick )
)+
  geom_point(size = 2) +
  geom_smooth(
    inherit.aes = FALSE,
    mapping = aes(x = Pick, y = Value),method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed",
    se = FALSE
  )+
  theme_bw() +
  xlab("Selection Order (Pick)") + ylab("Salary Value")

ggplot(
  data = data,
  mapping = aes(x = Value)
)+ geom_boxplot(
  coef = 1.5
) + theme_void() +
    xlab("Salary Value") +
    theme(
      axis.line.x = element_line(),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 12)
    )

ggplot(
  data = data,
  mapping = aes(
    y = Value,
    x = Pick,
    color = College
  ) )+
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    mapping = aes(y = predict(model)), formula = y ~ x,
    se = FALSE
  )+
  theme_bw() +
  xlab("Selection Order (Pick)") + ylab("Salary Value") +
  labs(
    color = "College"
  )

car::Anova(
  mod = interactionCheck,
  type = 3
)

ggplot(
  data = data.frame(
    residuals = residuals(model),
    fitted = fitted.values(model)
  ),
  mapping = aes(x = fitted, y = residuals))+
  geom_point(size = 2) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  )+ geom_smooth(
    formula = y ~ x,
    method = stats::loess,
    method.args = list(degree = 1),
    se = FALSE,
    size = 0.5
  )+
  theme_bw() +
  xlab("Fitted values (USD)") + ylab("Residuals (USD)")

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
  xlab("Measurement Order") +
  ylab("Residuals")

ggplot(
  data = data,
  mapping = aes(
    x = Pick,
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

outlierDetection <- rstatix::mahalanobis_distance(data[,c("Pick", "Value", "College", "Year")])

outlierDetection <- cbind(
  outlierDetection,
  factor = data$College
)

ggplot(
  data = outlierDetection,
  mapping = aes(
    y = Value,
    x = Pick,
    shape = is.outlier,
    color = factor
  ) )+
  geom_point(size = 3) +
  theme_bw() +xlab("Selection Order (Pick)") +
  ylab("Salary Value") +
  labs(
    color = "College",
    shape = "Potential Outlier"
  )


parameters::model_parameters(
  model = model,
  omega_squared = "partial",
  eta_squared = "partial",
  epsilon_squared = "partial"
) %>%
  dplyr::mutate( 
    Parameter = dplyr::case_when(
      Parameter == "Value" ~ "Salary Value (USD)",
      Parameter == "College" ~ "College",
      TRUE ~ Parameter
    )
  ) %>%
  dplyr::mutate(
    p = ifelse(
      test = is.na(p),
      yes = NA,
      no = pvalRound(p)
    )
  ) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Source", "SS", "df", "MS", "F", "p-value",
                  "Partial Omega Sq.", "Partial Eta Sq.", "Partial Epsilon Sq."),
    caption = "ANOVA Table for NFL Rookie Salary Study",
    align = c('l',rep('c',8)),
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("scale_down", "HOLD_position")
  )

rawPointEst <- dummy.coef(model)
rawPointEst <- unlist(rawPointEst)


names(rawPointEst) <- c(
  "Grand Mean",
  "Alabama", 
  "Auburn", 
  "Clemson", 
  "Florida", 
  "Georgia", 
  "Iowa", 
  "Miami (FL)", 
  "Michigan", 
  "Notre Dame", 
  "Ohio St.", 
  "Oklahoma", 
  "Oregon", 
  "Penn St.", 
  "2019", 
  "2020", 
  "2021", 
  "Pick"
)


data.frame("Estimate" = rawPointEst) %>%
  knitr::kable(
    digits = 2,
    caption = "Unadjusted Point Estimates from the NFL Rookie Salary Study",
    booktabs = TRUE,
    align = "c"
  ) %>%
  kableExtra::kable_styling(
    font_size = 12,
    latex_options = c("HOLD_position")
  )

emmOutKey <- emmeans::emmeans(
  object = model,
  specs = pairwise ~ College,
  adjust = "tukey",
  level = 0.85
)

as.data.frame(emmOutKey$emmeans) %>%
  knitr::kable(
    digits = 4,
    col.names = c("College", "Marginal Mean","SE", "DF",
                  "Lower Bound","Upper Bound"),
    caption = "Marginal Means-Tukey 85\\% Adjustment",
    align = c("l", rep("c", 5)),
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )

as.data.frame(emmOutKey$contrasts) %>%
  knitr::kable(
    digits = 4,
    col.names = c("Comparison", "Difference","SE", "DF",
                  "t Statistic","p-value"),
    caption = "Marginal Means-Tukey 85\\% Adjustment",
    align = c("l", rep("c", 5)),
    booktabs = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = c("HOLD_position")
  )

as.data.frame(
  eff_size(
    object = emmOutKey,
    sigma = sigma(model),
    edf = df.residual(model)
  )
) %>%
  dplyr::mutate(
    ps = probSup(effect.size),
    .after = effect.size
  ) %>%
  dplyr::select(contrast, effect.size, ps) %>%
  knitr::kable(
    digits = 3,
    col.names = c("College Comparison", "Cohen's d", "Probability of Superiority"),
    align = "lccc",
    caption = "Effect Sizes for each College",
    booktab = TRUE
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12,
    latex_options = "HOLD_position"
  )


{r caption="Data frame is now printed using `kable`.",render=lemon_print}

# Limitations

With any research project comes any number of limitations and problems that come up throughout the process of the work. We had a few specific limitations that we needed to address and adapt to work out solutions to get around them.

The first problem we encountered was data collection. At first, our group was planning on using the profootballreference.com website to retreive all of our necessary data. However, we quickly discovered that the salary values that were being used were not just rookie contracts. Since we were working with data from 2014 - 2019 NFL seasons, some of the rookie players in those years had already acquired new contracts that were not their rookie value, thus invalidating any results and conclusions we planned to make in relation to rookie contracts. With this issue, we found another data source spotrac.com, where we were able to acquire the rookie information and salary values (in USD) for all NFL rookies in the 2019 - 2021 NFL seasons. Thus, with a simple change in years being studied and a new data source, we were able to overcome this limitation.

The second problem we encountered was the use of the parametric shortcut for ANOVA. Our original plan was to try a parametric shortcut ANOVA model after checking our assumptions, but once we had created our plots for gaussian residuals assumptions and homoscadesticity, we quickly realized that these two assumptions failed very badly. We then tried to add transformations which only appeared to make the plots worse. Thus, this left us to continue with the nonparametric shortcut.
  
After further considerations, we realized that the nonparametric shortcut also would not work due to our lack of independence of observations, since draft pick order does affect salary value. Thus, we had to make one final switch to include draft pick order into our model, leaving our group with the ANCOVA method as our choice of study. 

Even though we encountered very important limitations through our research, we were able to adapt quickly and make-up the work we needed to complete in order to produce a proper ANCOVA analysis for NFL Rookie Salary Values.

\newpage

# Author Contributions

The authors of this report would like to acknowledge their individual contributions to the report.

+ Mitchell Wentzel contributed to the design of the study, collection of data, analysis of data, and writing of the report.
+ Mason Conrad contributed to the design of the study, collection of data, analysis of data, and writing of the report.
+ Luke Geier contributed to the design of the study, collection of data, analysis of data, and writing of the report.
+ Marquis Gee contributed to the design of the study, collection of data, analysis of data, and writing of the report.

