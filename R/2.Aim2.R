##  ##############################################  ##
##                    Aim 2.                        ##
##  ##############################################  ##

library(knitr)
library(dplyr)
library(tidyverse)
library(janitor)
library(pwr)
library(lme4) 
library(lmerTest)
library(simr)
library(broom.mixed)
library(ggplot2)

# read in the data
d <- readr::read_csv("data/oly_scored.csv")

# fit mixed-effects logistic regression model
m1 <- glmer(gold ~ age + (1 | id), data = d, family = binomial,
            control = glmerControl(optimizer = "bobyqa")) # add control to assist convergence 

# view model output
summary(m1)

# received error message "Model is nearly unidentifiable: very large eigenvalue - Rescale variables?"
d$age_scaled <- scale(d$age) # rescale age

# refit the model with the scaled age
m1_scaled <- glmer(gold ~ age_scaled + (1 | id), data = d, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

# view new model output 
(summary(m1_scaled))

# extracting fixed effects (estimates, standard errors, z-values, and p-values)
aim2_fixed <- tidy(m1_scaled, effects = "fixed") # tidy fixed effects
write.csv(aim2_fixed, "tidy_fixed_effects.csv", row.names = FALSE) # save tidy output

# extracting random effects variances
aim2_random <- tidy(m1_scaled, effects = "ran_pars") # tidy random effects
write.csv(aim2_random, "tidy_random_effects.csv", row.names = FALSE) # save tidy output

# extract summary statistics
aim2_summary_df <- data.frame(
  AIC = AIC(m1_scaled),
  BIC = BIC(m1_scaled),
  logLik = as.numeric(logLik(m1_scaled))  # converting logLik object to numeric
)

# save the model summary
write.csv(aim2_summary_df, "aim2_summary_output.csv", row.names = FALSE)

# Coefficient plot
aim2fig1 <- ggplot(aim2_fixed, aes(x = term, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Fixed Effects Coefficients", y = "Estimate", x = "")

ggsave("figs/aim2fig1.png", plot = aim2fig1, width = 8, height = 6, dpi = 300)