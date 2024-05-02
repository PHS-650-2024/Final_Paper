# This aim is to investigate the effect of gender on whether or not someone 
# received a gold medal (when they won a medal) in mixed-gender sports. We 
# hypothesize that men will be more likely to receive a gold medal than women. 

# Load necessary packages
library(lme4)
library(car)
library(ggplot2)
library(ggeffects)
library(tidyverse)

# Run generalized linear mixed effects model
m1 <- glmer(gold ~ sex_c + (1 | id), data = d_mixed, family = binomial("logit"))
anova_result <- Anova(m1, type = 3)

# Save the model and results as R data files in the Results folder
saveRDS(m1, file = "results/glmer_model.rds")
saveRDS(anova_result, file = "results/anova_results.rds")

# Predict probabilities for different levels of 'sex_c'
probabilities <- ggpredict(m1, terms = "sex_c")

# Plot the predicted probabilities
aim1_plot <- ggplot(probabilities, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  labs(x = "Sex", y = "Predicted Probability of Gold", color = "Group") +
  ggtitle("Predicted Probabilities of Gold by Sex") +
  theme_minimal()

# Save the bar plot to figs folder
ggsave("figs/aim1_plot_gold.png", aim1_plot, width = 8, height = 6)
