# This aim is to investigate the effect of gender on whether or not someone 
# received a gold medal (when they won a medal) in mixed-gender sports. We 
# hypothesize that men will be more likely to receive a gold medal than women. 

# Load necessary packages
library(lme4)
library(car)
library(ggplot2)
library(ggeffects)

# Run generalized linear mixed effects model
m1 <- glmer(gold ~ sex_c + (1 | id), data = d_mixed, family = binomial("logit"))
anova_result <- Anova(m1, type = 3)
m1

# Save the model and results as R data files in the Results folder
saveRDS(m1, file = "results/glmer_model.rds")
saveRDS(anova_result, file = "results/anova_results.rds")




# Save the bar plot to figs folder
ggsave("figs/fixed_effects_plot_bar.png", plot_fixed_effects_bar, width = 8, height = 6)



