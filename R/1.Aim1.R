# This aim is to investigate the effect of gender on whether or not someone 
# received a gold medal (when they won a medal) in mixed-gender sports. We 
# hypothesize that men will be more likely to receive a gold medal than women. 

# Load necessary packages
library(lme4)

# Run generalized linear mixed effect model
m1 <- glmer(gold ~ sex_c + (1 | id), data = d_mixed, family = binomial("logit"))

