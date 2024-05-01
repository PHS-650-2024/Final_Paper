# Load necessary packages
library(readr)
library(tidyverse)

# Set working directory to Final Paper folder
# setwd("Desktop/PHS 650/Final Paper")

# Read in the data
d <- read_csv("data/olympics.csv")

# Create a new, dichotomous "gold" variable from categorical "medal" variable by 
# recoding “medal” character variable to gold = 1, silver or bronze = 0, and no 
# medal = NA.
d$gold <- d$medal

d <- d %>%
  mutate(across(c(gold),
                ~dplyr::recode(.,
                               "Gold" = 1,
                               "Silver" = 0,
                               "Bronze" = 0)))

# Create a centered sex variable 
d$sex_c <- dplyr::recode(d$sex, "M" = -0.5, "F" = 0.5)

# Subset data to only include years 1994 and onward
d1994 <- subset(d, year >= 1994 & !is.na(age) & !is.na(weight) & !is.na(id) & !is.na(gold))

# Create a new 'event_gender' column and update 'event' column
d1994 <- d1994 %>%
  mutate(event_gender = str_extract(event, "(Men's|Women's|Mixed|\\(Men\\)'s)"),
         event = str_remove(event, "(Men's|Women's|Mixed|\\(Men\\)'s)"))

# Only include events where gender = mixed for subsetted data of 1994 onward
d_mixed <- d1994 %>% filter(event_gender == "Mixed")

# Recode gold as factor with levels 1 and 0
d1994$gold <- factor(d1994$gold, levels = c(1, 0))

# Recode id for grouping by athlete
d1994$id <- factor(d1994$id) 

# Export scored dataset with new variables
write.csv(d1994, file = "oly_scored.csv")
