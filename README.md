# Longitudinal-Analysis
Longitudinal Analysis using simulated dataset
library(dplyr)
library(tidyr)

#simulate your own dataset#
  
  num_subj <- 1000
  num_obs <- 10

  
  longitudinal_data <- expand.grid(

    ID = 1:num_subj,
    Time = 1:num_obs
  )

  set.seed(345)
  longitudinal_data <- longitudinal_data %>%
    mutate(
      Age = sample(30:80, num_subj, replace = TRUE)[ID],
      Gender = sample(c("Male", "Female"), num_subj, replace = TRUE)[ID],
      Treatment = sample(c("Placebo", "Drug1", "DrugB"), num_subj, replace = TRUE)[ID],
      Biomarker = rnorm(n(), mean = 100 - (Time*2), sd = 15),
      Outcome = Biomarker + rnorm(n(), mean = 0, sd = 10)
      
    )
  
  head(longitudinal_data)

  
  #Summary statistics#
  
  summary(longitudinal_data) 
  table(longitudinal_data$Treatment)

  #Exploratory data analysis#

  ggplot(longitudinal_data, aes(x = Time, y = Outcome, color = Treatment)) + 
    geom_point() +
    geom_smooth(method = "loess") +
    theme_minimal()

  #The outcome decreases smoothly with time with no drastic changes in the slope#
  
  
  #Distribution of outcome#
  ggplot(longitudinal_data, aes(x = Outcome)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    facet_wrap(~ Treatment) +
    theme_minimal()  
# The graph isn't skewed and appeard to be normally distributed hence the treatment doesn't seem to affect the outcome#
  
  
  #Check for collinearity#
  install.packages("GGally")
  library(GGally)
  ggpairs(longitudinal_data[, c("Age", "Biomarker", "Outcome")]) 
  
  #Biomarkers and outcome have a strong correlation of 0.844#
  
  
  library(car)
  
  # Fit a simple model (without random effects) to check VIF
  lm_model <- lm(Outcome ~ Age + Biomarker + Treatment, data = longitudinal_data)
  vif(lm_model)
  
  install.packages("lmerTest")
  library(lme4)
  library(lmerTest) 

  
  
  #Mixed effects model#
  
  lmm <- lmer(Outcome ~ Time * Treatment + Age + (1 | ID), data = longitudinal_data)
  
  #Since the model suggests no random intercepts, continue with linear regression#
  
  #Linear regression model#
  lmm <- lm(Outcome ~ Time * Treatment + Age + Biomarker, data = longitudinal_data)
  summary(lmm)
  
  
  #Visualize to confirm relation between biomarker and outcome#
  library(ggplot2)
  ggplot(longitudinal_data, aes(x = Biomarker, y = Outcome, color = Treatment)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()
  
  
  
  
