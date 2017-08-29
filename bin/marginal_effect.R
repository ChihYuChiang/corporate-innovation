"
Set up
"
library(tidyverse)
library(data.table)
library(modelr)
library(stringr)

options(digits=4)


#--Read in csvs
df1 <- fread("../data/df1.csv") %>% select(-Y2)
df2 <- fread("../data/df2.csv")


#--Function to get point estimates and standard errors
instant_effect <- function(model, target_var, on_var){
  #Get interaction term name
  int.name <- if(paste(c(target_var, on_var), collapse=":") %in% names(model$coef)) {
    paste(c(target_var, on_var), collapse=":")
  } else {
    paste(c(on_var, target_var), collapse=":")
  }
  
  #Store coefficients and covariance matrix
  beta.hat <- coef(model)
  cov <- vcov(model)
  
  #Possible set of values for on_var
  if(class(model)[[1]] == "lm"){
    on_values <- seq(min(model$model[[on_var]]), max(model$model[[on_var]]))
  } else {
    on_values <- seq(min(model$data[[on_var]]), max(model$data[[on_var]]))
  }
  
  #Calculate instantaneous effect
  instant_effect <- beta.hat[[target_var]] + beta.hat[[int.name]] * on_values
  
  #Calculate standard errors for instantaeous effect
  instant_effect.se <- sqrt(cov[target_var, target_var] +
                              on_values^2 * cov[int.name, int.name] +
                              2 * on_values * cov[target_var, int.name])
  
  #combine into data frame
  data_frame(on_values = on_values,
             effect = instant_effect,
             se = instant_effect.se)
}




"
Model
"
lm_1 <- lm(Y1 ~ . + (x13 + x16 + x19) * x10, data=df1)
lm_2 <- lm(y47 ~ . + (x3 + x12) * x17, data=df2)

summary(lm_1)




"
Graph
"
#--Discrete plot
instant_effect(lm_1, target_var="x19", on_var="x10") %>%
  ggplot(aes(on_values, effect,
             ymin=effect - 1.96 * se,
             ymax=effect + 1.96 * se)) +
  geom_pointrange() +
  geom_hline(yintercept=0, linetype=2) +
  labs(title="Marginal effect of x19",
       subtitle="By x10",
       x="x10",
       y="Estimated marginal effect")


#--Line plot
instant_effect(lm_1, target_var="x10", on_var="x13") %>%
  ggplot(aes(on_values, effect)) +
  geom_line() +
  geom_line(aes(y=effect - 1.96 * se), linetype=2) +
  geom_line(aes(y=effect + 1.96 * se), linetype=2) +
  geom_hline(yintercept=0) +
  labs(title="Marginal effect of x13",
       subtitle="By x10",
       x="x10",
       y="Estimated marginal effect")




"
Stat
"
#--Test instant effects on different on_var levels
linearHypothesis(lm_2, "age + 12 * age:educ")
linearHypothesis(lm_2, "age + 13 * age:educ")
linearHypothesis(lm_2, "age + 14 * age:educ")
linearHypothesis(lm_2, "age + 15 * age:educ")
linearHypothesis(lm_2, "age + 16 * age:educ")
linearHypothesis(lm_2, "age + 17 * age:educ")
linearHypothesis(lm_2, "age + 18 * age:educ")