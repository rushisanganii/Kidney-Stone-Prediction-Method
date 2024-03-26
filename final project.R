###Kidney Stone Method Prediction
library(ggplot2)
library(psych)
library(tidyr)
library(dplyr)
library(brms)
library(readxl)
KData <- read_excel("C:/Users/jasmi/Desktop/MCE DATA SCIENCE/DATA 200 STATISTICAL ANALYSIS FOR DATA SCIENCE/Assignment/kidney_stone_data.xlsx")
View(KData)

# Creating the summary table
summary_table <- KData %>%
  group_by(treatment) %>%
  summarise(
    total_success = sum(success),
    total_cases = n(),
    success_frequency = mean(success)
  )

print(summary_table)

# Creating the summary table stratified by treatment and stone size
summary_table_stratified <- KData %>%
  group_by(treatment, stone_size) %>%
  summarise(
    total_success = sum(success),
    total_cases = n(),
    success_frequency = mean(success)
  )
print(summary_table_stratified)

KData <- KData[,c(1,3)]


###table it
table(KData)
tab <- table(KData)

KData<- KData %>%mutate_at(1:2, as.factor)

### probability of success for treatment A
successA <-tab[1,2]/sum(tab[1,])
successA
successA <- (1-successA)

### probability of success for treatment B
successB <-tab[2,2]/sum(tab[2,])
successB
successB/(1-successB)

###plot it 
library(ggplot2)

ggplot(KData, aes(treatment, fill = success)) + 
  geom_bar(position = "dodge") +  
  theme_minimal() +
  xlab("Treatment") +
  scale_x_discrete(labels = c("0" = "A", "1" = "B")) +
  ylab("Count") +
  labs(fill = 'Success') +
  scale_fill_discrete(name = "Success", labels = c("No", "Yes"))

###Logistic regression
log_reg <- glm(success ~ treatment, data=KData, family="binomial")

###the intercept is statistically significant, but not the slope coefficient
###intercept tell us the log odds of success for treatment A

summary <- summary(log_reg)
summary

#storing coeffs
coeff <- summary$coefficients

#odds for the intercept
exp(coeff[1,1])

#odds for the slop coefficient
exp(coeff[2,1])

# Fit logistic regression model for stone_size predicting success
model_stone_size <- glm(success ~ stone_size, data = KData, family = "binomial")

# Display the summary of the regression model
summary(model_stone_size)

