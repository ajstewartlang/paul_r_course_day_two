library(tidyverse)
library(NHANES)
library(MASS)
library(car)
library(performance)
library(lme4)

NHANES_tidied <- NHANES %>% 
  distinct(ID, .keep_all = TRUE)

NHANES_tidied <- NHANES_tidied %>% 
  filter(!is.na(BMI) & !is.na(AgeMonths) & !is.na(Poverty) & !is.na(Pulse) & !is.na(BPSysAve))

model0 <-  lm(TotChol ~ 1, data = NHANES_tidied) 
model1 <- lm(TotChol ~ BMI + AgeMonths + Poverty + Pulse + BPSysAve, data = NHANES_tidied)
summary(model1)

vif(model1)

check_model(model1)

steplimitsf <- step(model0, scope = list (lower = model0, upper = model1), direction = "forward")
summary(steplimitsf)

model_final <- lm(TotChol ~ BMI + AgeMonths + Pulse + BPSysAve, data = NHANES_tidied)
anova(model1, model_final)            

model_no_BMI <- lm(TotChol ~ AgeMonths + Pulse + BPSysAve, data = NHANES_tidied)
anova(model_final, model_no_BMI)

#### Crime dataset

crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv") %>%
  separate(., col = "City, State", into = c("City", "State")) %>%
  rename(House_price = index_nsa) %>%
  rename(Violent_Crimes = "Violent Crimes")

crime_filtered <- filter(crime, Population < 2000000)

model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)

anova(model1, model2)

check_model(model2)

model_lmm <- lmer(Violent_Crimes ~ Population + (1 | City), data = crime_filtered)
summary(model2)
summary(model_lmm)
