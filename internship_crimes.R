#### application

library(tidyverse)
library(psych)
library(ggplot2)
library(dplyr)

## First, let's do the descriptive statistics

dataset_application %>%
  group_by(gender) %>%
  summarise(describe_rape = list(psych::describe(rape_at_16_or_over, quant = c(.25, .75)))) %>% 
  unnest(cols = c(describe_rape))

## The average number of rapes among man 16 or over in those 5 years is 1.461. 
## The average number of rapes among women 16 or over in those 5 years is 20.393. 


dataset_application %>%
  group_by(gender) %>%
  summarise(describe_sexual_assault = list(psych::describe(sexual_assault_13_over, quant = c(.25, .75)))) %>% 
  unnest(cols = c(describe_sexual_assault))

## The average number of sexual assaults in man 13 or over in those 5 years is 3.332.
## The average number of sexual assault in women 13 or over in those 5 years is 21.230.

## DOMESTIC ABUSE == 1 

dataset_application_domestic <- dataset_application_domestic %>%
  mutate(gender = as.factor(gender))

dataset_application_domestic <- dataset_application%>% filter(domestic_abuse_related == 1)

dataset_application_domestic <- dataset_application_domestic %>%
  mutate(gender = factor(gender, levels = c(0, 1), labels = c("male", "female")))

custom_colors <- c("male" = "blue", "female" = "pink")

## Rape frequency by gender when domestic abuse is the case. 

ggplot(dataset_application_domestic, aes(x = gender, y = rape_at_16_or_over, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Frequency of rape at 16 or over by gender inside home",
       x = "Gender",
       y = "Number of reported rapes at 16 or over") +
  theme_minimal()

## Sexual assault by gender when domestic abuse is the case.

ggplot(dataset_application_domestic, aes(x = gender, y = sexual_assault_13_over, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Frequency of sexual assault at 13 or over by gender inside home",
       x = "Gender",
       y = "Number of reported sexual assaults at 13 or over") +
  theme_minimal()


## DOMESTIC ABUSE == 0 

dataset_application_non_domestic <- dataset_application%>% filter(domestic_abuse_related == 0)
dataset_application_non_domestic <- dataset_application_non_domestic %>%
  mutate(gender = as.factor(gender))
dataset_application_non_domestic <- dataset_application_non_domestic %>%
  mutate(gender = factor(gender, levels = c(0, 1), labels = c("male", "female")))
custom_colors <- c("male" = "blue", "female" = "pink")

## Rape frequency by gender when domestic abuse is not the case. 

ggplot(dataset_application_non_domestic, aes(x = gender, y = rape_at_16_or_over, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Frequency of rape at 16 or over by gender outside home",
       x = "Gender",
       y = "Number of reported rapes at 16 or over") +
  theme_minimal()


## Sexual assault by gender when domestic abuse is not the case.

ggplot(dataset_application_non_domestic, aes(x = gender, y = sexual_assault_13_over, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) + 
  labs(title = "Frequency of sexual assault at 13 or over by gender outside home",
       x = "Gender",
       y = "Number of reported sexual assaults at 13 or over") +
  theme_minimal()


## GENDER == 1 

dataset_application_female <- dataset_application%>% filter(gender == 1)

dataset_application_female <- dataset_application_female %>%
  mutate(gender = ifelse(gender == 1, "female", gender))

dataset_application_female <- dataset_application_female  %>%
  mutate(domestic_abuse_related = ifelse(domestic_abuse_related == 1, "at home", 
                                         ifelse(domestic_abuse_related == 0, "outside home", 
                                                domestic_abuse_related)))

custom_colors_2 <- c("at home" = "pink", "outside home" = "purple")

## Rape frequency by domestic or non domestic abuse when it is a women 

ggplot(dataset_application_female, aes(x = domestic_abuse_related, y = rape_at_16_or_over, fill = domestic_abuse_related)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors_2) + 
  labs(title = "Frequency of rape at 16 or over in women (domestic vs non-domestic)",
       x = "Domestic or non-domestic",
       y = "Number of reported rapes at 16 or over",
       fill = "Location") +
  theme_minimal()


## Sexual assault by domestic or non domestic abuse when it is a women

ggplot(dataset_application_female, aes(x = domestic_abuse_related, y = sexual_assault_13_over, fill = domestic_abuse_related)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors_2) + 
  labs(title = "Frequency of sexual assault at 13 or over in women (domestic vs non-domestic)",
       x = "Domestic or non-domestic",
       y = "Number of reported sexual assaults at 13 or over",
       fill = "Location") +
  theme_minimal()

## GENDER == 0

dataset_application_male <- dataset_application%>% filter(gender == 0)

dataset_application_male <- dataset_application_male %>%
  mutate(gender = ifelse(gender == 0, "male", gender))

dataset_application_male <- dataset_application_male  %>%
  mutate(domestic_abuse_related = ifelse(domestic_abuse_related == 1, "at home", 
                                         ifelse(domestic_abuse_related == 0, "outside home", 
                                                domestic_abuse_related)))

custom_colors_3 <- c("at home" = "blue", "outside home" = "green")

## Rape frequency by domestic or non domestic abuse when it is a man 

ggplot(dataset_application_male, aes(x = domestic_abuse_related, y = rape_at_16_or_over, fill = domestic_abuse_related)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors_3) + 
  labs(title = "Frequency of rape at 16 or over in men (domestic vs non-domestic)",
       x = "Domestic or non-domestic",
       y = "Number of reported rapes at 16 or over",
       fill = "Location") +
  theme_minimal()

## Sexual assault by domestic or non domestic abuse when it is a man

ggplot(dataset_application_male, aes(x = domestic_abuse_related, y = sexual_assault_13_over, fill = domestic_abuse_related)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors_3) + 
  labs(title = "Frequency of sexual assault at 13 or over in men (domestic vs non-domestic)",
       x = "Domestic or non-domestic",
       y = "Number of reported sexual assaults at 13 or over",
       fill = "Location") +
  theme_minimal()

## tendency graphs

dataset_application <- dataset_application %>%
  mutate(
    gender = factor(gender, levels = c(0, 1), labels = c("male", "female")),
    domestic_abuse_related = factor(domestic_abuse_related, levels = c(0, 1), labels = c("outside home", "at home"))
  )

dataset_long <- dataset_application %>%
  gather(key = "variable", value = "value", rape_at_16_or_over, sexual_assault_13_over)

custom_colors_3 <- c("male" = "blue", "female" = "pink")


## graphs

ggplot(dataset_long %>% filter(variable == "rape_at_16_or_over"), 
       aes(x = year, y = value, color = gender, linetype = domestic_abuse_related, group = interaction(gender, domestic_abuse_related))) +
  geom_line(size = 1) +
  scale_color_manual(values = custom_colors_3) +
  labs(title = "Trend of Rape at 16 or Over by Gender and Location",
       x = "Year",
       y = "Number of Reported Rapes at 16 or Over",
       color = "Gender",
       linetype = "Location") +
  theme_minimal()

ggplot(dataset_long %>% filter(variable == "sexual_assault_13_over"), 
       aes(x = year, y = value, color = gender, linetype = domestic_abuse_related, group = interaction(gender, domestic_abuse_related))) +
  geom_line(size = 1) +
  scale_color_manual(values = custom_colors_3) +
  labs(title = "Trend of Sexual Assault by Gender and Location",
       x = "Year",
       y = "Number of Reported Sexual Assaults",
       color = "Gender",
       linetype = "Location") +
  theme_minimal()