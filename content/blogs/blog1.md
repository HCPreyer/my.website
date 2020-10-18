---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: This project illustrates the top 10 cities in California that donated money to Trump and Clinton.
draft: false
image: corona.jpg
keywords: ""
slug: ipsum
title: COVID-19 Public Use Data
---

```{r, cache=TRUE}
url <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"
covid_data <- vroom::vroom(url)%>%
  clean_names()
```
Given the data we have, we will produce two graphs that show death % rate, first by age group, sex, and whether the patient had co-morbidities or not.

```{r, cache=TRUE, fig.width=10}
covid_graph <- covid_data %>% 
select(medcond_yn, death_yn, sex, age_group) %>%
  filter(!medcond_yn %in% c("Missing", "Unknown", "Other", NA))%>% 
  filter(!sex %in% c("Missing", "Unknown", "Other", NA)) %>% 
  filter(!age_group %in% c("Missing", "Unknown", "Other", NA)) %>% 
  filter(!death_yn %in% c("Missing", "Unknown", "Other", NA)) %>% 
  mutate(death_yn = ifelse(death_yn == "Yes", 1, 0))%>% 
  mutate(medcond_yn = ifelse(medcond_yn == "Yes", "With comorbidities", "Without comorbidities")) %>% 
  group_by(age_group, sex, medcond_yn) %>% 
  summarise(death = prop(death_yn))
ggplot(covid_graph, aes(x = (death * 100), y = age_group)) +
  geom_col(fill = "dark blue") +
  facet_grid(rows = vars(medcond_yn), cols = vars(sex)) +
  geom_text(aes(label = round(death * 100, 1)), position = position_dodge(width = 1), hjust = -0.1) +
  labs(title = "Covid death % age group, sex and presence of co-morbidities",
       y = "", 
       x = "Percentage of Deaths in %",
       caption = "Source: CDC") +
  theme_bw()
```
And second, by age group, sex, and whether the patient was admitted to Intensive Care Unit (ICU) or not.

```{r}
covid_graph2 <- covid_data %>% 
select(icu_yn, death_yn, sex, age_group) %>%
  filter(!icu_yn %in% c("Missing", "Unknown", "Other", NA))%>% 
  filter(!sex %in% c("Missing", "Unknown", "Other", NA)) %>% 
  filter(!age_group %in% c("Missing", "Unknown", "Other", NA)) %>% 
  filter(!death_yn %in% c("Missing", "Unknown", "Other", NA)) %>% 
  mutate(death_yn = ifelse(death_yn == "Yes", 1, 0))%>% 
  mutate(icu_yn = ifelse(icu_yn == "Yes", "With comorbidities", "Without comorbidities")) %>% 
  group_by(age_group, sex, icu_yn) %>% 
  summarise(death = prop(death_yn))
ggplot(covid_graph2, aes(x = (death * 100), y = age_group)) +
  geom_col(fill = "dark orange") +
  facet_grid(rows= vars(icu_yn), cols = vars(sex)) +
  geom_text(aes(label = round(death * 100, 1)), position = position_dodge(width = 1), hjust = -0.1) +
  labs(title = "Covid death % age group, sex and presence of co-morbidities",
     y = "", 
     x = "Percentage of Deaths in %",
    caption = "Source: CDC") +
  theme_bw()