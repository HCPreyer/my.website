---
title: "Climate Change"
date: '2017-10-31T22:26:09-05:00'
description: Lorem Etiam Nullam
draft: no
image: Climate.jpeg
keywords: ''
slug: magna
categories:
- ''
- ''
---

Climate change is an important issue in our lives. We should try to be more aware of our environment and take better care of our world.

# Climate change and temperature anomalies 

```{r load-libraries, include=FALSE}
library(tidyverse)
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
library(infer)
library(scales)
library(tidytext)
library(plotly)

```


To define temperature anomalies we need to have a reference, or base, period which NASA clearly states is the period between 1951-1980.

We run the code below to load the file:
```{r weather_data, cache=TRUE}
weather <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")
```

We have two objectives in this section:

First is to select the year and the twelve month variables from the `weather` dataset. We do not need the others (J-D, D-N, DJF, etc.).
```{r tidyweather}
tidyweather <- weather %>% 
  select(c(1:13))
```
Second is to convert the data frame from wide to 'long' format.
```{r tidyweather_2}
tidyweather <- tidyweather %>% 
  pivot_longer(cols = 2:13, names_to = "Month", values_to = "delta")
```
## Plotting Information

Using `facet_wrap()` to produce a separate scatter plot for each month, again with a smoothing line, we can examine if the effect of increasing temperature is more pronounced in some months.

```{r facet_wrap}
tidyweather$Month = factor(tidyweather$Month, levels = month.abb)
ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  facet_wrap(~Month) +
  theme_bw() +
  labs (
    title = "Warmer Winters",
    subtitle = "Weather Anomalies by Month",
    caption = "Source: NASA",
    x = "Year",
    y = "Temperature Deviation")
```
Yes, based on the charts above, the effect of increasing temperature is more pronounced in a few months. For example, the effect seems more pronounced in the generally winter months of October through April than it does in the generally summer months of May through September. 

It is sometimes useful to group time into different periods to study historical data. For example, we often use decades such as 1970s, 1980s, 1990s, etc. to refer to a period of time. The code below creates a new data frame called `comparison` that groups data into five time periods: 1881-1920, 1921-1950, 1951-1980, 1981-2010 and 2011-present. 

We remove data before 1800 using `filter`. Then, we use the `mutate` function to create a new variable `interval` which contains information on which period each observation belongs to. We can assign the different periods using `case_when()`.

```{r intervals}
comparison <- tidyweather %>% 
  filter(Year>= 1881) %>%
  mutate(interval = case_when(
    Year %in% c(1881:1920) ~ "1881-1920",
    Year %in% c(1921:1950) ~ "1921-1950",
    Year %in% c(1951:1980) ~ "1951-1980",
    Year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"))
```
Now that we have the `interval` variable, we can create a density plot to study the distribution of monthly deviations (`delta`), grouped by the different time periods we are interested in.
```{r density_plot}
ggplot(comparison, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +
  theme_bw() +
  labs (title = "Density Plot for Monthly Temperature Anomalies",
        subtitle = "By Time Interval",
        caption = "Source: NASA",
        y = "Density",
        x = "Delta",
        fill = "Interval")
```
So far, we have been working with monthly anomalies. However, we might be interested in average annual anomalies. We can do this by using `group_by()` and `summarise()`, followed by a scatter plot to display the result. 
```{r averaging}
average_annual_anomaly <- tidyweather %>% 
  group_by(Year) %>%
  summarise(annual_average_delta = mean(delta, na.rm=TRUE)) 
ggplot(average_annual_anomaly, aes(x=Year, y= annual_average_delta))+
  geom_point()+
  geom_smooth() +
  theme_bw() +
  labs (subtitle = "Average Yearly Anomaly",
        title = "Warmer Times",
        caption = "Source: NASA",
        y = "Average Annual Delta")
```

## Confidence Interval for `delta`

Now we will construct a confidence interval for the average annual delta since 2011, both using a formula and using a bootstrap simulation with the `infer` package. Recall that the dataframe `comparison` has already grouped temperature anomalies according to time intervals; we are only interested in what is happening  between 2011-present.
```{r, calculate_CI_using_formula}
formula_ci <- comparison %>% 
  drop_na() %>% 
  filter(interval == "2011-present") %>% 
  group_by(Year) %>% 
  summarise(mean_delta = mean(delta),
            SD_delta = sd(delta),
            count_delta = n(),
            t_critical = qt(0.975, count_delta-1),
            SE_delta = sd(delta) / sqrt(n()),
            margin_delta = t_critical * SE_delta,
            delta_low = mean_delta - margin_delta,
            delta_high = mean_delta + margin_delta)
formula_ci
```

```{r, calculate_CI_using_bootstrap}
boot_delta <- comparison %>% 
  filter(interval == "2011-present") %>%
  specify(response = delta) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
percentile_ci <- boot_delta %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
visualize(boot_delta) + 
  shade_ci(endpoints = percentile_ci,fill = "khaki")+
  labs(subtitle = "Bootstrap CI for Weather Delta",
       title = "Bootstrapping Delta",
       caption = "Source: NASA",
       y = "Count",
       x = "Average Annual Delta") +
  theme_bw()
```
The bootstrap distribution is showing us a simulation of the average annual temperature anomalies for the period 2011 to present. Because there are only eight years and thus eight observations included in that data set, any confidence interval we establish will be very broad. Therefore, bootstrapping allows us to sample with replacement and thus create a larger data set that can establish a narrower confidence interval. The beige area indicated on the sampling distribution above represents the 95% confidence interval based on the bootstrapped sample. The result is that we can say with 95% confidence that the true average annual delta for the period 2011 to present lies between roughly 0.92 and 1.02. 