---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: This project illustrates the change of Trump´s net approval rate since 2017 

draft: false

keywords: ""
slug: aliquam
title: Trump 
image: trump_approval_margin.png
---
The goal of this project was to illustrate the change of the net approval rate (approve -disapprove) of Trump since 2017. 
The first section firstly imports the data and after using the glimpse function that provides a 
snapshot of the data, I transformed character values into dates.

```{r, cache=TRUE}
# Import approval polls data
approval_polllist <- read_csv(here::here('data','approval_polllist.csv'))

# or directly off fivethirtyeight website
# approval_polllist <- read_csv('https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

# Use `lubridate` to fix dates, as they are given as characters.

approval_polllist_clean <- approval_polllist %>%
    filter(subgroup=="Voters")%>%
    mutate(modeldate = mdy(modeldate), 
           startdate = mdy(startdate), 
           enddate=mdy(enddate),
           createddate=mdy(createddate), 
           timestamp=parse_date_time(timestamp, orders="HMSdmy")
           )

glimpse(approval_polllist_clean)

```
I am using ggplot to create the graphs in this part of the code. I caculate the net average
approval rate and further plot the error bars based on the confidence intervals of each data point. Lastly, I facet-wrap the graphs to create an individual plot for each year.

```{r, plot_Weekly_Trump_Approval_Ratings, fig1, fig.width = 12, fig.height=5}

library(aplot)
library(scales)
library(ggThemeAssist)  


plot_trump <- approval_polllist_clean %>%
  mutate (net_approval_rate = (adjusted_approve - adjusted_disapprove), 
          end_week = week(enddate), year =year(enddate)) %>% 
  group_by(year, end_week) %>% 
  summarize(mean_net = mean(net_approval_rate)) %>% 
  ungroup()

#dataset for calculating CI for each week per months
CI_plot <- approval_polllist_clean %>%
  mutate (net_approval_rate = (adjusted_approve - adjusted_disapprove), 
          end_week = week(enddate), year =year(enddate)) %>% 
  group_by(year, end_week) %>% 
  summarize(mean_net = mean(net_approval_rate), 
            std_trump = sd(net_approval_rate), 
            stan_error_trump = std_trump/sqrt(count(end_week)), 
            t_critical = qt(0.975,  count(end_week) -1), 
            lower_CI = mean_net - t_critical*stan_error_trump, 
            upper_CI = mean_net + t_critical*stan_error_trump)


#this graph plots the average points and adds two lines for the ci error bars; each year is depicted in a new graph
ggplot(CI_plot, aes(x=end_week, y=mean_net, color=as.factor(year))) +
  facet_wrap(~year) +
  geom_linerange(aes(ymax = upper_CI , ymin =  lower_CI), size=0) +
  geom_point(size=0.6) +
  geom_hline(yintercept=0, linetype="solid", color = "orange") + 
  geom_line(aes(y = upper_CI))+
  geom_line(aes(y = lower_CI))+
  geom_ribbon(aes(ymin=lower_CI,ymax=upper_CI), alpha=0.3) +
  geom_line() +
  labs(title ="Estimating Net Approval (approve-disapprove) for Donald Trump", 
       subtitle ="Weekly average of all polls", x="Week of year") +
  scale_y_continuous(labels = scales::number_format(digits = 1)) + 
  theme(panel.grid.major = element_line(colour = "whitesmoke"), 
        panel.background = element_rect(fill = "whitesmoke"), 
        legend.position = "none") +labs(y = "Average Net Approval (%) ") +
  scale_x_continuous(breaks= c(0,13, 26, 39, 52)) +
  scale_y_continuous(breaks=c(-20,0, -17.7,-15.0,-12.5,-10.0,-7.5,-5.0,-2.5,0.0,2.5,5.0,7.5)) +
  scale_color_manual(values=c("#FF736C", "#7DAE00", "#00BBBD", "#C47EFF"))
```
This graph shows that Trump was only approved by a higher percentage in the begging of his precidency. Aftwerwards the average net approval decreases to negative numbers, which implies a higher percentage of disapproving than approving. Througout the four years there were a few fluctuations based on decision made that caused more people to disapprove him. The CI-levels for each data point (shows by the top and bottom line) also indicate that the points lay within this range with a 95% confidence, which strengtehns the assumption that Trump was more disapproved than approved. 

 