---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: This project illustrates the top 10 cities in California that donated money to Trump and Clinton.
draft: false
image: challenge2.png
keywords: ""
slug: ipsum
title: Trump vs. Clinton
---
The goal of this project was to illustarte the top 10 cities in California that donated most money to Trump and Clinton.
First I saved two different dataframes CA_contributor and zipcodes in new varibales and merged them via left_join as one dataframe as CA_contributors only listed the zipcodes while the other one assigned the zipcodes to the cities. But first I had to transform the zipcodes into doubles to match the format of the zipcodes in the CA_contributor.


```{r, Summary_Clinton}


CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))
zipcodes_2016 <- vroom::vroom(here::here("data","zip_code_database.csv"))

library(scales)
library(patchwork)

#changing zip codes to double and joining with CA_contributors_2016 dataset
zipcodes_2016 <- zipcodes_2016 %>%
    mutate(zip=as.double(zip))
mix <- left_join( CA_contributors_2016, zipcodes_2016, by="zip")
``` 

I now filtered for Trump and Clinton respectively and made a list of the top 10 cities that donated the most money.
 
```{r, Summary_Trump}  

#Summarising contribution amounts for Hillary Clinton
CA_Clinton <- mix %>%
      filter(cand_nm=="Clinton, Hillary Rodham")%>%
      group_by(primary_city) %>%
      summarise(totalAmount=sum(contb_receipt_amt)) %>%
      arrange(desc(totalAmount))%>%
      mutate(primary_city = fct_reorder(primary_city, totalAmount)) %>%
      head(10)
      
  CA_Clinton
``` 
 
```{r, Summary_Trump}  

#Summarising contribution amounts for Hillary Clinton
  CA_Trump <- mix %>%
      filter(cand_nm=="Trump, Donald J.")%>%
      group_by(primary_city) %>%
      summarise(totalAmount=sum(contb_receipt_amt)) %>%
      arrange(desc(totalAmount))%>%
      mutate(primary_city = fct_reorder(primary_city, totalAmount)) %>%
      head(10)
      
  CA_Trump
```  
I now plotted both graphs of Trump and Clinton through the patchwork library next to each other.

```{r, fig3, fig.height = 5, fig.width = 10}

#Creating a bar plot for Clinton in descending order of contribution raised 
p1 <- ggplot(CA_Clinton, aes(x=totalAmount, y=primary_city)) + 
  geom_col(fill="#3182bd", size=0.30)  + 
  theme_bw() +
  facet_grid(~"Clinton, Hillary Rodham")+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + 
  scale_x_continuous(labels=dollar)

#Creating a bar plot for Trump in descending order of contribution raised  
p2 <- ggplot(CA_Trump, aes(x=totalAmount, y=primary_city)) + 
  geom_col(fill="#d73027", size=0.30) + 
  theme_bw() +
  facet_grid(~"Trump, Donald J.")+
  theme (axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + 
  scale_x_continuous(labels=dollar)

#Using the patchwork library to display the plots side by side
wrap_plots(p1,p2) + labs(x="Amount raised") + theme(axis.title.x = element_text(face="bold", hjust=-0.41)) + plot_annotation("Where did candidates raise most money?") 

```

The two graphs show clearly that Clinton overall received more money from California indicating a majority of democrats. Moreover, Trump and Clinton recieved most money from Los Angeles and San Diego and San Franscisco and Los Angeles respectively, which proves that large/highly populated citites donate more money than smaller ones.  



