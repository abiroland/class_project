# class_project
Predicting candidate percentage votes from state and county level data

---
Predicting the Role of County and State Demographics in the 2020 US Presidential
  Election
author: "Annika Meurs and Roland Abi"
date: "2023-05-08"
output:
  html_document:
    df_print: paged
  pdf_document: default
editor_options:
  markdown:
    wrap: 72
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

library(tidyverse)
library(ggpubr)
library(viridis)
library(geofacet)
library(rsample)
library(broom)
library(kableExtra)

source("../code/data_cleaning.R")
source("../code/modeling.R")
source("../code/exploratory_analysis.R")
```

# Abstract

This research project examines the relationship between county and state
demographic variables and the result of the 2020 presidential election
in those respective regions. Using a data set from Kaggle, we analyzed
the correlation between population race, occupation, method of
transportation, and gender. In this study, we used the data to find the
correlation between the variables and visualize the correlation using
`ggplot`. We additionally carried out a multiple linear regression
analysis to estimate the factors that led to an increase or decrease in
total percentage votes for each candidate. We concluded that within our
data set there is a significant correlation between a high percentage of
the population identifying as white and Donald Trump winning the
majority vote. We also observed that women were more likely to vote for
Joe Biden than Donald Trump. Meanwhile total percentage vote for Joe
Biden decreased by (0.1%) professionals, (8%) manual labor and (1%) men
respectively.

# Introduction

This project analyzes how state and county demographics influenced the
2020 election. Research indicates that voters are often moved by
candidate policies and a social connection to the candidate. We were
curious to investigate whether population demographics also played an
important role. Specifically, we looked at whether population race,
gender, method of transportation, and occupation had any effect on
election results.

Leighley and Nagler argue that voter preferences are influenced by
varying affiliations from cultural, economic, and social and even
religious affiliations. Voters tend to identify with candidates whose
policies are skewed toward their needs. Often this results in racial,
geographic, and religious variations. Studies have found that especially
in the US presidential elections, wealthy people have consistently
participate more in elections than poor people and that women are now
more likely to vote than men (Jan E. Leighley & Jonathan Nagler, 2013).

It is important to highlight the role people's perspective and
socio-economic desires on their voting choices. Several scholars argue
that socio-economic desires played a large role in determining the
results of the 2016 election. Studies have found a positive relationship
between rapid immigration/demographic change and voting for far right
candidates (Maggio, 2021). Hill, S. J., Hopkins, D. J., & Huber, G. A.
(2019) conducted a study on election results in of Florida, Georgia,
Michigan, Nevada, Ohio, Pennsylvania, and Washington and concluded that
immigration and demographic change were a strong determinant of the
results of the election in those regions.

The articles by Leighley and Nagler, Maggio, and Hill, Hopkins and Huber
emphasize the importance of candidate policies on election results.
However, we wanted to understand how demographics correlated with
election results. In a study similar to our research project, Andrew Van
Dam analyses the relationship between counties in the United States
where Trump won the majority and demographic variables. Using data
collected by Clio Andris and Xiaofan Liang, Van Dam finds that neither
do education levels, age, race, or income are correlated with a high
percentage of votes for Donald Trump. Van Dam concludes that the only
factor that is directly correlated with voter preferences is driving,
specifically the percentage of people who commute to work by car, Van
Dam concludes. According to the data 83% of workers drive to work
nationally, but in counties that voted for Biden only 80% commute by car
compared to 90% in Trump counties.

# Initial hypotheses

## Hypothesis 1:

-   There is no difference in voting choices across gender.

## Hypothesis 2:

-   There is no difference in voting choices across races in each
    states.

## Hypothesis 3:

-   There is is no association between occupation and voting choice.

## Hypothesis 4:

-   Mode of transportation will not influence voting choices.

# Data preparation

We conducted our research on a data set we found on Kaggle that used
data from FiveThirtyEight. This data set compiled data from the 2016 and
2020 presidential election, COVID cases and deaths, and demographic
variables by county in the United States. The data set included data on
all states except Alaska and including the District of Columbia. Before
cleaning, the data set had 4867 observations and 51 variables, including
NA observations.

Because we wanted to focus on demographic variables, specifically race,
gender, method of transportation, and occupation, we selected for those
variables. We then converted the Biden and Trump election results and
the gender variables into percentages so they would be comparable
against the other variables in the data set. In an effort to create a
smaller data set that was easier to analyze we aggregated variables. We
aggregated racial demographics into two variables, people of color,
including Black, Asian, Native and Pacific, and white. This adjustment
enabled us to compare the percentage of the white population against the
percentage of people of color. While aggregating the non-white
population into one category, ignores the variability between races, it
allows to clearly see the effect of the whiteness on election results.

We additionally aggregated occupation into two categories. We combined
professional and office work into one variable and construction,
production, and service occupations into a Manual Labor variable. We did
the same with methods of transportation by aggregating carpool, transit,
walk, and other into a variable we named Other Transportation.

After aggregating we were able to condense our data set into 3088
observations and 16 variables. This version of the data set was much
more manageable however, it was necessary to add that showed the states
and counties that Trump won and the ones that Biden won. We used
`case_when` to create a new variable that said Biden won when the the
percentage of votes for Biden were over 50%. The counties where less
than 50% of the votes went to Biden were labeled Trump won.

# Exploratory Data Analysis Select important graphs and numerical

We began our exploratory data analysis by focusing on the six states
where Trump won the highest percentage of votes, Oklahoma, Nebraska,
Tennessee, Missouri, Kansas, and Wyoming. In 2020, 78% of the population
in Oklahoma and Nebraska and 75% in Tennessee, Missouri, Kansas, and
Wyoming voted for Donald Trump. We chose to look at these states because
we wanted to see how much of a role demographic variables played in the
substantial support for Trump.

```{r, echo=FALSE, fig.align='center'}
plot3
```

This density plot shows the percentage of the population who identify as
white in Kansas, Missouri, Nebraska, Oklahoma, Tennessee, and Wyoming
and the election results in each county. As you are able to see in the
plot, the counties where Trump won had a higher white population than
the counties where Biden won.

```{r, echo=FALSE, fig.align='center'}
plot5
```

This scatter plot illustrates the relationship between the percentage of
the population that drives and election results. For this graph we
decided to include the six states where Biden won the largest percentage
of votes as a point of comparison against the six states where Trump the
highest majority. This data visualization shows that there is not a
clear difference in the percentage of the population that drives when
comparing the states that voted predominantly for Trump against those
who voted for Biden. The average driving population for all twelve
states hovers around 80%. This Van Dam's conclusion that there is a
positive correlation between the percentage of drivers and votes for
Donald Trump.

```{r, echo=FALSE, fig.align='center'}
plot4b
```

This scatter plot demonstrates the relationship between employment and
election results. In this graph we can see that in counties where Biden
won, a higher percentage of the population is employed as a professional
or office worker. The percentage of professional and office workers is
slightly less in counties where Trump won.

# Statistical analysis

For our project we developed a multiple linear regression analysis to
estimate the voting percentage for each candidate in 2020 presidential
election, based on the state demographics. Our model provided answers to
the following hypothesis:

### Hypothesis 1:

-   There is no difference in voting choices across gender: our results
    showed that more males voted for Trump 65.9% (p-value \<0.05) than
    females. while more females voted for Biden %1 (p-value \<0.05) than
    Trump.

### Hypothesis 2:

-   There is no difference in voting choices across races in each
    states: It was observed that a percentage increase in the number of
    votes by people of color led to a 48.3% (p-value \<0.05) decrease in
    the total percentage vote for Trump, While Biden's total votes
    experience a 3% (p-value \<0.05) increase for a percentage increase
    in the vote by people of color. Hence there is a difference in
    voting patterns across races in each states.

### Hypothesis 3:

-   There is is no association between occupation and voting choice. Our
    analysis showed that in both the Trump and Biden model there's an
    association between voting choice and manual labor. Total percentage
    of votes for Trump increased by 127.9% (p-value \<0.05) and
    decreased by 8% (p-value \<0.05) for Biden, for a percentage
    increase in votes for those electorates whose occupation are
    classified as manual labor.

### Hypothesis 4:

-   Mode of transportation will not influence voting choices: In the
    Trump model, we observed that percentage of votes by electorates who
    utilize public transportation system and drive, led to a 54.1%
    (p-value \<0.05) and 29.2% (p-value \<0.05) decrease respectively,
    in the total percentage of votes for Trump. While total percentage
    of votes for Biden increased by 2% (p-value \<0.05) for people who
    drive, and 4% (p-value \<0.05) for people who use public
    transportation. Consequently, we will conclude that mode of
    transportation influences voting choices.

\newpage

```{r, echo=FALSE}
table1
```

```{r, echo=FALSE}
table3
```

\newpage

# Summary

The goal of our research is to highlight demographics characteristics at
state and county levels that influences voters choices. We were able to
identify race gender, occupation, and mode of transportation as
important influence in an electorates choice of a candidate. Although,
voters preferences are influenced by many choices across cultural racial
and social-economic lines. As in previous research by Leighley and
Nagler, indeed voters preferences tends to be skewed towards a candidate
whose policies correlates with their needs. How study found that
majority of women were more likely to vote for Biden than for Trump,
this could be due to Biden's Affordable Care Act (ACA) that would allow
individuals to buy into a Medicare-like program.

Furthermore, the issue of racial inequality, has always cause a divide
across varying spectrum (Maggio, 2021). Biden's criminal justice reform
and immigration policies would have resonated with the people of color,
who are often times victims of the system. As highlighted in our study,
votes by people of color led to a 3% increase in the total percentage
votes for Biden. Trump's policy on the other hand which proposed to
limit legal immigration, particularly through family-based and diversity
visas would have pit him at odds with voters who identify as people of
color. Our findings corroborates those of Hill et'al(2019) which
reported immigration and demographic changes as a strong determinant of
the results of an election.

Our research highlighted mode of transportation, i.e. public
transportation or private to have influenced the total percentage of
votes by a candidate; however was in disagreement with findings by
Andrew Van Dam who reported that majority voters who commute to work by
car were more likely to vote for Trump. Our study on the other hand
found votes by people who drive to have decrease the total percentage
votes by Trump. This discordance could have been due to factors such as
the sample size, or method of analysis. Our study is in no way
comprehensive, several factors could stimulate voters loyalty towards a
particular candidate that may not have been captured in our study.
Giving our study has mainly focused on state and county level data,
further research could be undertaking to compare a candidates preference
and popularity at the state, county and individual level.

# References

Hill, S. J., Hopkins, D. J., & Huber, G. A. (2019). Local demographic
changes and US presidential voting, 2012 to 2016. *Proceedings of the
*National Academy of Sciences, 116(50), 25023-*
*25028.https://doi.org/10.1073/pnas.1909202116*

Jan E. Leighley & Jonathan Nagler. (2013). Who Votes Now?:
*Demographics, Issues, Inequality, and Turnout in the United States.*

Maggio, C. (2021). Demographic change and the 2016 presidential
election. *Social Science Research,*
*95,102459.https://doi.org/10.1016/j.ssresearch.2020.102459*

Van Dam, A. (2022, December 8). Analysis The Most Common Restaurant
Cuisine in Every State, and a Chain-Restaurant Mystery. *The Washington*
*Post.https://www.washingtonpost.com/business/2022/09/29/chain-restaurant-capitals*
