# final project code
# prepared by Annika Meurs and Roland Abi
# file started on 04/04/2023

# prep ----------------------------------------------------------------------
library(tidyverse)

county_stat <- read_csv("county.csv")

# data cleaning -------------------------------------------------------------
names(county_stat) #reading the column names and numbers
county_stat_clean <- county_stat %>% #selecting relevant columns
  select(2,3,9,10,19:27,32,34:43,46:51) %>%
  rename("Donald_Trump" = "percentage20_Donald_Trump", 
         "Joe_Biden" = "percentage20_Joe_Biden") %>%
  drop_na() #dropping all NA
colSums(is.na(county_stat_clean)) #checking to see if all NA dropped

pct<- county_stat_clean %>% #getting the percentage of men and women
  mutate(pct_men=
    Men/VotingAgeCitizen, 
    pct_women = 
      Women/VotingAgeCitizen,
    pct_employed = Employed/VotingAgeCitizen) %>%
  mutate( #Multiplying by 100 to make percentages
    Donald_Trump = Donald_Trump * 100, 
    Joe_Biden = Joe_Biden * 100, 
    pct_employed = pct_employed * 100, 
    pct_men = pct_men * 100, 
    pct_women = pct_women * 100
  )

county_stat_sml <- pct %>%
  mutate(
    #filtering for only White and POC
    POC = (Hispanic + Black + Native + Asian + Pacific), 
    #filtering for office jobs
    Professional_Office = (Professional + Office), 
    #filtering for manual labor jobs
    Manual_labor = (Production + Construction), 
    #filtering for those who drive and others
    Other_transp = (Carpool + Transit + Walk + OtherTransp)) %>%
  select (1:4, 8, 14, 16, 20, 30:37)
#creating a data set where it shows if Trump of Biden won
votes <- county_stat_sml %>%
  mutate(
  results = case_when(Joe_Biden > 50 ~ "Biden Won", 
                      Joe_Biden < 50 ~ "Trump Won"))


#aggregating the states together
state_stat <- aggregate(county_stat_sml[ , 3:15], by = list(county_stat_sml$state), FUN = mean)
#renaming group1 to be state
state_stat <- rename(state_stat, state = Group.1)
state_stat <- state_stat %>% #adding results to state state dataset
  mutate(
    results = case_when(Joe_Biden > 50 ~ "Biden Won", 
                        Joe_Biden < 50 ~ "Trump Won"))


#grouping states by highest percentage of votes for Trump/Biden


#creating smaller data sets of individual demographics to better visualize
gender <- pct %>%
  select(1:4,31,32)
race <- pct %>% 
  select(1:4, 7:12)
employment <- pct %>%
  select(1:4, 15:19)
poverty <- pct %>%
  select(1:4,14)
transp <- pct %>%
  select(1:4, 20:24)
work  <- pct %>% 
  select(1:4, 26:30)
  
# visualizing the correlation --------------------------------------------------

library(visdat)
vis_dat(county_stat_clean) 
county_stat_clean %>% select_if(is.numeric) %>% vis_cor()
library(skimr)
skim(county_stat_clean)

#visualizing the correlation in smaller plots to see the correlation better
e <- employment %>% select_if(is.numeric) %>% vis_cor()

g <- gender %>% select_if(is.numeric) %>% vis_cor()

r <- race %>% select_if(is.numeric) %>% vis_cor()

p <- poverty %>% select_if(is.numeric) %>% vis_cor()

t <- transp %>% select_if(is.numeric) %>% vis_cor()

w <- work %>% select_if(is.numeric) %>% vis_cor()

# Ommiting missing observations ----------------------------------------------
votes <- votes %>%
    select(everything()) %>%
    na.omit()
  
# Extracting variables for model ---------------------------------------------
datamodel <- votes %>%
  select(state,
    Donald_Trump, Joe_Biden, White, Drive, pct_men, pct_women,
    POC, Professional_Office, Manual_labor, Other_transp
  ) %>%
  rename(
    "men" = pct_men,
    "women" = pct_women
  )

view(datamodel)
