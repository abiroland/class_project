# final project code
# prepared by Annika Meurs and Roland Abi
# file started on 12/04/2023

# Prep --------------------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(viridis)
library(geofacet)

# Correlation analysis ----------------------------------------------------
plot1 <- ggpubr::ggarrange(
  r, e, g, t,  
  ncol = 2,
  nrow = 2, 
  common.legend = TRUE
)

#Distribution of votes for Trump ------------------------------------------
plot2 <- state_stat %>%
  group_by(round(Donald_Trump,0)) %>%
  ggplot(aes(x = round(Donald_Trump,0), 
             y = fct_reorder(state, round(Donald_Trump,0)))) +
  geom_point(size = 12, color = "lightblue") +
  facet_geo(~ state, grid = "us_state_grid1", scales = "free") +
  xlim(-50,200) +
  labs(x = "", y = "", 
       title = "Percentage of votes for Donald Trump",
       caption = "`county statistics` dataset from `FiveThirtyFive, Dataworld`") +
  viridis::scale_color_viridis(discrete = T, option = "A", end = 0.7) +
  geom_text(
    aes(label = round(Donald_Trump,0)), color = "red4", 
    size = 5, fontface = "bold", position = position_stack(vjust = 1, reverse = F))+ 
  theme_bw() +
theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title = element_text(size = 24),
  plot.subtitle = element_text(size = 18),
  strip.background = element_rect(color = "white")
)

# Percentage of the who identify as white ------------------------------------
plot3 <- votes %>%
  filter(state %in% c("MO","KS","OK","NE","TN","WY")) %>%
  ggplot(aes(x= White, fill= results)) +
  facet_wrap(~ state, ncol = 3, nrow = 2) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values =c("blue3","red4"))+
  labs(x = "Votes for Donald Trump", 
       y = " ", 
       title = "Percentage of the Population who Identify as White", 
       subtitle = "Among the Counties in the Six States with the highest percentage of votes for Trump", 
       caption = "`county statistics` dataset from `FiveThirtyEight`, Dataworld`") +  
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

# Votes who works in manual labour vs pct_employed
# Votes who works in manual labor vs pct_employed
plot4b <- votes %>%
  ggplot(aes(x = pct_employed, y = Professional_Office, color = state)) +
  geom_point() +
  facet_wrap(~ results) +
  labs(x = "Percent Employed", y = " Professional and Office workers", 
       caption = "`county statistics` dataset from `FiveThirtyFive, Dataworld`", 
       title = "Percentage of voters who are employed as 'Professional' or 'Office' workers 
       when compared to the percent employed") +
  theme_bw() + 
  theme(
    legend.position = "none",
  )

#Percentage of the population who don't drive ----------------------------------
plot5 <- county_stat_sml %>%
  filter(state == "KS"|
           state == "OK"|
           state == "NE"|
           state == "WY"|
           state == "MO"|
           state == "RI"|
           state == "CT"|
           state == "HI"|
           state == "MA"|
           state == "VT") %>%
  mutate(
    Biden = Joe_Biden * 100, 
    Trump = Donald_Trump * 100) %>%
  ggplot(aes(x = Trump, y = Drive, color = state, 
  )) +
  geom_point() +
  facet_wrap(~ (fct_reorder(state, Joe_Biden)), ncol = 5, nrow = 5) +
  labs(x = "Percentage of Votes for Trump", y = " ", 
       caption = "`county statistics` dataset from `FiveThirtyEight`", 
       title = "Comparing the Percentage of the Population Who Drive and Votes for Trump", 
       subtitle = "Among the Counties in the Six States with the most Republican and Democratic Votes") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "none")

