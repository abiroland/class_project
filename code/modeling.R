# modeling in final project assignment
# Prepared by: Annika and Roland
# File started on: 2023-04-22

# prep ----------------------------------------------------------------------
library(tidyverse)
library(rsample)
library(broom)
library(ggpubr)
library(GGally)
library(kableExtra)

#checking for model assumption (model1)--------------------------------------

#checking for assumption of normality. Where assumption is not met, 
#we do a transformation of the dataset

histplt <- datamodel %>%
  ggplot(aes(Donald_Trump)) +
  geom_histogram(bins = 8, fill = "midnightblue", color = I("blue")) +
  theme_bw() +
  labs(
    title = "histogram of response variable"
  )

qplt <- ggplot(datamodel, aes(sample = Donald_Trump)) +
  stat_qq() +
  stat_qq_line(color = I("blue")) +
  theme_bw() +
  labs(
    title = "normal plot of response variable"
  )
  
untplt <- ggarrange(histplt, qplt)

#We transform the data since the assumption of normality wasn't meant
#transform dataset 
trndatamodel <- datamodel %>%
  mutate(
    sqDonald_Trump = Donald_Trump*Donald_Trump
  )

tplt1 <- trndatamodel %>%
  ggplot(aes(sqDonald_Trump)) +
  geom_histogram(bins = 8, fill = "midnightblue", color = I("blue"))

tplt2 <- ggplot(trndatamodel, aes(sample = sqDonald_Trump)) +
  stat_qq() +
  stat_qq_line()

tnplt <- ggarrange(tplt1, tplt2)


#model 1: Trump model-------------------------------------------------

modtrump <- lm(sqDonald_Trump ~ POC + Drive + Other_transp + 
                  Professional_Office + Manual_labor + men + women,
                trndatamodel)



#modtrump <- lm(sqDonald_Trump ~ pop_POC + pop_drive + public_transp + 
 #                 Professional_Office + Manual_labor + pop_men + pop_women,
  #              newdata)

#multicollinearity diagnostics
library(car)
vif(modtrump)

#model ouput
tidy1<- tidy(modtrump, conf.int = T, conf.level = .95)

table1 <- tidy1 %>%
  kbl(caption = "Trump model") %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 10)

table1

#model summaries
glance1 <- glance(modtrump)
table2 <- glance1 %>%
  kbl(caption = "Trump model summary") %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 10)


modres1 <- augment(modtrump, data = trndatamodel)
head(modres1)

#Residual analysis model1----------------------------------------------
#plot of residual vs fitted values for mod3
DTresplt1 <- ggplot(modres1, aes( x = sqDonald_Trump,
                                  y = .resid)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title = "Plot1: Residuals vs percentage of votes for DT",
    x = "percentage of votes for DT",
    y = "Residuals"
  ) +
  theme_bw()


DTresplt2 <- ggplot(modres1) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot2: Residuals vs fits for percentage of votes for DT",
    x = "Fitted values of percentage of votes for DT",
    y = "Residuals"
  ) +
  theme_bw()

# plot of residual vs predictor variable (expenses)
ggplot(modres1) +
  geom_point(aes(
    x = POC,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on interaction between white and POC race",
    x = "white*POC",
    y = "Residuals"
  ) +
  theme_bw()

# plot of residual vs predictor variable (drive)
ggplot(modres1) +
  geom_point(aes(
    x = Drive,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on drive",
    x = "drive",
    y = "Residuals"
  ) +
  theme_bw()

# plot of residual vs predictor variable (other means of transportation)
ggplot(modres1) +
  geom_point(aes(
    x = Other_transp,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on other transp",
    x = "other means of transportation",
    y = "Residuals"
  ) +
  theme_bw()


# plot of residual vs predictor variable (professional office)
ggplot(modres1) +
  geom_point(aes(
    x = Professional_Office,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on professional office",
    x = "professional office",
    y = "Residuals"
  ) +
  theme_bw()

# plot of residual vs predictor variable (manual labor)
ggplot(modres1) +
  geom_point(aes(
    x = Manual_labor,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on manual labor",
    x = "manual labor",
    y = "Residuals"
  ) +
  theme_bw()

# plot of residual vs predictor variable (men)
ggplot(modres1) +
  geom_point(aes(
    x = men,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on men",
    x = "men",
    y = "Residuals"
  ) +
  theme_bw()

# plot of residual vs predictor variable (women)
ggplot(modres1) +
  geom_point(aes(
    x = women,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on women",
    x = "women",
    y = "Residuals"
  ) +
  theme_bw()

#qqplot of residuals for mod1
DTresplt3 <- ggplot(modres1, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals") +
  theme_bw()


#Histogram of residuals for mod1
DTresplt4 <- ggplot(modres1, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    x = "Residuals",
    y = "") +
  theme_bw()

resplt1 <- ggarrange(DTresplt1, DTresplt2, DTresplt3, DTresplt4)

#model2: Biden model---------------------------------------------------------


#checking for model assumption (model2)--------------------------------------

#checking for assumption of normality. Where assumption is not met, 
#we do a transformation of the dataset

histplt2 <- datamodel %>%
  ggplot(aes(Joe_Biden)) +
  geom_histogram(bins = 8, fill = "midnightblue", color = I("blue")) +
  theme_bw() +
  labs(
    title = "histogram of response variable"
  )

qplt2 <- ggplot(datamodel, aes(sample = Joe_Biden)) +
  stat_qq() +
  stat_qq_line(color = I("blue")) +
  theme_bw() +
  labs(
    title = "normal plot of response variable"
  )

untplt2 <- ggarrange(histplt2, qplt2)

#We transform the data since the assumption of normality wasn't meant
#transform dataset 
JBdata <- datamodel %>%
  mutate(
    sqrtJB = sqrt(Joe_Biden)
  )

tplt2a <- JBdata %>%
  ggplot(aes(sqrtJB)) +
  geom_histogram(bins = 8, fill = "midnightblue", color = I("blue")) +
  theme_bw()

tplt2b <- ggplot(JBdata, aes(sample = sqrtJB)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw()

tnplt2 <- ggarrange(tplt2a, tplt2b)

bidenmod1 <- lm(sqrtJB ~ POC + Drive + Other_transp + 
                  Professional_Office + Manual_labor + men + women,
                JBdata)

#model summaries
tidy2 <- tidy(bidenmod1, conf.int = T, conf.level = .95)
table3 <- tidy2  %>%
  kbl(caption = "Biden model") %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 10)

vif(bidenmod1)

glance2 <- glance(bidenmod1)
table4 <- glance2  %>%
  kbl(caption = "Biden model summary") %>%
  kable_classic(full_width = F, html_font = "Cambria", font_size = 10)

modres2 <- augment(bidenmod1, data = JBdata)
head(modres2)

#Residual analysis----------------------------------------------------------
JBresplt1 <- ggplot(modres2, aes( x = sqrtJB,
                                  y = .resid)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title = "Plot1: Residuals vs percentage of votes for JB",
    x = "percentage of votes for JB",
    y = "Residuals"
  ) +
  theme_bw()


JBresplt2 <- ggplot(modres2) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot2: Residuals vs fits for percentage of votes for JB",
    x = "Fitted values of percentage of votes for JB",
    y = "Residuals"
  ) +
  theme_bw()

#qqplot of residuals for mod1
JBresplt3 <- ggplot(modres2, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals") +
  theme_bw()


#Histogram of residuals for mod1
JBresplt4 <- ggplot(modres2, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    x = "Residuals",
    y = "") +
  theme_bw()

resplt2 <- ggarrange(JBresplt1, JBresplt2, JBresplt3, JBresplt4)