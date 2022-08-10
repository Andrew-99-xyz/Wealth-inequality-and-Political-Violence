#Set working directory
setwd("C:/Users/Andrew/Desktop/POLI170A R")

#Load libraries
library(tidyverse)
library(stargazer)
library(readxl)
library(corrplot)
library(qqplotr)

#Load data sets
gini = read.csv("2018_gini_pv_v1.csv") #GINI Coefficient Data set
pv = read.csv("2018_pv_dataset.csv") #Political Violence Data set
re_data = read.csv("religous_ethnic_tension.csv")#Religious/Ethnic Tension Scores Data set

#Clean and merge
pv = pv %>% select(-event_type, -sub_event_type, -actor2, 
                   -region, -country_name, -fatalities)
gini_pv = merge(gini, pv, by = "country")
re_data = re_data %>% select(-Country.Name, -X2018)
re_data = re_data %>% setNames(c("country", "tension_score"))
gini_pv_v2 = merge(gini_pv, re_data, by = "country") #Combined and cleaned data set

#Check if data is normal (summary and visualizing)

#GINI_2018 (Variable of GINI coefficients per country for the year 2018)
summary(gini_pv_v2$GINI_2018)
hist(gini_pv_v2$GINI_2018,
     main = "Distribution of GINI coefficents in 2018", 
     xlab = "Scores", 
     ylab = "Observations (%)")
ggplot(mapping = aes(sample = gini_pv_v2$GINI_2018)) + stat_qq_point(size = 2)
shapiro.test(gini_pv_v2$GINI_2018)

#Total_events (Variable of total number of political violence instances per country in 2018)
summary(gini_pv_v2$Total_events)
hist(gini_pv_v2$Total_events,
     main = "Distribution of Total events in 2018", 
     xlab = "events", 
     ylab = "Observations (%)")
ggplot(mapping = aes(sample = gini_pv_v2$Total_events)) + stat_qq_point(size = 2)
shapiro.test(gini_pv_v2$Total_events)

#tension_score (Variable of the religious/ethnic tension score per country in 2018)
summary(gini_pv_v2$tension_score)
hist(gini_pv_v2$tension_score,
     main = "Distribution of Religious/Ethnic Tension Scores in 2018",
     xlab = "Scores",
     ylab = "Observations (%)")
ggplot(mapping = aes(sample = gini_pv_v2$tension_score)) + stat_qq_point(size = 2)
shapiro.test(gini_pv_v2$tension_score)

#Transform non-normal data

#Total_events transformed into log_total_events (Variable for log of total number of political violence instances per country in 2018)
hist(log(gini_pv_v2$Total_events),
     main = "Distribution of Ln Total events in 2018", 
     xlab = "events", 
     ylab = "Observations (%)")
gini_pv_v2$log_total_events = log(gini_pv_v2$Total_events)
gini_pv_v2$log_total_events[gini_pv_v2$log_total_events == -Inf] = NA
ggplot(mapping = aes(sample = gini_pv$log_total_events)) + stat_qq_point(size = 2)
shapiro.test(gini_pv_v2$log_total_events)

#Analyze correlation between variables
plot(x=gini_pv_v2$log_total_events,y=gini_pv_v2$GINI_2018, 
     xlab = "Ln of total events",
     ylab = "Gini coefficent",
     main = "Scatterplot of Gini Coefficents & Ln of Total Events",
     pch = 21,
     bg = "black")
plot(x=gini_pv_v2$log_total_events,y=gini_pv_v2$tension_score, 
     xlab = "Ln of total events",
     ylab = "Tension Score",
     main = "Scatterplot of Tension Scores & Ln of Total Events",
     pch = 21,
     bg = "black")
cor.test(gini_pv_v2$log_total_events, gini_pv_v2$GINI_2018)
cor.test(gini_pv_v2$log_total_events, gini_pv_v2$tension_score)

#Analyze correlation via multiple linear regression
final_reg = lm(log_total_events~GINI_2018 + tension_score, data = gini_pv_v2)
summary(final_reg)

