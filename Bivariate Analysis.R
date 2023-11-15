library(descr)
library(readxl)
library(dplyr)
require(writexl)
library(writexl)
library(ggplot2)

install.packages("frequency")

install.packages("writexl")
Needs<-read.csv("/Users/willlivingston/Downloads/demographics and health literacy score data final.xlsx - Sheet1.csv")
View(Needs)

##Bivariate Analysis 
by(Needs$health_literacy_score, Needs$race, mean, na.rm = TRUE)
by(Needs$health_literacy_score, Needs$gend_id, mean, na.rm = TRUE)
by(Needs$health_literacy_score, Needs$employment_status, mean, na.rm = TRUE)
by(Needs$health_literacy_score, Needs$educ, mean, na.rm = TRUE)
by(Needs$health_literacy_score, Needs$tobacco_use, mean, na.rm = TRUE)
by(Needs$health_literacy_score, Needs$poverty, mean, na.rm = TRUE)

ggplot(data=Needs)+
  geom_boxplot(aes(x=race, y=health_literacy_score))+
  ggtitle('Association Between Race & Health Literacy Score')

ggplot(data=Needs)+
  geom_boxplot(aes(x=gend_id, y=health_literacy_score))+
  ggtitle('Association Between Gender & Health Literacy Score')

ggplot(data=Needs)+
  geom_boxplot(aes(x=employment_status, y=health_literacy_score))+
  ggtitle('Association Between Employment Status & Health Literacy Score')

ggplot(data=Needs)+
  geom_boxplot(aes(x=educ, y=health_literacy_score))+
  ggtitle('Association Between Education & Health Literacy Score')

ggplot(data=Needs)+
  geom_boxplot(aes(x=tobacco_use, y=health_literacy_score))+
  ggtitle('Association Between Tobacco Use & Health Literacy Score')

ggplot(data=Needs)+
  geom_boxplot(aes(x=poverty, y=health_literacy_score))+
  ggtitle('Association Between Poverty & Health Literacy Score')
