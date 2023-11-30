require(readxl)
library(dplyr)
require(writexl)
library(descr)
require(ggplot2)
library(ggplot2)
library(RColorBrewer)
require(plyr)
library(plyr)

needs_subset <- read_excel("Desktop/Fall 2023/Statistical Consulting /needs assessment total.xlsx")

needs_subset <- read_excel("needs assessment total.xlsx")

# joyce

j_vars <- c("id", "white_race", "black_africanam_race", "americanindian_aknative_race", "hawaiian_pacificisland_race", "asian_race", "hispanic_race", "gend_id")
joyce_sub<- needs_subset[ ,j_vars]

joyce_sub %>% 
  mutate(race = ifelse(white_race == "Checked", "White", NA)) -> joyce_sub

joyce_sub$race[joyce_sub$black_africanam_race == "Checked"] <- "Black or African American"
joyce_sub$race[joyce_sub$americanindian_aknative_race == "Checked"] <- "American Indian or Alaska Native"
joyce_sub$race[joyce_sub$hawaiian_pacificisland_race == "Checked"] <- "Hawaiian or Pacific Islander"
joyce_sub$race[joyce_sub$asian_race == "Checked"] <- "Asian"
joyce_sub$race[joyce_sub$hispanic_race == "Yes"] <- "Hispanic/LatinX"

joyce_sub$race <- as.factor(joyce_sub$race)

freq(joyce_sub$race)

freq(joyce_sub$gend_id)

joyce_sub %>% 
  mutate(gend_id2 = ifelse(gend_id %in% c("Agender", "Genderqueer, neither exclusively man nor woman"), "Non-binary", NA)) -> joyce_sub

joyce_sub$gend_id2[joyce_sub$gend_id %in% c("Transgender man", "Transgender woman")] <- "Transgender"
joyce_sub$gend_id2[joyce_sub$gend_id == "Man"] <- "Cisgender man"
joyce_sub$gend_id2[joyce_sub$gend_id == "Woman"] <- "Cisgender woman"

freq(joyce_sub$gend_id2)

vars2 <- c("id", "race", "gend_id2")
joyce_sub2 <- joyce_sub[ , vars2]
names(joyce_sub2) <- c("id", "race", "gend_id")

joyce_sub2$race <- as.factor(joyce_sub2$race)
joyce_sub2$gend_id <- as.factor(joyce_sub2$gend_id)

#write_xlsx(joyce_sub2, "joyce.xlsx")

# will
w_vars <- c("id", "age", "sex", "educ","orientation")
will_sub <- needs_subset[ ,w_vars]

will_sub$age[will_sub$age=='']<-NA
will_sub$sex[will_sub$sex=='']<-NA
will_sub$sex[will_sub$sex=='Decline to answer']<-NA
will_sub$educ[will_sub$educ=='']<-NA
will_sub$educ[will_sub$educ=='Decline to answer']<-NA
#Works Cited: https://bookdown.org/rwnahhas/IntroToR/collapse-a-categorical-variable-into-fewer-levels.html
will_sub$educ[will_sub$educ %in%c('Never attended school or only attended kindergarten','Grades 9 through 11 (Some high school)','Grades 1 through 8 (Elementary/Middle school)')] <- 'Less Than High school'
will_sub$age[will_sub$age=='']<-NA
will_sub$id[will_sub$id=='']<-NA
will_sub$orientation[will_sub$orientation=='']<-NA
will_sub$orientation[will_sub$orientation=='Decline to answer']<-NA
will_sub$orientation[will_sub$orientation %in%c('Not sure or questioning','Other (Please specify): {other_sexual_orientation}')] <- 'Other'

will_sub$educ <- as.factor(will_sub$educ)
will_sub$orientation <- as.factor(will_sub$orientation)
will_sub$sex <- as.factor(will_sub$sex)

#write_xlsx(will_sub,"will.xlsx")

# latonya

l_vars <- c("id", "no_pa", "housing_stable")
latonya_sub <- needs_subset[, l_vars]

latonya_sub$poverty<- ifelse(latonya_sub$no_pa == "Checked", "Not in Poverty", "In Poverty")

latonya_sub <- latonya_sub %>%
  mutate(
    stable_house = ifelse(
      housing_stable %in% c(
        "Staying with friends", "Staying with family", "Rent an apartment/house (alone or with others)",
        "Permanent single-room occupancy hotel", "School dormitory", "Own a home",
        "Other (Please specify): {other_living_situation}"
      ), "Housing Stable",
      ifelse(housing_stable == "Decline to answer", NA, "Housing Unstable")
    )
  )

freq(latonya_sub$stable_house)

latonya_sub$stable_house <- as.factor(latonya_sub$stable_house)
latonya_sub$poverty <- as.factor(latonya_sub$poverty)

latonya_sub2 <- latonya_sub[, c("id", "stable_house", "poverty")]

#write_xlsx(latonya_sub, "latonya.xlsx")

# quinn

q_vars <- c("id", "sub_use_no", "employ", "sub_use_na")
quinn_sub <- needs_subset[ ,q_vars]

quinn_sub$tobacco_use<-ifelse(quinn_sub$sub_use_no == "Checked", "No", "Yes")
quinn_sub$tobacco_use[quinn_sub$sub_use_na=="Checked"]<-NA

freq(quinn_sub$tobacco_use)

#employment
quinn_sub$employment_status[quinn_sub$employ %in% c("Full-time (30 or more hours per week)", "Part-time (fewer than 30 hours per week)")]<-"Employed"
quinn_sub$employment_status[quinn_sub$employ=="Unemployed"]<-"Unemployed"
quinn_sub$employment_status[quinn_sub$employ=="Student"]<-"Not in labor force"
quinn_sub$employment_status[quinn_sub$employ=="Retired"]<-"Not in labor force"
quinn_sub$employment_status[quinn_sub$employ=="Disabled, not able to work"]<-"Not in labor force"
quinn_sub$employment_status[quinn_sub$employ=="Other (Please specify): {other_employment_status}"]<-"Not in labor force"
quinn_sub$employment_status[quinn_sub$employ=="Decline to answer"]<-NA


freq(quinn_sub$employment_status)
freq(quinn_sub$employ)

quinn_sub$tobacco_use <- as.factor(quinn_sub$tobacco_use)
quinn_sub$employment_status <- as.factor(quinn_sub$employment_status)

quinn_sub2 <- quinn_sub[, c("id", "tobacco_use", "employment_status")]

#write_xlsx(quinn_sub, "quinn.xlsx")

# health literacy
vars <- c("id", "need_help_reading_med_materials", "written_trouble_understand", "hearing_trouble_understand", "conf_fill_forms")
hl_sub <- needs_subset[ ,vars]

hl_sub$need_help_reading_med_materials[hl_sub$need_help_reading_med_materials=="Decline to answer"]<-NA
hl_sub$written_trouble_understand[hl_sub$written_trouble_understand=="Decline to answer"]<-NA
hl_sub$hearing_trouble_understand[hl_sub$hearing_trouble_understand=="Decline to answer"]<-NA
hl_sub$conf_fill_forms[hl_sub$conf_fill_forms=="Decline to answer"]<-NA

hl_sub$need_help_reading_med_materials[hl_sub$need_help_reading_med_materials == "Always"] <- 5
hl_sub$need_help_reading_med_materials[hl_sub$need_help_reading_med_materials == "Often"] <- 4
hl_sub$need_help_reading_med_materials[hl_sub$need_help_reading_med_materials == "Sometimes"] <- 3
hl_sub$need_help_reading_med_materials[hl_sub$need_help_reading_med_materials == "Occasionally"] <- 2
hl_sub$need_help_reading_med_materials[hl_sub$need_help_reading_med_materials == "Never"] <- 1

hl_sub$written_trouble_understand[hl_sub$written_trouble_understand == "Always"] <- 5
hl_sub$written_trouble_understand[hl_sub$written_trouble_understand == "Often"] <- 4
hl_sub$written_trouble_understand[hl_sub$written_trouble_understand == "Sometimes"] <- 3
hl_sub$written_trouble_understand[hl_sub$written_trouble_understand == "Occasionally"] <- 2
hl_sub$written_trouble_understand[hl_sub$written_trouble_understand == "Never"] <- 1

hl_sub$hearing_trouble_understand[hl_sub$hearing_trouble_understand == "Always"] <- 5
hl_sub$hearing_trouble_understand[hl_sub$hearing_trouble_understand == "Often"] <- 4
hl_sub$hearing_trouble_understand[hl_sub$hearing_trouble_understand == "Sometimes"] <- 3
hl_sub$hearing_trouble_understand[hl_sub$hearing_trouble_understand == "Occasionally"] <- 2
hl_sub$hearing_trouble_understand[hl_sub$hearing_trouble_understand == "Never"] <- 1

hl_sub$conf_fill_forms[hl_sub$conf_fill_forms == "Not confident at all"] <- 5
hl_sub$conf_fill_forms[hl_sub$conf_fill_forms == "A little bit confident"] <- 4
hl_sub$conf_fill_forms[hl_sub$conf_fill_forms == "Somewhat confident"] <- 3
hl_sub$conf_fill_forms[hl_sub$conf_fill_forms == "Quite a bit confident"] <- 2
hl_sub$conf_fill_forms[hl_sub$conf_fill_forms == "Extremely confident"] <- 1

hl_sub$need_help_reading_med_materials <- as.numeric(hl_sub$need_help_reading_med_materials)
hl_sub$written_trouble_understand <- as.numeric(hl_sub$written_trouble_understand)
hl_sub$hearing_trouble_understand <- as.numeric(hl_sub$hearing_trouble_understand)
hl_sub$conf_fill_forms <- as.numeric(hl_sub$conf_fill_forms)

hl_sub <- na.omit(hl_sub)

hl_sub$health_literacy_score <- rowMeans(hl_sub[, c("need_help_reading_med_materials", "written_trouble_understand", "hearing_trouble_understand", "conf_fill_forms")])


jw <- merge(joyce_sub2, will_sub, by="id")
jwl <- merge(jw, latonya_sub2, by = "id")
jwlq <- merge(jwl, quinn_sub2, by = "id")
all_managed_needs_assessment <- merge(jwlq, hl_sub, by = "id")
write_xlsx(all_managed_needs_assessment, "all managed data.xlsx")

yelena7 <- all_managed_needs_assessment[, c("id", "race", "gend_id", "employment_status", "educ", 
                                            "tobacco_use", "age", "poverty", "need_help_reading_med_materials", 
                                            "written_trouble_understand", "hearing_trouble_understand", "conf_fill_forms")]
yelena7_omit <- na.omit(yelena7)

# after omitting all NAs for all 7 variables, 650 obvs

yelena6 <- all_managed_needs_assessment[, c("id", "race", "gend_id", "employment_status", "educ", 
                                            "tobacco_use", "poverty", "need_help_reading_med_materials", 
                                            "written_trouble_understand", "hearing_trouble_understand", "conf_fill_forms")]
yelena6_omit <- na.omit(yelena6)

# after omitting all NAs for all minus the age variable, 701 obvs


edited6 <- all_managed_needs_assessment[, c("id", "race", "gend_id", "employment_status", "educ", 
                                            "tobacco_use", "poverty", "health_literacy_score")]
edited6_omit <- na.omit(edited6)
write_xlsx(edited6_omit, "demographics and health literacy score data final.xlsx")

#######################################
# DESCRIPTIVE STATS AND VISUALIZATIONS
#######################################
# race
ggplot(needs_final) +
  geom_bar(aes(x = race), fill = "#0473c2") +
  geom_text(aes(x = race, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Race") +
  ylab ("Number of Patients") +
  xlab ("Patient Race") + theme(text = element_text(size=20))

# gender identity
ggplot(needs_final) +
  geom_bar(aes(x = gend_id), fill = "#cd544c") +
  geom_text(aes(x = gend_id, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Gender Identity") +
  ylab ("Number of Patients") +
  xlab ("Patient Gender Identity") + theme(text = element_text(size=20))

# education 
ggplot(needs_final) +
  geom_bar(aes(x = educ), fill = "#a1a475") +
  geom_text(aes(x = educ, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Education Level") +
  ylab ("Number of Patients") +
  xlab ("Patient Education Level") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=20))  

# poverty status 
ggplot(needs_final) +
  geom_bar(aes(x = poverty), fill = "#a63030") +
  geom_text(aes(x = poverty, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Poverty Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Poverty Status") + theme(text = element_text(size=20))  

# substance use 
ggplot(needs_final) +
  geom_bar(aes(x = tobacco_use), fill = "#7294d4") +
  geom_text(aes(x = tobacco_use, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Tobacco Use Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Tobacco Use Status") + theme(text = element_text(size=20))  

# employment 
ggplot(needs_final) +
  geom_bar(aes(x = employment_status), fill = "#798e87") +
  geom_text(aes(x = employment_status, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Employment Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Employment Status") + theme(text = element_text(size=20))  

# need help reading medical materials
ggplot(needs_final) +
  geom_bar(aes(x = need_help_reading_med_materials), fill = "#003b67") +
  geom_text(aes(x = need_help_reading_med_materials, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of How Often Patients Need Help Reading Medical Materials From Medical Providers") +
  ylab ("Number of Patients") +
  xlab ("How Often Patients Need Help") + theme(text = element_text(size=20))

# trouble understanding written info about medical condition
ggplot(needs_final) +
  geom_bar(aes(x = written_trouble_understand), fill = "#7aa6dd") +
  geom_text(aes(x = written_trouble_understand, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of How Often Patients Have Problems Learning About Medical Conditions 
          Due to Trouble Understanding Written Information") +
  ylab ("Number of Patients") +
  xlab ("How Often Patients Have Trouble Understanding Written Information") + theme(text = element_text(size=20))

# have trouble understanding what is told 
ggplot(needs_final) +
  geom_bar(aes(x = hearing_trouble_understand), fill = "#74a188") +
  geom_text(aes(x = hearing_trouble_understand, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of How Often Patients Have Trouble Understanding What They Are Told About Medical Conditions") +
  ylab ("Number of Patients") +
  xlab ("How Often Patients Have Trouble Understanding What They Are Told About Medical Conditions") + theme(text = element_text(size=20))

# confidence filling out medical forms alone
ggplot(needs_final) +
  geom_bar(aes(x = conf_fill_forms), fill = "#f0c000") +
  geom_text(aes(x = conf_fill_forms, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of How Confident Patients Are Filling Out Medical Forms By Themselves") +
  ylab ("Number of Patients") +
  xlab ("Patient Confidence") + theme(text = element_text(size=20))

# health literacy score
ggplot(needs_final) +
  geom_bar(aes(x = conf_fill_forms), fill = "#f0c000") +
  geom_text(aes(x = conf_fill_forms, label = after_stat(count)), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of How Confident Patients Are Filling Out Medical Forms By Themselves") +
  ylab ("Number of Patients") +
  xlab ("Patient Confidence") + theme(text = element_text(size=20))

#ggplot(data=needs_final) +
#  geom_point(aes(x=conf_fill_forms, y=educ))

#ggplot(data=needs_final) +
#  geom_jitter(aes(x=educ, y=conf_fill_forms),  fun="mean", color="navy") +
#  ylab("Confidence Filling Out Forms") +
#  xlab("Education")+ 
#  ggtitle("The Relationship b/t Education and Confidence Filling out Forms")

#Latent Class Analysis with Covariates

#First, make sure all variables are factors and subset the data to include only LCA classification 
#variables and covariates

require(plyr)
library(plyr)
require(poLCA)
library(poLCA)

demographics_and_health_literacy_score_data_final <- read_excel("demographics and health literacy score data final.xlsx")

# recode response values to numbers starting at "1" (It's a poLCA thing)
demographics_and_health_literacy_score_data_final$race<-revalue(demographics_and_health_literacy_score_data_final$race, c("American Indian or Alaska Native"="1", "Asian"="2", "Black or African American"="3",
                                            "Hawaiian or Pacific Islander"="4","Hispanic/LatinX"="5", "White"="6"))
demographics_and_health_literacy_score_data_final$gend_id<-revalue(demographics_and_health_literacy_score_data_final$gend_id, 
                            c("Non-binary"="1", "Transgender"="2", "Cisgender man"="3", "Cisgender woman"="4"))
demographics_and_health_literacy_score_data_final$employment_status<-revalue(demographics_and_health_literacy_score_data_final$employment_status, c("Employed"="1", "Unemployed"="2", "Not in labor force"="3"))
demographics_and_health_literacy_score_data_final$educ<-revalue(demographics_and_health_literacy_score_data_final$educ, c("College graduate"="1", "Grade 12 or GED (High school graduate)"="2", 
                                            "Graduate School (Master's degree or above)"="3", "Less Than High school"="4",
                                            "Some college or technical school"="5"))
demographics_and_health_literacy_score_data_final$tobacco_use<-revalue(demographics_and_health_literacy_score_data_final$tobacco_use, c("No"="1", "Yes"="2"))
demographics_and_health_literacy_score_data_final$poverty<-revalue(demographics_and_health_literacy_score_data_final$poverty, c("In Poverty"="1", "Not in Poverty"="2"))

# make sure all variables are factors

str(demographics_and_health_literacy_score_data_final$race)
str(demographics_and_health_literacy_score_data_final$gend_id)
str(demographics_and_health_literacy_score_data_final$employment_status)
str(demographics_and_health_literacy_score_data_final$educ)
str(demographics_and_health_literacy_score_data_final$tobacco_use)
str(demographics_and_health_literacy_score_data_final$poverty)

demographics_and_health_literacy_score_data_final$race<-as.factor(demographics_and_health_literacy_score_data_final$race)
demographics_and_health_literacy_score_data_final$gend_id<-as.factor(demographics_and_health_literacy_score_data_final$gend_id)
demographics_and_health_literacy_score_data_final$employment_status<-as.factor(demographics_and_health_literacy_score_data_final$employment_status)
demographics_and_health_literacy_score_data_final$educ<-as.factor(demographics_and_health_literacy_score_data_final$educ)
demographics_and_health_literacy_score_data_final$tobacco_use<-as.factor(demographics_and_health_literacy_score_data_final$tobacco_use)
demographics_and_health_literacy_score_data_final$poverty<-as.factor(demographics_and_health_literacy_score_data_final$poverty)

#Second, define the LCA formula. Variables in parentheses are the latent class classification variables. 
#Variables outside of the parentheses are covariates (not included in the LCA). 
#Finally, run the LCA specifying a range of classes
f <- cbind(race,gend_id,employment_status,educ,tobacco_use,
           poverty) ~ 1

lCA1 <- poLCA(f,demographics_and_health_literacy_score_data_final, nclass=1,nrep=15) 
lCA2 <- poLCA(f,demographics_and_health_literacy_score_data_final, nclass=2,nrep=15, graphs = T)
lCA3 <- poLCA(f,demographics_and_health_literacy_score_data_final, nclass=3,nrep=15, graphs = T)

#add variable to data set with all variables so it can be used as predictor variable:
demographics_and_health_literacy_score_data_final$class <- lCA3$predclass

demographics_and_health_literacy_score_data_final$class<-as.factor(demographics_and_health_literacy_score_data_final$class)

myAnovaResults <- aov(health_literacy_score ~ class, data = demographics_and_health_literacy_score_data_final) 
summary(myAnovaResults)

# for post-hoc test
myAnovaResults <- aov(health_literacy_score ~ class, data = demographics_and_health_literacy_score_data_final) 
TukeyHSD(myAnovaResults)

#Calculate average health literacy scores across classes 

class_probabilities <- lCA3$posterior

# Assign each individual to the class with the highest probability
class_assignments <- apply(class_probabilities, 1, which.max)

# Create indicator variables for each class
class_indicators <- model.matrix(~ class_assignments - 1)

# Combine the indicator variables with your original dataset
data_with_indicators <- cbind(demographics_and_health_literacy_score_data_final, class_indicators)

# Calculate averages for each variable within each class
class_averages <- aggregate(. ~ class_assignments, data = data_with_indicators, mean)
class_averages


ggplot(data=demographics_and_health_literacy_score_data_final)+
  geom_boxplot(aes(x=class, y=health_literacy_score, fill=class))+
  ggtitle('Association Between Class & Health Literacy Score')+
  theme(axis.text.x=element_text(hjust=1))+
  ylab("Health Literacy Score")+
  xlab("Class")+
  scale_fill_manual(values=c("cornflowerblue","brown2","darkgoldenrod1"))
