require(readxl)
library(dplyr)
require(writexl)
library(descr)

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
quinn_sub$employment_status[quinn_sub$employ %in% c("Full-time (30 or more hours per week)", "Part-time (fewer than 30 hours per week)", "Unemployed")]<-"In labor force"
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
hl_sub$need_help_reading_med_materials <- as.factor(hl_sub$need_help_reading_med_materials)
hl_sub$written_trouble_understand <- as.factor(hl_sub$written_trouble_understand)
hl_sub$hearing_trouble_understand <- as.factor(hl_sub$hearing_trouble_understand)
hl_sub$conf_fill_forms <- as.factor(hl_sub$conf_fill_forms)

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






