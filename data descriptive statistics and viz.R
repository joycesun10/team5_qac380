require(readxl)
require(ggplot2)

needs <- read_excel("all managed data.xlsx")

needs_final <- needs[, c("id", "race", "gend_id", "employment_status", "educ", 
                                            "tobacco_use", "poverty", "need_help_reading_med_materials", 
                                            "written_trouble_understand", "hearing_trouble_understand", "conf_fill_forms")]
needs_final <- na.omit(needs_final)

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
