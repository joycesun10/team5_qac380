require(ggplot2)
require(readxl)

demographics <- read_excel("managed demographics.xlsx")

# race
ggplot(demographics) +
  geom_bar(aes(x = race), fill = "#0473c2") +
  geom_text(aes(x = race, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Race") +
  ylab ("Number of Patients") +
  xlab ("Patient Race") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))

# household size
ggplot(demographics) +
  geom_histogram(aes(x = house_size), fill = "#f0c000") +
  ggtitle("Distribution of Patient Household Size") +
  ylab ("Number of Patients") +
  xlab ("Patient Household Size") + theme(text = element_text(size=28))

# gender identity
ggplot(demographics) +
  geom_bar(aes(x = gend_id), fill = "#cd544c") +
  geom_text(aes(x = gend_id, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Gender Identity") +
  ylab ("Number of Patients") +
  xlab ("Patient Gender Identity") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))

# housing stability
ggplot(demographics) +
  geom_bar(aes(x = stable_house), fill = "#7aa6dd") +
  geom_text(aes(x = stable_house, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Housing Stability Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Housing Stability Status") + theme(text = element_text(size=28))

# sex at birth
ggplot(demographics) +
  geom_bar(aes(x = sex), fill = "#003b67") +
  geom_text(aes(x = sex, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Sex at Birth") +
  ylab ("Number of Patients") +
  xlab ("Patient Sex at Birth") + theme(text = element_text(size=28))

# sexual orientation
ggplot(demographics) +
  geom_bar(aes(x = orientation), fill = "#8e7700") +
  geom_text(aes(x = orientation, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Sexual Orientation") +
  ylab ("Number of Patients") +
  xlab ("Patient Sexual Orientation") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))  

# education 
ggplot(demographics) +
  geom_bar(aes(x = educ), fill = "#a1a475") +
  geom_text(aes(x = educ, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Education Level") +
  ylab ("Number of Patients") +
  xlab ("Patient Education Level") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))  

# poverty status 
ggplot(demographics) +
  geom_bar(aes(x = public_assist), fill = "#a63030") +
  geom_text(aes(x = public_assist, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Poverty Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Poverty Status") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))  

# substance use 
ggplot(demographics) +
  geom_bar(aes(x = tobacco_use), fill = "#7294d4") +
  geom_text(aes(x = tobacco_use, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Substsance Use Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Substance Use Status") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))  

# age
ggplot(demographics) +
  geom_histogram(aes(x = age), fill = "#74a188") +
  ggtitle("Distribution of Patient Age") +
  ylab ("Number of Patients") +
  xlab ("Patient Age") + theme(text = element_text(size=28))

# employment 
ggplot(demographics) +
  geom_bar(aes(x = employment_status), fill = "#798e87") +
  geom_text(aes(x = employment_status, label = ..count..), stat = "count", vjust = -0.5, color = "black", size = 8) +
  ggtitle("Distribution of Patient Employment Status") +
  ylab ("Number of Patients") +
  xlab ("Patient Employment Status") + theme(axis.text.x = element_text(angle = 30, hjust=1), text = element_text(size=28))  













