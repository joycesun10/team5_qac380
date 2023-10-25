require(readxl)

needs <- read_excel("all managed data.xlsx")

# if we use the first 6 variables yelena gave us

yelena6 <- needs[, c("id", "race", "gend_id", "employment_status", "educ", 
                                            "tobacco_use", "poverty", "need_help_reading_med_materials", 
                                            "written_trouble_understand", "hearing_trouble_understand", "conf_fill_forms")]
yelena6_omit <- na.omit(yelena6)
