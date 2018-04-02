library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

# READING THE COPA DATASET

COPA_Summary <- read_csv("C:/Users/vishw/OneDrive/Documents/Academics/UC Harris/Career Development/California Policy Labs/R_Assignments_Project/COPA_Summary.csv",
                         col_types = cols(COMPLAINT_DATE = col_date(format = "%m/%d/%Y %I:%M:%S %p")))


# EXTRACTING DATA FOR THE STUDY PERIOD

COPA_study <- filter(COPA_Summary, COMPLAINT_DATE >= as.Date("2013-12-01"), 
                     COMPLAINT_DATE <= as.Date("2017-12-30"),
                     ASSIGNMENT != "BIA") %>%
                     arrange(COMPLAINT_DATE)

rm(COPA_Summary)

# FINDING MOST COMMON TYPES OF REPORTS

COPA_top_categories <- COPA_study %>%
                       group_by(CURRENT_CATEGORY) %>%
                       summarise(number_of_incidents = n()) %>%
                       ungroup() %>%
                       mutate(percentage_occurrence = number_of_incidents/(sum(number_of_incidents))) %>%
                       arrange(desc(percentage_occurrence)) 

COPA_top_categories$CURRENT_CATEGORY <- factor(COPA_top_categories$CURRENT_CATEGORY, 
                                               levels = COPA_top_categories$CURRENT_CATEGORY[order(-COPA_top_categories$percentage_occurrence)])

ggplot(filter(COPA_top_categories, CURRENT_CATEGORY %in% c("Excessive Force", "Taser Notification", "Miscellaneous", "Verbal Abuse", "Civil Suits"))) +
  geom_bar(mapping = aes(x = CURRENT_CATEGORY, y = percentage_occurrence), stat = "identity") +
  theme_economist_white()

# TREND ANALYSIS 

# TREND LINE DATASET FOR DIFFERENT CATEGORIES
COPA_all_categories_trend <- COPA_study %>%
                             group_by(yr = year(COMPLAINT_DATE), mon = month(COMPLAINT_DATE), type = CURRENT_CATEGORY) %>%
                             summarise(number_of_reports = n()) %>%
                             mutate(year_mon = as.Date(paste0(yr, "-", mon, "-01"))) %>%
                             arrange(year_mon, desc(number_of_reports))

# TREND LINE DATASET FOR ALL CATEGORIES TOGETHER
COPA_total_incident_trend <- COPA_all_categories_trend %>%
                             ungroup() %>%
                             group_by(year_mon) %>%
                             summarise(total_num_incidents = sum(number_of_reports)) %>%
                             arrange(year_mon, desc(total_num_incidents))

# TREND PLOT FOR THE TOP TWO CATEGORIES, AS A PERCENTAGE OF TOTAL NUMBER OF INCIDENTS
ggplot(filter(COPA_all_categories_trend, type %in% c("Taser Notification", "Excessive Force"))) +
  geom_area(mapping = aes(x = year_mon, y = number_of_reports, fill = type)) +
  geom_line(data = COPA_total_incident_trend, mapping = aes(x = year_mon, y = total_num_incidents))

# TIME SERIES DECOMPOSITION OF TOTAL NUMBER OF INCIDENTS, AND INCIDENTS OF EXCESSIVE FORCE AND TASER NOTIFICATION

# FOR TOTAL NUMBER OF INCIDENTS
COPA_total_incident_ts <- ts(COPA_total_incident_trend$total_num_incidents, frequency = 12, start = c(2013, 12), end = c(2017, 12))

total_incidents_fit <- stl(COPA_total_incident_ts, s.window = "periodic")
plot(total_incidents_fit)

# FOR EXCESSIVE FORCE
COPA_force_data <- COPA_all_categories_trend %>%
                   ungroup() %>%
                   filter(type == "Excessive Force") %>%
                   arrange(year_mon)
                    
COPA_force_ts <- ts(COPA_force_data$number_of_reports, frequency = 12, start = c(2013, 12), end = c(2017, 12))
force_fit <- stl(COPA_force_ts, s.window = "periodic")
plot(force_fit)


# FOR TASER NOTIFICATION

COPA_taser_data <- COPA_all_categories_trend %>%
                   ungroup() %>%
                   filter(type == "Taser Notification") %>%
                   arrange(year_mon)

COPA_taser_ts <- ts(COPA_taser_data$number_of_reports, frequency = 12, start = c(2013, 12), end = c(2017, 12))
taser_fit <- stl(COPA_taser_ts, s.window = 12)
plot(taser_fit)

