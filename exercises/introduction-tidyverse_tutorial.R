# R Script for tutorial "Introduction to the Tidyverse"
# using data from TidyTuesday
# Dataset #10: 2019-03-05
#
# Meetup #2
# R-Ladies Frankfurt
#
# 23 May 2019

# Load packages
library(here)
library(readr)
library(dplyr)
library(tidyr)

# Import data from github
jobsGender <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
employedGender <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv")

# Save data locally
write_csv(jobsGender, path = here("data", "jobsGender.csv"))
write_csv(employedGender, path = here("data", "employedGender.csv"))

# Inspect data
class(jobsGender)
glimpse(jobsGender)
head(jobsGender)

class(employedGender)
glimpse(employedGender)
head(employedGender)

# Tidy data
jobsTidy <- gather(jobsGender, 
                   key = "workerGender", 
                   value = "earnings",
                   c(workers_male, workers_female))

# Inspect values of variable workerGender
unique(jobsTidy$workerGender)

# Separate column workerGender into three new columns
jobsTidySep <- separate(jobsTidy, 
                        col = workerGender,
                        into = c("totalEarnings", "earningsTotal", "gender"),
                        sep = "_",
                        remove = TRUE)

# Join two datasets
joinJobs <- inner_join(jobsTidySep, 
                       employedGender,
                       by = "year")

# Select variables of interest
joinJobsShort <- select(joinJobs, 
                        c(year:minor_category, earnings, gender, full_time_female, full_time_male))

# Filter women
jobsFilter <- filter(joinJobsShort, gender == "female")

# Summarise
jobSummary <- summarise(jobsFilter, earnMean = mean(earnings, na.rm = TRUE))

# Code with pipe
pipeSummary <- joinJobs %>%
  select(c(year:minor_category, earnings, gender, full_time_female, full_time_male)) %>% 
  filter(gender == "female") %>% 
  group_by(occupation, major_category, minor_category) %>% 
  summarise(earningsMean = mean(earnings, na.rm = TRUE)) %>%
  arrange(desc(earningsMean))
