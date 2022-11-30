# clear all
rm(list = ls())

library(stringr)
library(dplyr)
library(tidyr)


load("./data/intermediate2.RData")
load("./data/mothers_data_pivot.RData")
load("./data/mothers_data_pivot_rel.RData")


all_data <- all_data %>% rename("child_id" =  "ID.CODE.OF.CHILD" ,
                                "mother_id" = "ID.CODE.OF.MOTHER.OF.CHILD")

time_of_y <- all_data %>% select(c('mother_id', 'child_id', 'gap_at_y', 'year_at_y', 'yob_child'))

# (a) religion raised (constant)
# update: removed the collapsed variable
religion_raised <- mothers_data_pivot_rel %>%
  filter(question %in% c("IN WHAT RELIGION WAS R RAISED? "),
         !is.na(value)) %>%
  mutate(flag = ifelse(question == "IN WHAT RELIGION WAS R RAISED? ", 1, 0)) %>% 
  group_by(MOTHER_ID) %>%
  mutate(max_flag = max(flag)) %>%
  filter(flag == max_flag) %>%
  distinct(MOTHER_ID, .keep_all = TRUE) %>%
  rename(mother_religion_raised = value) %>%
  select(MOTHER_ID, mother_religion_raised)


all_data <- left_join(all_data,
                      religion_raised,
                      by = c("mother_id" = "MOTHER_ID"))

rm(religion_raised)

# (b) current religion (at time of Y)


religion_curr <- mothers_data_pivot_rel %>%
  filter(question %in% c("PRESENT RELIGIOUS AFFILIATION "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  filter(year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  mutate(max_year = max(year)) %>%
  filter(year == max_year)  %>%
  rename(mother_religion_current = value) %>%                  
  select(c(MOTHER_ID, child_id, year_at_y, gap_at_y, mother_religion_current))


all_data <- left_join(all_data,
                      religion_curr,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y"))

rm(religion_curr)

# (c) religious frequency (average)

religion_freq <- mothers_data_pivot %>%
  filter(question %in% c("FREQ OF RS RELIGIOUS ATTENDANCE "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  filter(year >= yob_child,
         year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  summarize(religion_freq = mean(value, na.rm = TRUE))


all_data <- left_join(all_data,
                      religion_freq,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y")) %>%
              group_by(mother_id) %>% # impute missing
              mutate(avg_religion_freq = mean(religion_freq, na.rm = TRUE),
                     religion_freq = ifelse(is.na(religion_freq), ifelse(is.nan(avg_religion_freq), NA, avg_religion_freq), religion_freq)) %>%
              select(-c('avg_religion_freq'))


rm(religion_freq)

# (d) HGC (at Y)

hgc <- mothers_data_pivot %>%
  filter(question %in% c('HGC '),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>% 
  filter(year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  summarize(hgc = max(value))

all_data <- left_join(all_data,
                      hgc,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y"))
rm(hgc)

# (e) ATTITUDES - woman's place in the home
att_qs <- unique(mothers_data_pivot$question)[substr(unique(mothers_data_pivot$question), 1, 8) == "FAM ATND"]
att_names <- gsub(" ", "_", trimws(tolower(gsub('[[:punct:] ]+',' ', substr(att_qs, 12,nchar(att_qs))))))

for(i in seq(1, length(att_qs))){
  
  attitude <- mothers_data_pivot %>% 
    filter(question == att_qs[i],
           !is.na(value)) %>%
    left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
    filter(year <= year_at_y) %>%
    mutate(years_from_yob = abs(yob_child - year),
           year_after_yob = ifelse(year > yob_child, 1, 0))
  
  # captures the closest reading to the child's birth
  attitude_at_birth <- attitude %>%
    group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
    mutate(min_years_from_yob = min(years_from_yob)) %>%
    filter(years_from_yob == min_years_from_yob) %>%
    mutate(max_year_after_yob = max(year_after_yob)) %>%
    filter(year_after_yob == max_year_after_yob) %>%
    select(c(MOTHER_ID, child_id, year_at_y, gap_at_y, value)) 
  
  # captures the reading at or just before time of y
  attitude_at_y <- attitude %>%
    group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
    mutate(max_year = max(year)) %>%
    filter(year == max_year) %>%
    select(c(MOTHER_ID, child_id, year_at_y, gap_at_y, value))
  
  # joins
  all_data <- left_join(all_data, 
                        attitude_at_birth, 
                        by = c("mother_id" = "MOTHER_ID",
                               "child_id" = "child_id",
                               "year_at_y" = "year_at_y",
                               "gap_at_y" = "gap_at_y")) 
  names(all_data)[length(names(all_data))] <- paste0(att_names[i], "_birth")
  
  
  all_data <- left_join(all_data, 
                        attitude_at_y, 
                        by = c("mother_id" = "MOTHER_ID",
                               "child_id" = "child_id",
                               "year_at_y" = "year_at_y",
                               "gap_at_y" = "gap_at_y"))
  names(all_data)[length(names(all_data))] <- paste0(att_names[i], "_y")
  
  rm(attitude, attitude_at_birth, attitude_at_y)
}


# (f) hours worked in past calendar year at Y
hours_worked <- mothers_data_pivot %>%
  filter(question %in% c("HRS WRKD IN PAST CAL YR "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  filter(year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  mutate(max_year = max(year)) %>%
  filter(year == max_year)  %>%
  rename(hours_works_at_y = value) %>%                  
  select(c(MOTHER_ID, child_id, year_at_y, gap_at_y, hours_works_at_y))


all_data <- left_join(all_data,
                      hours_worked,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y"))


# (g) number of jobs every reported = at Y
num_jobs <- mothers_data_pivot %>%
  filter(question %in% c("NUM JOBS EVER REPORTED "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  filter(year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  mutate(max_year = max(year)) %>%
  filter(year == max_year)  %>%
  rename(num_jobs_ever_at_y = value) %>%                  
  select(c(MOTHER_ID, child_id, year_at_y, gap_at_y, num_jobs_ever_at_y))


all_data <- left_join(all_data,
                      num_jobs,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y"))

rm(num_jobs)

# (h) poverty status average
poverty <- mothers_data_pivot %>%
  filter(question %in% c("POVERTY STATUS "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  filter(year >= yob_child,
         year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  summarize(avg_poverty_status = mean(value, na.rm = TRUE))


all_data <- left_join(all_data,
                      poverty,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y"))


rm(poverty)

# (i) family income - average 
inc <- mothers_data_pivot %>%
  filter(question %in% c("TOTAL NET FAMILY INCOME "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  filter(year >= yob_child,
         year <= year_at_y) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  summarize(avg_fam_inc = mean(value, na.rm = TRUE))


all_data <- left_join(all_data,
                      inc,
                      by = c("mother_id" = "MOTHER_ID",
                             "child_id" = "child_id",
                             "year_at_y" = "year_at_y",
                             "gap_at_y" = "gap_at_y"))


rm(inc)

# (j) race / ethnicity
race_eth <- mothers_data_pivot %>%
            filter(question %in% c("RACL/ETHNIC COHORT /SCRNR "),
                   !is.na(value)) %>%
            select(MOTHER_ID, value) %>%
            rename(mother_race = value)

all_data <- left_join(all_data, 
                      race_eth,
                      by = c("mother_id" = "MOTHER_ID"))

rm(race_eth)

# (k) US citizen - latest before Y (or whatever is reported)
citizen <-  mothers_data_pivot %>%
  filter(question %in% c("IS R CITIZEN OF US "),
         !is.na(value)) %>%
  left_join(time_of_y, by=c("MOTHER_ID"="mother_id")) %>%
  mutate(year_thresh = ifelse(year <= year_at_y, year, -1)) %>%
  group_by(MOTHER_ID, child_id, year_at_y, gap_at_y) %>%
  mutate(max_year = max(year_thresh),
         num_reports = n()) %>%
  filter(year == max_year | num_reports ==1)  %>%
  rename(is_citizen = value)



# Final cleaning steps

# Rename and recode Y
all_data <- rename(all_data, treat_alike_scale = BOYS.GIRLS.SHOULD.BE.TREATED.ALIKE.)
all_data$treat_alike_binary <- recode(all_data$treat_alike_scale, `1` = 1, `2` = 1, `3` = -1, `4` = -1)
all_data <- all_data[, c(1,2,3,ncol(all_data),4:(ncol(all_data)-1))]

# Drop NAs
all_data <- na.omit(all_data)

# One-hot encode
all_data <- fastDummies::dummy_cols(all_data, remove_selected_columns=TRUE)


# Create a version where feature vars are standardized
# Needs debugging; creating NAs
#all_data_standardized <- all_data %>% 
#  mutate_at(vars(-c('child_id', 'mother_id', 'treat_alike_scale', 'treat_alike_binary')), ~(scale(.) %>% as.vector))



# Keep only all_data in environment
rm(list=setdiff(ls(), c("all_data", "all_data_standardized")))

write.csv(all_data, file="./data/all_data.csv", row.names=FALSE)
