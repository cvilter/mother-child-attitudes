# clear all
rm(list = ls())

library(stringr)
library(dplyr)
library(tidyr)

new_data <- read.table('./data/age.dat', sep=' ')
names(new_data) <- c('R0000100',
'R0000300',
'R0000500',
'R0173600',
'R0214700',
'R0214800')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0173600 <- factor(data$R0173600,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0),
labels=c("CROSS MALE WHITE",
"CROSS MALE WH. POOR",
"CROSS MALE BLACK",
"CROSS MALE HISPANIC",
"CROSS FEMALE WHITE",
"CROSS FEMALE WH POOR",
"CROSS FEMALE BLACK",
"CROSS FEMALE HISPANIC",
"SUP MALE WH POOR",
"SUP MALE BLACK",
"SUP MALE HISPANIC",
"SUP FEM WH POOR",
"SUP FEMALE BLACK",
"SUP FEMALE HISPANIC",
"MIL MALE WHITE",
"MIL MALE BLACK",
"MIL MALE HISPANIC",
"MIL FEMALE WHITE",
"MIL FEMALE BLACK",
"MIL FEMALE HISPANIC"))
  data$R0214700 <- factor(data$R0214700,
levels=c(1.0,2.0,3.0),
labels=c("HISPANIC",
"BLACK",
"NON-BLACK, NON-HISPANIC"))
  data$R0214800 <- factor(data$R0214800,
levels=c(1.0,2.0),
labels=c("MALE",
"FEMALE"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$R0000300[17.0 <= data$R0000300 & data$R0000300 <= 99999.0] <- 17.0
data$R0000300 <- factor(data$R0000300,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0),
labels=c("0: < 1",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10",
"11",
"12",
"13",
"14",
"15",
"16",
"17 TO 99999: 17+"))
data$R0000500[0.0 <= data$R0000500 & data$R0000500 <= 56.0] <- 0.0
data$R0000500[73.0 <= data$R0000500 & data$R0000500 <= 99999.0] <- 73.0
data$R0000500 <- factor(data$R0000500,
levels=c(0.0,57.0,58.0,59.0,60.0,61.0,62.0,63.0,64.0,65.0,66.0,67.0,68.0,69.0,70.0,71.0,72.0,73.0),
labels=c("0 TO 56: < 57",
"57",
"58",
"59",
"60",
"61",
"62",
"63",
"64",
"65",
"66",
"67",
"68",
"69",
"70",
"71",
"72",
"73 TO 99999: 73+"))
return(data)
}

varlabels <- c("ID# (1-12686) 79",
"DATE OF BIRTH - MONTH 79",
"DATE OF BIRTH - YR 79",
"SAMPLE ID  79 INT",
"RACL/ETHNIC COHORT /SCRNR 79",
"SEX OF R 79"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("CASEID_1979",
"Q1-3_A~M_1979",
"Q1-3_A~Y_1979",
"SAMPLE_ID_1979",
"SAMPLE_RACE_78SCRN",
"SAMPLE_SEX_1979")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "mother_demo" with value labels.
mother_demo <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
mother_demo <- qnames(mother_demo)

# Produce summaries for the raw (uncategorized) data file
# summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "mother_demo" data file.
# mother_demo <- vallabels(new_data)
# mother_demo <- vallabels_continuous(new_data)
#summary(mother_demo)

#************************************************************************************************************

rm(new_data)

mother_demo <- mother_demo %>%
                rename(mother_id = "CASEID_1979",
                       yob_mother = "Q1-3_A~Y_1979") %>%
                select(c(mother_id, yob_mother)) %>%
                mutate(yob_mother = 1900 + yob_mother)

save(mother_demo, file = "./data/mother_demographics.RData")
