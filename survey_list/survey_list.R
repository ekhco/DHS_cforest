rm(list=ls()); gc()

setwd("~/QSU/DHS_ranforest/abstract")
surveys <- read.csv("~/QSU/DHS_ranforest/abstract/survey_list.csv")
head(surveys)
str(surveys)

surveys$country <- as.character(surveys$country)
surveys$survey <- as.character(surveys$survey)

surveys_hiv <- surveys[(surveys$include %in% 1), ]

# how many surveys have HIV test results?
nrow(surveys_hiv)

# how many surveys have generalized HIV epidemic
surveys_hivepi <- surveys_hiv[surveys_hiv$Prevalence > 0.01, ]
nrow(surveys_hivepi)

head(surveys_hivepi)
sum(surveys_hivepi$HIV_pos)
sum(surveys_hivepi$HIV_pos, surveys_hivepi$HIV_neg)

# distribution of countries
data.frame(table(surveys_hivepi$country))

# --------------------------------------------------------
# how will I know if variables show up more frequently
# than they should across the 58 surveys?
#
# NULL distribution of variable frequency:
#    randomly select variables that are "fake important"
#    by "fake-ruta" method, same # of vars that Boruta
#    would have selected. make a distribution of variable
#    frequencies over 58 surveys.
#
# count actual frequency of Boruta-predicted variables
# those that are more frequent than the 95% are much more
# frequenty than random! so I guess they must mean something.

# put in the inverse-weight by country


# distribution of years
data.frame(table(surveys_hivepi$year))
hist(surveys_hivepi$year)

# distribution of Prevalence
hist(surveys_hivepi$Prevalence)

# get a list of surveys to include

surveys_hivepi$filepath <- as.character(surveys_hivepi$filepath)
write.csv(surveys_hivepi[,c("filepath","Prevalence")], "survey_list_include.csv", row.names=FALSE)
