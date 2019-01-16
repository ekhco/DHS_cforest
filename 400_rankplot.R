# ------------------------------------------------------------
# creates a figure of the rank of importance for variables
# across country-surveys - Eric Chow, 2018-12-03
# ------------------------------------------------------------

rm(list=ls())
gc()

library(tidyverse)
library(haven)
library(foreign)
library(caret)
library(stringr)
library(ggplot2)
library(plyr)

setwd("~/QSU/DHS_boruta")
# source("0_fn.R")        # load functions
# source("10_boruta.R")   # load main boruta main function

bvars <- read.dta("output/MF_validation_1_58_w1-3_2018_12_1_AWS.dta")
head(bvars)
summary(bvars)
bvars$varname <- as.character(bvars$varname)

# READ IN A DICTIONARY of labels and merge in
dict <- read.csv("data/dictionary.csv")
head(dict)
dict$varname <- as.character(dict$var)
dict <- dict[,c("varname", "label")]

bvars <- join(bvars, dict, by="varname", type="left", match="first")
head(bvars)

# create a variable of var and label
bvars$varlabelled <- str_c(bvars$varname, " - ", bvars$label)

# drop hiv03
bvars <- bvars[!(bvars$varname %in% "hiv03"), ]

# calcuate the median rank for each var
var_med_rank <- tapply(bvars$rank, bvars$varname, median)
var_med_rank[order(var_med_rank)]


# distribution of importance
hist(bvars$importance, xlim=c(-0.003, 0.003), breaks=500)

# rank and importance
plot(importance ~ rank, data = bvars[bvars$SURVEY == "zm63", ])

# how often do vars show up?
var_freq <- data.frame(table(bvars$varname))
var_freq <- var_freq[order(-1*var_freq$Freq),]
hist(var_freq$Freq)
names(var_freq) <- c("varname", "freq")
head(var_freq)

bvars_ <- join(bvars, var_freq, by="varname", type="left", match="first")
head(bvars_)

# only vars that show up > 3times
bvars_f3 <- bvars_[bvars_$freq >= 3 , ]
bvars_f12 <- bvars_[bvars_$freq >= 12 , ]
bvars_f18 <- bvars_[bvars_$freq >= 18 , ]

bvars_f18_m <- bvars_f18[bvars_f18$SEX == 1, ] # MALE
bvars_f18_f <- bvars_f18[bvars_f18$SEX == 2, ] # FEMALE


# what is the dist of ranks? - Rank is weirdly bimodal
summary(bvars$rank)
hist(bvars$rank)


pdf("figs/var_rank_18M_12.6.2018.pdf", height=12, width=8)
    # the boxplot SEX == 1 (MALE)
    ggplot() +
      geom_boxplot(data=bvars_f18_m, aes(y = rank, x = reorder(varlabelled, -1*rank, FUN=median))) +
      coord_flip() + xlab("variable") # + ylim(-0.0025, 0.0025)
dev.off()

pdf("figs/var_rank_18F_12.6.2018.pdf", height=12, width=8)
    # the boxplot SEX == 2 (FEMALE)
    ggplot() +
      geom_boxplot(data=bvars_f18_f, aes(y = rank, x = reorder(varlabelled, -1*rank, FUN=median))) +
      coord_flip() + xlab("variable") # + ylim(-0.0025, 0.0025)
dev.off()
