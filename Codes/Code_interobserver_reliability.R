## Clear memory
rm(list = ls())

## Set up packages ##
library(readxl) #import excel files
library(dplyr) #Data formatting
library(tidyverse) #Data formatting
library(irr) ##inter observer reliability test

#import dataset
data <- read_excel("~/Desktop/Chapter2,3_Alarmcall/Github_codes_data_MS/data_interobserver_reliability.xlsx") 
attach(data)
str(data)

## Create datasets for the different behaviours from the two different observers
before_nbc <- data[c(3,5)] #baseline begging for the two observers
during_nbc <- data[c(4,6)] #playback begging for the two observers
before_hibc <- data[c(7,9)] #baseline high intensity begging for the two observers
during_hibc <- data[c(8,10)] #playback high intensity begging for the two observers
before_lookup <- data[c(11,13)] #baseline lookup for the two observers
during_lookup <- data[c(12,14)] #playback lookup for the two observers
before_gape <- data[c(15,17)] #baseline gape for the two observers
during_gape <- data[c(16,18)] #playback begging for the two observers

#Conduct the interobserver reliability test for the different behaviors for baseline and during call playback
icc(before_nbc, model = "oneway", type = 'agreement', unit = 'single')
icc(during_nbc, model = "oneway", type = 'consistency', unit = 'single')
icc(before_hibc, model = "oneway", type = 'consistency', unit = 'single')
icc(during_hibc, model = "oneway", type = 'consistency', unit = 'single')
icc(before_lookup, model = "oneway", type = 'consistency', unit = 'single')
icc(during_lookup, model = "oneway", type = 'consistency', unit = 'single')
icc(before_gape, model = "oneway", type = 'consistency', unit = 'single')
icc(during_gape, model = "oneway", type = 'consistency', unit = 'single')
