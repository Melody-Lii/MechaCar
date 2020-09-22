setwd("~/Desktop/MechaCar")
install.packages("tidyverse")
install.packages("jsonlite")

# MPG Regression
mechaCar <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F)
View(mechaCar)

lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data=mechaCar) #generate multiple linear regression model

summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data=mechaCar))

lm(mpg ~ vehicle.length,mechaCar)

summary(lm(mpg ~ vehicle.length,mechaCar))

# Suspension Coil Summary
sus_coil <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #read in dataset
View(sus_coil)

library(tidyverse)
PSI <- unlist(sus_coil["PSI"])
sum_table <- summary(sus_coil$PSI)

summary_sus_coil = sus_coil %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI), StdDev_PSI=sd(PSI))
View(summarize_sus_coil)

# SUS_Coil t-tests

lot_1 = sus_coil%>%filter(Manufacturing_Lot == "Lot1")
View(lot_1)
lot_2 = sus_coil%>%filter(Manufacturing_Lot == "Lot2")
View(lot_2)
lot_3 = sus_coil%>%filter(Manufacturing_Lot == "Lot3")
View(lot_3)

t.test((lot_1$PSI), mu=1500)
t.test((lot_2$PSI), mu=1500)
t.test((lot_3$PSI), mu=1500)

