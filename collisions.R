# NYPD Data Analyst Assignment - Problem #2
# Chris Pecaut April 20, 2016

library(ggplot2)
library(plyr)
library(caret)

setwd("/Users/principledhomeostasis/Desktop/Harris/Crime/NYPD")

# Rename column names in easier format
p2 <- read.csv("Problem2.csv", sep=",", header=T, check.names = FALSE)
p2 <- rename(p2, c("CONTRIBUTING FACTOR VEHICLE 1"="factor1", "CONTRIBUTING FACTOR VEHICLE 2"="factor2",
                   "CONTRIBUTING FACTOR VEHICLE 3"="factor3", "CONTRIBUTING FACTOR VEHICLE 4"="factor4",
                   "CONTRIBUTING FACTOR VEHICLE 5"="factor5"))
p2 <- rename(p2, c("VEHICLE TYPE CODE 1"="vehicle1", "VEHICLE TYPE CODE 2"="vehicle2",
                   "VEHICLE TYPE CODE 3"="vehicle3", "VEHICLE TYPE CODE 4"="vehicle4",
                   "VEHICLE TYPE CODE 5"="vehicle5"))
p2 <- rename(p2, c("NUMBER OF PERSONS KILLED"="killed"))
p2 <- rename(p2, c("BOROUGH"="borough", "ZIP CODE"="zip"))

# Most prevalent factors
sort(summary(p2$factor1))

# Create data frame of fatalities alone to do summary statistics
fatal <- subset(p2, killed > 0)

# Strip out hours from time field
fatal$timestamp <- strptime(fatal$TIME, format="%H:%M")
fatal$hours <- as.numeric(format(fatal$timestamp, format="%H"))
p2$timestamp <- strptime(p2$TIME, format="%H:%M")
p2$hours <- as.numeric(format(p2$timestamp, format="%H"))

# Plot hour of collision histogram for fatalities and collisions
hist(p2$hours, xlim=c(0,24), main="Time of All Collisions (n=100,000)", xlab="Hour of Day", ylab="# of Collisions")
hist(fatal$hours, breaks=24, main="Time of Fatalities (n=119)", xlab="Hour of Day", ylab="# of Collisions")

# Strip out year to look at year breakdown for fatalities and collisions
fatal$DATE <- as.Date(fatal$DATE)
fatal$year <- as.numeric(format(fatal$DATE, format="%Y"))
fatal$month <- as.numeric(format(fatal$DATE, format="%M"))

p2$DATE <- as.Date(p2$DATE)
p2$year <- as.numeric(format(p2$DATE, format="%Y"))
p2$month <- as.numeric(format(p2$DATE, format="%M"))

table(fatal$year)
table(p2$year)


# Create numeric version of factors for histograms
p2$factor1num <- as.numeric(p2$factor1)

# plot histogram of factors in fatalities
levels(p2$factor1)

# Plot histogram of factors
hist(p2$factor1num,  main="Factors listed in 1st Field")

# Remove Unspecified factor and plot histogram
hist(p2$factor1num[!p2$factor1num==46], breaks=48, main="Factors listed in 1st Field", xlab="Factor #")

# Create data frame without "Unspecified column" for factor analysis of year/borough/zip
factor1null <- subset(p2, factor1num!=46)

# Create individual year data frames to compare 
f2015 <- subset(p2, factor1num!=46 & year==2015)
f2014 <- subset(p2, factor1num!=46 & year==2014)
f2013 <- subset(p2, factor1num!=46 & year==2013)
f2012 <- subset(p2, factor1num!=46 & year==2012)

# Bar plots of factors across years show rough similarity at first glance
barplot(prop.table(table(f2015$factor1num)), main="2015")
barplot(prop.table(table(f2014$factor1num)), main="2014")
barplot(prop.table(table(f2013$factor1num)), main="2013")
barplot(prop.table(table(f2012$factor1num)), main="2012")

# Create histograms of boroughs by year
hist(as.numeric(p2$borough))
hist(as.numeric(f2015$borough))
hist(as.numeric(f2014$borough))
hist(as.numeric(f2013$borough))
hist(as.numeric(f2012$borough))

# Create histograms of zip codes by year
hist(as.numeric(p2$zip), breaks=length(unique(p2$zip)))
hist(as.numeric(f2015$zip), breaks=length(unique(f2015$zip)))
hist(as.numeric(f2014$zip), breaks=length(unique(f2014$zip)))
hist(as.numeric(f2013$zip), breaks=length(unique(f2013$zip)))
hist(as.numeric(f2012$zip), breaks=length(unique(f2012$zip)))


# Create logit model for predicting fatalities

# Convert multiple columns of categorical collison factors into dummy variable
# in order to recombine all factors into a single, comprehensive dummy for each factor
dmy <- dummyVars("~ factor1 + factor2 + factor3 + factor4 + factor5", data=p2, fullRank=T)
trsf <- data.frame(predict(dmy, newdata=p2))

# Create new data frame for logit model and dummy variable
model = data.frame(p2$killed)
model <- rename(model, c("p2.killed"="killed"))

# Make all fatalities = 1 (only 4 fatalies = 2)
model$killed[model$killed!=0] <- 1

# Combine all collision factors from 5 collison factor fields into a single field
#
# This process also eliminate duplicate entries across fields for the same factor
model$Accelerator.Defective <- apply(trsf[,c("factor1.Accelerator.Defective", "factor2.Accelerator.Defective")], 1, sum, na.rm=TRUE)
model$Accelerator.Defective[model$Accelerator.Defective!=0] <- 1
table(model$Accelerator.Defective)

model$Aggressive.Driving.Road.Rage <- apply(trsf[,c("factor1.Aggressive.Driving.Road.Rage", "factor2.Aggressive.Driving.Road.Rage", "factor3.Aggressive.Driving.Road.Rage")], 1, sum, na.rm=TRUE)
model$Aggressive.Driving.Road.Rage[model$Aggressive.Driving.Road.Rage!=0] <- 1
table(model$Aggressive.Driving.Road.Rage)

model$Alcohol.Involvement <- apply(trsf[,c("factor1.Alcohol.Involvement", "factor2.Alcohol.Involvement", "factor3.Alcohol.Involvement")], 1, sum, na.rm=TRUE)
model$Alcohol.Involvement[model$Alcohol.Involvement!=0] <- 1
table(model$Alcohol.Involvement)

model$Backing.Unsafely <- apply(trsf[,c("factor1.Backing.Unsafely", "factor2.Backing.Unsafely", "factor3.Backing.Unsafely")], 1, sum, na.rm=TRUE)
model$Backing.Unsafely[model$Backing.Unsafely!=0] <- 1
table(model$Backing.Unsafely)

model$Brakes.Defective <- apply(trsf[,c("factor1.Brakes.Defective", "factor2.Brakes.Defective", "factor3.Brakes.Defective")], 1, sum, na.rm=TRUE)
model$Brakes.Defective[model$Brakes.Defective!=0] <- 1
table(model$Brakes.Defective)

model$Cell.Phone..hand.held. <- apply(trsf[,c("factor1.Cell.Phone..hand.held.", "factor2.Cell.Phone..hand.held.")], 1, sum, na.rm=TRUE)
model$Cell.Phone..hand.held.[model$Cell.Phone..hand.held.!=0] <- 1
table(model$Cell.Phone..hand.held.)

model$Driver.Inattention.Distraction <- apply(trsf[,c("factor1.Driver.Inattention.Distraction", "factor2.Driver.Inattention.Distraction", "factor3.Driver.Inattention.Distraction", "factor4.Driver.Inattention.Distraction")], 1, sum, na.rm=TRUE)
model$Driver.Inattention.Distraction[model$Driver.Inattention.Distraction!=0] <- 1
table(model$Driver.Inattention.Distraction)

model$Driver.Inexperience <- apply(trsf[,c("factor1.Driver.Inexperience", "factor2.Driver.Inexperience", "factor3.Driver.Inexperience", "factor4.Driver.Inexperience")], 1, sum, na.rm=TRUE)
model$Driver.Inexperience[model$Driver.Inexperience!=0] <- 1
table(model$Driver.Inexperience)

model$Drugs..Illegal <- apply(trsf[,c("factor1.Drugs..Illegal.", "factor2.Drugs..Illegal.")], 1, sum, na.rm=TRUE)
model$Drugs..Illegal[model$Drugs..Illegal!=0] <- 1
table(model$Drugs..Illegal)

model$Failure.to.Keep.Right <- apply(trsf[,c("factor1.Failure.to.Keep.Right", "factor2.Failure.to.Keep.Right", "factor3.Failure.to.Keep.Right", "factor4.Failure.to.Keep.Right", "factor5.Failure.to.Keep.Right")], 1, sum, na.rm=TRUE)
model$Failure.to.Keep.Right[model$Failure.to.Keep.Right!=0] <- 1
table(model$Failure.to.Keep.Right)

model$Failure.to.Yield.Right.of.Way <- apply(trsf[,c("factor1.Failure.to.Yield.Right.of.Way", "factor2.Failure.to.Yield.Right.of.Way", "factor3.Failure.to.Yield.Right.of.Way", "factor4.Failure.to.Yield.Right.of.Way", "factor5.Failure.to.Yield.Right.of.Way")], 1, sum, na.rm=TRUE)
model$Failure.to.Yield.Right.of.Way[model$Failure.to.Yield.Right.of.Way!=0] <- 1
table(model$Failure.to.Yield.Right.of.Way)

model$Fatigued.Drowsy <- apply(trsf[,c("factor1.Fatigued.Drowsy", "factor2.Fatigued.Drowsy", "factor3.Fatigued.Drowsy", "factor4.Fatigued.Drowsy", "factor5.Fatigued.Drowsy")], 1, sum, na.rm=TRUE)
model$Fatigued.Drowsy[model$Fatigued.Drowsy!=0] <- 1
table(model$Fatigued.Drowsy)

model$Fell.Asleep <- apply(trsf[,c("factor1.Fell.Asleep", "factor2.Fell.Asleep", "factor3.Fell.Asleep", "factor4.Fell.Asleep")], 1, sum, na.rm=TRUE)
model$Fell.Asleep[model$Fell.Asleep!=0] <- 1
table(model$Fell.Asleep)

model$Following.Too.Closely <- apply(trsf[,c("factor1.Following.Too.Closely", "factor2.Following.Too.Closely", "factor3.Following.Too.Closely", "factor4.Following.Too.Closely")], 1, sum, na.rm=TRUE)
model$Following.Too.Closely[model$Following.Too.Closely!=0] <- 1
table(model$Following.Too.Closely)

model$Glare <- apply(trsf[,c("factor1.Glare", "factor2.Glare", "factor3.Glare")], 1, sum, na.rm=TRUE)
model$Glare[model$Glare!=0] <- 1
table(model$Glare)

model$Headlights.Defective <- apply(trsf[,c("factor1.Headlights.Defective", "factor2.Headlights.Defective")], 1, sum, na.rm=TRUE)
model$Headlights.Defective[model$Headlights.Defective!=0] <- 1
table(model$Headlights.Defective)

model$Illness <- apply(trsf[,c("factor1.Illness", "factor2.Illness", "factor3.Illness")], 1, sum, na.rm=TRUE)
model$Illness[model$Illness!=0] <- 1
table(model$Illness)

model$Lane.Marking.Improper.Inadequate <- apply(trsf[,c("factor1.Lane.Marking.Improper.Inadequate", "factor2.Lane.Marking.Improper.Inadequate")], 1, sum, na.rm=TRUE)
model$Lane.Marking.Improper.Inadequate[model$Lane.Marking.Improper.Inadequate!=0] <- 1
table(model$Lane.Marking.Improper.Inadequate)

model$Lost.Consciousness <- apply(trsf[,c("factor1.Lost.Consciousness", "factor2.Lost.Consciousness", "factor3.Lost.Consciousness", "factor4.Lost.Consciousness")], 1, sum, na.rm=TRUE)
model$Lost.Consciousness[model$Lost.Consciousness!=0] <- 1
table(model$Lost.Consciousness)

model$Obstruction.Debris <- apply(trsf[,c("factor1.Obstruction.Debris", "factor2.Obstruction.Debris", "factor3.Obstruction.Debris", "factor4.Obstruction.Debris", "factor5.Obstruction.Debris")], 1, sum, na.rm=TRUE)
model$Obstruction.Debris[model$Obstruction.Debris!=0] <- 1
table(model$Obstruction.Debris)

model$Other.Electronic.Device <- apply(trsf[,c("factor1.Other.Electronic.Device", "factor2.Other.Electronic.Device", "factor3.Other.Electronic.Device", "factor4.Other.Electronic.Device")], 1, sum, na.rm=TRUE)
model$Other.Electronic.Device[model$Other.Electronic.Device!=0] <- 1
table(model$Other.Electronic.Device)

model$Other.Lighting.Defects <- apply(trsf[,c("factor1.Other.Lighting.Defects", "factor2.Other.Lighting.Defects")], 1, sum, na.rm=TRUE)
model$Other.Lighting.Defects[model$Other.Lighting.Defects!=0] <- 1
table(model$Other.Lighting.Defects)

model$Other.Vehicular <- apply(trsf[,c("factor1.Other.Vehicular", "factor2.Other.Vehicular", "factor3.Other.Vehicular", "factor4.Other.Vehicular", "factor5.Other.Vehicular")], 1, sum, na.rm=TRUE)
model$Other.Vehicular[model$Other.Vehicular!=0] <- 1
table(model$Other.Vehicular)

model$Outside.Car.Distraction <- apply(trsf[,c("factor1.Outside.Car.Distraction", "factor2.Outside.Car.Distraction", "factor3.Outside.Car.Distraction", "factor4.Outside.Car.Distraction", "factor5.Outside.Car.Distraction")], 1, sum, na.rm=TRUE)
model$Outside.Car.Distraction[model$Outside.Car.Distraction!=0] <- 1
table(model$Outside.Car.Distraction)

model$Oversized.Vehicle <- apply(trsf[,c("factor1.Oversized.Vehicle", "factor2.Oversized.Vehicle", "factor3.Oversized.Vehicle")], 1, sum, na.rm=TRUE)
model$Oversized.Vehicle[model$Oversized.Vehicle!=0] <- 1
table(model$Oversized.Vehicle)

model$Passenger.Distraction <- apply(trsf[,c("factor1.Passenger.Distraction", "factor2.Passenger.Distraction")], 1, sum, na.rm=TRUE)
model$Passenger.Distraction[model$Passenger.Distraction!=0] <- 1
table(model$Passenger.Distraction)

model$Passing.or.Lane.Usage.Improper <- apply(trsf[,c("factor1.Passing.or.Lane.Usage.Improper", "factor2.Passing.or.Lane.Usage.Improper")], 1, sum, na.rm=TRUE)
model$Passing.or.Lane.Usage.Improper[model$Passing.or.Lane.Usage.Improper!=0] <- 1
table(model$Passing.or.Lane.Usage.Improper)

model$Pavement.Defective <- apply(trsf[,c("factor1.Pavement.Defective", "factor2.Pavement.Defective")], 1, sum, na.rm=TRUE)
model$Pavement.Defective[model$Pavement.Defective!=0] <- 1
table(model$Pavement.Defective)

model$Pavement.Slippery <- apply(trsf[,c("factor1.Pavement.Slippery", "factor2.Pavement.Slippery", "factor3.Pavement.Slippery", "factor4.Pavement.Slippery", "factor5.Pavement.Slippery")], 1, sum, na.rm=TRUE)
model$Pavement.Slippery[model$Pavement.Slippery!=0] <- 1
table(model$Pavement.Slippery)

model$Pedestrian.Bicyclist.Other.Pedestrian.Error.Confusion <- apply(trsf[,c("factor1.Pedestrian.Bicyclist.Other.Pedestrian.Error.Confusion",
                                                                             "factor2.Pedestrian.Bicyclist.Other.Pedestrian.Error.Confusion")], 1, sum, na.rm=TRUE)
model$Pedestrian.Bicyclist.Other.Pedestrian.Error.Confusion[model$Pedestrian.Bicyclist.Other.Pedestrian.Error.Confusion!=0] <- 1
table(model$Pedestrian.Bicyclist.Other.Pedestrian.Error.Confusion)

model$Physical.Disability <- apply(trsf[,c("factor1.Physical.Disability", "factor2.Physical.Disability", "factor3.Physical.Disability")], 1, sum, na.rm=TRUE)
model$Physical.Disability[model$Physical.Disability!=0] <- 1
table(model$Physical.Disability)

model$Prescription.Medication <- apply(trsf[,c("factor1.Prescription.Medication", "factor2.Prescription.Medication", "factor3.Prescription.Medication", "factor4.Prescription.Medication")], 1, sum, na.rm=TRUE)
model$Prescription.Medication[model$Prescription.Medication!=0] <- 1
table(model$Prescription.Medication)

model$Reaction.to.Other.Uninvolved.Vehicle <- apply(trsf[,c("factor1.Reaction.to.Other.Uninvolved.Vehicle", "factor2.Reaction.to.Other.Uninvolved.Vehicle")], 1, sum, na.rm=TRUE)
model$Reaction.to.Other.Uninvolved.Vehicle[model$Reaction.to.Other.Uninvolved.Vehicle!=0] <- 1
table(model$Reaction.to.Other.Uninvolved.Vehicle)

model$Shoulders.Defective.Improper <- apply(trsf[,c("factor1.Shoulders.Defective.Improper", "factor2.Shoulders.Defective.Improper")], 1, sum, na.rm=TRUE)
model$Shoulders.Defective.Improper[model$Shoulders.Defective.Improper!=0] <- 1
table(model$Shoulders.Defective.Improper)

model$Steering.Failure <- apply(trsf[,c("factor1.Steering.Failure", "factor2.Steering.Failure")], 1, sum, na.rm=TRUE)
model$Steering.Failure[model$Steering.Failure!=0] <- 1
table(model$Steering.Failure)

model$Tire.Failure.Inadequate <- apply(trsf[,c("factor1.Tire.Failure.Inadequate", "factor2.Tire.Failure.Inadequate")], 1, sum, na.rm=TRUE)
model$Tire.Failure.Inadequate[model$Tire.Failure.Inadequate!=0] <- 1
table(model$Tire.Failure.Inadequate)

model$Tow.Hitch.Defective <- apply(trsf[,c("factor1.Tow.Hitch.Defective", "factor2.Tow.Hitch.Defective")], 1, sum, na.rm=TRUE)
model$Tow.Hitch.Defective[model$Tow.Hitch.Defective!=0] <- 1
table(model$Tow.Hitch.Defective)

model$Traffic.Control.Device.Improper.Non.Working <- apply(trsf[,c("factor1.Traffic.Control.Device.Improper.Non.Working", "factor2.Traffic.Control.Device.Improper.Non.Working", "factor3.Traffic.Control.Device.Improper.Non.Working", "factor4.Traffic.Control.Device.Improper.Non.Working", "factor5.Traffic.Control.Device.Improper.Non.Working")], 1, sum, na.rm=TRUE)
model$Traffic.Control.Device.Improper.Non.Working[model$Traffic.Control.Device.Improper.Non.Working!=0] <- 1
table(model$Traffic.Control.Device.Improper.Non.Working)

model$Traffic.Control.Disregarded <- apply(trsf[,c("factor1.Traffic.Control.Disregarded", "factor2.Traffic.Control.Disregarded", "factor3.Traffic.Control.Disregarded", "factor4.Traffic.Control.Disregarded", "factor5.Traffic.Control.Disregarded")], 1, sum, na.rm=TRUE)
model$Traffic.Control.Disregarded[model$Traffic.Control.Disregarded!=0] <- 1
table(model$Traffic.Control.Disregarded)

model$Turning.Improperly <- apply(trsf[,c("factor1.Turning.Improperly", "factor2.Turning.Improperly", "factor3.Turning.Improperly", "factor4.Turning.Improperly")], 1, sum, na.rm=TRUE)
model$Turning.Improperly[model$Turning.Improperly!=0] <- 1
table(model$Turning.Improperly)

model$Unsafe.Lane.Changing <- apply(trsf[,c("factor1.Unsafe.Lane.Changing", "factor2.Unsafe.Lane.Changing")], 1, sum, na.rm=TRUE)
model$Unsafe.Lane.Changing[model$Unsafe.Lane.Changing!=0] <- 1
table(model$Unsafe.Lane.Changing)

model$Unsafe.Speed <- apply(trsf[,c("factor1.Unsafe.Speed", "factor2.Unsafe.Speed", "factor3.Unsafe.Speed")], 1, sum, na.rm=TRUE)
model$Unsafe.Speed[model$Unsafe.Speed!=0] <- 1
table(model$Unsafe.Speed)

# Create a table of the final, accumulated results of collision factors 
sort(colSums(model))

# Create logit model on fatalities using new comprehensive dummy variables for collision factor
logit <- glm(killed ~ ., family=binomial(link="logit"), data=model)

summary(logit)

# Calculate the logit scalar in order to provide interpretable values to the logit results
LogitScalar <- mean(dlogis(predict(logit, type = "link")))
sort(LogitScalar * coef(logit))

