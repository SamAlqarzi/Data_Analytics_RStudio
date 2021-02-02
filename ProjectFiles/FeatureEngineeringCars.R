#libraries 

library(dplyr)

#read_data
cars <- read.csv('~/Downloads/QantMethods/Project/cars.csv')

#view data
View(cars)

#check unique values for each column
unique(cars$color)
unique(cars$engine_has_gas)
unique(cars$engine_fuel)
unique(cars$engine_type)
unique(cars$body_type)
unique(cars$transmission)
unique(cars$manufacturer_name)
unique(cars$drivetrain)
unique(cars$state)

# encode car transsmission type 
cars$transmission <- ifelse(cars$transmission == 'automatic', 1,0)
# encode car engine_has_gas 
cars$engine_has_gas <- ifelse(cars$engine_has_gas == 'True', 1,0)
# encode car has_warranty
cars$has_warranty <- ifelse(cars$has_warranty == 'True', 1,0)
# encode car is_exchangeable
cars$is_exchangeable <- ifelse(cars$is_exchangeable == 'True', 1,0)
# dummy encode car color
for(unique_value in unique(cars$color)){
  cars[paste("Color", unique_value, sep = "_")] <- ifelse(cars$color == unique_value, 1, 0)
}
#dummy encode engine_fuel
for(i in unique(cars$engine_fuel)){
  cars[paste("EngFuel", i, sep = "_")] <- ifelse(cars$engine_fuel == i, 1, 0)
}
# dummy encode engine type
for(i in unique(cars$engine_type)){
  cars[paste("EngType", i, sep = "_")] <- ifelse(cars$engine_type == i, 1, 0)
}
#dummy encode drivetrain
for(i in unique(cars$drivetrain)){
  cars[paste(i,"drivTrain", sep = "_")] <- ifelse(cars$drivetrain == i, 1, 0)
}
#encode state
for(i in unique(cars$state)){
  cars[paste("state",i, sep = "_")] <- ifelse(cars$state == i, 1, 0)
}
# subsetting data
cars <- subset(cars, select = c(1,2,5,8,12,3,6,11,15:19,30,31:57,10))
View(cars)
#arrange columns 
carsData <- cars[,c(1,2,7,8,11,10,3,4:6,12:42,9)]
View(carsData)
summary(carsData)
# missing values
which(is.na(carsData))
carsData <- na.omit(carsData)

#correlations 
correl <- cor(carsData[,c(7:42)])
                         
View(correl) 
# narrowing the data
findata <- carsData[,c(1,2,3,7,9,10,11,35,36,40,41,42)]
usdcar <- findata[findata$manufacturer_name=='Volkswagen' 
           | findata$manufacturer_name=="Opel" 
           | findata$manufacturer_name=='BMW' 
           | findata$manufacturer_name=='Ford', ]  
View(usdcar)
# change data type/ create numeric variables out of categorical ones
as.numeric(usdcar$year_produced)
as.numeric(usdcar$number_of_photos)
usdcar$manuName_new <- as.numeric(as.factor(usdcar$manufacturer_name))
#findata$modName_new <- as.numeric(as.factor(findata$model_name))

# find all the unique values of a column
length(unique(usdcar$manufacturer_name))
# table the data based on categories, find frequencies
#tab <- table(findata$manufacturer_name)
#View(tab)

# check for datatypes of the columns
str(findata)

# test for linear relations with plots 
plot(findata$modName_new,findata$price_usd)

# modeling
mod1 <- lm(price_usd ~ ., data = usdcar)
summary(mod1)
mod2 <- lm(price_usd ~ odometer_value + has_warranty + number_of_photos +
           state_new ,data = findata)


#save the dataset
write.csv(carsData,'~/Downloads/QantMethods/CarsData.csv')
