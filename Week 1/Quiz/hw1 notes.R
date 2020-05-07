data <- read.csv("hw1_data.csv")
z <- data[ , "Ozone"]
bad <- is.na(z)
zreal <- z[!bad]

data[zreal[zreal > 31], ]
#Can't figure out how to do it this way

data[which(data$Ozone > 31 & data$Temp > 90), "Solar.R"]
#mean of Solar.R subset where Ozone>31 and Temp>90
mean(data[which(data$Ozone > 31 & data$Temp > 90), "Solar.R"])

#Mean of Temp when Month == 6
mean(data[which(data$Month == 6), "Temp"])

#Max ozone in May
O.May <- data[which(data$Month == 5), "Ozone"]
bad <- is.na(O.May)
max(O.May[!bad])