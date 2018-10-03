# reading the file after manually removing the header rows in Excel
uberdata <- read.csv("Uber Request Data.csv")
View(uberdata)

## ------------------ Cleaning uberdata df --------------------
# 1. look for duplicate values
sum(duplicated(uberdata$Request.id)) # no duplicate values

# 2. Missing values
sum(is.na(uberdata)) # 6564 missing values
summary(uberdata) # Driver.id has 2650 missing values and Drop.timestamp has 3914 missing values
unique(uberdata[(is.na(uberdata$Driver.id)),]$Status) # Driver.id has missing values only for "No Cars Available" Status. This is because no drivers are allocated when cars are not available.
unique(uberdata[(is.na(uberdata$Drop.timestamp)),]$Status) # Driver.id has missing values only for "No Cars Available" & "Cancelled" Status. This is because there cannot be a drop time when cars are cancelled or cars are not available.
# These incidences of missing values are in line with the business logic. 

# 3. Check individual columns
str(uberdata) # Pickup.point is a factor with 2 levels: "Airport","City" & Status is a factor with 3 levels: "Trip Completed", "Cancelled", "No Cars Available"

summary(uberdata$Request.id) # seems okay
summary(uberdata$Status) # seems okay
summary(uberdata$Driver.id) # missing values are in line with business logic
summary(uberdata$Request.timestamp) # inconsistency in date-time formats
summary(uberdata$Drop.timestamp) # inconsistency in date-time formats

# Ensure that dates and times are in proper format
library(stringr) # loading library for string manipulation 
uberdata1 <- uberdata

# Prepare dates in the format of "DD-MM-YYYY" format for Request.timestamp column values
uberdata1$Request.timestamp <- str_replace_all(uberdata1$Request.timestamp, "\\/", "\\-") 
uberdata1$Request.timestamp <- str_replace_all(uberdata1$Request.timestamp, "-7", "-07")
# Prepare times in the format of "HH:MM:SS" format for Request.timestamp column values
uberdata1[str_length(uberdata1$Request.timestamp) < str_length("11-07-2016 00:00:00"),5] <- paste(uberdata1[str_length(uberdata1$Request.timestamp) < str_length("11-07-2016 00:00:00"),5],":00",sep="")
uberdata1[str_length(uberdata1$Request.timestamp) < str_length("11-07-2016 00:00:00"),5] <- str_replace(uberdata1[str_length(uberdata1$Request.timestamp) < str_length("11-07-2016 00:00:00"),5],"6 ","6 0")
# Prepare dates in the format of "DD-MM-YYYY" format for Drop.timestamp column values
uberdata1$Drop.timestamp <- str_replace_all(uberdata1$Drop.timestamp, "\\/", "\\-") 
uberdata1$Drop.timestamp <- str_replace_all(uberdata1$Drop.timestamp, "-7", "-07")
# Prepare times in the format of "HH:MM:SS" format for Drop.timestamp column values
uberdata1[(str_length(uberdata1$Drop.timestamp) < str_length("11-07-2016 00:00:00") & !(is.na(uberdata1$Drop.timestamp))),6] <- paste(uberdata1[(str_length(uberdata1$Drop.timestamp) < str_length("11-07-2016 00:00:00") & !(is.na(uberdata1$Drop.timestamp))),6],":00",sep="")
uberdata1[(str_length(uberdata1$Drop.timestamp) < str_length("11-07-2016 00:00:00") & !(is.na(uberdata1$Drop.timestamp))),6] <- str_replace(uberdata1[(str_length(uberdata1$Drop.timestamp) < str_length("11-07-2016 00:00:00") & !(is.na(uberdata1$Drop.timestamp))),6],"6 ","6 0")

View(uberdata1)

uberdata1$request_time <- as.POSIXlt(uberdata1$Request.timestamp, format = "%d-%m-%Y %H:%M:%S") # Convert date-time values of Request.timestamp variable into a POSIXlt class
uberdata1$request_hour <- format(uberdata1$request_time,"%H") # Extract hour information of ride request in a separate variable "request_hour"
uberdata1$request_day <- format(uberdata1$request_time,"%D") # Extract day information of ride request in a separate variable "request_day"

write.csv(uberdata1, "uberdata_clean.csv")


## ------------------ Plotting to visualize problems and gap areas --------------------

library(ggplot2) # loading library for visualization in R
ggplot(uberdata1, aes(x=factor(uberdata1$request_hour), fill=factor(uberdata1$Status))) + geom_bar() + labs(x="Hour of Request", y="Number of requests received", title="Request Status wise Uber rides") # Plots hour wise frequency of requests that are completed, cancelled or shown as "no cars available"

# Based on the frequency of requests, we group hours of requests into a variable called time_slot(levels: "Early Mornings","Daytime","Late Evenings","Night")
uberdata2 <- uberdata1
uberdata2$request_hour <- as.numeric(uberdata2$request_hour)
uberdata2[(uberdata2$request_hour>=22 | uberdata2$request_hour<=4),c("time_slot")] <- "Night (22 to 5)"
uberdata2[(uberdata2$request_hour>=5 & uberdata2$request_hour<=9),c("time_slot")] <- "Early Mornings (5 to 10)"
uberdata2[(uberdata2$request_hour>=10 & uberdata2$request_hour<=16),c("time_slot")] <- "Daytime (10 to 17)"
uberdata2[(uberdata2$request_hour>=17 & uberdata2$request_hour<=21),c("time_slot")] <- "Late Evenings (17 to 22)"
uberdata2$time_slot <- factor(uberdata2$time_slot, levels=c("Night (22 to 5)","Early Mornings (5 to 10)","Daytime (10 to 17)","Late Evenings (17 to 22)"))

# Based on the Pickup.point, we add another variable called request_type(levels:"Airport-City","City-Airport")
uberdata2[(uberdata2$Pickup.point=="Airport"),c("request_type")] <- "Airport-City"
uberdata2[(uberdata2$Pickup.point=="City"),c("request_type")] <- "City-Airport"
uberdata2$request_type <- factor(uberdata2$request_type)

View(uberdata2)

ggplot(uberdata2, aes(x=uberdata2$time_slot, fill=uberdata2$Status)) + geom_bar() + labs(x="Time Slot", y="Number of requests received", title="Request Status wise Uber rides") # Plots time slot wise frequency of requests that are completed, cancelled or shown as "no cars available"

# Problem areas pertain to rides being cancelled or showing "no cars available". 
uber_problem <- subset(uberdata2, uberdata2$Status!="Trip Completed") # Subsets problematic requests which are not serviced.

ggplot(uber_problem, aes(x=uber_problem$time_slot, fill=uber_problem$Status)) + geom_bar() + labs(x="Time Slot", y="Number of requests not serviced", title="Demand Supply Gap in Uber rides") # Plots request status wise distribution of only problematic requests

ggplot(uber_problem, aes(x=uber_problem$time_slot, fill=uber_problem$request_type)) + geom_bar(alpha = 1, position = position_dodge(width=0.9)) + labs(x="Time Slot", y="Number of requests not serviced", title="Demand Supply Gap in Uber rides") # Plots request type wise distribution of only problematic requests

uber_problem1 <- subset(uber_problem, uber_problem$Status=="Cancelled")
ggplot(uber_problem1, aes(x=uber_problem1$time_slot, fill=uber_problem1$request_type)) + geom_bar(alpha = 1, position = position_dodge(width=0.9)) + labs(x="Time Slot", y="Number of requests cancelled", title="Demand Supply Gap in Uber rides") # Plots request type wise distribution of cancelled requests

uber_problem2 <- subset(uber_problem, uber_problem$Status=="No Cars Available")
ggplot(uber_problem2, aes(x=uber_problem2$time_slot, fill=uber_problem2$request_type)) + geom_bar(alpha = 1, position = position_dodge(width=0.9)) + labs(x="Time Slot", y="Number of requests showing No Cars Available", title="Demand Supply Gap in Uber rides") # Plots request type wise distribution of requests showing No Cars Available


## ------------------------------------ Appendix ------------------------------------------
# Checking for variation in demand supply gap across days of the week
ggplot(uber_problem, aes(x=factor(uber_problem$request_day), fill=uber_problem$Status)) + geom_bar() + labs(x="Date", y="Number of requests not serviced", title="Demand Supply Gap in Uber rides") # Plots day wise distribution of only problematic requests

ggplot(uber_problem, aes(x=factor(uber_problem$request_day), fill=uber_problem$request_type)) + geom_bar(alpha = 1, position = position_dodge(width=0.9)) + labs(x="Date", y="Number of requests not serviced", title="Demand Supply Gap in Uber rides") # Plots request type wise distribution of only problematic requests across days of the week
# No significant variation is seen in the demand supply gap across different days of the week









