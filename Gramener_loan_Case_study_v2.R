library(tidyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/5065575/Desktop/Useful Downloads/IIIT Bangalore/Course 2 - Statistics and Exploratory Data Analytics/Group Project 2 - Gramener EDA Case Group Project/loan")
# Reading Loan File to R
loan <- read.csv("loan.csv",stringsAsFactors = F,header = T)

# Cleaning file to remove unnecessary or not useful data to NULL
# Starting with column name "collections_12_mths_ex_med" till end, 
# everything is not useful in our analysis as it is marked NA or single value
loan[,50:111] <- NULL

# getting distribution of Loan Status
ggplot(loan,aes(x=loan_status,fill="red")) + geom_bar(stat="count")

# Getting a slice with Loan Status Charged off/Fully Paid/Current for further analysis
Charged_off <- subset(loan,loan$loan_status=="Charged Off")
Current <- subset(loan,loan$loan_status=="Current")
Full_Paid <- subset(loan,loan$loan_status=="Fully Paid")

# Checking for loan_status based on grade
ggplot(loan,aes(x=grade,fill=loan_status)) + geom_bar(stat = "count",position="fill")
# As seen above higher the grade, lower chances of default

# Charged off are more concentrated towards higher interest rates. Which is an 
# indication that higher interest rates are creating issues forcing folks to default

ggplot(Charged_off,aes(x=int_rate)) + geom_bar(stat="count")
ggplot(Full_Paid,aes(x=int_rate)) + geom_bar(stat="count")

# min and average loan amount is higher for charged off

# getting values group by Loan_status
mean_data <- loan %>% group_by(loan_status) %>% summarise(mean_funded_amt=mean(funded_amnt),mean_annual_inc = mean(annual_inc),mean_inst = mean(installment))
min_data <- loan %>% group_by(loan_status) %>% summarise(min_funded_amt=min(funded_amnt),min_annual_inc=min(annual_inc),min_inst=min(installment))
# plotting charts here to get loan_status based on mean values calculated above
ggplot(mean_data,aes(x=loan_status,y=mean_funded_amt,fill="red")) + geom_bar(stat = "identity") 
ggplot(mean_data,aes(x=loan_status,y=mean_annual_inc,fill="red")) + geom_bar(stat = "identity")
ggplot(mean_data,aes(x=loan_status,y=mean_inst,fill="red")) + geom_bar(stat = "identity")

ggplot(min_data,aes(x=loan_status,y=min_funded_amt,fill="red")) + geom_bar(stat="identity")
ggplot(min_data,aes(x=loan_status,y=min_annual_inc,fill="red")) + geom_bar(stat="identity")
ggplot(min_data,aes(x=loan_status,y=min_inst,fill="red")) + geom_bar(stat="identity")

# getting dti values group by Loan_status and sub-grade
mean_dti <- loan %>% group_by(sub_grade,loan_status) %>% summarise(mean_dti=mean(dti))

# Charged off are more concentrated towards higher dti. Which is an 
# indication that higher dti are creating issues forcing folks to default

ggplot(Charged_off,aes(x=dti,fill="red")) + geom_bar(stat="count")
ggplot(Full_Paid,aes(x=dti,fill="red")) + geom_bar(stat="count")

# dti is higher for charged off
ggplot(mean_dti, aes(x=sub_grade, y=mean_dti, group=loan_status, colour=loan_status)) + geom_line(aes(linetype=loan_status), size=1) + geom_point()
