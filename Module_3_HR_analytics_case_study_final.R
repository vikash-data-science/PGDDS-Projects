library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

## Getting all data 
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_data <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_data <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time_data <- read.csv("in_time.csv",stringsAsFactors = F)
out_time_data <- read.csv("out_time.csv",stringsAsFactors = F)

## Renaming 1st column in time sheets to EmployeeID
colnames(in_time_data)[1] <- c("EmployeeID")
colnames(out_time_data)[1] <- c("EmployeeID")

str(general_data)
str(employee_data)
str(manager_data)
str(in_time_data)
str(out_time_data)

sum(is.na(general_data))
sum(is.na(employee_data))
sum(is.na(manager_data))
sum(is.na(in_time_data))
sum(is.na(out_time_data))

length(unique(general_data$EmployeeID))   ### 4410 unique values based on EmployeeID
length(unique(employee_data$EmployeeID))  ### 4410 unique values based on EmployeeID
length(unique(manager_data$EmployeeID))   ### 4410 unique values based on EmployeeID
length(unique(in_time_data$EmployeeID))            ### 4410 unique values based on X
length(unique(out_time_data$EmployeeID))           ### 4410 unique values based on X

## Checking identical customerID across these datasets. 
setdiff(general_data$EmployeeID,employee_data$EmployeeID)  # Identical
setdiff(general_data$EmployeeID,manager_data$EmployeeID)   # Identical
setdiff(in_time_data$EmployeeID,out_time_data$EmployeeID)  # Identical  
setdiff(general_data$EmployeeID,in_time_data$EmployeeID)   # Identical

## Getting median of time difference of in and out times to office_time dataframe
in_time_data <- in_time_data[order(in_time_data$EmployeeID),]
out_time_data <- out_time_data[order(out_time_data$EmployeeID),]
time_in_office <- in_time_data
for (i in 2:ncol(in_time_data))
{
  in_time <- in_time_data[,i]
  out_time <- out_time_data[,i]
  value <- difftime(out_time,in_time,units="mins")
  time_in_office[,i] <- as.numeric(value)  
}
time_in_office[,263] <- apply(time_in_office[,2:262],1,median,na.rm=T)
colnames(time_in_office)[263] <- c("MedianTime")
office_time <- time_in_office[,c(1,263)]

## Getting to master file 
hrdata1 <- merge(general_data,employee_data, by="EmployeeID",all = F)
hrdata2 <- merge(hrdata1,manager_data, by="EmployeeID",all=F)
hrdata3 <- merge(hrdata2,office_time,by="EmployeeID",all=F)
str(hrdata3)

sapply (hrdata3, function(x) sum(is.na(x)))
## Removing NA values (Only 110 such rows out of a total of 4410 rows) and EmployeeCount and Over18 variables since it has same value for all rows
hrdata <- na.omit(hrdata3)
HR_Data <- hrdata[,-c(9,16)]
## Using MedianTime to create Workload variable. Since StandardHours=8, 
## create Workload=High if MedianTime(in hours)>9,=Low if MedianTime(in hours)<7 and Medium otherwise
HR_Data$Workload <- ifelse(HR_Data$MedianTime>540,"High",ifelse(HR_Data$MedianTime<420,"Low","Medium"))
## Removing StandardHours (only one level data) and MedianTime variables
HR_Data <- HR_Data[,-c(16,28)]

### At this stage there are no NA values in dataframe

## Doing EDA here on set of choosen values
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


plot_grid(ggplot(HR_Data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(HR_Data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

## Attrition doesn't have any strong relation with any of the above

plot_grid(ggplot(HR_Data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(), 
          ggplot(HR_Data, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Data, aes(x=factor(Workload),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

## not high correlation with Attrition for above attributes

### Need to check correlation for numeric variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

## Checking for age, Distancefromhome, MonthlyIncome, NumCompaniesworked, percentSalaryHike
## Totalworkingyears, Yearsatcompany, Yearssincelastpromotion, YearswithcurrentManager

plot_grid(ggplot(HR_Data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(HR_Data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(HR_Data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Data, aes(NumCompaniesWorked))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(PercentSalaryHike))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(TotalWorkingYears))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(YearsAtCompany))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(YearsSinceLastPromotion))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Data, aes(YearsWithCurrManager))+ geom_histogram(),
          ggplot(HR_Data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

## Correlation between numeric variables

plot_grid(ggplot(HR_Data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR_Data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(HR_Data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(HR_Data, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(HR_Data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(HR_Data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)  
plot_grid(ggplot(HR_Data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(HR_Data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(HR_Data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

library(GGally)
ggpairs(HR_Data[,c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked",
                   "PercentSalaryHike","TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion",
                   "YearsWithCurrManager")])
HR_Data_backup <- HR_Data

################################################################
# Feature standardisation

## Normalising continuous features
HR_Data$Age <- scale(HR_Data$Age)
HR_Data$DistanceFromHome <- scale(HR_Data$DistanceFromHome)
HR_Data$MonthlyIncome <- scale(HR_Data$MonthlyIncome)
HR_Data$NumCompaniesWorked <- scale(HR_Data$NumCompaniesWorked)
HR_Data$PercentSalaryHike <- scale(HR_Data$PercentSalaryHike)
HR_Data$TotalWorkingYears <- scale(HR_Data$TotalWorkingYears)
HR_Data$YearsAtCompany <- scale(HR_Data$YearsAtCompany)
HR_Data$YearsSinceLastPromotion <- scale(HR_Data$YearsSinceLastPromotion)
HR_Data$YearsWithCurrManager <- scale(HR_Data$YearsWithCurrManager)

## converting target variable Attrition to factors as 1,0
HR_Data$Attrition <- ifelse(HR_Data$Attrition=="Yes",1,0)

## Checking Attrition Rate
Attrition <- sum(HR_Data$Attrition)/nrow(HR_Data)
Attrition  ## Attrition percent is 16.16%

########################################################################
# splitting the data between train and test
HR_Data_iv <- HR_Data[,-1]
set.seed(100)
indices = sample.split(HR_Data_iv$Attrition, SplitRatio = 0.7)
train_iv = HR_Data_iv[indices,]
test_iv = HR_Data_iv[!(indices),]

########################################################################
# Applying WOE and IV to select variables with better predictive power

# creating a dataframe of categorical features - (Excluding continuous/response variables at columns 1,2,5,12,13,14,16,18,19,20)
train_chr <- train_iv[,-c(1,2,5,12,13,14,16,18,19,20)]

# converting categorical attributes to factor
train_fact <- data.frame(sapply(train_chr, function(x) factor(x)))
str(train_fact)

# Final dataset
train_final<- cbind(train_iv[,c(1,2,5,12,13,14,16,18,19,20)],train_fact) 
View(train_final) #3010 obs. of 26 variables

library(Information)
IV <- create_infotables(data=train_final, y="Attrition", bins=10, parallel=FALSE)
IV_Value = data.frame(IV$Summary)
View(IV_Value)

# Excluding variables with IV < 0.02: PercentSalaryHike, PerformanceRating, JobInvolvement, Education, StockOptionLevel, Gender
train_data <- train_final[,-c(6,25,24,13,19,15)]

########################################################################
# Run Logistic Regression on remaining variables

# creating dummy variables for factor attributes
train_data_fact <- train_data[,10:20]
dummies<- data.frame(sapply(train_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data = train_data_fact))[,-1]))

# Final dataset
train_model <- cbind(train_data[,1:9],dummies) 
View(train_model) #3010 obs. of 49 variables

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train_model, family = "binomial")
summary(model_1)

# Stepwise selection
library("MASS")
model_2 <- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

# Excluding EducationField.xLife.Sciences (High VIF & Low Significance)
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales + EducationField.xMarketing + 
    EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
    JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xMarried + MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_3)
vif(model_3)

# MaritalStatus.xMarried (VIF>2 & Low Significance)
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales + EducationField.xMarketing + 
    EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
    JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_4)
vif(model_4) # cannot exclude any more variable based on vif 
#as most of them have low vif; those with higher vif are very significant and not correlated

# Excluding EducationField.xMarketing due to lower significance
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
    JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_5)

# Excluding EducationField.xMedical due to lower significance
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    EducationField.xOther + EducationField.xTechnical.Degree + 
    JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_6)

# Excluding EducationField.xOther due to lower significance
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    EducationField.xTechnical.Degree + 
    JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_7)

# Excluding JobRole.xHuman.Resources due to lower significance
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    EducationField.xTechnical.Degree + 
    JobLevel.x2 + JobLevel.x5 + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_8)

# Excluding EducationField.xTechnical.Degree due to lower significance
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 + JobLevel.x5 + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_9)

# Excluding JobLevel.x5 due to lower significance
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 + JobRole.xManager + 
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_10)

# Excluding JobRole.xManager due to lower significance
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 +
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle + TrainingTimesLastYear.x1 + 
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_11)

# Excluding TrainingTimesLastYear.x1 due to lower significance
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 +
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle +
    TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_12)

# Excluding TrainingTimesLastYear.x4 due to lower significance
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 +
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle +
    TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_13)

# Excluding TrainingTimesLastYear.x5 due to lower significance
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 +
    JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle +
    TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_14)

# Excluding JobRole.xManufacturing.Director due to lower significance
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobLevel.x2 +
    JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle +
    TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_15)

# Excluding JobLevel.x2 due to lower significance
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle +
    TrainingTimesLastYear.x6 + 
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_16)

# Excluding TrainingTimesLastYear.x6 due to lower significance
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
    BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
    Department.xSales +
    JobRole.xResearch.Director + 
    JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
    MaritalStatus.xSingle +
    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
    WorkLifeBalance.x4 + Workload.xLow + Workload.xMedium, family = "binomial", 
    data = train_model)
summary(model_17)

########################################################################
# With 24 significant variables in the model (At 0.001 level of significance)

final_model<- model_17
########################################################################
# Call:
# glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
#     YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
#     BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
#     Department.xSales + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
#     JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
#     EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
#     JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
#     WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
#     Workload.xLow + Workload.xMedium, family = "binomial", data = train_model)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.6813  -0.5681  -0.3530  -0.1719   3.9534  
#
# Coefficients:
#                                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         1.11406    0.40495   2.751 0.005939 ** 
# Age                                -0.33944    0.08046  -4.219 2.45e-05 ***
# NumCompaniesWorked                  0.35364    0.05981   5.913 3.37e-09 ***
# TotalWorkingYears                  -0.60131    0.11129  -5.403 6.55e-08 ***
# YearsSinceLastPromotion             0.65123    0.07751   8.402  < 2e-16 ***
# YearsWithCurrManager               -0.48805    0.08776  -5.561 2.68e-08 ***
# BusinessTravel.xTravel_Frequently   1.35924    0.24692   5.505 3.70e-08 ***
# BusinessTravel.xTravel_Rarely       0.75443    0.22656   3.330 0.000869 ***
# Department.xResearch...Development -0.99323    0.23509  -4.225 2.39e-05 ***
# Department.xSales                  -0.92892    0.24519  -3.789 0.000152 ***
# JobRole.xResearch.Director          0.86152    0.23167   3.719 0.000200 ***
# JobRole.xResearch.Scientist         0.52317    0.14414   3.629 0.000284 ***
# JobRole.xSales.Executive            0.57324    0.14492   3.955 7.64e-05 ***
# MaritalStatus.xSingle               0.92448    0.11640   7.942 1.99e-15 ***
# EnvironmentSatisfaction.x2         -0.63258    0.16926  -3.737 0.000186 ***
# EnvironmentSatisfaction.x3         -0.78805    0.15672  -5.028 4.94e-07 ***
# EnvironmentSatisfaction.x4         -0.96402    0.15883  -6.070 1.28e-09 ***
# JobSatisfaction.x2                 -0.66483    0.17451  -3.810 0.000139 ***
# JobSatisfaction.x3                 -0.53376    0.15230  -3.505 0.000457 ***
# JobSatisfaction.x4                 -1.13220    0.16394  -6.906 4.98e-12 ***
# WorkLifeBalance.x2                 -1.19884    0.22684  -5.285 1.26e-07 ***
# WorkLifeBalance.x3                 -1.38318    0.21111  -6.552 5.68e-11 ***
# WorkLifeBalance.x4                 -1.14204    0.26572  -4.298 1.72e-05 ***
# Workload.xLow                      -1.74916    0.15319 -11.418  < 2e-16 ***
# Workload.xMedium                   -1.32413    0.13890  -9.533  < 2e-16 ***
# ---
# Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2089.4  on 2985  degrees of freedom
# AIC: 2139.4
#
# Number of Fisher Scoring iterations: 6
#
#######################################################################

### Variable Importance
var_imp <- data.frame(varImp(final_model, scale=FALSE))
var_imp_data <- cbind(rownames(var_imp),var_imp)
colnames(var_imp_data)[1] <- "Predictor_Variables"
var_imp_sorted <- var_imp_data[order(var_imp_data$Overall,decreasing = TRUE),]

#######################################################################

### Model Evaluation

### Test Data ####
# Transformation of test data in line with the model
# creating a dataframe of categorical features - (Excluding continuous/response variables at columns 1,2,5,12,13,14,16,18,19,20)
test_chr <- test_iv[,-c(1,2,5,12,13,14,16,18,19,20)]

# converting categorical attributes to factor
test_fact <- data.frame(sapply(test_chr, function(x) factor(x)))
str(test_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(test_fact, 
                            function(x) data.frame(model.matrix(~x-1,data = test_fact))[,-1]))

# Final dataset
test_final<- cbind(test_iv[,c(1,2,5,12,13,14,16,18,19,20)],dummies) 
View(test_final) #1290 obs. of 62 variables

#predicted probabilities of Attrition=1 for test data
test_pred = predict(final_model, type = "response", 
                              newdata = test_final[,-2])


# Let's see the summary 
summary(test_pred)
test_final$prob <- test_pred
View(test_final)

# Let's use the probability cutoff of 50% and build the confusion matrix
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test_final$Attrition==1,"Yes","No"))
table(test_actual_attrition,test_pred_attrition)
library(e1071)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

#########################################################################################
# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cut-off values from 0.01 to 0.80 for plotting and initializing a matrix of 100 X 3.
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff #0.1855556

# Let's choose a cutoff value of 0.1856 for final model
test_cutoff_attrition <- factor(ifelse(test_pred >=0.1856, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc                                # Accuracy is 0.7658915
sens                               # Sensitivity is 0.7607656
spec                               # Specificity is 0.7668825
View(test_final)
conf_final

##################################################################################################
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  No Yes
#        No  829  50
#        Yes 252 159
#                                           
#                Accuracy : 0.7659          
#                  95% CI : (0.7418, 0.7888)
#     No Information Rate : 0.838           
#     P-Value [Acc > NIR] : 1               
#                                           
#                   Kappa : 0.3797          
#  Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.7608          
#             Specificity : 0.7669          
#          Pos Pred Value : 0.3869          
#          Neg Pred Value : 0.9431          
#              Prevalence : 0.1620          
#          Detection Rate : 0.1233          
#    Detection Prevalence : 0.3186          
#       Balanced Accuracy : 0.7638          
#                                           
#        'Positive' Class : Yes    
##################################################################################################

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)         # 0.5276481 (typically should be above 0.4 and in top deciles for a good model)
plot(performance_measures_test,col="black",lty=3, lwd=3)

####################################################################
# Lift & Gain Chart 
# plotting the lift chart

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

View(Attrition_decile)
# Gain of 82.29665 and a lif of 2.057416 by the 4th decile

####################################################################
# Lift & Gain Chart Tables as used in presentation
# Plotting the Gain and Lift chart

library(dplyr)
result = data.frame(cbind(test_actual_attrition, test_pred, test_cutoff_attrition))
result[,"bucket"] = ntile(-result[,"test_pred"], 10)

gaintable <- arrange(mutate(summarise(group_by(result,bucket),
                            observations=length(test_actual_attrition),
                            attrition_event=sum(test_actual_attrition,na.rm=TRUE)),
                     cum_attrition=cumsum(attrition_event),
                     attrition_perfectmodel=ifelse(ifelse(cumsum(observations)<=sum(attrition_event),observations,sum(attrition_event)-lag(cumsum(observations)))<0,0,ifelse(cumsum(observations)<=sum(attrition_event),observations,sum(attrition_event)-lag(cumsum(observations)))),
                     Gain_model=cum_attrition/sum(attrition_event)*100,
                     Gain_random=bucket*(100/10),
                     Gain_perfectmodel=cumsum(attrition_perfectmodel)/sum(attrition_event)*100,
                     Cumlift = Gain_model/Gain_random),
                     bucket)

library(ggplot2)
ggplot(gaintable, aes(x=gaintable$bucket)) + 
       geom_line(aes(y = gaintable$Gain_model, colour = "Gain_model")) + 
       geom_line(aes(y = gaintable$Gain_random, colour = "Gain_random")) +
       geom_line(aes(y = gaintable$Gain_perfectmodel, colour = "Gain_perfectmodel")) +
       scale_x_continuous(name="Decile Bucket",breaks=c(0,1,2,3,4,5,6,7,8,9,10),limits=c(0,10),expand=c(0,0)) +
       scale_y_continuous(name="Gain(%)",breaks=c(0,10,20,30,40,50,60,70,80,90,100),limits=c(0,100),expand=c(0,0)) +
       ggtitle("Gain Chart")

ggplot(gaintable, aes(x=gaintable$bucket)) + 
       geom_line(aes(y = gaintable$Cumlift, colour = "Lift_model")) + 
       geom_line(aes(y = rep(1,10), colour = "Lift_random")) + 
       scale_x_continuous(name="Decile Bucket",breaks=c(0,1,2,3,4,5,6,7,8,9,10),limits=c(0,10),expand=c(0,0)) +
       scale_y_continuous(name="Lift",breaks=c(0,1,2,3,4),limits=c(0,4),expand=c(0,0)) +
       ggtitle("Lift Chart")

### Plotting KS-Statistic Tables as used in presentation
ks_table <- arrange(mutate(summarise(group_by(result,bucket),
                            observations=length(test_actual_attrition),
                            attrition_event=sum(test_actual_attrition,na.rm=TRUE),
                            non_attrition_event=observations-attrition_event),
                     cum_attrition=cumsum(attrition_event),
                     cum_non_attrition=cumsum(non_attrition_event),
                     cum_attrition_pct=cum_attrition/sum(attrition_event)*100,
                     cum_non_attrition_pct=cum_non_attrition/sum(non_attrition_event)*100,
                     attrition_nonattrition_diff_pct=cum_attrition_pct-cum_non_attrition_pct),
                     bucket)

ks_table <- ks_table[c("bucket","observations","attrition_event","cum_attrition","cum_attrition_pct","non_attrition_event","cum_non_attrition","cum_non_attrition_pct","attrition_nonattrition_diff_pct")]

max(ks_table$attrition_nonattrition_diff_pct)     # 52.70107

### Plotting ROC Curve and calculating Gini
total_obs <- sum(ks_table$observations)
total_attrition <- sum(ks_table$attrition_event)
roctable <- arrange(mutate(summarise(group_by(result,bucket),
                            observations=length(test_actual_attrition),
                            attrition_event=sum(test_actual_attrition,na.rm=TRUE)),
                     non_attrition_event=observations-attrition_event,
                     cum_attrition=cumsum(attrition_event),
                     cum_non_attrition=cumsum(non_attrition_event),
                     attrition_perfectmodel=ifelse(ifelse(cumsum(observations)<=sum(attrition_event),observations,sum(attrition_event)-lag(cumsum(observations)))<0,0,ifelse(cumsum(observations)<=sum(attrition_event),observations,sum(attrition_event)-lag(cumsum(observations)))),
                     non_attrition_perfectmodel=observations-attrition_perfectmodel,
                     cum_attrition_perfectmodel=cumsum(attrition_perfectmodel),
                     cum_non_attrition_perfectmodel=cumsum(non_attrition_perfectmodel),
                     cum_attrition_random=bucket*(total_attrition/10),
                     cum_non_attrition_random=cumsum(observations)-cum_attrition_random,
                     tpr_model=cum_attrition/total_attrition,
                     fpr_model=cum_non_attrition/(total_obs-total_attrition),
                     tpr_perfect=cum_attrition_perfectmodel/total_attrition,
                     fpr_perfect=cum_non_attrition_perfectmodel/(total_obs-total_attrition),
                     tpr_random=cum_attrition_random/total_attrition,
                     fpr_random=cum_non_attrition_random/(total_obs-total_attrition)),
                     bucket)

model_roc <- data.frame(cbind(c("model_roc"),roctable$tpr_model,roctable$fpr_model))
random_roc <- data.frame(cbind(c("random_roc"),roctable$tpr_random,roctable$fpr_random))
perfect_roc <- data.frame(cbind(c("perfect_roc"),roctable$tpr_perfect,roctable$fpr_perfect))
rocdata <- rbind(model_roc,random_roc,perfect_roc)
roc_data <- rename(rocdata, roc_type=X1, tpr=X2, fpr=X3)
roc_data$tpr <- as.numeric(as.character(roc_data$tpr))
roc_data$fpr <- as.numeric(as.character(roc_data$fpr))

ggplot() + geom_line(data=roc_data, aes(x=roc_data$fpr, y=roc_data$tpr, color = roc_type), size=1) +
        scale_x_continuous(name="FPR",breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),limits=c(0,1),expand=c(0,0)) +
        scale_y_continuous(name="TPR",breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),limits=c(0,1),expand=c(0,0)) +
        ggtitle("ROC Curve")
library(MLmetrics)
Gini(y_pred=final_model$fitted.values, y_true=train_model$Attrition)       # 0.6246291

#############################################################################