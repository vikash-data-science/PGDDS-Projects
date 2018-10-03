# Linear Regression Model Assignment - Predict car prices
carpr1 <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)
View(carpr1)
str(carpr1)

# Checking for duplicate rows
nrow(unique(carpr1))             # This gives the same number of rows as in the original data which means that there are no duplicate rows.

# Checking for missing values
length(which(is.na(carpr1)))     # There are no missing values

# Separating the car company name from car model name and storing it in variable, carnm
library(stringr)
carnm <- str_to_lower(word(carpr1$CarName), locale="en")
# Standardizing different spellings of same car company name
carnm[which(carnm=="maxda")] <- "mazda"
carnm[which(carnm=="porcshce")] <- "porsche"
carnm[which(carnm=="toyouta")] <- "toyota"
carnm[which(carnm=="vokswagen" | carnm=="vw")] <- "volkswagen"
# Replacing carname with car company name
carpr2 <- cbind(carpr1[,-3],carnm)
# Removing car_id from data
carpr2 <- carpr2[,-1]
View(carpr2)
str(carpr2)

#########################################################################
# Create dummy variables for categorical variables. 
# Then combine them with the numeric columns of the original dataframe.

# Variable: carnm
dummy_1 <- data.frame(model.matrix( ~carpr2$carnm, data = carpr2))
carpr3 <- cbind(carpr2[,-25],dummy_1[,-1])
# Variable: fuelsystem
dummy_1 <- data.frame(model.matrix( ~carpr3$fuelsystem, data = carpr3))
carpr3 <- cbind(carpr3[,-16],dummy_1[,-1])
# Variable: cylindernumber
dummy_1 <- data.frame(model.matrix( ~carpr3$cylindernumber, data = carpr3))
carpr3 <- cbind(carpr3[,-14],dummy_1[,-1])
# Variable: enginetype
dummy_1 <- data.frame(model.matrix( ~carpr3$enginetype, data = carpr3))
carpr3 <- cbind(carpr3[,-13],dummy_1[,-1])
# Variable: drivewheel
dummy_1 <- data.frame(model.matrix( ~carpr3$drivewheel, data = carpr3))
carpr3 <- cbind(carpr3[,-6],dummy_1[,-1])
# Variable: carbody
dummy_1 <- data.frame(model.matrix( ~carpr3$carbody, data = carpr3))
carpr3 <- cbind(carpr3[,-5],dummy_1[,-1])
# Variable: symboling
carpr3$symboling <- factor(carpr3$symboling)
dummy_1 <- data.frame(model.matrix( ~carpr3$symboling, data = carpr3))
carpr3 <- cbind(carpr3[,-1],dummy_1[,-1])

# Convert factors with 2 levels to numerical variables
carpr4 <- carpr3
# enginelocation: 1-front, 0-rear
carpr4$enginelocation <- factor(carpr4$enginelocation)
levels(carpr4$enginelocation)<-c(1,0)
carpr4$enginelocation <- as.numeric(levels(carpr4$enginelocation))[carpr4$enginelocation]
# doornumber: 1-four, 0-two
carpr4$doornumber <- factor(carpr4$doornumber)
levels(carpr4$doornumber)<-c(1,0)
carpr4$doornumber <- as.numeric(levels(carpr4$doornumber))[carpr4$doornumber]
# aspiration: 1-std, 0-turbo
carpr4$aspiration <- factor(carpr4$aspiration)
levels(carpr4$aspiration)<-c(1,0)
carpr4$aspiration <- as.numeric(levels(carpr4$aspiration))[carpr4$aspiration]
# fueltype: 1-diesel, 0-gas
carpr4$fueltype <- factor(carpr4$fueltype)
levels(carpr4$fueltype)<-c(1,0)
carpr4$fueltype <- as.numeric(levels(carpr4$fueltype))[carpr4$fueltype]
carpr_data <- carpr4
#####################################################################

# Divide into training and test data set
set.seed(100)
trainindices= sample(1:nrow(carpr_data), 0.7*nrow(carpr_data))
train = carpr_data[trainindices,]
test = carpr_data[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

# Lets load the library in which stepAIC function exists
library(MASS)
step <- stepAIC(model_1, direction="both")
step

# AIC output model
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + carpr2.carnmporsche + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberfour + carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodysedan + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2 + 
                carpr3.symboling3, data = train)
summary(model_2)

# Let us check for multicollinearity. we will apply the VIF function to check the 
# multicollinearity of the independent variables and remove the variables with 
# VIF>2 in order of their insignificance.
library(car)
vif(model_2)

# Highest VIFs: curbweight>enginesize>cylindernumberfour>carlength>carbodysedan
# Although, all variables have a p value below 0.05, the number of variables is still too large.
# We can continue removing the variables till the significance level is 0.001.
# Since carbodysedan variable is also insignificant, we remove carbodysedan and run the model again.
model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + carpr2.carnmporsche + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberfour + carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2 + 
                carpr3.symboling3, data = train)
summary(model_3)
vif(model_3)

# Highest VIFs: curbweight>enginesize>cylindernumberfour>carlength>boreratio>carwidth
# All the above variables are significant. The variables, curbweight and enginesize, still have
# the highest VIFs with very high significance since the beginning. So, it will be a
# good idea to check their correlation as they might be highly correlated.
cor(train$curbweight,train$enginesize)
# The correlation is ~ 87%, indicating that the variables are highly correlated.
# So, remove the variable (curbweight) with lower significance level out of the two and build model_4.
model_4 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + carpr2.carnmporsche + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberfour + carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2 + 
                carpr3.symboling3, data = train)
summary(model_4)
vif(model_4)

# Highest VIFs: enginesize>cylindernumberfour>carlength>carwidth>boreratio
# Since carlength variable is also insignificant, we remove carlength and run the model again
model_5 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + carpr2.carnmporsche + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberfour + carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2 + 
                carpr3.symboling3, data = train)
summary(model_5)
vif(model_5)

# Highest VIFs: enginesize>cylindernumberfour>boreratio>carwidth
# All the above variables are significant. The variables, cylindernumberfour and enginesize, still have
# the highest VIFs with very high significance since the beginning. So, it will be a
# good idea to check their correlation as they might be highly correlated.
cor(train$enginesize,train$carpr3.cylindernumberfour)
# The correlation is ~ 62%, indicating that the variables are highly correlated.
# So, remove the variable (cylindernumberfour) with lower significance level out of the two and build model_6.
model_6 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + carpr2.carnmporsche + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2 + 
                carpr3.symboling3, data = train)
summary(model_6)
vif(model_6)

# Highest VIFs: enginesize>carwidth>enginelocation>symboling3>carnmporsche
# enginelocation, symboling3, carnmporsche have similarly high VIFs but symboling3 is insignificant.
# So we remove symboling3 and build model_7.
model_7 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + carpr2.carnmporsche + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2, data = train)
summary(model_7)
vif(model_7)

# Highest VIFs: enginesize>carwidth>enginelocation>carnmporsche>boreratio
# enginelocation, carnmporsche have similarly high VIFs but carnmporsche is insignificant.
# So we remove carnmporsche and build model_8.
model_8 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + carheight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2, data = train)
summary(model_8)
vif(model_8)

# Highest VIFs: enginesize>carwidth>boreratio>enginetypeohc
# All the above variables are significant. The variables, enginesize and carwidth, still have
# the highest VIFs with very high significance since the beginning. So, it will be a
# good idea to check their correlation as they might be highly correlated.
cor(train$enginesize,train$carwidth)
# The correlation is ~ 75%, indicating that the variables are highly correlated.
# So, remove the variable (carwidth) with lower significance level out of the two and build model_9.
model_9 <- lm(formula = price ~ aspiration + enginelocation +  
                carheight + enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2, data = train)
summary(model_9)
vif(model_9)

# Highest VIFs: enginesize>enginetypeohc>boreratio>stroke>cylindernumbertwo>carheight
# Since carheight is insignificant, we remove carheight and build model_10
model_10 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling1 + carpr3.symboling2, data = train)
summary(model_10)
vif(model_10)

# Highest VIFs: enginesize>enginetypeohc>boreratio>cylindernumbertwo>stroke>fuelsystemmpfi>symboling1
# Since symboling1 is insignificant, we remove symboling1 and build model_11
model_11 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemmpfi + carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_11)
vif(model_11)

# Highest VIFs: enginesize>enginetypeohc>boreratio>stroke>fuelsystemmpfi>cylindernumbertwo
# Since fuelsystemmpfi is insignificant, we remove fuelsystemmpfi and build model_12
model_12 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + boreratio + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_12)
vif(model_12)

# Highest VIFs: enginesize>enginetypeohc>boreratio>stroke>enginetypeohcv>cylindernumbertwo
# Since boreratio is insignificant, we remove boreratio and build model_13
model_13 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohc + carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_13)
vif(model_13)

# Highest VIFs: enginesize>enginetypeohc>stroke>cylindernumbertwo>enginetypeohcv
# Since enginetypeohc is insignificant, we remove enginetypeohc and build model_14
model_14 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + carpr2.carnmchevrolet + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_14)
vif(model_14)

# Highest VIFs: enginesize>stroke>enginetypeohcv>cylindernumberthree>carnmchevrolet>fuelsystemspdi>cylindernumberfive
# Since carnmchevrolet is insignificant, we remove carnmchevrolet and build model_15
model_15 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemspdi + carpr3.cylindernumberfive + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_15)
vif(model_15)

# Highest VIFs (>2): enginesize>stroke>enginetypeohcv>fuelsystemspdi>cylindernumberfive
# Since cylindernumberfive is insignificant, we remove cylindernumberfive and build model_16
model_16 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.fuelsystemspdi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_16)
vif(model_16)

# Highest VIFs (>2): enginesize>stroke>enginetypeohcv>fuelsystemspdi
# Since fuelsystemspdi is insignificant, we remove fuelsystemspdi and build model_17
model_17 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.enginetypeohcv + carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_17)
vif(model_17)

# Highest VIFs (>2): enginesize>stroke>enginetypeohcv
# All the above variables are significant. We check their correlations as they might be highly correlated.
cor(train$enginesize,train$stroke)                                      # ~ 21% correlation
cor(train$enginesize,train$carpr3.enginetypeohcv)                       # ~ 61% correlation
cor(train$stroke,train$carpr3.enginetypeohcv)                           # ~ 4.5% correlation
# The correlation of enginesize and enginetypeohcv is ~ 61%, indicating that the variables are highly correlated.
# So, remove the variable (enginetypeohcv) with lower significance level out of the two and build model_18.
model_18 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                stroke + peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_18)
vif(model_18)

# Both variables, enginesize and stroke have VIF>2, but also has very high significance. 
# So, we will have to check the impact of the removal of these variables on the model.
# We first remove stroke variable (higher VIF and p-value) and see the impact on adjusted R-square in model_19.
model_19 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + carpr3.carbodywagon + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_19)
vif(model_19)

# Adjusted R-square changes from 0.91 in model_18 to 0.90 in model_19 and all VIFs are < 2.
# So, we accept model_19 and will now remove variables only on the basis of significance/p-values.
# Since carbodywagon is most insignificant, we remove carbodywagon and build model_20.
model_20 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr2.carnmsubaru + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_20)

# Since carnmsubaru is most insignificant, we remove carnmsubaru and build model_21.
model_21 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmhonda + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_21)

# Since carnmhonda is most insignificant, we remove carnmhonda and build model_22.
model_22 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + carpr2.carnmmazda + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_22)

# Since carnmmazda is most insignificant, we remove carnmhonda and build model_23.
model_23 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw + 
                carpr2.carnmdodge + 
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_23)

# Since carnmdodge is most insignificant, we remove carnmdodge and build model_24.
model_24 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhardtop + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_24)

# Since carbodyhardtop is most insignificant, we remove carbodyhardtop and build model_25.
model_25 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                peakrpm + carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_25)

# Since peakrpm is most insignificant, we remove peakrpm and build model_26.
model_26 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0 + carpr3.symboling2, data = train)
summary(model_26)

# Since symboling2 is most insignificant, we remove symboling2 and build model_27.
model_27 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback + 
                carpr3.symboling0, data = train)
summary(model_27)

# Since symboling0 is most insignificant, we remove symboling0 and build model_28.
model_28 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + carpr2.carnmplymouth + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_28)

# Since carnmplymouth is most insignificant, we remove carnmplymouth and build model_29.
model_29 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumberthree + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_29)

# Since cylindernumberthree is most insignificant, we remove cylindernumberthree and build model_30.
model_30 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmmitsubishi + 
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_30)

# Since carnmmitsubishi is most insignificant, we remove carnmmitsubishi and build model_31.
model_31 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmsaab + carpr3.fuelsystemmfi + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_31)

# Since fuelsystemmfi is most insignificant, we remove fuelsystemmfi and build model_32.
model_32 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr2.carnmsaab + 
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_32)

# Since carnmsaab is most insignificant, we remove carnmsaab and build model_33.
model_33 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr3.cylindernumbertwelve + carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_33)

# Since cylindernumbertwelve is most insignificant, we remove cylindernumbertwelve and build model_34.
model_34 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr3.cylindernumbertwo + 
                carpr3.carbodyhatchback, data = train)
summary(model_34)

# Since carbodyhatchback is most insignificant, we remove carbodyhatchback and build model_35.
model_35 <- lm(formula = price ~ aspiration + enginelocation +  
                enginesize + 
                carpr2.carnmaudi + carpr2.carnmbmw +  
                carpr3.cylindernumbertwo, data = train)
summary(model_35)
# Model_35: R-squared is 0.8848 and Adjusted R-squared is 0.8797.

#############################################################################
# Call:
# lm(formula = price ~ aspiration + enginelocation + enginesize + 
#   carpr2.carnmaudi + carpr2.carnmbmw + carpr3.cylindernumbertwo, 
#   data = train)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6898.5 -1694.2   -48.6  1254.1  8129.9 
#
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                7149.802   2218.868   3.222 0.001592 ** 
# aspiration                -2862.426    678.000  -4.222 4.41e-05 ***
# enginelocation           -12205.639   1813.345  -6.731 4.32e-10 ***
# enginesize                  155.880      5.843  26.680  < 2e-16 ***
# carpr2.carnmaudi           4896.022   1384.816   3.536 0.000557 ***
# carpr2.carnmbmw            9326.044   1407.820   6.624 7.46e-10 ***
# carpr3.cylindernumbertwo   9636.999   1569.220   6.141 8.45e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 3020 on 136 degrees of freedom
# Multiple R-squared:  0.8848,	Adjusted R-squared:  0.8797 
# F-statistic: 174.1 on 6 and 136 DF,  p-value: < 2.2e-16
##############################################################################

# Predicting the results in test dataset
Predict_1 <- predict(model_35,test[,-18])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
# [1] 0.7762938
# This means R-squared on test data is 0.7762938.

# Variables significant in predicting the price of a car: aspiration, enginelocation, enginesize, carnmaudi, carnmbmw, cylindernumbertwo
# As per test data results, ~78% of the car price variation is explained by the linear model of the significant variables identified.
# As per training data results, ~88% of the car price variation is explained by the linear model of the significant variables identified.



