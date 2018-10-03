############################ BeerMart Beer Recommendation System #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 4.1 Split
# 4.2 Cross-validation
# 5. Model Evaluation and Comparison
# 6. Model Recommendations
# 7. Key Observations and Insights
###################################################################################################

###########################################################################################################################################################
# 1. Business Understanding: 
# The objective is to build a collaborative filtering recommendation system where customers will be recommended the beer that they are most likely to buy.
###########################################################################################################################################################

###########################################################################################################################################################
# 2. Data Understanding: 
# Beer data which is a database of ratings provided by users. Each record is composed of a beer's name, the name of the user along with ratings provided by 
# users. All ratings are on a scale from 1 to 5 with 5 being the best.
# Number of Columns: 3 -> beer_beerid, review_profilename, review_overall
# Number of Rows: 475984
###########################################################################################################################################################

###########################################################################################################################################################
# 3. Data Preparation: 
# Loading Necessary libraries
library(recommenderlab)
library(dplyr)
library(ggplot2)

# Loading Data
beer_df <- read.csv("beer_data.csv")

# Structure of the dataset
str(beer_df)
beer_df1 <- beer_df[,c(2,1,3)]          # Rearranging columns to user | item | rating format
View(beer_df1)

# Understanding Dimensions
dim(beer_df1)
# 475984      3

# Exploring the data
beer_df1$beer_beerid = factor(beer_df1$beer_beerid)
summary(beer_df1)
#      review_profilename  beer_beerid     review_overall 
# northyorksammy:  1846   2093   :   987   Min.   :0.000  
# mikesgroove   :  1379   412    :   976   1st Qu.:3.500  
# BuckeyeNation :  1338   1904   :   905   Median :4.000  
# Thorpe429     :  1072   1093   :   848   Mean   :3.815  
# ChainGangGuy  :  1046   92     :   818   3rd Qu.:4.500  
# NeroFiddled   :  1031   4083   :   808   Max.   :5.000  
# (Other)       :468272   (Other):470642

# Checking missing values and blank space entries
sapply(beer_df1, function(x) sum(is.na(x)))
# review_profilename        beer_beerid     review_overall
#                  0                  0                  0
# There appears to be no missing values. However, we also need to check for only blank spaces as column entries.
sapply(beer_df1, function(x) length(which(nchar(trimws(x))==0)))
# review_profilename        beer_beerid     review_overall
#                100                  0                  0
# There are 100 rows in review_profilename column with blank spaces. So, we remove these rows from our analysis.
beer_df2 <- beer_df1[which(nchar(trimws(beer_df1$review_profilename))!=0),]
# Removing duplicate ratings for the same beer_beerid and review_profilename combination
beer_df3 <- distinct(beer_df2, review_profilename, beer_beerid, .keep_all = TRUE)
dim(beer_df3)
# 474462      3

# Since collaborative filtering algorithms do matrix processing which can get computationally intensive, we filter out beer ids and review ids which don't 
# have significant number of reviews.
# Filtering only those beers that have at least N number of reviews
beerid_grp <- arrange(summarise(group_by(beer_df3, beer_beerid), freq=n(), na.rm=T),desc(freq))
quantile(beerid_grp$freq, probs = seq(0, 1, by= 0.05))
# 0%   5%  10%  15%  20%  25%  30%  35%  40%  45%  50%  55%  60%  65%  70%  75%  80% 85%  90%  95% 100% 
#  1    1    1    1    1    1    1    1    1    2    2    2    2    3    4    5    7  12   21   51  977
# Since 95 percentile of beer id wise rating frequencies fall at 51 reviews, we choose only those beers that have at least 50 reviews, i.e. N=50
beer_df4 <- merge(beer_df3, beerid_grp, by="beer_beerid")
beer_df5 <- beer_df4[which(beer_df4$freq >= 50),c(1,2,3)]
# Filtering only those users that have at least certain number of reviews
userid_grp <- arrange(summarise(group_by(beer_df5, review_profilename), freq=n(), na.rm=T),desc(freq))
quantile(userid_grp$freq, probs = seq(0, 1, by= 0.05))
# 0%   5%  10%  15%  20%  25%  30%  35%  40%  45%  50%  55%  60%  65%  70%  75%  80% 85%  90%  95% 100% 
#  1    1    1    1    1    1    1    1    2    2    3    3    4    6    7   10   15  24   40   76  518 
# Since 80 percentile of user id wise rating frequencies fall at 15 reviews, we choose only those users that have at least 15 reviews.
beer_df6 <- merge(beer_df5, userid_grp, by="review_profilename")
beer_df7 <- beer_df6[which(beer_df6$freq >= 15),c(1,2,3)]

# Exploring the clean dataset
dim(beer_df7)
# 246836      3
length(unique(beer_df7$beer_beerid))
# 2064
length(unique(beer_df7$review_profilename))
# 4056
str(beer_df7)
# 'data.frame':	246836 obs. of  3 variables:
#  $ review_profilename: Factor w/ 22498 levels "","0110x011",..: 2 2 2 2 2 2 2 2 2 2 ...
#  $ beer_beerid       : Factor w/ 40308 levels "3","4","5","6",..: 22897 19823 1148 5459 23083 819 6818 4391 5215 7624 ...
#  $ review_overall    : num  4.5 3.5 4.5 4 4 4 4 4 5 4.5 ...

# Convert data frame to a realratingMatrix type for further analysis
r1 <- as(beer_df7, "realRatingMatrix")
class(r1)
# [1] "realRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"

# Exploring the realratingMatrix data
dimnames(r1)
rowCounts(r1)
colCounts(r1)
rowMeans(r1)
colMeans(r1)

# How similar are the first ten users with each other?
similar_users <- similarity(r1[1:10, ], method = "cosine", which = "users")
# Similarity matrix
as.matrix(similar_users)
# Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")
# Inference: Users 1, 2, 3, 7 and 10 are similar

# How similar are the first ten items with each other?
similar_items <- similarity(r1[,1:10], method = "cosine", which = "items")
# Similarity matrix
as.matrix(similar_items)
# Visualise similarity matrix
image(as.matrix(similar_items), main = "Item similarity")
#Inference: Items 3 and 5 are similar

# Visualizing ratings
qplot(getRatings(r1), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(r1)) # Skewed to the right
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   3.500   4.000   3.872   4.500   5.000 
# Overall average of ratings: 3.872

# Visualizing ratings after normalization
qplot(getRatings(normalize(r1, method = "Z-score")), main = "Histogram of normalized ratings", xlab = "Rating") 
summary(getRatings(normalize(r1, method = "Z-score"))) # seems better
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.5747 -0.5339  0.1267  0.0000  0.6977  3.2027

# How many beers did people rate on average
qplot(rowCounts(r1), binwidth = 10, 
      main = "Beers Rated on average", 
      xlab = "# of users", 
      ylab = "# of beers rated")
# Most users rate less number of beers and they get tired of rating at a logarithmic pace. Very few users have rated higher number of beers.

# What is the mean rating of each beer
qplot(colMeans(r1), binwidth = 1, 
      main = "Mean rating of Beers", 
      xlab = "Rating", 
      ylab = "# of beers")

# Understand users and ratings:

# What are the unique values of ratings
sort(unique(getRatings(r1)), decreasing = TRUE)
# 5.0 4.5 4.0 3.5 3.0 2.5 2.0 1.5 1.0

summary(colMeans(r1))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.258   3.661   3.869   3.808   4.042   4.640 
# The average beer ratings: 3.808

summary(rowMeans(r1))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.824   3.755   3.906   3.883   4.033   4.733
# The average user ratings: 3.883

summary(colCounts(r1))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.0    60.0    86.0   119.6   142.0   698.0
# The average number of ratings given to the beers: 119.6

summary(rowCounts(r1))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.00   23.00   38.00   60.86   73.00  518.00
# The average number of ratings given to the users: 60.86
###########################################################################################################################################################

###########################################################################################################################################################
# 4. Model Building
# List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models) # 9 types of models
# "ALS_realRatingMatrix"          "ALS_implicit_realRatingMatrix"
# "IBCF_realRatingMatrix"         "POPULAR_realRatingMatrix"     
# "RANDOM_realRatingMatrix"       "RERECOMMEND_realRatingMatrix" 
# "SVD_realRatingMatrix"          "SVDF_realRatingMatrix"        
# "UBCF_realRatingMatrix"

# description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")
# In this assignment, we will compare user based and item based collaborative filtering.
# Checking the parameters of these two models
recommender_models$UBCF_realRatingMatrix$parameters
# $`method`
# [1] "cosine"
# $nn
# [1] 25
# $sample
# [1] FALSE
# $normalize
# [1] "center"

recommender_models$IBCF_realRatingMatrix$parameters
# $`k`
# [1] 30
# $method
# [1] "Cosine"
# $normalize
# [1] "center"
# $normalize_sim_matrix
# [1] FALSE
# $alpha
# [1] 0.5
# $na_as_zero
# [1] FALSE

# 4.1. Split
# Divide data into train and test 
scheme <- evaluationScheme(r1, method = "split", train = .9,
                           k = 1, given = 10, goodRating = 4)
?evaluationScheme
# Arguments:
# train and test
#    Here we create an evaluation scheme which splits the users into a training set (90%) and a test set (10%). 
# k=1 denotes split.
# given 
#    During testing, the Given=x protocol presents the algorithm with only x randomly chosen items for the test user, and the algorithm is evaluated by how #    well it is able to predict the withheld items. For this test set, 10 items(given argument) will be given to the recommender algorithm and the other
#    items will be held out for computing the error. The modelling was also carried out for given=2, given=3 and given=5 keeping all other parameters 
#    constant. However, given=10 yielded better results so we have used given=10.
# With goodRating=4, all items with actual user rating of greater or equal 4 are considered positives in the evaluation process.

scheme
# Evaluation scheme with 10 items given
# Method: 'split' with 1 run(s).
# Training set proportion: 0.900
# Good ratings: >=4.000000
# Data set: 4056 x 2064 rating matrix of class 'realRatingMatrix' with 246836 ratings.

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=25, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# 5. Model Evaluation and Comparison
# Run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
# UBCF run fold/sample [model time/prediction time]
# 	       1  Warning: Unknown parameters: minRating
# Available parameter (with default values):
# method	 =  cosine
# nn	 =  25
# sample	 =  FALSE
# normalize	 =  center
# verbose	 =  FALSE
# [0.32sec/24.67sec] 
# IBCF run fold/sample [model time/prediction time]
#          1  [136.89sec/0.48sec] 
class(results)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
# See precision/recall
plot(results, "prec/rec", annotate=TRUE)
# UBCF performs better than IBCF as per the ROC and precision/recall curves.

# Evaluating prediction error of algorithms
# Creating recommender models using training data on both algorithms
rec1 <- Recommender(getData(scheme, "train"), "UBCF",
                    param=list(normalize = "Z-score", method="Cosine", nn=25, minRating=3))
rec2 <- Recommender(getData(scheme, "train"), "IBCF", 
                    param=list(normalize = "Z-score"))

# Compute predicted ratings for the known part of the test data (10 items for each user) using the two algorithms
p1 <- predict(rec1, getData(scheme, "known"), type="ratings")
p1
# 406 x 2064 rating matrix of class 'realRatingMatrix' with 833924 ratings.
p2 <- predict(rec2, getData(scheme, "known"), type="ratings")
p2
# 406 x 2064 rating matrix of class 'realRatingMatrix' with 58099 ratings.

# Calculate the error between the prediction and the unknown part of the test data
error <- rbind(UBCF = calcPredictionAccuracy(p1, getData(scheme, "unknown")),
               IBCF = calcPredictionAccuracy(p2, getData(scheme, "unknown")))
error
#           RMSE       MSE       MAE
# UBCF 0.6806577 0.4632949 0.5093671
# IBCF 0.9914376 0.9829484 0.6947671
# UBCF performs better than IBCF in terms of prediction error as well.

# 4.2. Cross-validation
# Divide data into train and test 
scheme <- evaluationScheme(r1, method = "split", train = .9,
                           k = 4, given = 10, goodRating = 4)
# k=4 denotes 4-fold cross-validation
scheme
# Evaluation scheme with 10 items given
# Method: 'split' with 4 run(s).
# Training set proportion: 0.900
# Good ratings: >=4.000000
# Data set: 4056 x 2064 rating matrix of class 'realRatingMatrix' with 246836 ratings.

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=25, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# 5. Model Evaluation and Comparison
# Run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
# UBCF run fold/sample [model time/prediction time]
# 	       1  Warning: Unknown parameters: minRating
# Available parameter (with default values):
# method	 =  cosine
# nn	 =  25
# sample	 =  FALSE
# normalize	 =  center
# verbose	 =  FALSE
# [0.26sec/23.95sec]
# 	       2  Warning: Unknown parameters: minRating
# Available parameter (with default values):
# method	 =  cosine
# nn	 =  25
# sample	 =  FALSE
# normalize	 =  center
# verbose	 =  FALSE
# [0.27sec/24.18sec]
# 	       3  Warning: Unknown parameters: minRating
# Available parameter (with default values):
# method	 =  cosine
# nn	 =  25
# sample	 =  FALSE
# normalize	 =  center
# verbose	 =  FALSE
# [0.3sec/24.04sec]
# 	       4  Warning: Unknown parameters: minRating
# Available parameter (with default values):
# method	 =  cosine
# nn	 =  25
# sample	 =  FALSE
# normalize	 =  center
# verbose	 =  FALSE
# [0.29sec/23.82sec]
# IBCF run fold/sample [model time/prediction time]
#          1  [134.43sec/0.21sec]
#          2  [132.06sec/0.21sec]
#          3  [131.03sec/0.19sec]
#          4  [131.61sec/0.22sec]

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
# See precision/recall
plot(results, "prec/rec", annotate=TRUE)
# UBCF performs better than IBCF as per the ROC and precision/recall curves.

# Evaluating prediction error of algorithms
# Creating recommender models using training data on both algorithms
rec1 <- Recommender(getData(scheme, "train"), "UBCF",
                    param=list(normalize = "Z-score", method="Cosine", nn=25, minRating=3))
rec2 <- Recommender(getData(scheme, "train"), "IBCF", 
                    param=list(normalize = "Z-score"))

# Compute predicted ratings for the known part of the test data (10 items for each user) using the two algorithms
p1 <- predict(rec1, getData(scheme, "known"), type="ratings")
p1
# 406 x 2064 rating matrix of class 'realRatingMatrix' with 833924 ratings.
p2 <- predict(rec2, getData(scheme, "known"), type="ratings")
p2
# 406 x 2064 rating matrix of class 'realRatingMatrix' with 50720 ratings.

# Calculate the error between the prediction and the unknown part of the test data
error <- rbind(UBCF = calcPredictionAccuracy(p1, getData(scheme, "unknown")),
               IBCF = calcPredictionAccuracy(p2, getData(scheme, "unknown")))
error
#           RMSE       MSE       MAE
# UBCF 0.6741788 0.4545171 0.5044452
# IBCF 1.0117599 1.0236582 0.7493795
# UBCF performs better than IBCF in terms of prediction error as well.

# Comparing model evaluation results above for with and without cross-validation using UBCF and IBCF, we conclude that UBCF with 4-fold cross-validation 
# should be deployed. 
###########################################################################################################################################################

###########################################################################################################################################################
# 6. Model Recommendations
# Names of the top 5 beers that you would recommend to the users "cokes":
rec_user1 <- predict(rec1, r1["cokes",], n=5)
as(rec_user1, "list")
# $`cokes`
# [1] "7971" "88"   "571"  "30"   "412"

# Names of the top 5 beers that you would recommend to the users "genog":
rec_user2 <- predict(rec1, r1["genog",], n=5)
as(rec_user2, "list")
# $`genog`
# [1] "7971"  "599"   "875"   "571"   "34420"

# Names of the top 5 beers that you would recommend to the users "giblet":
rec_user3 <- predict(rec1, r1["giblet",], n=5)
as(rec_user3, "list")
# $`giblet
# [1] "7971"  "39"    "61"    "19960" "1904"
###########################################################################################################################################################

###########################################################################################################################################################
# Key Observations and Insights:
# 1. The above analysis is based on the beer rating data which includes beer ids having at least 50 reviews and user ids having at least 15 reviews. This 
#    restriction has been used so that the algorithm processing is not computationally intensive and can run faster. However, with more data, we may see 
#    better results.
# 2. The models used in the analysis are User based collaborative filtering (UBCF) & Item based collaborative filtering (IBCF). While IBCF is a less memory 
#    intensive approach than UBCF, UBCF gives better results than IBCF for the above analysis. 
# 3. Evaluation methods used in the analysis include ROC curve, precision/recall curve and prediction error in terms of RMSE, MSE & MAE.
# 4. Cosine similarity has been used to calculate similarity between users and items. The computation of similarity can also be done by other methods like 
#    Pearson distance, Jaccard distance, etc.
###########################################################################################################################################################

