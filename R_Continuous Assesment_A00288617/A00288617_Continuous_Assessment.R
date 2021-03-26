# R continuous Assessment by A00288617 
# Linear Regression (predicting the Lifetime.Post.Total.Reach)

# Data Processing

# install.packages("caTools")
library(caTools)
# install.packages("ggplot2")
library(ggplot2)

# Reading data from csv file
facebook = read.csv("data/dataset_Facebook.csv")
facebook

# get the summary of the data set
summary(facebook)


# Correlation of Variables in facebook
cor(facebook$Lifetime.Post.Total.Reach,facebook$Paid) # N/A
cor(facebook$Lifetime.Post.Total.Reach, facebook$Page.total.likes) # -0.08050363
cor(facebook$Lifetime.Post.Total.Reach, facebook$Post.Month) # -0.1005938
cor(facebook$Lifetime.Post.Total.Reach, facebook$Post.Weekday)
cor(facebook$Lifetime.Post.Total.Reach, facebook$Post.Hour) # 0.004084784
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.Post.Total.Impressions) # 0.6949889
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.Engaged.Users) # 0.5707588
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.Post.Consumers) # 0.4780405
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.Post.Consumptions) # 0.3245533
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page) # 0.3223649
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.Post.reach.by.people.who.like.your.Page) # 0.7431629
cor(facebook$Lifetime.Post.Total.Reach, facebook$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post) # 0.4010003
cor(facebook$Lifetime.Post.Total.Reach, facebook$comment) # 0.427325
cor(facebook$Lifetime.Post.Total.Reach, facebook$like) # N/A
cor(facebook$Lifetime.Post.Total.Reach, facebook$Share) # N/A
cor(facebook$Lifetime.Post.Total.Reach, facebook$Total.Interactions) # 0.5386761


# Removing some variables from the train data set that are not necessary
facebook$Paid = NULL # Has no correlation to the dependent variable
facebook$Type = NULL # factor Type variable
facebook$Category = NULL # Factor type variable

# checking the scatterplot
pairs(facebook)
hist(facebook$Lifetime.Post.Total.Reach)
# set the seed
set.seed(50)

# Divide the data set into training and test data

# setting the sample
face_sample = sample(1: nrow(facebook), size=0.8*nrow(facebook))
train = data.frame(facebook[face_sample,]) 
test = data.frame(facebook[-face_sample,])

# Getting the History and the summary of the training data set
hist(train$Lifetime.Post.Total.Reach)
summary(train)


# Fitting the model

# model-1:
model1 = lm(Lifetime.Post.Total.Reach ~ Page.total.likes + Post.Month + Post.Weekday + Post.Hour+ Lifetime.Post.Total.Impressions + Lifetime.Engaged.Users + Lifetime.Post.Consumers 
            + Lifetime.Post.Consumptions + Lifetime.Post.Impressions.by.people.who.have.liked.your.Page 
            + Lifetime.Post.reach.by.people.who.like.your.Page 
            + Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post + comment + like + Share 
            + Total.Interactions, data = train)
# Multiple R-squared:  0.963,	Adjusted R-squared:  0.9617 
summary(model1)
# Plotting Model1
plot(model1)

# Model-2:  # after checking the p values
# Removing Post.Weekday and Post.Hour and Total.Interactions

model2 = lm(Lifetime.Post.Total.Reach ~ Page.total.likes + Lifetime.Post.Total.Impressions + Lifetime.Engaged.Users + Lifetime.Post.Consumers 
            + Lifetime.Post.Consumptions + Lifetime.Post.Impressions.by.people.who.have.liked.your.Page 
            + Lifetime.Post.reach.by.people.who.like.your.Page 
            + Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post + comment + like + Share 
            , data = train)
# Multiple R-squared:  0.9626,	Adjusted R-squared:  0.9615
summary(model2)
plot(model2)

# Model-3: after checking the p values
# Removing Post.consumers and Post.Consumptions

model3 = lm(Lifetime.Post.Total.Reach ~ Page.total.likes + Lifetime.Post.Total.Impressions + Lifetime.Engaged.Users  
            + Lifetime.Post.Impressions.by.people.who.have.liked.your.Page 
            + Lifetime.Post.reach.by.people.who.like.your.Page 
            + Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post + comment + like + Share 
            , data = train)
summary(model3)
# Multiple R-squared:  0.9624,	Adjusted R-squared:  0.9615 
plot(model3)

# Model-4 
# Creating a model with the most significant Variables
model4 = lm(Lifetime.Post.Total.Reach ~ Page.total.likes + Post.Month + Lifetime.Post.Impressions.by.people.who.have.liked.your.Page 
            + Lifetime.Post.reach.by.people.who.like.your.Page 
            + Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post+ comment + like + Share, data = train)

summary(model4) # Multiple R-squared:  0.5687,	Adjusted R-squared:  0.5654 

# RESULT - MODEL 3 IS BETTER THAN THE OTHER MODELS

# Prediction of values using the model 1 

# Prediction Using Model 3 
predict(model3, test, interval = "confidence")
predict(model3, test, interval = "predict")
predict(model3,test)


# Plotting the predicted values for the train data set using the linear model
ggplot() + geom_point(aes(x = test$Page.total.likes + test$Post.Month + test$Post.Weekday + test$Post.Hour+ test$Lifetime.Post.Total.Impressions + test$Lifetime.Engaged.Users + test$Lifetime.Post.Consumers 
                          + test$Lifetime.Post.Consumptions + test$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page
                          + test$Lifetime.Post.reach.by.people.who.like.your.Page 
                          + test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post + test$comment + test$like + test$Share 
                          + test$Total.Interactions , y = test$Lifetime.Post.Total.Reach), colour = 'red') + geom_line(aes(x = test$Page.total.likes + test$Post.Month + test$Post.Weekday + test$Post.Hour+ test$Lifetime.Post.Total.Impressions + test$Lifetime.Engaged.Users + test$Lifetime.Post.Consumers 
                                                                                                                             + test$Lifetime.Post.Consumptions + test$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page 
                                                                                                                             + test$Lifetime.Post.reach.by.people.who.like.your.Page 
                                                                                                                             + test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post + test$comment + test$like + test$Share 

                                                                                                                             + test$Total.Interactions,  y = predict(model3, newdata = test)), colour = 'blue') + ggtitle('Lifetime.Post.Total.Reach vs  Model1 on Test Data Set (Linear Regression)') + xlab('Model 1 Training data') + ylab('Lifetime.Post.Total.Reach')

# POLYNOMIAL REGRESSIION


data(facebook)
pairs(facebook)


# Looking at the best model using linear regression
summary(model1) # Multiple R-squared:  0.963,	Adjusted R-squared:  0.9617

# Creating a model with values that are likely to have polynomial relations with the dependent variable using the scatter plot
# Model 1 (with degree = 2) and using the train data set
prmodel1 = lm(Lifetime.Post.Total.Reach ~ polym(Page.total.likes , Post.Hour , Lifetime.Post.Total.Impressions 
                                                , Lifetime.Engaged.Users , Lifetime.Post.Consumers , Lifetime.Post.Consumptions 
                                                , Lifetime.Post.reach.by.people.who.like.your.Page 
                                                , Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post , like
                                                , degree = 2, raw = T), data = train)
summary(prmodel1) # Multiple R-squared:  0.9632,	Adjusted R-squared:  0.9577 # No Improvement on model1

#Model 2 (with degree = 3) and using the train data set
prmodel2 = lm(Lifetime.Post.Total.Reach ~ polym(Page.total.likes , Post.Hour , Lifetime.Post.Total.Impressions 
                                                , Lifetime.Engaged.Users , Lifetime.Post.Consumers , Lifetime.Post.Consumptions 
                                                , Lifetime.Post.reach.by.people.who.like.your.Page 
                                                , Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post , like
                                                , degree = 3, raw = T), data = train)
summary(prmodel2) # Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9996  # Big improvement on model1

# Model 3 (with degree = 4) and using the full data set
prmodel3 = lm(Lifetime.Post.Total.Reach ~ polym(Page.total.likes , Post.Hour , Lifetime.Post.Total.Impressions 
                                                , Lifetime.Engaged.Users , Lifetime.Post.Consumers , Lifetime.Post.Consumptions 
                                                , Lifetime.Post.reach.by.people.who.like.your.Page 
                                                , Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post , like
                                                , degree = 4, raw = T), data = facebook)
summary(prmodel3) # Multiple R-squared:      1,	Adjusted R-squared:      1  # Model is seen to have a perfect fit

# model 3.1 (with degree 4) and using the train data set
prmodel3.1 = lm(Lifetime.Post.Total.Reach ~ polym(Page.total.likes , Post.Hour , Lifetime.Post.Total.Impressions 
                                                , Lifetime.Engaged.Users , Lifetime.Post.Consumers , Lifetime.Post.Consumptions 
                                                , Lifetime.Post.reach.by.people.who.like.your.Page 
                                                , Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post , like
                                                , degree = 4, raw = T), data = train)
# therefore, polynomial regression is better at predicting the Lifetime Post Total Reach than linear regression
#plot(prmodel3)
#
# Prediction Using prmodel 3, 3.1 and 2 
predict(prmodel3, test, interval = "confidence")
predict(prmodel3, test, interval = "predict")
predict(prmodel3,test) # far off/ wrong values
predict(prmodel3.1, test) # perfect values
predict(prmodel2, test) # reasonable values

# Plotting the predicted values for the data set using the polynomial model
ggplot() + geom_point(aes(x = test$Page.total.likes + test$Post.Hour + test$Lifetime.Post.Total.Impressions 
                          + test$Lifetime.Engaged.Users + test$Lifetime.Post.Consumers + test$Lifetime.Post.Consumptions 
                          + test$Lifetime.Post.reach.by.people.who.like.your.Page 
                          + test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post 
                          + test$like, y = test$Lifetime.Post.Total.Reach), colour = 'red') + geom_line(aes(x = test$Page.total.likes + test$Post.Hour 
                                                                                                                                               + test$Lifetime.Post.Total.Impressions + test$Lifetime.Engaged.Users + test$Lifetime.Post.Consumers + test$Lifetime.Post.Consumptions 
                                                                                                                                               + test$Lifetime.Post.reach.by.people.who.like.your.Page 
                                                                                                                                               + test$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post + test$like,  y = predict(prmodel2, newdata = test)), colour = 'blue') + ggtitle('Lifetime.Post.Total.Reach vs  prModel2on Test Data set Polynomial Regression') + xlab('prmodel2 data') + ylab('Lifetime.Post.Total.Reach')

