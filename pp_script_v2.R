##########################################################################
# 1. Optional: Run this part of the script to investigate pre-processing.#
# NOT Unit tested. This part only for investigation. Go to step 2 for    #
# for beginning of actual pre-processing pipeline.                       #
##########################################################################

# Load data
train <- read.csv('train.csv')
summary(train)
# test <- read.csv('test.csv')

# Remove rows with response -1.
train <- remove.weird.response(train)
summary(train)

# Reorder columns so numeric and factor data are grouped together.
train <- reorder(train)
summary(train)

# Cluster by first digit of zip code. This creates a multisate
# geographic clusters.
train <- cluster.zip(train)
summary(train)

# Perform Box-Cox transformation of data to make numerical predictors
# a little more well-behaved.
result <- my.boxcox(train, plot = T)
train <- result$train
summary(train)

# Perform centering and scaling of data. This makes linear classifiers
# like logistic regression and SVM work better.
result <- my.center.and.scale(train, plot = T)
train <- result$train
summary(train)

# Remove NA's be replacing by median (numeric) or mode (factor).
result <- my.na.poofer(train)
train <- result$train
summary(train)

##########################################################################
########### 2. Steps that need to be done one time all at once. ##########
##########################################################################
train <- read.csv('train.csv')
test <- read.csv('test.csv')
pp.result = my.preprocess(train,test)
pp.train <- pp.result$train
pp.test <- pp.result$test
summary(pp.train)
summary(pp.test)
write.csv(pp.train, file = "pp_train_v2.csv", row.names = F)
write.csv(pp.test, file = "pp_test_v2.csv", row.names = F)

##########################################################################
# 3. These steps need to be performed on train and cv/test set every time.
##########################################################################

# Load pre-processed data.
col.classes = c('integer','integer','integer','numeric','numeric',
                'numeric','integer','factor','factor','factor',
                'factor','factor','factor','factor','factor',
                'factor','integer','integer')
pp.train <- read.csv('pp_train_v2.csv', colClasses = col.classes)
pp.test <- read.csv('pp_test_v2.csv', colClasses = col.classes)

# Perform training-set based transformations.
t.result = my.transform(pp.train, pp.test)

# View results.
t.train <- t.result$train
t.test <- t.result$test
summary(t.train)
summary(t.test)

##########################################################################
###### 4. Test case: 1st order Logistic regression without ###############
###### cross-validation. #################################################
##########################################################################
model <- glm(cancel ~ ., data = t.train[,1:17], family = binomial)
y.hat <- predict(model, type = 'response')
y.hat.test <- predict(model, data = t.test, type = 'response')
proportion <- 1 - mean(t.train$cancel)
cutoff <- quantile(y.hat, proportion)
training.prediction <- (y.hat > cutoff)*1
training.accuracy = mean(t.train$cancel == training.prediction)
print(training.accuracy)

##########################################################################
###### 5. Test case: 2nd order Logistic regression without ###############
###### cross-validation. Not implemented. ################################
##########################################################################
my.formula <- as.formula("cancel ~ tenure + n.adults + n.children + 
                         premium + len.at.res + ni.age + claim.ind +
                         ni.marital.status + ni.gender + sales.channel +
                         coverage.type + dwelling.type + credit +
                         house.color + year + zip.code")