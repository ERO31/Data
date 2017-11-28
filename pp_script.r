##########################################################################
####### Run this part of the script to investigate processing ############
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
################ Or, do all the preprocessing at once. ###################
##########################################################################
train <- read.csv('train.csv')
result = my.preprocess(train)
train <- result$train
summary(train)

##########################################################################
##### Full model 1st order Logistic regression, just as a first pass #####
##########################################################################
model <- glm(cancel ~ ., data = train[,1:17], family = binomial)
y.hat <- predict(model, data = train, type = 'response')
proportion <- 1 - mean(train$cancel)
cutoff <- quantile(y.hat, proportion)
training.accuracy = mean(train$cancel == (y.hat > cutoff)*1)
print(training.accuracy)