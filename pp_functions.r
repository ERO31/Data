library(forecast)
library(RANN)
library(caret)
library(e1071)

# This functions removes observations with response -1.
remove.weird.response <- function(x) {
    return (subset(x, cancel != -1))
}

# This functions reorders columns so numerical, binary, and factor data 
# are clustered together.
reorder <- function(x) {
    y <- x[,c(1,3,4,7,11,14,2,6,5,8,9,10,12,13,15,16,17,18)]
    y$claim.ind <- as.factor(x$claim.ind)
    y$ni.marital.status <- as.factor(x$ni.marital.status)
    y$year <- as.factor(x$year)
    return (y)
}

# This function replaces zip code by it's first digit, providing a 
# region clustering of zip codes.
cluster.zip <- function(x) {
    x$zip.code <- as.factor(floor(x$zip.code/10000))
    return (x)
}

# The following function performs BoxCox transformation of vector x given
# parmater lambda.
transform.boxcox <- function(x,lambda) {
    if (lambda == 0) {return(log(x))}
    else {return((x^lambda-1)/lambda)}
}

my.boxcox <- function(train, test = NULL, plot = F) {

    n <- 6
    lambda <- seq(0,n)
    y.train <- matrix(0, nrow = nrow(train), ncol = n)
    if (!is.null(test)) {
        y.test <- matrix(0, nrow = nrow(train), ncol = n)
    }

    # Prepare objects for plotting (optional)
    if (plot == T) {
        par(mfrow=c(3,4))
        var.label = c('tenure','n.adults','n.children','premium','len.at.res',
                    'ni.age')
    }
    
    for (i in 1:n) {
        
        # Find best value of Box Cox parameter.
        lambda[i] = BoxCox.lambda(train[,i])
        
        # Perform Box Cox transformation.
        y.train[,i] = transform.boxcox(train[,i],lambda[i])
        if (!is.null(test)) {
            y.test[.t,i] = transform.boxcox(test[,i],lambda[i])
        }

        # Plot.
        if (plot == T) {
            hist(train[,i], xlab = 'Count', ylab = 'Frequency', 
                 main = paste(var.label[i],'(before)'))
            hist(y.train[,i], xlab = 'Count', ylab = 'Frequency',
                 main = paste(var.label[i],'(after)'))
        }
        
    }
    return(list(train = train, test = test))
}

my.center.and.scale <- function(train, test = NULL, plot = T) {
    # Center and scale.
    n <- 16
    my.object <- preProcess(train[,1:n], method=c("center","scale"))
    y.train <- predict(my.object, train[,1:n])
    if (!is.null(test)) {
        test[,1:n] = predict(my.object, test[,1:n])
    }
    
    # Plot, if so desired.
    if (plot == T) {
        par(mfrow=c(3,4))
        var.label = c('tenure','n.adults','n.children','premium','len.at.res',
                      'ni.age')
        for (i in 1:6) {
            hist(train[,i], xlab = 'Count', ylab = 'Frequency', 
                 main = paste(var.label[i],'(before)'))
            hist(y.train[,i], xlab = 'Count', ylab = 'Frequency',
                 main = paste(var.label[i],'(after)'))
        }
    }
    train[,1:n] <- y.train
    return(list(train = train, test = test))
}

my.factor.to.dummy <- function(train, test = NULL) {
    # Convert factor's to dummy's.
    my.object <- dummyVars( ~ claim.ind + ni.marital.status + ni.gender +
                            sales.channel + coverage.type +
                            dwelling.type + credit + house.color + year +
                            zip.code,train, fullRank = T)
    train <- cbind(train[,1:6],predict(my.object, train),train[,17:18])
    if (!is.null(test)) {
        test = predict(my.object, test)
    }
    return(list(train = train, test = test))
}

my.na.poofer <- function(train, test = NULL) {
    
    # Remove NA's using classification tree imputation.
    n <- 16
    for (i in 1:n) {
        idx = which(is.na(train[,i]))
        #if (!is.null(test)) {test.idx = which(is.na(test[,i]))}
        if (class(train[,i])=="numeric") {
            train[idx,i] = median(train[,i], na.rm = T)
            #if (!is.null(test)) {test[test.idx,i] = which(is.na(test[,i]))}
        }
        else if (class(train[,i])=="factor") {
            train[idx,i] = labels(which.max(summary(train[,i])))
            #if (!is.null(test)) {
            #    test[test.idx,i] = labels(which.max(summary(class(test[,i]))))
            #}
        }
    }
    return(list(train = train, test = test))
}

my.preprocess = function(train,test = NULL) {
    
    # Remove weird -1 responses.
    train <- remove.weird.response(train)
    if (!is.null(test)) { test = remove.weird.response(test)}
    
    # Reorder columns so numeric and factor are next to each other.
    train <- reorder(train)
    
    # Cluster by zip code.
    train <- cluster.zip(train)
    if (!is.null(test)) { test = cluster.zip(test)}
    
    # Center, scale, and Box Cox transformation.
    pp.object <- preProcess(train[,1:16], method=c("BoxCox","center","scale"))
    train <- predict(pp.object, train)
    if (!is.null(test)) { test = predict(pp.object, test) }
    
    # Impute NA's.
    result <- my.na.poofer(train,test)
    
    return(result)
    
}