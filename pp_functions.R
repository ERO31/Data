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
    y$year <- factor(x$year)
    return (y)
}

# This function replaces zip code by it's first digit, providing a 
# region clustering of zip codes.
cluster.zip <- function(x) {
    x$zip.code <- factor(floor(x$zip.code/10000))
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
    
    # Remove NA's using median (numeric) or mode (factor).
    n <- 16
    for (i in 1:n) {
        
        # Find NA indices.
        idx = which(is.na(train[,i]))
        if (!is.null(test)) {idx2 = which(is.na(test[,i]))}
        
        # If numeric or int, replace NA's with median.
        if (class(train[,i])=="numeric" | class(train[,i])=="integer") {
            med <- median(train[,i], na.rm = T)
            train[idx,i] = med
            if (!is.null(test) & length(idx2) != 0) { 
                test[idx2,i] = med 
                }
        }
        
        # If factor, replaces NA's with mode.
        else if (class(train[,i])=="factor") {
            mod <- labels(which.max(summary(train[,i])))
            train[idx,i] = mod
            if (!is.null(test) & length(idx2) != 0) { 
                test[idx2,i] = mod 
                }
        }
    }
    return(list(train = train, test = test))
}

my.preprocess = function(train,test) {
    
    # One-time preprocessing for insurance data.
    
    # Remove weird -1 responses.
    train <- remove.weird.response(train)
    
    # Reorder columns so numeric and factor are next to each other.
    train <- reorder(train)
    test <- reorder(test)
    
    # Cluster by zip code.
    train <- cluster.zip(train)
    test <- cluster.zip(test)
    
    return(list(train = train, test = test))
}

my.transform = function(train,test = NULL) {
    
    # Pre-processing that needs to be done every time a model is
    # fitted.
    
    # Impute NA's.
    result <- my.na.poofer(train,test)
    
    # Center, scale, and Box Cox transformation.
    caret.object <- preProcess(result$train[,1:16], 
                               method=c("BoxCox","center","scale"))
    result$train <- predict(caret.object, result$train)
    if (!is.null(result$test)) { test = predict(caret.object, result$test) }
    
    return(result)
}