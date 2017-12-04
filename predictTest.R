library(gbm)
library(ROCR)

set.seed(8051)

data.train = read.csv("pp_train.csv")
data.test = read.csv("pp_test.csv")

data.train = na.omit(data.train)
data.train = data.train[data.train$ni.age < 100,]

for(i in 1:16){
  idx = which(is.na(data.test[,i]))
  
  if (class(data.test[,i])=="numeric" | class(data.test[,i])=="integer") {
    med <- median(data.test[,i], na.rm = T)
    data.test[idx,i] = med
  }
  else if (class(data.test[,i])=="factor") {
    mod <- labels(which.max(summary(data.test[,i])))
    data.test[idx,i] = mod
  }
}

data.test$dwelling.type[data.test$dwelling.type == "Landlord"] = "House"

logit = glm(cancel ~  
              factor(claim.ind) + sales.channel + credit + factor(zip.code) + dwelling.type + coverage.type + 
              n.adults*n.children + (tenure + len.at.res) * ni.age + premium + ni.marital.status,
            data = data.train, family = binomial(link = "logit") )

pred.prob = predict(logit, data.test, type = "response")
pred.class = ifelse(pred.prob > 1/6,1,0)

output = list(id = data.test$id, prob = pred.prob, class = pred.class)
write.csv(output,file = "group4.csv")




