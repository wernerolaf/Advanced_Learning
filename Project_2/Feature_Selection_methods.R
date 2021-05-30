library('randomForest')
library('Boruta')
library('rmcfs')


setwd('C:/Users/Mateusz/Desktop/AML 2')

### Artificial dataset

data_train <- read.csv('Data/artificial_train.data', header = FALSE, sep = ' ')[1:500]
for(i in 1:ncol(data_train)){
  data_train[i] <- (data_train[i]-min(data_train[i]))/(max(data_train[i])-min(data_train[i]))
}
data_train['y'] <- read.csv('Data/artificial_train.labels', header = FALSE, sep = ' ')
data_train$y <- as.factor(ifelse(data_train$y == 1, 1, 0))
#data_valid <- read.csv('Data/artificial_valid.data', header = FALSE, sep = ' ')[1:500]

# Importance based on tree
mod_rf <- randomForest(y~., data=data_train)
imp <- importance(mod_rf)
vars21_imp <- rownames(imp)[imp >= Rfast::nth(varImpPlot(mod_rf), 21, descending = T)]
varImpPlot(mod_rf)
write.csv(imp[vars21_imp,], 'RF_importance.csv')

# Boruta algorithm
mod_bor <- Boruta(x=data_train[,-ncol(data_train)], y=data_train$y)
vars21_bor <- colnames(mod_bor$ImpHistory)[mod_bor$ImpHistory[95,1:500] > 0]
plot(mod_bor)
write.csv(mod_bor$ImpHistory[95,1:500][vars21_bor], 'Boruta_importance.csv')

# Monte Carlo Feature Selection
mod_rcfs <- mcfs(y~., data=data.frame(data_train), cutoffPermutations=0)
plot(mod_rcfs)
res <- head(mod_rcfs$RI[c('attribute', 'RI')], 21)
write.csv(res, 'MCFS_importance.csv', row.names = FALSE)

# Logistic regression
logit_model <- glm(y~., data=data_train, family=binomial)
names(logit_model$coefficients)[summary(logit_model)$coefficients[,4] < 0.05]


