######################## Logistic Regression  62.68% ##############################################################################
set.seed(101)
#2. Splitting into training and test sets
#Train/Test on season since we are predicting a tourney
train_set <- compTour %>% filter(Season <= 2015)
test_set <- compTour %>% filter(Season > 2015)

#Train logistic regression model with Team1Win as dependent on Seed1, Seed2, and WinPct
log_reg <- glm(Team1Win ~ Seed1 + Seed2 + Team1WinPct, 
               family = binomial(link = 'logit'), 
               data = train_set)
summary(log_reg)

#Predicting with the test set
prob_pred <- predict(log_reg, type = 'response', newdata = test_set)
#Assign 1 if greater than 0.50 and 0 otherwise 
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
#Confusion matrix to see how accurate
cm <- table(test_set[,17],y_pred)
cm
cat((cm[1,1] + cm[2,2]) / sum(cm), "accuracy")

#Train logistic model and add Daynum and Numot

log_reg <- glm(Team1Win ~ Seed1 + Seed2 + Team1WinPct + Daynum + Numot, 
               family = binomial(link = 'logit'), 
               data = train_set)
summary(log_reg)

prob_pred <- predict(log_reg, type = 'response', newdata = test_set)
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

cm <- table(test_set[,17],y_pred)
cm
cat((cm[1,1] + cm[2,2]) / sum(cm), "accuracy")

#Train logistic model using Detailed Season data
train_set <- finalTour %>% filter(Season <= 2015)
test_set <- finalTour %>% filter(Season > 2015)

log_reg <- glm(as.formula(paste(colnames(train_set)[15], "~",
                                 paste(colnames(train_set)[c(6,10,16:29)], collapse = "+"),
                                 sep = "")),
               family = binomial(link = 'logit'),
               data = train_set)
summary(log_reg)

#Drop DiffFtm which was causing rank-deficientcy
train_set <- train_set[ , c(1:20,22:29)]
test_set <- test_set[ , c(1:20,22:29)]

log_reg <- glm(as.formula(paste(colnames(train_set)[15], "~",
                                paste(colnames(train_set)[c(6,10,16:28)], collapse = "+"),
                                sep = "")),
               family = binomial(link = 'logit'),
               data = train_set)
summary(log_reg)

prob_pred <- predict(log_reg, type = 'response', newdata = test_set)
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

cm <- table(test_set[,15], y_pred)
cm
cat((cm[1,1] + cm[2,2]) / sum(cm), "accuracy")

#K-folds Cross Validation with 10 folds`
folds <- createFolds(train_set, k = 10)
cv <- lapply(folds, function(x){
  train_fold <- train_set[-x,]
  test_fold <- test_set[-x,]
  log_reg <- glm(as.formula(paste(colnames(train_set)[15], "~",
                                 paste(colnames(train_set)[c(6,10,16:29)], collapse = "+"),
                                 sep = "")),
                family = binomial(link = 'logit'),
                data = train_fold)
  prob_pred <- predict(log_reg, type = 'response', newdata = test_fold)
  y_pred <- ifelse(prob_pred > 0.5, 1, 0)
  cm <- table(test_fold[,15], y_pred)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))
cat(accuracy, 'accuracy')

###################### Random Forest 65.67% ###################################################################################
fitControl <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 3
                           )

rfgrid <- expand.grid(mtry = c(4,5,6))
forest <- train(as.formula(paste(colnames(train_set)[15], "~", 
                                 paste(colnames(train_set)[c(6,10,16:28)], collapse = "+"), 
                                 sep = '')), data = train_set, method = 'rf', trControl = fitControl, tuneGrid = rfgrid)
forest <- randomForest(as.formula(paste(colnames(train_set)[15], "~", 
                                        paste(colnames(train_set)[c(6,10,16:28)], collapse = "+"), 
                                        sep = '')), data = train_set, ntree = 400, mtry = 6)

prob_pred <- predict(forest, newdata = test_set, type = 'response')
cm <- table(test_set[,15],prob_pred)
sum(diag(cm)) / sum(cm)

importance(forest)
summary(forest)
varImpPlot(forest)
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
rf_plot <- plot(forest, log = 'y', main = 'MSE rates for Random Forest')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="Trees", ylab="")
legend("top", c('OOB','Lose','Win'),cex=0.8,fill=1:4)

###################### Boosting  70.14% ###################################################################################

gbmgrid <- expand.grid(n.trees = c(5,7,9)*1000,
                       interaction.depth = c(2,4,6),
                       shrinkage = c(.01, .005, .001),
                       n.minobsinnode = c(10,20,40)
)
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats = 3)

gbm <- train(as.formula(paste(colnames(train_set)[15],'~',paste(colnames(train_set)[c(6,10,16:28)], collapse = '+'),sep = '')), 
             data = train_set, method = 'gbm', trControl = fitControl, tuneGrid = gbmgrid, nTrain = .5)
plot(gbm)

train_set$Team1Win <- as.numeric(train_set$Team1Win)
train_set$Team1Win <- train_set$Team1Win - 1
test_set$Team1Win <- as.numeric(test_set$Team1Win)
test_set$Team1Win <- test_set$Team1Win - 1

boost <- gbm(as.formula(paste(colnames(train_set)[15],'~',paste(colnames(train_set)[c(6,10,16:28)], collapse = '+'),
                        sep = '')), data = train_set, distribution = 'bernoulli', n.trees = 5000, 
             train.fraction = 0.5,interaction.depth = 6, n.minobsinnode = 10, verbose = TRUE )

prob_pred <- predict(boost, newdata = test_set, n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
cm <- table(test_set[,15], y_pred)
cm
sum(diag(cm)) / sum(cm)

summary(boost)


###################### Predicting the 2017 Tournament ################################################################## 
# Round 1
train_set <- finalTour
test_set <- r1
train_set$Team1Win <- as.numeric(train_set$Team1Win)
train_set$Team1Win <- train_set$Team1Win - 1

boost <- gbm(as.formula(paste(colnames(train_set)[15],'~',paste(colnames(train_set)[c(6,10,16:29)], collapse = '+'),
                              sep = '')), data = train_set, distribution = 'bernoulli', n.trees = 5000, 
             train.fraction = 0.5,interaction.depth = 6, n.minobsinnode = 10, verbose = TRUE )

prob_pred <- predict(boost, newdata = test_set[,c(6,8,41:54)], n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
test_set$Team1Win <- y_pred
test_set$probability <- prob_pred
test_set$nextRound1 <- ifelse(test_set$Team1Win == 1, test_set$Slot, NA)
test_set$nextRound2 <- ifelse(test_set$Team1Win == 0, test_set$Slot, NA)
test_set <- test_set[, c(1:12,55,56,13:54)] #Round 1 predictions
df <- seeds2017 %>% left_join(test_set[,c(9,11)], by = c('Team' = 'Team1')) %>% left_join(test_set[,c(10,12)], by = c('Team' = 'Team2'))
df <- df[-which(rowMeans(is.na(df)) > .2),]
df$nextRound <- ifelse(is.na(df$nextRound1), df$nextRound2, df$nextRound1)
df <- df[,c(1:3,6)]
df <- df %>% mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3)))

#Round 2
r2 <- slots[33:48,1:4]
r2 <- r2 %>% left_join(df, by = c('Season' = 'Season', 'Strongseed' = 'nextRound')) %>% 
  left_join(df, by = c('Season' = 'Season', 'Weakseed' = 'nextRound')) %>% 
  rename(Seed1 = Seed.x, Seed2 = Seed.y, Region1 = Region.x, Region2 = Region.y, Team1 = Team.x, Team2 = Team.y)

r2 <- r2 %>% left_join(season2017, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(r2)[11:24] <- paste('Team1Avg', colnames(r2)[11:24], sep = '')
r2 <- r2 %>% left_join(season2017, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(r2)[25:38] <- paste('Team2Avg', colnames(r2)[25:38], sep = '')
r2 <- cbind(r2, r2[,11:24] - r2[,25:38])
colnames(r2)[39:52] <- paste('Diff', colnames(season2017)[3:16], sep='')

prob_pred <- predict(boost, newdata = r2[,c(5,8,39:52)], n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
r2$Team1Win <- y_pred
r2$probability <- prob_pred
r2$nextRound1 <- ifelse(r2$Team1Win == 1, r2$Slot, NA)
r2$nextRound2 <- ifelse(r2$Team1Win == 0, r2$Slot, NA)
r2 <- r2[, c(1:10,53:56,11:52)] #Round 1 predictions
df <- seeds2017 %>% left_join(r2[,c(6,13)], by = c('Team' = 'Team1')) %>% left_join(r2[,c(9,14)], by = c('Team' = 'Team2'))
df <- df[-which(rowMeans(is.na(df)) > .2),]
df$nextRound <- ifelse(is.na(df$nextRound1), df$nextRound2, df$nextRound1)
df <- df[,c(1:3,6)]
df <- df %>% mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3)))

#Round 3
r3 <- slots[49:56,1:4]
r3 <- r3 %>% left_join(df, by = c('Season' = 'Season', 'Strongseed' = 'nextRound')) %>% 
  left_join(df, by = c('Season' = 'Season', 'Weakseed' = 'nextRound')) %>% 
  rename(Seed1 = Seed.x, Seed2 = Seed.y, Region1 = Region.x, Region2 = Region.y, Team1 = Team.x, Team2 = Team.y)

r3 <- r3 %>% left_join(season2017, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(r3)[11:24] <- paste('Team1Avg', colnames(r3)[11:24], sep = '')
r3 <- r3 %>% left_join(season2017, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(r3)[25:38] <- paste('Team2Avg', colnames(r3)[25:38], sep = '')
r3 <- cbind(r3, r3[,11:24] - r3[,25:38])
colnames(r3)[39:52] <- paste('Diff', colnames(season2017)[3:16], sep='')

prob_pred <- predict(boost, newdata = r3[,c(5,8,39:52)], n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
r3$Team1Win <- y_pred
r3$probability <- prob_pred
r3$nextRound1 <- ifelse(r3$Team1Win == 1, r3$Slot, NA)
r3$nextRound2 <- ifelse(r3$Team1Win == 0, r3$Slot, NA)
r3 <- r3[, c(1:10,53:56,11:52)] #Round 1 predictions
df <- seeds2017 %>% left_join(r3[,c(6,13)], by = c('Team' = 'Team1')) %>% left_join(r3[,c(9,14)], by = c('Team' = 'Team2'))
df <- df[-which(rowMeans(is.na(df)) > .2),]
df$nextRound <- ifelse(is.na(df$nextRound1), df$nextRound2, df$nextRound1)
df <- df[,c(1:3,6)]
df <- df %>% mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3)))

# Round 4
r4 <- slots[57:60,1:4]
r4 <- r4 %>% left_join(df, by = c('Season' = 'Season', 'Strongseed' = 'nextRound')) %>% 
  left_join(df, by = c('Season' = 'Season', 'Weakseed' = 'nextRound')) %>% 
  rename(Seed1 = Seed.x, Seed2 = Seed.y, Region1 = Region.x, Region2 = Region.y, Team1 = Team.x, Team2 = Team.y)

r4 <- r4 %>% left_join(season2017, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(r4)[11:24] <- paste('Team1Avg', colnames(r4)[11:24], sep = '')
r4 <- r4 %>% left_join(season2017, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(r4)[25:38] <- paste('Team2Avg', colnames(r4)[25:38], sep = '')
r4 <- cbind(r4, r4[,11:24] - r4[,25:38])
colnames(r4)[39:52] <- paste('Diff', colnames(season2017)[3:16], sep='')

prob_pred <- predict(boost, newdata = r4[,c(5,8,39:52)], n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
r4$Team1Win <- y_pred
r4$probability <- prob_pred
r4$nextRound1 <- ifelse(r4$Team1Win == 1, r4$Slot, NA)
r4$nextRound2 <- ifelse(r4$Team1Win == 0, r4$Slot, NA)
r4 <- r4[, c(1:10,53:56,11:52)] #Round 1 predictions
df <- seeds2017 %>% left_join(r4[,c(6,13)], by = c('Team' = 'Team1')) %>% left_join(r4[,c(9,14)], by = c('Team' = 'Team2'))
df <- df[-which(rowMeans(is.na(df)) > .2),]
df$nextRound <- ifelse(is.na(df$nextRound1), df$nextRound2, df$nextRound1)
df <- df[,c(1:3,6)]
df <- df %>% mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3)))

#Round 5
r5 <- slots[61:62,1:4]
r5 <- r5 %>% left_join(df, by = c('Season' = 'Season', 'Strongseed' = 'nextRound')) %>% 
  left_join(df, by = c('Season' = 'Season', 'Weakseed' = 'nextRound')) %>% 
  rename(Seed1 = Seed.x, Seed2 = Seed.y, Region1 = Region.x, Region2 = Region.y, Team1 = Team.x, Team2 = Team.y)

r5 <- r5 %>% left_join(season2017, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(r5)[11:24] <- paste('Team1Avg', colnames(r5)[11:24], sep = '')
r5 <- r5 %>% left_join(season2017, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(r5)[25:38] <- paste('Team2Avg', colnames(r5)[25:38], sep = '')
r5 <- cbind(r5, r5[,11:24] - r5[,25:38])
colnames(r5)[39:52] <- paste('Diff', colnames(season2017)[3:16], sep='')

prob_pred <- predict(boost, newdata = r5[,c(5,8,39:52)], n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
r5$Team1Win <- y_pred
r5$probability <- prob_pred
r5$nextRound1 <- ifelse(r5$Team1Win == 1, r5$Slot, NA)
r5$nextRound2 <- ifelse(r5$Team1Win == 0, r5$Slot, NA)
r5 <- r5[, c(1:10,53:56,11:52)] #Round 1 predictions
df <- seeds2017 %>% left_join(r5[,c(6,13)], by = c('Team' = 'Team1')) %>% left_join(r5[,c(9,14)], by = c('Team' = 'Team2'))
df <- df[-which(rowMeans(is.na(df)) > .2),]
df$nextRound <- ifelse(is.na(df$nextRound1), df$nextRound2, df$nextRound1)
df <- df[,c(1:3,6)]
df <- df %>% mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3)))

#Round 6
r6 <- slots[63,1:4]
r6 <- r6 %>% left_join(df, by = c('Season' = 'Season', 'Strongseed' = 'nextRound')) %>% 
  left_join(df, by = c('Season' = 'Season', 'Weakseed' = 'nextRound')) %>% 
  rename(Seed1 = Seed.x, Seed2 = Seed.y, Region1 = Region.x, Region2 = Region.y, Team1 = Team.x, Team2 = Team.y)

r6 <- r6 %>% left_join(season2017, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(r6)[11:24] <- paste('Team1Avg', colnames(r6)[11:24], sep = '')
r6 <- r6 %>% left_join(season2017, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(r6)[25:38] <- paste('Team2Avg', colnames(r6)[25:38], sep = '')
r6 <- cbind(r6, r6[,11:24] - r6[,25:38])
colnames(r6)[39:52] <- paste('Diff', colnames(season2017)[3:16], sep='')

prob_pred <- predict(boost, newdata = r6[,c(5,8,39:52)], n.trees = 1000,type = 'response')
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
r6$Team1Win <- y_pred
r6$probability <- prob_pred
r6$nextRound1 <- ifelse(r6$Team1Win == 1, r6$Slot, NA)
r6$nextRound2 <- ifelse(r6$Team1Win == 0, r6$Slot, NA)
r6 <- r6[, c(1:10,53:56,11:52)] #Round 1 predictions
df <- seeds2017 %>% left_join(r6[,c(6,13)], by = c('Team' = 'Team1')) %>% left_join(r6[,c(9,14)], by = c('Team' = 'Team2'))
df <- df[-which(rowMeans(is.na(df)) > .2),]
df$nextRound <- ifelse(is.na(df$nextRound1), df$nextRound2, df$nextRound1)
df <- df[,c(1:3,6)]
df <- df %>% mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3)))

################# Final Predictions #############################################################################################
finalPredictions <- rbind(test_set,r2,r3,r4,r5,r6)
finalPredictions <- finalPredictions[, c(1:8,11:14)]
finalPredictions <- finalPredictions %>% left_join(teams, by = c('Team1' = 'Team_Id')) %>% left_join(teams, by = c('Team2' = 'Team_Id')) %>%
  rename(Name1 = Team_Name.x, Name2 = Team_Name.y)
finalPredictions <- finalPredictions[, c(1:8,13:14,11:12)]
round_acc <- data.frame(row.names = c('Round 1', 'Round 2', 'Round 3', 'Round 4', 'Round 5', 'Round 6'), 
                        c(78.125,43.75,37.5,50,50,0))
colnames(round_acc) <- 'Accuracy'
round_acc
