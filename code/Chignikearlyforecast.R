# notes ----
# chigkik forecast
# sarah.power@alaska.gov
# 11/06/2018

# load ----
library(tidyverse)
library(lubridate)
library(ggrepel)
library(broom)#for cleaning up data, used in predction
library(caret)#used for cross validation 
library(here)
options(scipen=999)

# data ----
chig <- read_csv('data/Chignik Early_Run2019fx.csv') %>%
  select(-oage5, -Total) %>%
  filter(outmigration_year >= 1998 & outmigration_year < 2017) %>%
  mutate(oage2_log = log(oage2),
         oage3_log = log(oage3))

# analysis ----
oage1_median <- median(chig$oage1, na.rm = TRUE) 
oage4_median <- median(chig$oage4, na.rm = TRUE)
oage1_med1090 <- quantile(chig$oage1, probs = c(0.5, 0.10, 0.90), na.rm = TRUE) 
oage4_med1090 <- quantile(chig$oage4, probs = c(0.5, 0.10, 0.90), na.rm = TRUE) 

summary(chig$oage1)

# model for ocean age 3 (oage3)
oage3_model_1 <- lm(oage3 ~ oage2, data = chig)
#preds_oage3_model_1 <- evaluate_model(oage3_model_1, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage3_model_1)
summary(oage3_model_1) # show results
r2 = format(summary(oage3_model_1)$r.squared, digits = 3)
RSS <- c(crossprod(oage3_model_1$residuals))
MSE <- RSS / length(oage3_model_1$residuals)
(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage2=  135057.48) #put in 2016 year oage2 *Note need to automate this

newpoint <- broom::augment(oage3_model_1, newdata = new_data)
(pred <- pred3 <- predict(oage3_model_1, newdata = new_data, interval = "prediction", level = 0.90))
lwr <- pred[2]
upr <- pred[3]
conf3 <- predict(oage3_model_1, newdata = new_data, interval = "confidence", level = 0.90)

#Use to make 95% CI and PI 
minoage2 <- round(min(chig$oage2, na.rm = TRUE),0)
maxoage2 <- round(max(chig$oage2, na.rm = TRUE),0)
predx <- data.frame(oage2 = seq(from = minoage2, to = maxoage2, by = (maxoage2-minoage2)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(oage3_model_1, newdata = predx, interval = "confidence", level = 0.90))

# ... prediction interval
pred.int <- cbind(predx, predict(oage3_model_1, newdata = predx, interval = "prediction", level = 0.90))

m <- oage3_model_1
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage3 ~ oage2, df);
  eq <- substitute(italic(oage3) == a + b %.% italic(oage2)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage2, y = fit)) +
  geom_point(data = chig, aes(x = oage2, y = oage3)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage2, y = oage3, label = outmigration_year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = "2017"), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  expand_limits(y=c(0 , 80000)) +
  expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage2") +
  ylab("oage3") +
  ggtitle("oage3 vs age2")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage3 ~ oage2, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage3_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage3_pred, obs = chig$oage3) #To get training RMSE
#Compare training RMSE to (full) RMSE




# log models

# model for ocean age 3 (oage3)
oage3_log_model_1 <- lm(oage3_log ~ oage2, data = chig)
#preds_oage3_log_model_1 <- evaluate_model(oage3_log_model_1, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage3_log_model_1)
summary(oage3_log_model_1) # show results
r2 = format(summary(oage3_log_model_1)$r.squared, digits = 3)
RSS <- c(crossprod(oage3_log_model_1$residuals))
MSE <- RSS / length(oage3_log_model_1$residuals)
(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage2=  135057.48) #put in 2016 year oage2 *Note need to automate this

newpoint <- broom::augment(oage3_log_model_1, newdata = new_data)
(pred <- predict(oage3_log_model_1, newdata = new_data, interval = "prediction", level = 0.90))
pred <- pred3log <-exp(pred)
lwr <- pred[2]
upr <- pred[3]
conf3log <-exp(predict(oage3_log_model_1, newdata = new_data, interval = "confidence", level = 0.90))

#Use to make 95% CI and PI 
minoage2 <- round(min(chig$oage2, na.rm = TRUE),0)
maxoage2 <- round(max(chig$oage2, na.rm = TRUE),0)
predx <- data.frame(oage2 = seq(from = minoage2, to = maxoage2, by = (maxoage2-minoage2)/19))

# ... confidence interval
conf.int <- cbind(predx, exp(predict(oage3_log_model_1, newdata = predx, interval = "confidence", level = 0.90)))

# ... prediction interval
pred.int <- cbind(predx, exp(predict(oage3_log_model_1, newdata = predx, interval = "prediction", level = 0.90)))

m <- oage3_log_model_1
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage3_log ~ oage2, df);
  eq <- substitute(italic(oage3_log) == a + b %.% italic(oage2)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage2, y = fit)) +
  geom_point(data = chig, aes(x = oage2, y = oage3)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage2, y = oage3, label = outmigration_year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = exp(.fitted)), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = "2017"), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  expand_limits(y=c(0 , 80000)) +
  expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage2") +
  ylab("oage3") +
  ggtitle("oage3_log vs age2")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage3_log ~ oage2, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage3_log_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage3_log_pred, obs = chig$oage3_log) #To get training RMSE
#Compare training RMSE to (full) RMSE


#######################################
#log model with results/graph in log scale
oage3_log_model_1 <- lm(oage3_log ~ oage2, data = chig)
#preds_oage3_log_model_1 <- evaluate_model(oage3_log_model_1, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage3_log_model_1)
summary(oage3_log_model_1) # show results
r2 = format(summary(oage3_log_model_1)$r.squared, digits = 3)
RSS <- c(crossprod(oage3_log_model_1$residuals))
MSE <- RSS / length(oage3_log_model_1$residuals)
(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage2=  135057.48) #put in 2016 year oage2 *Note need to automate this

newpoint <- broom::augment(oage3_log_model_1, newdata = new_data)
(pred <- predict(oage3_log_model_1, newdata = new_data, interval = "prediction", level = 0.90))
#pred <- exp(pred)
lwr <- pred[2]
upr <- pred[3]
predict(oage3_log_model_1, newdata = new_data, interval = "confidence", level = 0.90)

#Use to make 95% CI and PI 
minoage2 <- round(min(chig$oage2, na.rm = TRUE),0)
maxoage2 <- round(max(chig$oage2, na.rm = TRUE),0)
predx <- data.frame(oage2 = seq(from = minoage2, to = maxoage2, by = (maxoage2-minoage2)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(oage3_log_model_1, newdata = predx, interval = "confidence", level = 0.90))

# ... prediction interval
pred.int <- cbind(predx, predict(oage3_log_model_1, newdata = predx, interval = "prediction", level = 0.90))

m <- oage3_log_model_1
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage3_log ~ oage2, df);
  eq <- substitute(italic(oage3_log) == a + b %.% italic(oage2)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage2, y = fit)) +
  geom_point(data = chig, aes(x = oage2, y = oage3_log)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage2, y = oage3_log, label = outmigration_year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = "2017"), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  #expand_limits(y=c(0 , 80000)) +
  #expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage2") +
  ylab("oage3") +
  ggtitle("oage3_log vs age2")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage3_log ~ oage2, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage3_log_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage3_log_pred, obs = chig$oage3_log) #To get training RMSE
#Compare training RMSE to (full) RMSE

############################################
# Now do the same for age2 regressed on age1 
# model for ocean age 3 (oage2)
oage2_model_1 <- lm(oage2 ~ oage1, data = chig)
#preds_oage2_model_1 <- evaluate_model(oage2_model_1, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage2_model_1)
summary(oage2_model_1) # show results
r2 = format(summary(oage2_model_1)$r.squared, digits = 3)
RSS <- c(crossprod(oage2_model_1$residuals))
MSE <- RSS / length(oage2_model_1$residuals)
(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage1= 553.69615 ) #put in 2017 year oage1 *Note need to automate this

newpoint <- broom::augment(oage2_model_1, newdata = new_data)
(pred <- pred2 <- predict(oage2_model_1, newdata = new_data, interval = "prediction", level = 0.90))
lwr <- pred[2]
upr <- pred[3]
conf2 <- predict(oage2_model_1, newdata = new_data, interval = "confidence", level = 0.90)

#Use to make 95% CI and PI 
minoage1 <- round(min(chig$oage1, na.rm = TRUE),0)
maxoage1 <- round(max(chig$oage1, na.rm = TRUE),0)
predx <- data.frame(oage1 = seq(from = minoage1, to = maxoage1, by = (maxoage1-minoage1)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(oage2_model_1, newdata = predx, interval = "confidence", level = 0.90))

# ... prediction interval
pred.int <- cbind(predx, predict(oage2_model_1, newdata = predx, interval = "prediction", level = 0.90))

m <- oage2_model_1
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage2 ~ oage1, df);
  eq <- substitute(italic(oage2) == a + b %.% italic(oage1)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage1, y = fit)) +
  geom_point(data = chig, aes(x = oage1, y = oage2)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage1, y = oage2, label = outmigration_year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage1, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage1, y = .fitted, label = "2017"), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  expand_limits(y=c(0 , 80000)) +
  expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage1") +
  ylab("oage2") +
  ggtitle("oage2 vs age1")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage2 ~ oage1, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage2_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage2_pred, obs = chig$oage2) #To get training RMSE
#Compare training RMSE to (full) RMSE




# log models

# model for ocean age 2 (oage2)
oage2_log_model_1 <- lm(oage2_log ~ oage1, data = chig)
#preds_oage2_log_model_1 <- evaluate_model(oage2_log_model_1, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage2_log_model_1)
summary(oage2_log_model_1) # show results
r2 = format(summary(oage2_log_model_1)$r.squared, digits = 3)
RSS <- c(crossprod(oage2_log_model_1$residuals))
MSE <- RSS / length(oage2_log_model_1$residuals)
(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage1=  553.69615) #put in 2016 year oage1 *Note need to automate this

newpoint <- broom::augment(oage2_log_model_1, newdata = new_data)
(pred <- predict(oage2_log_model_1, newdata = new_data, interval = "prediction", level = 0.90))
pred <- pred2log <- exp(pred)
lwr <- pred[2]
upr <- pred[3]
conf2log <-exp(predict(oage2_log_model_1, newdata = new_data, interval = "confidence", level = 0.90))

#Use to make 95% CI and PI 
minoage1 <- round(min(chig$oage1, na.rm = TRUE),0)
maxoage1 <- round(max(chig$oage1, na.rm = TRUE),0)
predx <- data.frame(oage1 = seq(from = minoage1, to = maxoage1, by = (maxoage1-minoage1)/19))

# ... confidence interval
conf.int <- cbind(predx, exp(predict(oage2_log_model_1, newdata = predx, interval = "confidence", level = 0.90)))

# ... prediction interval
pred.int <- cbind(predx, exp(predict(oage2_log_model_1, newdata = predx, interval = "prediction", level = 0.90)))

m <- oage2_log_model_1
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage2_log ~ oage1, df);
  eq <- substitute(italic(oage2_log) == a + b %.% italic(oage1)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage1, y = fit)) +
  geom_point(data = chig, aes(x = oage1, y = oage2)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage1, y = oage2, label = outmigration_year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = exp(.fitted)), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage1, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage1, y = .fitted, label = "2017"), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  expand_limits(y=c(0 , 80000)) +
  expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage1") +
  ylab("oage2") +
  ggtitle("oage2_log vs 0age1")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage2_log ~ oage1, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage2_log_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage2_log_pred, obs = chig$oage2_log) #To get training RMSE
#Compare training RMSE to (full) RMSE


#######################################
#log model with results/graph in log scale
oage2_log_model_1 <- lm(oage2_log ~ oage1, data = chig)
#preds_oage2_log_model_1 <- evaluate_model(oage2_log_model_1, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage2_log_model_1)
summary(oage2_log_model_1) # show results
r2 = format(summary(oage2_log_model_1)$r.squared, digits = 3)
RSS <- c(crossprod(oage2_log_model_1$residuals))
MSE <- RSS / length(oage2_log_model_1$residuals)
(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage1=  553.69615) #put in 2016 year oage1 *Note need to automate this

newpoint <- broom::augment(oage2_log_model_1, newdata = new_data)
(pred <- predict(oage2_log_model_1, newdata = new_data, interval = "prediction", level = 0.90))
#pred <- exp(pred)
lwr <- pred[2]
upr <- pred[3]
predict(oage2_log_model_1, newdata = new_data, interval = "confidence", level = 0.90)

#Use to make 95% CI and PI 
minoage1 <- round(min(chig$oage1, na.rm = TRUE),0)
maxoage1 <- round(max(chig$oage1, na.rm = TRUE),0)
predx <- data.frame(oage1 = seq(from = minoage1, to = maxoage1, by = (maxoage1-minoage1)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(oage2_log_model_1, newdata = predx, interval = "confidence", level = 0.90))

# ... prediction interval
pred.int <- cbind(predx, predict(oage2_log_model_1, newdata = predx, interval = "prediction", level = 0.90))

m <- oage2_log_model_1
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage2_log ~ oage1, df);
  eq <- substitute(italic(oage2_log) == a + b %.% italic(oage1)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage1, y = fit)) +
  geom_point(data = chig, aes(x = oage1, y = oage2_log)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage1, y = oage2_log, label = outmigration_year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage1, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage1, y = .fitted, label = "2017"), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  #expand_limits(y=c(0 , 80000)) +
  #expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage1") +
  ylab("oage2") +
  ggtitle("oage2_log vs oage1")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage2_log ~ oage1, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage2_log_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage2_log_pred, obs = chig$oage2_log) #To get training RMSE

oage1_med1090 
oage4_med1090 

pred3
conf3
pred3log
conf3log

pred2
conf2
pred2log
conf2log
r2
#doug Eggers method:
# sqrt(Sum of squares of  1/2 90PI range for each age class)

drange_sq <- function(vector){
  ((vector[3]-vector[2])/2)^2
}

point_est <- sum(oage1_med1090[1], pred2[1], pred3[1], oage4_med1090[1])
lwr_est <- sum(oage1_med1090[2],
    pred2[2],
    pred3[2],
    oage4_med1090[2])

upr_est <- sum(oage1_med1090[3],
               pred2[3],
               pred3[3],
               oage4_med1090[3])
pred_est <- c(point_est, lwr_est, upr_est)

derange <-sqrt(sum(drange_sq(oage1_med1090),
         drange_sq(oage4_med1090),
         drange_sq(pred2),
         drange_sq(pred3)))

eggerslm <- c(point_est, point_est - derange, point_est + derange)



lm_output <- rbind(oage1_med1090, pred2log, pred3log, oage4_med1090, pred_est, eggerslm )

point_estlog <- sum(oage1_med1090[1], pred2log[1], pred3log[1], oage4_med1090[1])

lwr_estlog <- sum(oage1_med1090[2],
               pred2log[2],
               pred3log[2],
               oage4_med1090[2])

upr_estlog <- sum(oage1_med1090[3],
               pred2log[3],
               pred3log[3],
               oage4_med1090[3])
pred_estlog <- c(point_estlog, lwr_estlog, upr_estlog)

derangelog <-sqrt(sum(drange_sq(oage1_med1090),
                   drange_sq(oage4_med1090),
                   drange_sq(pred2log),
                   drange_sq(pred3log)))

eggerslog <- c(point_estlog, point_estlog - derangelog, point_estlog + derangelog)

loglm_output <- rbind(oage1_med1090, pred2log, pred3log, oage4_med1090, pred_estlog, eggerslog )

rbind(lm_output, loglm_output)
