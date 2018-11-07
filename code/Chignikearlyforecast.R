# notes ----
# chigkik forecast
# sarah.power@alaska.gov
# 11/06/2018

# load ----
library(tidyverse)
library(lubridate)
library(broom)#for cleaning up data, used in predction
library(caret)#used for cross validation 
library(here)
options(scipen=999)

# data ----
chig <- read_csv('data/Chignik Early_oage32019fx.csv') %>%
  select(-oage5, -Total) %>%
  filter(outmigration_year >= 1995 | outmigration_year < 2015) %>%
  mutate(oage2_log <- log(oage2),
         oage3_log <- log(oage3))

# analysis ----
oage1_median <- median(chig$oage1, na.rm = TRUE) 
oage4_median <- median(chig$oage4, na.rm = TRUE)

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
(pred <- predict(oage3_model_1, newdata = new_data, interval = "prediction", level = 0.95))
lwr <- pred[2]
upr <- pred[3]
predict(oage3_model_1, newdata = new_data, interval = "confidence", level = 0.95)

#Use to make 95% CI and PI 
minoage2 <- round(min(chig$oage2, na.rm = TRUE),0)
maxoage2 <- round(max(chig$oage2, na.rm = TRUE),0)
predx <- data.frame(oage2 = seq(from = minoage2, to = maxoage2, by = (maxoage2-minoage2)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(oage3_model_1, newdata = predx, interval = "confidence", level = 0.95))

# ... prediction interval
pred.int <- cbind(predx, predict(oage3_model_1, newdata = predx, interval = "prediction", level = 0.95))

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
# The font below is sized for power point presentation. Make smaller if in a document.
g.pred <- ggplot(pred.int, aes(x = oage2, y = fit)) +
  geom_point(data = chig, aes(x = oage2, y = oage3)) + #plots all the points
  geom_text(data = chig, aes(x = oage2, y = oage3, label = outmigration_year), size = 6) +
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
postResample(pred = oage3_pred, obs = chig$oage3) #To get trainign RMSE
#Compare training RMSE to (full) RMSE




# log models
oage3_model_2 <- lm(oage3 ~ oage2_log, data = chig)
preds_oage3_model_2 <- evaluate_model(oage3_model_2, data = chig)
preds_oage3_model_2$model_oage3 <- exp(preds_oage3_model_2$model_output)

#CI on log transformed model



oage3_model_3_log_log <- lm(oage3_log ~ oage2_log, data = chig)
preds_oage3_model_3_log_log  <- evaluate_model(oage3_model_3_log_log , data = chig)