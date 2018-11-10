# notes ----
# chignik forecast with SST data
# sarah.power@alaska.gov
# 11/06/2018

# load ----
library(tidyverse)
library(lubridate)
library(ggrepel)
library(broom)#for cleaning up data, used in predction
library(caret)#used for cross validation 
library(car)
library(here)

options(scipen=999)

# data ----
chig <- read_csv('data/Chignik Early_Run2019fx.csv') %>%
  select(-oage5, -Total) %>%
  rename(year = outmigration_year) %>%
  filter(year >= 2003 & year < 2016) %>%
  mutate(oage2_log = log(oage2),
         oage3_log = log(oage3))

SSTdf <- read_csv('data/month620sst.csv') %>%
  filter(month == "4") %>% #selected April
  select(-X1, -month) %>%
  mutate(year = year + 1)

chig <- merge(chig, SSTdf, by = "year")

# analysis ----

#median values are used for the ocean age 1's and 4's:
oage1_med1090 <- quantile(chig$oage1, probs = c(0.5, 0.10, 0.90), na.rm = TRUE) 
oage4_med1090 <- quantile(chig$oage4, probs = c(0.5, 0.10, 0.90), na.rm = TRUE) 


#regression model for ocean age 3 (oage3)
oage3_model_sst <- lm(oage3 ~ oage2 + month_mean, data = chig)
layout(matrix(c(1,2,3,4),2,2))
plot(oage3_model_sst)
summary(oage3_model_sst) # show results
r2 = format(summary(oage3_model_sst)$adj.r.squared, digits = 3)
#RSS <- c(crossprod(oage3_model_sst$residuals))
#MSE <- RSS / length(oage3_model_sst$residuals)
#(RMSE <- sqrt(MSE))

#use to plot the new predicted point on the graph
new_data <- data.frame(oage2 =  135057.48, month_mean = 4.460578) #put in 2016 year oage2 *Note need to automate this
chig$lmresid <- residuals.lm(oage3_model_sst)
chig$lm_percent_off <- round(chig$lmresid/chig$oage3 *100, 0)

mean(chig$lm_percent_off)

newpoint <- broom::augment(oage3_model_sst, newdata = new_data)
(pred <- pred3 <- predict(oage3_model_sst, newdata = new_data, interval = "prediction", level = 0.90))
lwr <- pred[2]
upr <- pred[3]
conf3 <- predict(oage3_model_sst, newdata = new_data, interval = "confidence", level = 0.90)

#Use to make 95% CI and PI 
minoage2 <- round(min(chig$oage2, na.rm = TRUE),0)
maxoage2 <- round(max(chig$oage2, na.rm = TRUE),0)
predx <- data.frame(oage2 = seq(from = minoage2, to = maxoage2, by = (maxoage2-minoage2)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(oage3_model_sst, newdata = predx, interval = "confidence", level = 0.90))

# ... prediction interval
pred.int <- cbind(predx, predict(oage3_model_sst, newdata = predx, interval = "prediction", level = 0.90))

m <- oage3_model_sst
#Add text to the graph
lm_eqn <- function(df){
  m <- lm(oage3 ~ oage2 + month_mean, df);
  eq <- substitute(italic(oage3) == a + b %.% italic(oage2)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$adj.r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqn(chig)
# 
g.pred <- ggplot(pred.int, aes(x = oage2, y = fit, color = month_mean)) +
  geom_point(data = chig, aes(x = oage2, y = oage3, color = month_mean)) + #plots all the points
  geom_text_repel(data = chig, aes(x = oage2, y = oage3, color = month_mean, label = year), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  #geom_text(data = newpoint, aes(x = oage2, y = .fitted, label = round(.fitted, 0 )), adj = 1, size = 6) +  
  geom_text(data = newpoint, aes(x = oage2, y = .fitted, , color = month_mean, label = year), adj = 6) +  
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #geom_text(x = 5, y = 75000, label = lm_eqn(chig), parse = TRUE, adj = 0, size = 10) +
  #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  coord_cartesian(ylim = c(0, 3500000), xlim = c(0, 550000)) +
  theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  xlab("oage2") +
  ylab("oage3") +
  ggtitle("oage3 vs age2 + sst")

g.pred  

#Repeated K- fold Cross validation

# define training control 
train_control <- trainControl(method="repeatedcv", number=9, repeats=4)
#I used number of K-folds = 4 since I have 4*5 = 210 is lose to the 20 years of data I have

# train the model
model <- train(oage3 ~ oage2 + month_mean, data=chig, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

oage3_pred <- predict(model, chig) # necessary step to get training RMSE
postResample(pred = oage3_pred, obs = chig$oage3) #To get training RMSE
#Compare training RMSE to (full) RMSE
