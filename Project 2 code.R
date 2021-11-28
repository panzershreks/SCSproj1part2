# Loading in and organising the data
library(dplyr)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(car)

data <- dat
summary(data)
data$tree_sp_eu <- factor(data$tree_sp_eu)
data$id <- factor(data$id)


#Omitting unnecessary data

data_beech <- filter(data, tree_sp_eu == "Rbu") #makes dataset for beech trees
data_beech <- subset(data_beech, select = -c(id, s_veg, tree_sp_eu, H_spec))
data_beech <- na.omit(data_beech) #removing all the NA values

data_spruce <- filter(data, tree_sp_eu == "Gfi") #makes dataset for spruce trees
data_spruce <- subset(data_spruce, select = -c(id, s_veg, tree_sp_eu, H_spec))
data_spruce <- na.omit(data_spruce)


# Analysis of data for Beech trees
mod1 <- lm(nbv_ratio ~ ., data = data_beech) #Model with all variables
summary(mod1)

drop_mod1 <- drop1(mod1, test = "F")        #Backward selection using drop
step_mod1 <- step(mod1)                     #Backward selection using step
step_mod1B <- step(mod1, k = log(4004))     #Backward selection using step using BICsummary

par(mfrow = c(2,2))
plot(step_mod1)    #plots the graphs to check model assumptions, Q-Q plots are odd at the end tail, residuals has a weird shape
#there are two points that have a leverage of 1, could be outliers (1590, 2422)
#Could imply that this model does not fit very well

#mod2 <- lm(nbv_ratio ~ . -prec_y_lag1, data = data_beech)
#summary(mod2) # -Last year's variables
#step_mod2 <- step(mod2)
#step_mod2B <- step(mod2, k = log(4004))

mod3 <- lm(nbv_ratio ~ . -spei_24_oct, data = data_beech)
summary(mod3) # -spei24
step_mod3 <- step(mod3)
step_mod3B <- step(mod3, k = log(4004))

mod4 <- lm(nbv_ratio ~ . -spei_24_oct -prec_y_lag1, data = data_beech)
summary(mod4) # -spei24 -Last year's variables
step_mod4 <- step(mod4)
step_mod4B <- step(mod4, k = log(4004))

mod5 <- lm(nbv_ratio ~ . -spei_12_oct -prec_y, data = data_beech)
summary(mod5)  # -spei12 -This year's variables
step_mod5 <- step(mod5)
step_mod5B <- step(mod5, k = log(4004))

#mod6 <- lm(nbv_ratio ~ . -spei_12_oct -prec_y_lag1, data = data_beech)
#summary(mod6)  # -spei12 -Last year's variables
#step_mod6 <- step(mod6)
#step_mod6B <- step(mod6, k = log(4004))

#mod7 <- lm(nbv_ratio ~ . -spei_12_oct, data = data_beech)
#summary(mod7)  # -spei12
#step_mod7 <- step(mod7)
#step_mod7B <- step(mod7, k = log(4004))

mod8 <- lm(nbv_ratio ~ . -spei_24_oct -prec_y, data = data_beech)
summary(mod8)  # -spei24 -this year's variables
step_mod8 <- step(mod8)
step_mod8B <- step(mod8, k = log(4004))
#Step mod 1 and 3 have the best fit by far.

#mod9 <- lm(nbv_ratio ~ . -prec_y_lag1, data = data_beech)
#summary(mod9)  # -last year's precipitation
#step_mod9 <- step(mod9)
#step_mod9B <- step(mod9, k = log(4004))

mod10 <- lm(nbv_ratio ~ . -spei_12_oct -prec_y_lag1, data = data_beech)
summary(mod10)  # -spei12 -Last year's precipitation
step_mod10 <- step(mod10)
step_mod10B <- step(mod10, k = log(4004))

lm_site <- lm(nbv_ratio ~ geol_no + soil_no + soil_ty_no + humus_no + water_no + nutri_no + slope_dir + skel_no + depth_no + alt_m + slope_deg, data = data_beech)
summary(lm_site)  #Model using only site variables

#Conclude that variables to remove: prec_y_lag1 or prec_y (keep the other lag variables), 24-month SPEI
#Best model for now is step_mod8B

base_model <- lm(nbv_ratio ~ . -prec_y -spei_12_oct -y_utm -x_utm , data = data_beech)
step_model <- step(base_model, k = log(nrow(data_beech)), scope = list(lower = ~ tree_age))
step_modelA <- step(base_model, scope = list(lower = ~ tree_age))
final_model <- update(step_model, . ~ . +spei_24_oct -alt_m, data = data_beech)


anova(step_model, update(step_model, . ~ . - y_utm ))

#Graphical summaries
g1 <- ggplot(data_beech, aes(year, nbv_ratio)) + geom_point() + stat_smooth(method = "lm") + theme(axis.title = element_text(size = 16)) 

#ggplot(data_beech, aes(year, globrad_y)) + geom_point()  + stat_smooth(method = "lm")

#ggplot(data_beech, aes(year, prec_y_lag1)) + geom_point()  + stat_smooth(method = "lm")

g2 <- ggplot(data_beech, aes(prec_y_lag1, nbv_ratio)) + geom_point() + stat_smooth(method = "lm") + theme(axis.title = element_text(size = 16)) 

g3 <- ggplot(data_beech, aes(geol_no, nbv_ratio)) + geom_boxplot() + scale_x_discrete(guide = guide_axis(n.dodge = 2), labels = c('MCB','PGB','NRC', 'TrS', 'TCS', 'L', 'TeS', 'GFSLM', 'LL')) + theme(axis.title = element_text(size = 16)) 

g4 <- ggplot(data_beech, aes(tree_age, nbv_ratio)) + geom_point()  + stat_smooth(method = "lm") + theme(axis.title = element_text(size = 16)) 

#ggplot(data_beech, aes(spei_3_may, nbv_ratio)) + geom_point()  + stat_smooth(method = "lm") 

#ggplot(data_beech, aes(alt_m, nbv_ratio)) + geom_point()  + stat_smooth(method = "lm")

g5 <- ggplot(data_beech, aes(tmin_may, nbv_ratio)) + geom_point()  + stat_smooth(method = "lm") + theme(axis.title = element_text(size = 16)) 

g6 <- ggplot(data_beech, aes(d_veg, nbv_ratio)) + geom_point()  + stat_smooth(method = "lm") + theme(axis.title = element_text(size = 16)) 
g_plot <- ggarrange(g1, g4, g2, g5, g6, g3, ncol = 2, nrow = 3)
g_plot

#Confidence intervals
lower <- summary(final_model)$coefficients[,1] - 1.96*summary(final_model)$coefficients[,2]
upper <- summary(final_model)$coefficients[,1] + 1.96*summary(final_model)$coefficients[,2]
mean <- summary(final_model)$coefficients[,1]
CI <- cbind(lower, mean, upper)[-1,]
ci_df <- data.frame(CI)
knitr::kable(ci_df)






#Checking model assumptions
par(mfrow = c(2,2))
plot(final_model)
#Our model is right-skewed (positively skewed)











 