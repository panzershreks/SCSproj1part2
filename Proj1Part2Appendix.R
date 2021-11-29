# Load packages and data
library(dplyr)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(car)
library(olsrr)
load(file = "foresthealth.rda")

# ---------- Data ---------- #

# Basic pre-processing
data <- dat
summary(data)
data$tree_sp_eu <- factor(data$tree_sp_eu)
data$id <- factor(data$id)
levels(data$geol_no) <- c("MCB", "PGB", "NRC", "TrS", "TCS", "L", "TeS", "GFSLM", "LL")

# Create data set for Beech
data_beech <- filter(data, tree_sp_eu == "Rbu") # makes dataset for beech trees
data_beech <- subset(data_beech, select = -c(H_spec, id, x_utm, y_utm, s_veg, prec_y, spei_12_oct, tree_sp_eu))
data_beech <- na.omit(data_beech) # removing all the NA values

# ---------- Model ---------- #

# Model selection process
full_mod <- lm(nbv_ratio ~ ., data = data_beech)
aic_mod <- step(full_mod)
bic_mod <- step(full_mod, k = log(nrow(data_beech)))
anova(aic_mod, bic_mod)

# Check/add other variables of interest
bic_add_spei24 <- update(bic_mod, . ~ . + spei_24_oct, data = data_beech)
anova(bic_mod, bic_add_spei24) # p-value of 0.009143 < 1%

spei24_add_Es <- update(bic_add_spei24, . ~ . + Es, data = data_beech)
anova(add_spei24, spei24_add_Es) # p-value of 0.01519 > 1%

# Check for collinearity using VIF
ols_vif_tol(bic_add_spei24) # VIF of alt_m & tmean_y >6
cor(data_beech$alt_m, data_beech$tmean_y) # correlation of -0.8259334

rm_alt_m <- update(bic_add_spei24, . ~ . - alt_m, data = data_beech)
ols_vif_tol(rm_alt_m) # all VIFs <5
anova(rm_alt_m, bic_add_spei24) # p-value of 0.002198 < 1%

rm_tmean_y <- update(bic_add_spei24, . ~ . - tmean_y, data = data_beech)
ols_vif_tol(rm_tmean_y) # all VIFs <5
anova(rm_tmean_y, bic_add_spei24) # p-value of 0.0006639 < 1%

# Final model
model_final <- rm_alt_m

# Check model assumptions (Figure 1)
par(mfrow = c(2,2))
plot(model_final)

# ---------- Summary statistics ---------- #

# Mean defoliation increase per year
defo_rate <- data_beech %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(defo = mean(nbv_ratio)) %>%
  mutate(defo_change = defo - lag(defo)) %>%
  na.omit
mean(defo_rate$defo_change) # 0.5% average increase per year

# Defoliation by category 1989
defo_1989 <- data_beech %>%
  filter(year == 1989) %>%
  mutate(def_class = cut(nbv_ratio, breaks = c(0,0.1,.25,.6,.99,1))) %>%
  subset(select = c(nbv_ratio, def_class)) %>% 
  group_by(def_class) %>% 
  count() 
defo_1989 <- defo_1989 %>% mutate(freq = n/sum(defo_1989$n))
defo_1989

# Defoliation by category 2016
defo_2016 <- data_beech %>%
  filter(year == 2016) %>%
  mutate(def_class = cut(nbv_ratio, breaks = c(0,0.1,.25,.6,.99,1))) %>%
  subset(select = c(nbv_ratio, def_class)) %>% 
  group_by(def_class) %>% 
  count()
defo_2016 <- defo_2016 %>% mutate(freq = n/sum(defo_2016$n))
defo_2016

# Graphical Summaries (Figure 2)
g1 <- (ggplot(data_beech, aes(year, nbv_ratio)) 
       + geom_point() 
       + stat_smooth(method = "lm") 
       + theme(axis.title = element_text(size = 16))) 
g3 <- (ggplot(data_beech, aes(prec_y_lag1, nbv_ratio)) 
       + geom_point() 
       + stat_smooth(method = "lm") 
       + theme(axis.title = element_text(size = 16)))
g6 <- (ggplot(data_beech, aes(geol_no, nbv_ratio)) 
        + geom_boxplot() 
        + scale_x_discrete(guide = guide_axis(n.dodge = 2),
                           labels = c('MCB','PGB','NRC', 'TrS', 'TCS', 'L', 'TeS', 'GFSLM', 'LL'))
        + theme(axis.title = element_text(size = 16)))
g2 <- (ggplot(data_beech, aes(tree_age, nbv_ratio)) 
       + geom_point()  
       + stat_smooth(method = "lm") 
       + theme(axis.title = element_text(size = 16))) 
g4 <- (ggplot(data_beech, aes(tmin_may, nbv_ratio)) 
       + geom_point()  
       + stat_smooth(method = "lm") 
       + theme(axis.title = element_text(size = 16))) 
g5 <- (ggplot(data_beech, aes(et0_y_lag1, nbv_ratio)) 
       + geom_point()  
       + stat_smooth(method = "lm") 
       + theme(axis.title = element_text(size = 16))) 
g_plot <- ggarrange(g1, g2, g3, g4, g5, g6, ncol = 2, nrow = 3)
g_plot


# Geology distribution over all sites measured (Figure 3)
geol_dist <- data_beech %>%
  group_by(tpi500) %>%
  slice(1L) %>%
  group_by(geol_no) %>%
  count()
geol_dist <- geol_dist %>% mutate(freq = n/sum(geol_dist$n),
                                  perc = freq*100)
geol_dist

geol_slice <- data_beech %>% group_by(tpi500) %>% slice(1L)
ggplot(geol_slice, aes(x = geol_no)) +geom_bar()

# ---------- Model Results ---------- #

# Confidence Intervals
Lower <- summary(model_final)$coefficients[,1] - 1.96*summary(model_final)$coefficients[,2]
Upper <- summary(model_final)$coefficients[,1] + 1.96*summary(model_final)$coefficients[,2]
Mean <- summary(model_final)$coefficients[,1]
p_value <- summary(model_final)$coefficients[,4]
CI <- cbind(Lower, Mean, Upper, p_value)[-1,]
CI <- signif(CI, 3)
ci_df <- data.frame(CI)
#write.csv(ci_df, "./CI.csv", row.names = TRUE) # export df to .csv
summary(model_final)




