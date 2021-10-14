
#PROBLEM 3 KNN
attach(Household_Pulse_data)
Dat_Northeast <- subset(Household_Pulse_data, (Household_Pulse_data$INCOME == 1) & (Household_Pulse_data$AGE > 30) & (Household_Pulse_data$AGE < 50))
detach()
attach(Dat_Northeast)


summary(Dat_Northeast)

commute_indx <- factor((Commute_car + 2*Commute_bus + 3*Commute_subway + 4*Commute_rail + 5*Commute_other), levels=c(1,2,3,4,5),labels = c("Car","Bus","Subway","Rail","Other"))

norm_varb <- function(X_in) { (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

#now we use differnt variables to predict some regresion 

norm_inc_tot <- norm_varb(INCTOT)
norm_poverty <- norm_varb(POVERTY)
norm_rent <- norm_varb(RENT)
norm_cost_electricity <- norm_varb(COSTELEC)
norm_cost_gas <- norm_varb(COSTGAS)
norm_cost_water <- norm_varb(RENTCUR)
norm_cost_fuelhome <- norm_varb(COSTFUEL)
norm_cost_water <- norm_varb( Water)
norm_white <- norm_varb(white)
norm_gq <- norm_varb(GQ)
norm_pvt_insurance <- norm_varb(has_PvtHealthIns)

data_use_prelim <- data.frame(norm_inc_tot,norm_poverty,norm_rent,norm_cost_electricity,norm_cost_gas,norm_cost_water)
                             
good_obs_data_use <- complete.cases(data_use_prelim,commute_indx)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(commute_indx,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.9)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(Dat_Northeast)
prop.table(summary(Dat_Northeast))

summary(Dat_Northeast)
require(class)
for (indx in seq(1, 8, by= 3)) {
  pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_borough == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}
# In this Knn, we arfe tring to run a regresion  with people of 30 years old and 50 years old. Also train to come up with information of comute for them.

# Additionally, we increased  the amount of variables to the knn in order to increase the accuracy 
# of predicting which type of method people used to commute.
# Furthermore, I trying to use trasportation as my mojor investigation but some how i dont have the data avaliable to run the regresion. 
# In conclusion, the KNN is a decent predictor when aiming at the commuting methods of people that live in
# Northeast  depending on the added variables.
cl_data_n <- as.numeric(cl_data)
model_ols1 <- lm(cl_data_n ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_rent
                 + train_data$norm_cost_electricity + train_data$norm_cost_gas + train_data$norm_cost_water
                 + train_data$norm_cost_fuelhome + train_data$norm_puma 
                 + train_data$norm_white + train_data$norm_pvt_insurance + train_data$norm_gq)         
y_hat <- fitted.values(model_ols1)
summary(model_ols_v1)
mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_rent
                   + train_data$norm_cost_electricity + train_data$norm_cost_gas + train_data$norm_cost_water
                   + train_data$norm_cost_fuelhome + train_data$norm_puma 
                   + train_data$norm_white + train_data$norm_pvt_insurance + train_data$norm_gq)

y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])

cl_data_n1 <- as.numeric(cl_data_n == 2)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_rent
                   + train_data$norm_cost_electricity + train_data$norm_cost_gas + train_data$norm_cost_water
                   + train_data$norm_cost_fuelhome + train_data$norm_puma 
                   + train_data$norm_white + train_data$norm_pvt_insurance + train_data$norm_gq)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])

cl_data_n1 <- as.numeric(cl_data_n == 3)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_rent
                   + train_data$norm_cost_electricity + train_data$norm_cost_gas + train_data$norm_cost_water
                   + train_data$norm_cost_fuelhome + train_data$norm_puma 
                   + train_data$norm_white + train_data$norm_pvt_insurance + train_data$norm_gq)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])

cl_data_n1 <- as.numeric(cl_data_n == 4)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_rent
                   + train_data$norm_cost_electricity + train_data$norm_cost_gas + train_data$norm_cost_water
                   + train_data$norm_cost_fuelhome + train_data$norm_puma 
                   + train_data$norm_white + train_data$norm_pvt_insurance + train_data$norm_gq)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])

cl_data_n1 <- as.numeric(cl_data_n == 5)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_poverty + train_data$norm_rent
                   + train_data$norm_cost_electricity + train_data$norm_cost_gas + train_data$norm_cost_water
                   + train_data$norm_cost_fuelhome + train_data$norm_puma 
                   + train_data$norm_white + train_data$norm_pvt_insurance + train_data$norm_gq)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
detach()
detach()











# Doing  a statistical test of the difference in average age between working people 
#in the Bronx vs working people in Brooklyn. What is the 95% confidence interval for the 
#difference in means?

attach(Dat_Northeast)

difference_age <- lm( AGE ~ DEGFIELD + FEMALE)
confint(difference_age)

difference_wage <- INCWAGE[DEGFIELD == "Business"] -
  table <- matrix(INCWAGES, age_DEGFIELD,age_FEMALE, col= 2, byrow = TRUE)



#difference of averege is depdent variables in regression?
#the null hypothesis is that their is no difference in wages between the independet variables

Household_Pulse_data <- as.factor (Household_Pulse_data)

levels_n <- read.csv("Household csv")
levels_orig <- levels(Household_Pulse_data) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(Household_Pulse_data) <- levels_new$New_Level



age_Northeast <- AGE(Dat_Northeast$EEDUC "Business")





#Using the Household _pulse data,  i was treying tonarrow this further - 
# what are the socioeconomic of bus/subway in the various places.
# What is the wealthiest  how  area and how do the people living there tend to commute?

# NOTE: Due to the lack of service and undertanding I was unable to load any data. I did everythig base on my class knowledge.



summary(INCWAGE[ Commute_bus == 1)
summary(bus_wage1)

bus_wage2 <- INCWAGE[ Commute_bus ==2)
summary(bus_wage2)







