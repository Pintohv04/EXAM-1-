


# Question #2 

boxplot()



<!-- end list -->
  
  
  attach(Dat_Northeast)

detach()
attach(Dat_Northeast)

summary(Household_Pulse_data)

xtabs(~EEDUC + RECVDVACC + GENID_DESCRIBE)
, , GENID_DESCRIBE = NA

model_temp1 <- lm(EEDUC <- lm(RECVDVACC ~ AGE + female + AfAm= + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college )
    
                  
 model_temp1 <- lm(EEDUC <- lm(RECVDVACC ~ AGE + female=1  + AfAm= + Asian=0 + Amindian=0 + race_oth + Hispanic=1 + educ_hs + educ_somecoll + educ_college )
                                    
                
                  
 plot(model_temp1)

tabs <- lm(EEDUC ~ RECVDVACC  + GENID_DESCRIBE )
           
  + educ_nohs + educ_hs + educ_somecoll
 + educ_college + educ_advdeg)

summary(model_temp1)
confint(model_temp1)