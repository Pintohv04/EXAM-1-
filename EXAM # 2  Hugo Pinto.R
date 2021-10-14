
# Hugo Pinto 
# Exam #1 

# question #1


# first lets create  a subgroup to work the entire test

x=rnorm(RECVDVACC== "yes got vaxx")
Y=rnorm(REGION== "Northeast")
t.test(y,x)
  Welch two Sample test
 
  data:  x and y
  t = -0.30839, df = 929.12, p-value = 0.7579
  alternative hypothesis: true difference in means is not equal to 0
  95 percent confidence interval:
    -0.1454516  0.1059464
  sample estimates:
    mean of x   mean of y 
  -0.03481415 -0.01506158
  
  
  
  
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
 


