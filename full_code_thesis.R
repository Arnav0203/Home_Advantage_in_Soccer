setwd("~/UMN/thesis/data analysis")
library(gdata)
thes1a<-read.xls('Reg2.xlsx')
mean(thes1a$Home_result)
summary(thes1a)
str(thes1a)
library(gtsummary)
#thes1a$Home_result<-as.factor(thes1a$Home_result)

#table(thes1a$Home_result, thes1a$COVID_dummy)
##Summaries
mean(thes1a$Home_goals[thes1a$COVID_dummy==1]) #1.302
mean(thes1a$Home_goals[thes1a$COVID_dummy==0]) #1.535
mean(thes1a$Away_goals[thes1a$COVID_dummy==1]) #1.1801
mean(thes1a$Away_goals[thes1a$COVID_dummy==0]) #1.1806
library(dplyr)
thes1b<-mutate(thes1a,total_goals=Home_goals+Away_goals) %>%
  mutate(thes1a,Goal_diff=Home_goals-Away_goals) %>% mutate(thes1a,total_YC=Home_YC+Away_YC) %>%
  mutate(thes1a,YC_diff=Home_YC-Away_YC)

Pre<-thes1b[thes1b$COVID_dummy==0,]
Post<-thes1b[thes1b$COVID_dummy==1,]
summary(Pre)
summary(Post)

#1c for COVID and result as factors
thes1c<-thes1b
thes1c$Home_result<-as.factor(thes1c$Home_result)
thes1c$COVID_dummy<-as.factor(thes1c$COVID_dummy)
levels(thes1c$COVID_dummy) <- list(Pre = "0", Post = "1")
levels(thes1c$Home_result) <- list(Win = "1", Draw="0.5", Loss="0")

thes1c %>% tbl_summary(by=COVID_dummy,statistic = list(all_continuous() ~ "{mean}",
                                                       all_categorical() ~ "{n} / {N} ({p}%)"),
                       digits = all_continuous() ~ 2) %>% add_p()


thes1c %>%
  tbl_cross(
    row = Home_result,
    col = COVID_dummy,
    percent = "cell"
  ) %>%
  add_p()
#

#thes1a$Home_result=factor(thes1a$Home_result,levels=c("loss","draw","win"))
#polr(Home_result~COVID_dummy+Home_goals+Away_goals+Home_YC,Away_YC,data=thes1a)
#thes1b$Home_result<-as.numeric(thes1b$Home_result)
lm1<-lm((COVID_dummy)~Home_result,data=thes1b)
summary(lm1)


str(thes1b)
#thes1b$COVID_dummy<-as.factor(thes1b$COVID_dummy)
log1<-glm(COVID_dummy~Home_goals+Away_goals+Home_YC+Away_YC,data=thes1b)
summary(log1)



log2<-glm(factor(COVID_dummy)~total_goals,data=thes1b)
summary(log2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.411082   0.020009  20.545  < 2e-16 ***
#   total_goals -0.020004   0.006452  -3.101  0.00196 ** 


log3 <- glm(total_goals~factor(COVID_dummy),data=thes1b)
t1 <- tbl_regression(mod1, exponentiate = TRUE)
t1


log3<-lm(total_goals~factor(COVID_dummy),data=thes1b)
summary(log3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           2.71570    0.04521  60.072  < 2e-16 ***
#   factor(COVID_dummy)1 -0.23412    0.07551  -3.101  0.00196 ** 

#Interpretation(if glm): log odds decrease by 0.234 as COVID_dummy increases by 1(post); Odds ratio=exp(-0.234)=0.79;
#So total goals decrease by 21% post-COVID


#log2<-glm(Home_goals+Away_goals+Home_YC+Away_YC~COVID_dummy,data=thes1b)
#summary(log2)

log4<-lm(total_YC~factor(COVID_dummy),data=thes1b)
summary(log4)

log5<-lm((COVID_dummy)~total_YC+total_goals,data=thes1b)
summary(log5)

ord1<-polr(factor(Home_result)~factor(COVID_dummy)+total_goals,data=thes1b)
summary(ord1)


#continued.....
library(MASS)
ol1<-polr(Home_result~(COVID_dummy),data=thes1c)
summary(ol1)


# Call:
#   polr(formula = Home_result ~ (COVID_dummy), data = thes1c)
# 
# Coefficients:
#   Value Std. Error t value
# COVID_dummyPost 0.2439    0.08561   2.849
# 
# Intercepts:
#   Value   Std. Error t value
# Win|Draw  -0.1826  0.0539    -3.3866
# Draw|Loss  0.8644  0.0572    15.1008
# 
# Residual Deviance: 4382.178 
# AIC: 4388.178 

exp(0.2439) #1.276 #0.24 is log odds
#Interpretation: The odds of a win for the home team rather than draw or loss is 1.276 times pre pandemic compared to when playing in empty stadiums.
#https://stats.idre.ucla.edu/r/faq/ologit-coefficients/

predict(ol1,data.frame(COVID_dummy="Pre"),type="probs")
predict(ol1,data.frame(COVID_dummy="Post"),type="probs")

#Hypothesis 1
#table from summary
t.test(Pre$Home_result,Post$Home_result)

#Hypothesis 2a:
t.test(Pre$Home_goals,Post$Home_goals)
#Supported!

#Hypothesis 2b
t.test(Pre$Away_goals,Post$Away_goals)
#Not supported

#Hypothesis 3a
t.test(Pre$Home_YC,Post$Home_YC)
#Not supported//

#Hyp 3b
t.test(Pre$Away_YC,Post$Away_YC)
#Supported!
`
#Extra
#1
t.test(Pre$total_goals,Post$total_goals)
#significant! 

log3<-lm(total_goals~factor(COVID_dummy),data=thes1b)
summary(log3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           2.71570    0.04521  60.072  < 2e-16 ***
#   factor(COVID_dummy)1 -0.23412    0.07551  -3.101  0.00196 ** 

#INTERPRETATION: On average, 0.23 less goals per match post-COVID.

#2
t.test(Pre$total_YC,Post$total_YC)

log4<-lm(total_YC~factor(COVID_dummy),data=thes1b)
summary(log4)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.22256    0.05057  63.721  < 2e-16 ***
#   factor(COVID_dummy)1 -0.33034    0.08447  -3.911 9.51e-05 ***

#INTERPRETATION: 0.33 less YC per match post-COVID.

#1.447476-1.444748

#NEW
#evaluate if home advantage is present even in empty stadiums- familiarity of conditions 
t.test(Pre$Home_goals,Pre$Away_goals)
t.test(Post$Home_goals,Post$Away_goals)

