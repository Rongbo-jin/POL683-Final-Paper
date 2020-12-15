library(haven)
library(censReg)
library(dplyr)
library(MASS)
library(ggplot2)
library(brms)
library(VGAM)
library(GGally)
library(nnet)
library(dplyr)
library(smicd)
library(stargazer)
anes <- read_dta("/Users/macintoshhd/Desktop/replication /Reconsidering the Relationship between Authoritarianism and Republican Support in 2016 and Beyond/merge201213nes.dta")
panel_16 <-  read_dta("/Users/macintoshhd/Desktop/replication /Reconsidering the Relationship between Authoritarianism and Republican Support in 2016 and Beyond/2016 panel study.dta")

# save(anes, file="anes.RData")

# panel_16 wave 1-3 feeling thermometer towards Trump
summary(panel_16$w1trumpft)
hist(panel_16$w1trumpft, main="Figure 1. Histogram of Trump Feeling Thermometer, Wave 1")
summary(panel_16$w2trumpft)
hist(panel_16$w2trumpft, main="Figure 2. Histogram of Trump Feeling Thermometer, Wave 2")
summary(panel_16$w3trumpft)
table(panel_16$w3trumpft)
hist(panel_16$w3trumpft, main="Figure 3. Histogram of Trump Feeling Thermometer, Wave 3")
table(panel_16$w3trumpft)
table(panel_16$w2trumpft)

table(panel_16$w1trumpft)
table(panel_16$w2trumpft)
table(panel_16$w3trumpft)
# panel_16 wave 1-3 feeling thermometer towards Republican party
summary(panel_16$w1repft)
hist(panel_16$w1repft, main="Figure 4. Histogram of Republican Party Feeling Thermometer, Wave 1")
summary(panel_16$w2repft)
hist(panel_16$w2repft, main="Figure 5. Histogram of Republican Party Feeling Thermometer, Wave 2")
summary(panel_16$w3repft)
hist(panel_16$w3repft, main="Figure 6. Histogram of Republican Party Feeling Thermometer, Wave 3")

table(panel_16$w1repft)
table(panel_16$w2repft)
table(panel_16$w3repft)

# panel_16 authoritarianism
hist(panel_16$w1authoritarianism, main="Figure 9. Histogram of Authoritarianism Level, Wave 1")
hist(panel_16$w2authoritarianism, main="Figure 10. Histogram of Authoritarianism Level, Wave 2")
table(panel_16$w1authoritarianism)
table(panel_16$w2authoritarianism)


# ANES 2012-13 feeling thermometer towards Mitt Romney
summary(anes$romneyft12)
hist(anes$romneyft12, main="Figure 7. Histogram of Mitt Romney Feeling Thermometer (2012)")
summary(anes$romneyft13)
hist(anes$romneyft13, main="Figure 8. Histogram of Mitt Romney Feeling Thermometer (2013)")
# ANES 2012-13 feeling thermometer towards Republican Party
summary(anes$ft_rep12)
hist(anes$ft_rep12)

table(anes$romneyft12)
table(anes$romneyft13)

table(anes$ft_rep12)

# ANES 2012-13 authoritarianism 
hist(anes$w1authoritarianism)
hist(anes$w2authoritarianism)

table(anes$w1authoritarianism)
table(anes$w2authoritarianism)


### Luuttig's model
## panel_16
# Table 1 feeling thermometer as DV
lm1 <- lm(w2trumpft ~ w1trumpft + w1authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm1)
lm2 <- lm(w3trumpft ~ w2trumpft + w2authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm2)
lm3 <- lm(w2repft ~ w1repft + w1authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm3)
lm4 <- lm(w3repft ~ w2repft + w2authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm4)

tobit_1 <- censReg(w2trumpft ~ w1trumpft + w1authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16, left = 0.1)
summary(tobit_1)
tobit_2 <- censReg(w3trumpft ~ w2trumpft + w2authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16, left = 0.05)
summary(tobit_2)
tobit_3 <- censReg(w2repft ~ w1repft + w1authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16, left = 0.1, right = 0.9)
summary(tobit_3)
tobit_4 <- censReg(w3repft ~ w2repft + w2authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16, left = 0.2)
summary(tobit_4)

stargazer(tobit_1, tobit_3, tobit_2, tobit_4, 
          covariate.labels=c("Trump FT-W1", "GOP FT-W1","Auth-W1", "Trump FT-W2", "GOP FT-W2","Auth-W2", "Education", "Income", "Age", "White", "Black", "Sex"), 
          type="text", out="word")


# using VGAM
t_1 <- vglm(w2trumpft ~ w1trumpft + w1authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16, tobit(Upper = 1, Lower = 0, type.f = "cens"))
summary(t_1)
t_2 <- vglm(w3trumpft ~ w2trumpft + w2authoritarianism + educ01 + income01 + age + white + black + sex, data = panel_16, tobit(Upper = 1, Lower = 0, type.f = "cens"))
summary(t_2)

t_3 <- vglm(w2authoritarianism ~ w1authoritarianism + w1trumpft + educ01 + income01 + age + white + black + sex, data = panel_16, tobit(Upper = 0.75, type.f = "cens"))
summary(t_3)
t_4 <- vglm(w2authoritarianism ~ w1authoritarianism + w1repft + educ01 + income01 + age + white + black + sex, data = panel_16, tobit(Upper = 0.75, type.f = "cens"))
summary(t_4)


# Table 2 authoritarianism as DV
lm5 <- lm(w2authoritarianism ~ w1authoritarianism + w1trumpft + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm5)
lm6 <- lm(w2authoritarianism ~ w1authoritarianism + w1repft + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm6)

lm7 <- lm(w1authoritarianism ~ w1trumpft + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm7)
lm8 <- lm(w2authoritarianism ~ w2repft + educ01 + income01 + age + white + black + sex, data = panel_16)
summary(lm8)

tobit_5 <- censReg(w2authoritarianism ~ w1authoritarianism + w1trumpft + educ01 + income01 + age + white + black + sex, data = panel_16, right = 0.75)
summary(tobit_5)
tobit_6 <- censReg(w2authoritarianism ~ w1authoritarianism + w1repft + educ01 + income01 + age + white + black + sex, data = panel_16, right = 0.75)
summary(tobit_6)

stargazer(tobit_5, tobit_6, 
          covariate.labels=c("Auth-W1", "Trump FT-W1", "GOP FT-W1", "Education", "Income", "Age", "White", "Black", "Sex"),
          type="text")

# censoring on w2trumpft
panel_16$censor1 <- ifelse(panel_16$w2trumpft==0, "right",
                           ifelse(panel_16$w2trumpft==1, "left", "none"))

table(panel_16$censor1)

fit1 <- brm( w2trumpft| cens(censor1) ~ w1trumpft + w1authoritarianism + educ01 + income01 + age + white + black + sex,
            data = panel_16, family = gaussian)
summary(fit1)
plot(fit1)


# censoring on w3trumpft
panel_16$censor2 <- ifelse(panel_16$w3trumpft==0, "right",
                           ifelse(panel_16$w3trumpft==1, "left", "none"))

table(panel_16$censor2)

fit2 <- brm( w3trumpft| cens(censor2) ~ w2trumpft + w2authoritarianism + educ01 + income01 + age + white + black + sex,
             data = panel_16, family = gaussian)
summary(fit2)
plot(fit2)

# considering the censoring in ordinal dependent variable in a linear regression 
intervals_1 <- c(-Inf, 0, 0.25, 0.5, 0.75, 1)
panel_16$w2authoritarianism_1 <- NA
panel_16$w2authoritarianism_1 <- cut(panel_16$w2authoritarianism, intervals_1)
table(panel_16$w2authoritarianism_1)
panel_16 <- as.data.frame(panel_16)
b1 <- semLm(formula = w2authoritarianism_1 ~ w1authoritarianism + w1trumpft + educ01 + income01 + age + white + black + sex, data = panel_16, 
           classes=intervals_1, bootstrap.se = TRUE)
b2 <- semLm(formula = w2authoritarianism_1 ~ w1authoritarianism + w1repft + educ01 + income01 + age + white + black + sex, data = panel_16, 
            classes=intervals_1, bootstrap.se = TRUE)

summary(b1)
summary(b2)

## ANES 2012-13
m1 <- lm(romneyft13 ~ w1authoritarianism + romneyft12 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m1)
m2 <- lm(w2authoritarianism ~ w1authoritarianism + romneyft12 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m2)
m3 <- lm(w2authoritarianism ~ w1authoritarianism + ft_rep12 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m3)

t1 <- censReg(romneyft13 ~ w1authoritarianism + romneyft12 + age + educ01 + income01 + white + black + sex, data = anes, left = 0.2)
summary(t1)
t2 <- censReg(w2authoritarianism ~ w1authoritarianism + romneyft12 + age + educ01 + income01 + white + black + sex, data = anes, right = 0.6)
summary(t2)
t3 <- censReg(w2authoritarianism ~ w1authoritarianism + ft_rep12 + age + educ01 + income01 + white + black + sex, data = anes, right = 0.6)
summary(t3)
stargazer(t1, t2, t3, 
          covariate.labels=c("Auth-W1", "Romney FT-12", "GOP FT-12", "Age", "Education", "Income",  "White", "Black", "Sex"),
          type="text")


# party id
table(anes$pid713)
m4 <- lm(pid713 ~ w1authoritarianism + pid12 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m4)
m5 <- lm(w2authoritarianism ~ w1authoritarianism + pid12 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m5)

n1 <- multinom(as.factor(pid713)~w1authoritarianism + pid12 + age + educ01 + income01 + white + black + sex, anes)
summary(n1)

table(panel_16$w1pid7)

table(panel_16$w2pid7)
table(panel_16$w3pid3)


# considering the censoring in ordinal dependent variable in a linear regression 
intervals_2 <- c(-Inf, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1)
intervals_3 <- c(-Inf, 0, 0.25, 0.5, 0.75, 1)
anes$w2authoritarianism_1 <- NA
anes$w2authoritarianism_1 <- cut(anes$w2authoritarianism, intervals_3) %>% as.factor()
anes <- as.data.frame(anes) #%>% na.omit()
b3 <- semLm(w2authoritarianism_1 ~ w1authoritarianism + ft_rep12 + age + educ01 + income01 + white + black + sex, data = anes,
            classes = intervals_3, bootstrap.se = TRUE)
summary(b3)

# OLS model
# ANES 12-13
m_1 <- lm(romneyft12 ~ w1authoritarianism + age + educ01 + income01 + white + black + sex, data = anes)
summary(m_1)
m_2 <- lm(w1authoritarianism ~ romneyft12 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m_2)
m_3 <- lm(w2authoritarianism ~ romneyft13 + age + educ01 + income01 + white + black + sex, data = anes)
summary(m_3)
m_4 <- lm(romneyft13 ~ w2authoritarianism + age + educ01 + income01 + white + black + sex, data = anes)
summary(m_4)

