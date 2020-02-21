library(here)
library(readstata13)
library(ggplot2)
library(gridExtra)
library(margins)
library(knitr)
library(MASS)

graphics.off()
rm(list = ls())

suppressWarnings({
  nhis = read.dta13(file = "nhis2000.dta", nonint.factors = TRUE)
})

# Q1
nhis$unhealthy = ifelse((nhis$health == "Poor" | 
                           nhis$health == "Fair"), 1, 0)

# Q2
nhis$faminc_level = ifelse(nhis$faminc_gt75 == 1, "High",
                           ifelse(nhis$faminc_20t75 == 1, "Medium", "Low"))
nhis$faminc_level = factor(nhis$faminc_level, 
                           levels = c("Low", "Medium", "High"))
nhis$educ_level = ifelse(nhis$edyrs < 12, "less_than_high_school",
                         ifelse(nhis$edyrs == 12, "high_school",
                                ifelse(nhis$edyrs >= 13 & nhis$edyrs <= 15, "some_college",
                                       ifelse(nhis$edyrs == 16, "complete_college",
                                              ifelse(nhis$edyrs > 16, "post_grad", NA)))))
nhis$educ_level = factor(nhis$educ_level, 
                         levels = c("less_than_high_school",
                                    "high_school",
                                    "some_college",
                                    "complete_college",
                                    "post_grad"))

nhis = na.omit(nhis)
p1 = ggplot(data = nhis, aes(x = faminc_level, y = mort5)) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  facet_grid(rows = nhis$sex) + 
  xlab("Level of Family Income") + 
  ylab("Rate of mortality")
p2 = ggplot(data = nhis, aes(x = faminc_level, y = unhealthy)) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  facet_grid(rows = nhis$sex) + 
  xlab("Level of Family Income") +  
  ylab("Fair/poor health")
p3 = ggplot(data = nhis, aes(x = educ_level, y = mort5)) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  facet_grid(rows = nhis$sex) + 
  xlab("Education Level") + 
  ylab("Rate of mortality")
p4 = ggplot(data = nhis, aes(x = educ_level, y = unhealthy)) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  facet_grid(rows = nhis$sex) + 
  xlab("Education Level") + 
  ylab("Fair/poor health")
p5 = grid.arrange(p1,p2,p3,p4, nrow = 2)
p5

# Q3
nhis$age = as.numeric(nhis$age)
nhis$age[is.na(nhis$age)] = 85
nhis$faminc_20less = ifelse(nhis$faminc_gt75 == 1, 0,
                            ifelse(nhis$faminc_20t75 == 1, 0, 1))

fm1 = (mort5 ~ age + white + black + hisp + edyrs + faminc_level)
lp1 = summary(lm(data = nhis, fm1))$coefficient
pb1 = summary(glm(data = nhis, fm1, family = binomial(link = "probit")))$coefficient
pbm1 = margins(glm(data = nhis, fm1, family = binomial(link = "probit")))
lg1 = summary(glm(data = nhis, fm1, family = binomial(link = "logit")))$coefficient
lgm1 = margins(glm(data = nhis, fm1, family = binomial(link = "logit")))

fm2 = (unhealthy ~ age + white + black + hisp + edyrs + faminc_level)
lp2 = summary(lm(data = nhis, fm2))$coefficient
pb2 = summary(glm(data = nhis, fm2, family = binomial(link = "probit")))$coefficient
pbm2 = margins(glm(data = nhis, fm2, family = binomial(link = "probit")))
lg2 = summary(glm(data = nhis, fm2, family = binomial(link = "logit")))$coefficient
lgm2 = margins(glm(data = nhis, fm2, family = binomial(link = "logit")))

# Q4
kable(lg1)
black_white_diff = lg1[3,1] - (lg1[4,1] + lg1[8,1])
black_white_std = (lg1[3,1]^2 + lg1[4,1]^2 + lg1[8,1]^2)^0.5
t_score = black_white_diff/black_white_std
print(t_score)

black_rich = nhis[nhis$black == 1 & nhis$faminc_level == "High",]$mort5
white_poor = nhis[nhis$white == 1 & nhis$faminc_level == "Low",]$mort5
t.test(black_rich, white_poor)

# Q6
fm3 = (mort5 ~ age + white + black + hisp + edyrs + faminc_level + hypertenev + smokev + diabeticev)
lg3 = glm(data = nhis, fm3, family = binomial(link = "logit"))
summary(lg3)$coefficients

# Q7
nhis$health_level = ifelse(nhis$health == "Poor", 1,
                           ifelse(nhis$health == "Fair", 2,
                                  ifelse(nhis$health == "Good", 3,
                                         ifelse(nhis$health == "Very Good", 4, 
                                                ifelse(nhis$health == "Excellent", 5, NA))))) 
fm4 = mort5 ~ health_level
summary(lm(data = nhis, fm4))$coefficient
p6 = ggplot(data = nhis, aes(x = health_level, y = mort5)) + 
  geom_bar(stat = "summary", fun.y = "mean") + 
  facet_grid(rows = nhis$sex) + 
  xlab("Level of Self-reported health") + 
  ylab("Rate of mortality")
p6

# Q8
fm5 = (health_level ~ age + white + black + hisp + edyrs + faminc_level)
nhis$health_level = as.factor(nhis$health_level)
pb3 = polr(data = nhis, fm5)
kable(summary(pb3)$coefficient)
