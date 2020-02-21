library(readstata13)
library(ggplot2)
library(gridExtra)
library(margins)
library(knitr)
library(MASS)
library(lme4)
library(nlme)
library(plm)

graphics.off()
rm(list = ls())

suppressWarnings({
  nd = read.dta13(file = "nlsy_deming.dta", nonint.factors = TRUE)
  source('data_description.R')
})

## Q1
get_sd = function(x){
  x = na.omit(x)
  if(length(unique(x)) == 2){
    return(((mean(x)*(1-mean(x)))/length(x))^0.5)
  }else{
    return(sd(x))
  }
}
sum_nd = rbind.data.frame(apply(nd, 2, mean, na.rm = TRUE),
                          apply(nd, 2, get_sd))
colnames(sum_nd) = names(nd)
rownames(sum_nd) = c("Mean", "Std")
print(sum_nd)

nd_head_start = nd[nd$head_start == 1,]
sum_nd_head_start = rbind.data.frame(apply(nd_head_start, 2, mean, na.rm = TRUE),
                                     apply(nd_head_start, 2, get_sd))
colnames(sum_nd_head_start) = names(nd)
rownames(sum_nd_head_start) = c("Mean", "Std")
print(sum_nd_head_start)

diff_df = sum_nd_head_start - sum_nd
diff_df[2,] = (sum_nd[2,]^2 + sum_nd_head_start[2,]^2)^0.5
print(diff_df)

## Q2
fm2 = comp_score_5to6 ~ head_start
lm2 = rlm(formula = fm2, data = nd)
kable(summary(lm2)$coefficient)

## Q3
fm3 = comp_score_5to6 ~ head_start + (1|mom_id)
lm3 = lmer(fm3, data = nd, REML=TRUE)
kable(summary(lm3)$coefficient)

## Q4
fm4.1 = comp_score_5to6 ~ head_start + factor(mom_id)
fm4.2 = comp_score_5to6 ~ head_start + factor(mom_id) + 
  hispanic + black + firstborn + lninc_0to3 + 
  momed + dadhome_0to3

lm4.1 = lm(fm4.1, data = nd)
lm4.2 = lm(fm4.2, data = nd)
kable(summary(lm4.1)$coefficient[1:2,])
kable(summary(lm4.2)$coefficient[1:2,])

## Q5
to_z = function(x){
  return((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
}
nd$zscore_5to6 = to_z(nd$comp_score_5to6)
nd$zscore_7to10 = to_z(nd$comp_score_7to10)
nd$zscore_11to14 = to_z(nd$comp_score_11to14)
fm5.1 = zscore_5to6 ~ head_start + factor(mom_id) 
fm5.2 = zscore_7to10 ~ head_start + factor(mom_id) 
fm5.3 = zscore_11to14 ~ head_start + factor(mom_id)
lm5.1 = lm(fm5.1, data = nd)
lm5.2 = lm(fm5.2, data = nd)
lm5.3 = lm(fm5.3, data = nd)
kable(summary(lm5.1)$coefficient[1:2,])
kable(summary(lm5.2)$coefficient[1:2,])
kable(summary(lm5.3)$coefficient[1:2,])

## Q6
nd$repeat_grade = nd$`repeat`
fm6.1 = repeat_grade ~ head_start + factor(mom_id)
fm6.2 = learndis ~ head_start + factor(mom_id)
fm6.3 = hsgrad ~ head_start + factor(mom_id)
fm6.4 = somecoll ~ head_start + factor(mom_id)
fm6.5 = idle ~ head_start + factor(mom_id)
fm6.6 = fphealth ~ head_start + factor(mom_id)
fm6.7 = nd ~ head_start + factor(mom_id)
lm6.1 = lm(fm6.1, data = nd)
lm6.2 = lm(fm6.2, data = nd)
lm6.3 = lm(fm6.3, data = nd)
lm6.4 = lm(fm6.4, data = nd)
lm6.5 = lm(fm6.5, data = nd)
lm6.6 = lm(fm6.6, data = nd)
kable(summary(lm6.1)$coefficient[1:2,])
kable(summary(lm6.2)$coefficient[1:2,])
kable(summary(lm6.3)$coefficient[1:2,])
kable(summary(lm6.4)$coefficient[1:2,])
kable(summary(lm6.5)$coefficient[1:2,])
kable(summary(lm6.6)$coefficient[1:2,])

## Q7
fm6.1 = repeat_grade ~ head_start + hispanic + black + male + factor(mom_id)
fm6.2 = learndis ~ head_start + hispanic + black + male + factor(mom_id)
fm6.3 = hsgrad ~ head_start + hispanic + black + male + factor(mom_id)
fm6.4 = somecoll ~ head_start + hispanic + black + male + factor(mom_id)
fm6.5 = idle ~ head_start + hispanic + black + male + factor(mom_id)
fm6.6 = fphealth ~ head_start + hispanic + black + male + factor(mom_id)
fm6.7 = nd ~ head_start + hispanic + black + male + factor(mom_id)
lm6.1 = lm(fm6.1, data = nd)
lm6.2 = lm(fm6.2, data = nd)
lm6.3 = lm(fm6.3, data = nd)
lm6.4 = lm(fm6.4, data = nd)
lm6.5 = lm(fm6.5, data = nd)
lm6.6 = lm(fm6.6, data = nd)
kable(summary(lm6.1)$coefficient[1:5,])
kable(summary(lm6.2)$coefficient[1:5,])
kable(summary(lm6.3)$coefficient[1:5,])
kable(summary(lm6.4)$coefficient[1:5,])
kable(summary(lm6.5)$coefficient[1:5,])
kable(summary(lm6.6)$coefficient[1:5,])

