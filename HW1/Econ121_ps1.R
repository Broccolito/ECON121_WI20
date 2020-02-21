cps=as.matrix(ps1_cps,ncol=7)

uhrsworkt=as.numeric(cps[,4])
##eluminate nonnumeric 
for (i in length(uhrsworkt):1) {
  isna=is.na(uhrsworkt[i])
  if (isna) {
    cps=cps[-i,]
  }
}
uhrsworkt=as.numeric(cps[,4])
wkswork1=as.numeric(cps[,6])

for (i in length(uhrsworkt):1) {
  if (uhrsworkt[i]<35) {cps=cps[-i,]}  #Drop anyone who worked fewer than 50 weeks
  else if (wkswork1[i]<50) {cps=cps[-i,]}  #or fewer than 35 hours in a typical week
}
uhrsworkt=as.numeric(cps[,4])
wkswork1=as.numeric(cps[,6])
incwage=as.numeric(cps[,7])
for (i in length(incwage):1) {
  if(incwage[i]==0){cps=cps[-i,]}
}
uhrsworkt=as.numeric(cps[,4])  #hours usually worked per week at
wkswork1=as.numeric(cps[,6])  #weeks worked last year
incwage=as.numeric(cps[,7])  #yearly wage and salary income
hrswork1=wkswork1*uhrsworkt  #annual work hours
hrwage=incwage/hrswork1  #hourly wage
lghrwage=log(hrwage)  #log hourly wage


for (i in 1:nrow(cps)) {
  #Race dummies for "white", "black", "other"
  if(cps[i,3]=="Black"){cps[i,3]=0}
  else if(cps[i,3]=="White"){cps[i,3]=1}
  else{cps[i,3]=2}
  
  #A new education variable to measure years of schooling
  if(cps[i,5]=="Grades 1, 2, 3 or 4"){cps[i,5]=2.5}
  else if(cps[i,5]=="Grades 1"){cps[i,5]=1}
  else if(cps[i,5]=="Grades 2"){cps[i,5]=2}
  else if(cps[i,5]=="Grades 3"){cps[i,5]=3}
  else if(cps[i,5]=="Grades 4"){cps[i,5]=4}
  else if(cps[i,5]=="Grades 5 or 6"){cps[i,5]=5.5}
  else if(cps[i,5]=="Grades 5"){cps[i,5]=5}
  else if(cps[i,5]=="Grades 6"){cps[i,5]=6}
  else if(cps[i,5]=="Grades 7 or 8"){cps[i,5]=7.5}
  else if(cps[i,5]=="Grades 7"){cps[i,5]=7}
  else if(cps[i,5]=="Grades 8"){cps[i,5]=8}
  else if(cps[i,5]=="Grades 9"){cps[i,5]=9}
  else if(cps[i,5]=="Grade 10"){cps[i,5]=10}
  else if(cps[i,5]=="Grade 11"){cps[i,5]=11}
  else if(cps[i,5]=="Grades 12"){cps[i,5]=12}
  else if(cps[i,5]=="12th grade, no diploma"){cps[i,5]=12}
  else if(cps[i,5]=="12th grade, diploma unclear"){cps[i,5]=12}
  else if(cps[i,5]=="High school diploma or equivalent"){cps[i,5]=12}
  else if(cps[i,5]=="1 year of college"){cps[i,5]=13}
  else if(cps[i,5]=="Some college but no degree"){cps[i,5]=14}
  else if(cps[i,5]=="2 years of college"){cps[i,5]=14}
  else if(cps[i,5]=="Associate's degree, occupational/vacational program"){cps[i,5]=14}
  else if(cps[i,5]=="Associate's degree, academic program"){cps[i,5]=15}
  else if(cps[i,5]=="3 years of college"){cps[i,5]=15}
  else if(cps[i,5]=="4 years of college"){cps[i,5]=16}
  else if(cps[i,5]=="Bachelor's degree"){cps[i,5]=16}
  else if(cps[i,5]=="5 years of college"){cps[i,5]=17}
  else if(cps[i,5]=="5+ years of college"){cps[i,5]=17}
  else if(cps[i,5]=="6+ years of college"){cps[i,5]=18}
  else if(cps[i,5]=="Master's degree"){cps[i,5]=18}
  else if(cps[i,5]=="Professional school degree"){cps[i,5]=18}
  else if(cps[i,5]=="Doctorate degree"){cps[i,5]=21}
  else{cps[i,5]=0}
  
  #Sex dummies for "Male", "Female"
  if(cps[i,2]=="Male"){cps[i,2]=1}
  else if(cps[i,2]=="Female"){cps[i,2]=2}
}
age=as.numeric(cps[,1])  #age variable
sex=as.numeric(cps[,2])  #sex variable
race=as.numeric(cps[,3])  #race variable
educ=as.numeric(cps[,5])  #education years

exper=age-educ-5
exper2=exper*exper

#Mincerian Wage Equation
mincerian=summary(lm(lghrwage~educ+exper+exper2))$coefficient
#Extended Mincerian Wage Equation
extdMincerian=summary(lm(lghrwage~educ+race+sex+exper+exper2))$coefficient
tscore=(extdMincerian[3,2]-extdMincerian[1,2])/
  sqrt((extdMincerian[3,2])^2+(extdMincerian[2,2])^2)  #significant

#men data
maleData=list()
femaleData=list()
for (i in 1:length(sex)) {
  if (cps[i,2]==1) 
    {maleData=rlist::list.append(maleData,cps[i,])}
  else {femaleData=rlist::list.append(femaleData,cps[i,])}
}

