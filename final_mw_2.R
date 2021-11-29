library(shiny)
library(shinydashboard)
library(readxl)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(mvtnorm)
library(quadprog)
library(dplyr)
library(data.table)
library(plyr)
library(matrixStats)
library(stats)
library(MASS)
library(data.table)
library(plyr)
library(psych)
library(dplyr)
Path <- 'c:/R/'

#인구수 추계
birth_man <-read_excel(paste(Path , "birth_man.xlsx", sep = ''))
people2019_man<-read_excel(paste(Path , "people2019_man.xlsx", sep = ''))
death_man <- read_excel(paste(Path , "death_man.xlsx", sep = ''))


people2019_man <- people2019_man[-1,]
peo <- people2019_man[2:101,3,drop = FALSE]
result_man <- birth_man[ , 4:52,drop = FALSE]
result_man <- rbind.fill(result_man, peo)

deathNum_m <- data.frame(rep(1:nrow(result_man),1))
for(i in 1:48){
  result_man[,i] <- as.numeric(result_man[,i])
  print(result_man[, i])
  #class(death_man[,(3+i)])
  deathNum_m <- cbind(deathNum_m , result_man[,i,drop = FALSE]* death_man[,(3+i),drop = FALSE])  
  for(j in 1:100){
    if(j==100){
      result_man[j+1,i+1] <- result_man[j+1, i] - deathNum_m[j+1,i+1] + result_man[j,i] - deathNum_m[j,i+1]
    }
    else{
      result_man[j+1, i+1] = result_man[j,i] - deathNum_m[j,i+1]
    }
  }
}
deathNum_m <- deathNum_m[,2:ncol(deathNum_m)]

birth_woman <-read_excel(paste(Path , "birth_woman.xlsx", sep = ''))
people2019_woman<-read_excel(paste(Path , "people2019_woman.xlsx", sep = ''))
death_woman <- read_excel(paste(Path , "death_woman.xlsx", sep = ''))


people2019_woman <- people2019_woman[-1,]
peo <- people2019_woman[2:101,3,drop = FALSE]
result_woman <- birth_woman[ , 4:52,drop = FALSE]
result_woman <- rbind.fill(result_woman, peo)

deathNum_w <- data.frame(rep(1:nrow(result_woman),1))
for(i in 1:48){
  result_woman[,i] <- as.numeric(result_woman[,i])
  print(result_woman[, i])
  deathNum_w <- cbind(deathNum_w , result_woman[,i,drop = FALSE]* death_woman[,(3+i),drop = FALSE])  
  for(j in 1:100){
    if(j==100){
      result_woman[j+1,i+1] <- result_woman[j+1, i] - deathNum_w[j+1,i+1] + result_woman[j,i] - deathNum_w[j,i+1]
    }
    else{
      result_woman[j+1, i+1] = result_woman[j,i] - deathNum_w[j,i+1]
    }
  }
}
deathNum_w <- deathNum_w[,2:ncol(deathNum_w)]

#5세별 묶기
df_showSum_m  <- data.frame('연도'=numeric(),'인구수'= numeric(),'연령'= integer())
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      people<-sum(result_man[1:20,i])
      age<-'0-19'
      df_showSum_m  <-rbind(df_showSum_m , data.frame('연도'=year,'인구수'=people,'연령'=age))
    }
    else if(j==11){
      year<- 2018+i
      people<-sum(result_man[66:101,i])
      age<- '65-'
      df_showSum_m  <-rbind(df_showSum_m , data.frame('연도'=year,'인구수'=people,'연령'=age))
    }
    else{
      year<- 2018+i
      people<-sum(result_man[(11+5*j):(15+5*j),i])
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_showSum_m  <-rbind(df_showSum_m , data.frame('연도'=year,'인구수'=people,'연령'=age))
      
    }
  }
}
#1세별 묶기
df_test_m  <- data.frame('연도'=numeric(),'인구수'= numeric(),'연령'= integer())
for(i in 1:49){
  for(j in 1:101){
    year<- 2018+i
    people<-result_man[j,i]
    age<- j
    df_test_m <-rbind(df_test_m , data.frame('연도'=year,'인구수'=people,'연령'=age))
  }
}

#5세별 묶기
df_showSum_w  <- data.frame('연도'=numeric(),'인구수'= numeric(),'연령'= integer())
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      people<-sum(result_woman[1:20,i])
      age<-'0-19'
      df_showSum_w  <-rbind(df_showSum_w , data.frame('연도'=year,'인구수'=people,'연령'=age))
    }
    else if(j==11){
      year<- 2018+i
      people<-sum(result_woman[66:101,i])
      age<- '65-'
      df_showSum_w  <-rbind(df_showSum_w , data.frame('연도'=year,'인구수'=people,'연령'=age))
    }
    else{
      year<- 2018+i
      people<-sum(result_woman[(11+5*j):(15+5*j),i])
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_showSum_w  <-rbind(df_showSum_w , data.frame('연도'=year,'인구수'=people,'연령'=age))
      
    }
  }
}
#1세별 묶기
df_test_w  <- data.frame('연도'=numeric(),'인구수'= numeric(),'연령'= integer())
for(i in 1:49){
  for(j in 1:101){
    year<- 2018+i
    people<-result_woman[j,i]
    age<- j
    df_test_w <-rbind(df_test_w , data.frame('연도'=year,'인구수'=people,'연령'=age))
  }
}


#전체 추계 인구(남자+여자)
result_sum<-result_man+result_woman


# 평균소득 추계
income_man <-read_excel(paste(Path , "income_man.xlsx", sep = ''))
income_woman <-read_excel(paste(Path , "income_woman.xlsx", sep = ''))


B<- function(x){
  x <- as.numeric(x)
  x <- x*1.02}


resultIncome_m <-income_man[2:nrow(income_man),3:ncol(income_man),drop = FALSE]
resultIncome_m <- data.frame(resultIncome_m)
for(i in 1:48){
  toadd <- data.frame(lapply(resultIncome_m[i], B))
  colnames(toadd) <- 2019+i
  resultIncome_m <- cbind(resultIncome_m , toadd)
}
df_income_m <- data.frame('연도'=numeric(),'평균소득'= numeric(),'연령'= integer())
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      income<-resultIncome_m[j,i]
      age<-'0-19'
      df_income_m <-rbind(df_income_m, data.frame('연도'=year,'평균소득'=income,'연령'=age))
    }
    else if(j==11){
      year<- 2018+i
      income<-resultIncome_m[j,i]
      age<- '65-'
      df_income_m <-rbind(df_income_m, data.frame('연도'=year,'평균소득'=income,'연령'=age))
    }
    else{
      year<- 2018+i
      income<-resultIncome_m[j,i]
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_income_m <-rbind(df_income_m, data.frame('연도'=year,'평균소득'=income,'연령'=age))
      
    }
  }
}
resultIncome_w <-income_woman[2:nrow(income_woman),3:ncol(income_woman),drop = FALSE]
resultIncome_w <- data.frame(resultIncome_w)
for(i in 1:48){
  toadd <- data.frame(lapply(resultIncome_w[i], B))
  colnames(toadd) <- 2019+i
  resultIncome_w <- cbind(resultIncome_w , toadd)
}
df_income_w <- data.frame('연도'=numeric(),'평균소득'= numeric(),'연령'= integer())
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      income<-resultIncome_w[j,i]
      age<-'0-19'
      df_income_w <-rbind(df_income_w, data.frame('연도'=year,'평균소득'=income,'연령'=age))
    }
    else if(j==11){
      year<- 2018+i
      income<-resultIncome_w[j,i]
      age<- '65-'
      df_income_w <-rbind(df_income_w, data.frame('연도'=year,'평균소득'=income,'연령'=age))
    }
    else{
      year<- 2018+i
      income<-resultIncome_w[j,i]
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_income_w <-rbind(df_income_w, data.frame('연도'=year,'평균소득'=income,'연령'=age))
      
    }
  }
}

earn <- 0.638 # 근로소득비율
biz <- 0.218 #사업소득비율
asset <- 0.067 #재산소득비율
transfer <- 0.077 #이전소득비율
income_ratio <- data.frame('소득종류'= c('근로소득','사업소득','재산소득','이전소득'), '소득' = c(63.8, 21.8, 6.7, 7.7)) # 시각화 테이블

#근로자수 추계

employment_man <- read_excel(paste(Path , "employment_man.xlsx", sep = ''))
employment_woman <- read_excel(paste(Path , "employment_woman.xlsx", sep = ''))
employment_sum <- read_excel(paste(Path , "employment_sum.xlsx", sep = ''))
social_insurance<- read_excel(paste(Path ,"social_insurance_age.xlsx", sep = ''))

emp1 <-as.numeric(unlist(employment_man[2:12,3]))
emp2 <-as.numeric(unlist(employment_woman[2:12,3]))

mulEmp<- function(x,j,gender){
  x <- as.numeric(x)
  if(gender==1) # 남자 
    x <- x*emp1[j]/100
  else      # 여자
    x <- x*emp2[j]/100
}

for(i in 1:49){
  for(j in 1:11){
    if(j==11){
      toappend <- c(toappend, mulEmp(sum(result_man[66:101,i]), j, 1))
    }
    else if(j==1){
      toappend <- c(mulEmp(sum(result_man[(11+5*j):(15+5*j),i]), j, 1))
    }
    else{
      toappend <- c(toappend, mulEmp(sum(result_man[(11+5*j):(15+5*j),i]), j, 1))
    }
  }
  
  if(i==1){
    work_m <- data.frame( '2019' =toappend)
  }
  else{
    work_m <- cbind(work_m, data.frame(x = toappend))
    colnames(work_m)[i] <- 2018+i
  }
}
rownames(work_m) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60-64세','65세이상')

for(i in 1:49){
  for(j in 1:11){
    if(j==11){
      toappend <- c(toappend, mulEmp(sum(result_woman[66:101,i]), j, 2))
    }
    else if(j==1){
      toappend <- c(mulEmp(sum(result_woman[(11+5*j):(15+5*j),i]), j, 2))
    }
    else{
      toappend <- c(toappend, mulEmp(sum(result_woman[(11+5*j):(15+5*j),i]), j, 2))
    }
  }
  
  if(i==1){
    work_w <- data.frame( '2019' =toappend)
  }
  else{
    work_w <- cbind(work_w, data.frame(x = toappend))
    colnames(work_w)[i] <- 2018+i
  }
}
rownames(work_w) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60-64세','65세이상')

<<<<<<< HEAD
#전체 취업자 수
work_s=work+m+work_s

=======
#보험료납부금액 추계
>>>>>>> 61bd81584b6e1ad5b22ab44f4f5756a58b9a284a
pension_insur = 0.09 #보험료율
health_insur = 0.0683
employment_insur = 0.016


pension_m<-0.737 #국민연금가입율
health_m<-0.796 #건강보험가입율
employ_m<-0.764  #고용보험가입률

pension_w= 0.644
health_w=0.688
employ_w=0.661

social_m1<- work_m*pension_m #사회보험가입자
social_m2<- work_m*health_m
social_m3<- work_m*employ_m

social_w1<- work_w*pension_w
social_w2<- work_w*health_w
social_w3<- work_w*employ_w

df_showInsur_m <- data.frame('연도' = integer(), '연령'= integer(), '국민연금'= numeric(), '건강보험'= numeric(), '고용보험'= numeric())
k <- 1
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      income_earn <- as.numeric(df_income_m[k,2]) * earn # 근로소득
      income_earn
      k <- k+1
      pension <- social_m1[j,i] * income_earn * pension_insur
      pension
      health <- social_m2[j,i] * income_earn * health_insur
      employ <- social_m3[j,i] * income_earn * employment_insur
      age<-'15-19'
      df_showInsur_m <-rbind(df_showInsur_m, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
    else if(j==11){
      year<- 2018+i
      income_earn <- as.numeric(df_income_m[k,2]) * earn # 근로소득
      k <- k+1
      pension <- social_m1[j,i] * income_earn * pension_insur
      health <- social_m2[j,i] * income_earn * health_insur
      employ <- social_m3[j,i] * income_earn * employment_insur
      age<- '65-'
      df_showInsur_m <-rbind(df_showInsur_m, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
    else{
      year<- 2018+i
      income_earn <- as.numeric(df_income_m[k,2]) * earn # 근로소득
      k <- k+1
      pension <- social_m1[j,i] * income_earn * pension_insur
      health <- social_m2[j,i] * income_earn * health_insur
      employ <- social_m3[j,i] * income_earn * employment_insur
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_showInsur_m <-rbind(df_showInsur_m, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
  }
}


df_showInsur_w <- data.frame('연도' = integer(), '연령'= integer(), '국민연금'= numeric(), '건강보험'= numeric(), '고용보험'= numeric())
k <- 1
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      income_earn <- as.numeric(df_income_w[k,2]) * earn # 근로소득
      k <- k+1
      pension <- social_w1[j,i] * income_earn * pension_insur
      health <- social_w2[j,i] * income_earn * health_insur
      employ <- social_w3[j,i] * income_earn * employment_insur
      age<-'15-19'
      df_showInsur_w <-rbind(df_showInsur_w, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
    else if(j==11){
      year<- 2018+i
      income_earn <- as.numeric(df_income_w[k,2]) * earn # 근로소득
      k <- k+1
      pension <- social_w1[j,i] * income_earn * pension_insur
      health <- social_w2[j,i] * income_earn * health_insur
      employ <- social_w3[j,i] * income_earn * employment_insur
      age<- '65-'
      df_showInsur_w <-rbind(df_showInsur_w, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
    else{
      year<- 2018+i
      income_earn <- as.numeric(df_income_w[k,2]) * earn # 근로소득
      k <- k+1
      pension <- social_w1[j,i] * income_earn * pension_insur
      health <- social_w2[j,i] * income_earn * health_insur
      employ <- social_w3[j,i] * income_earn * employment_insur
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_showInsur_w <-rbind(df_showInsur_w, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
  }
}

