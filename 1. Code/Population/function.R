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
library(data.table)
library(plyr)
library(psych)
library(dplyr)
library(MASS)
library(tibble)
library(tseries)
library(corrplot)
library(PerformanceAnalytics)
library(zoo)
library(roll)
library(ggplot2)
library(xts)
library(dygraphs)
library(forecast)
library(gridExtra)
select <- dplyr::select
options("scipen" = 100) 
Path <- '/Users/ko-eunseo/Documents/ALM system/'
#Path <- 'C:/R/'

pension_insur = 0.09  #보험료율- 고정이라 인자로 안 넣었음
health_insur = 0.0683
employment_insur = 0.008
earn <- 0.638 # 근로소득비율 
biz <- 0.218 #사업소득비율
asset <- 0.067 #재산소득비율
transfer <- 0.077 #이전소득비율
pension_m<-0.737 #국민연금가입율 - 연령별 x
health_m<-0.796 #건강보험가입율
employ_m<-0.765  #고용보험가입률
pension_w <- 0.644
health_w <- 0.688
employ_w <- 0.661
inflation <- 2
wage_increase_rate <- 2
income_local_ratio <- 0.5 # 사업장가입자 소득 대비 지역가입자 소득비율
#인구수 관련 Input
birth_man <-read_excel(paste(Path , "birth_man.xlsx", sep = '')) #출생아 수
people2019_man<-read_excel(paste(Path , "people2019_man.xlsx", sep = '')) #2019년 인구수
death_man <- read_excel(paste(Path , "death_man.xlsx", sep = '')) #사망률
birth_woman <-read_excel(paste(Path , "birth_woman.xlsx", sep = ''))
people2019_woman<-read_excel(paste(Path , "people2019_woman.xlsx", sep = ''))
death_woman <- read_excel(paste(Path , "death_woman.xlsx", sep = ''))
# Input Data Import 
income_man <-read_excel(paste(Path , "income_man.xlsx", sep = ''))
income_woman <-read_excel(paste(Path , "income_woman.xlsx", sep = ''))
income_sum <-read_excel(paste(Path , "income_sum.xlsx", sep = ''))

employment_man <- read_excel(paste(Path , "employment_man.xlsx", sep = '')) #고용률(15~65세)
employment_woman <- read_excel(paste(Path , "employment_woman.xlsx", sep = ''))
social_insurance <-read_excel(paste(Path , "social_insurance_age.xlsx", sep = ''))
pension_compose <-read_excel(paste(Path , "pension_compose.xlsx", sep = ''))
old_pensionReceive_age <-read_excel(paste(Path , "old_pensionReceive_age.xlsx", sep = ''))
names(old_pensionReceive_age) = c('연령','합계','남자','여자')
old_pensionReceive_age <- old_pensionReceive_age[-(1:2),]
old_bereaved_rate <-read_excel(paste(Path , "old_bereaved_rate.xlsx", sep = ''))
disorder_pensionReceive_age <-read_excel(paste(Path , "disorder_pensionReceive_age.xlsx", sep = ''))
names(disorder_pensionReceive_age) = c('연령','합계','남자','여자')
disorder_pensionReceive_age <- disorder_pensionReceive_age[-(1:2),]
disorder_bereaved_rate <-read_excel(paste(Path , "disorder_bereaved_rate.xlsx", sep = ''))
total_receiver <- read_excel(paste(Path, "total_receiver.xlsx",sep = ''))
pensionReceive <- read_excel(paste(Path, "pensionReceive.xlsx",sep = ''))
pensionReceive <- pensionReceive[,-1]
names(pensionReceive) <- c('급여종류','수급자수','지급금액')


# 남녀 인구수 추계 함수
result_total<- function(birth, people2019 , death) {
  # Input 
  ## birth : 출생아수
  ## people2019 : 2019년 data
  ## death : 사망률
  
  # Return
  ## input(남/녀)에 따른 인구수 추계
  
  people2019 <- people2019[-1,]
  peo <- people2019[2:101,3,drop = FALSE] 
  result <- birth[ , 4:52,drop = FALSE]
  result <- rbind.fill(result, peo) #인구수 추계 dataframe
  deathNum <- data.frame(rep(1:nrow(result),1)) #사망자 수
  
  for(i in 1:48){
    result[,i] <- as.numeric(result[,i])
    deathNum <- cbind(deathNum , result[,i,drop = FALSE]* death[,(3+i),drop = FALSE])
  
    for(j in 1:100){
      if(j==100){
        result[j+1,i+1] <- result[j+1, i] - deathNum[j+1,i+1] + result[j,i] - deathNum[j,i+1]
      }
      else{
        result[j+1, i+1] = result[j,i] - deathNum[j,i+1]
      }
    }
  }
  deathNum <- cbind(deathNum , result[,49,drop = FALSE]* death[,52,drop = FALSE]) 
  deathNum <- deathNum[,2:ncol(deathNum)]
  
  my_list<-list('인구수'=result, '사망자'= deathNum ) 
  return(my_list) # 남녀인구수추계
}
#ResTot <- result_total(birth_man , people2019_man , death_man , birth_woman , people2019_woman , death_woman)


# 소득 관련 함수 - 임금상승률, 물가상승률 적용
# 필요 함수 정의
growth<- function(x, inflation, wage_increase_rate){
  x <- as.numeric(x)
  x <- x*((inflation+ wage_increase_rate)/100+1)
  return(x)
}
avgIncome <- function(income, inflation, wage_increase_rate){
  # Input 
  ## income_man
  ## income_woman
  ## income_sum
  ## inflation 
  
  # Return
  ## input(2019년 data) 에 소득증가율 반영한 평균소득
  
  # Input 데이터에 대한 prerprocessing
  resultIncome <-income[2:nrow(income),3:ncol(income),drop = FALSE]
  resultIncome <- data.frame(resultIncome)
  
  for(i in 1:48){
    toadd <- data.frame(growth(unlist(resultIncome[i]), inflation, wage_increase_rate))
    colnames(toadd) <- 2019+i
    resultIncome <- cbind(resultIncome , toadd)
  }
  
  return(resultIncome) 
}
#AvgIncom <- avgIncome(income_man,income_woman,income_sum , inflation, wage_increase_rate)

# Data preprocessing
emp1 <-as.numeric(unlist(employment_man[2:12,3]))
emp2 <-as.numeric(unlist(employment_woman[2:12,3]))

# 필요 함수 정의
mulEmp<- function(x,j,gender){
  x <- as.numeric(x)
  if(gender==1) # 남자 
    x <- x*emp1[j]/100
  else      # 여자
    x <- x*emp2[j]/100
  return(x)
}

# 근로자 수
work_total <- function(employment_rate, population){
  # Input 
  ## employment_rate - 고용률 (employment_man or employment_woman)
  ## population - 인구 (result_man or result_woman)

  # Return 
  ## 근로자수(연령별 5세단위)
  for(i in 1:49){
    for(j in 1:11){
      if(j==11){
        toappend <- c(toappend, mulEmp(sum(population[66:101,i]), j, 1))
      }
      else if(j==1){
        toappend <- c(mulEmp(sum(population[(11+5*j):(15+5*j),i]), j, 1)) # 고용률 5세단위, 15~19세부터 시작
      }
      else{
        toappend <- c(toappend, mulEmp(sum(population[(11+5*j):(15+5*j),i]), j, 1))
      }
    }
    
    if(i==1){
      work <- data.frame( '2019' =toappend)
    }
    else{
      work <- cbind(work, data.frame(x = toappend))
      colnames(work)[i] <- 2018+i
    }
  }
  rownames(work) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60-64세','65세이상')

  return(work) # input(남자/여자)에 따른 근로자수(연령별 5세단위)
}

#WorkTot <- work_total(employment_man, employment_woman,ResTot$남자,ResTot$여자)

#국민연금 가입자 세분화

pension_sub <- function(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w){
  pension_compose <-read_excel(paste(Path , "pension_compose.xlsx", sep = ''))
  #가입자 18~19세, 55~56세까지라 사업장가입자 구하기 위해 근로자 다시 묶음
  for(i in 1:49){
    for(j in 1:9){
      if(j==1){
        toappend <- c(mulEmp(sum(result_man[19:20,i]), j, 1))
        toappend2 <- c(mulEmp(sum(result_woman[19:20,i]), j, 2))
      }
      else{
        toappend <- c(toappend, mulEmp(sum(result_man[(11+5*j):(15+5*j),i]), j, 1))
        toappend2 <- c(toappend2, mulEmp(sum(result_woman[(11+5*j):(15+5*j),i]), j, 2))
      }
    }
    
    if(i==1){
      work_m_d <- data.frame( '2019' =toappend)
      work_w_d <- data.frame( '2019' =toappend2)
    }
    else{
      work_m_d <- cbind(work_m_d, data.frame(x = toappend))
      work_w_d <- cbind(work_w_d, data.frame(x = toappend2))
      colnames(work_m_d)[i] <- 2018+i
      colnames(work_w_d)[i] <- 2018+i
    }
  }
  rownames(work_m_d) = c('18-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세')
  rownames(work_w_d) = c('18-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세')
  
  social_m_d<- work_m_d*pension_m#국민연금 사업장 가입자수
  social_w_d<- work_w_d*pension_w
  random_last_m <- data.frame('2019'=work_m[10,1] * 0.076) #임의계속가입자 - 60세이상인구 * 60세이상 국민연금가입률
  random_last_w <- data.frame('2019'=work_w[10,1] * 0.076)
  local_social_m <- social_m_d
  local_social_w <- social_w_d
  random_social_m <- social_m_d
  random_social_w <- social_w_d
  for(i in 1:49){
    for(j in 1:9){
      #사업장가입자 대비 임의가입, 지역가입자 비율로 나머지 둘 구함
      local_by_work_m <- (as.numeric(pension_compose[(3*j+3),5]) + as.numeric(pension_compose[(3*j+2),8])/2)/ as.numeric(pension_compose[(3*j+3),4])
      local_by_work_w <- (as.numeric(pension_compose[(3*j+4),5]) + as.numeric(pension_compose[(3*j+2),8])/2)/ as.numeric(pension_compose[(3*j+4),4])
      random_by_work_m <- as.numeric(pension_compose[(3*j+3),6])/ as.numeric(pension_compose[(3*j+3),4])
      random_by_work_w <- as.numeric(pension_compose[(3*j+4),6])/ as.numeric(pension_compose[(3*j+3),4])
      local_social_m[j,i] <- (social_m_d[j,i] * local_by_work_m)
      local_social_w[j,i] <- (social_w_d[j,i] * local_by_work_w)
      random_social_m[j,i] <- (social_m_d[j,i]) * random_by_work_m
      random_social_w[j,i] <- (social_w_d[j,i]) * random_by_work_w
    }
    if(i>1){
      random_last_m <- cbind(random_last_m, data.frame('e'=work_m[10,i] * 0.076 )) #임의계속가입자
      random_last_w <- cbind(random_last_w, data.frame('e'=work_w[10,i] * 0.076 ))
    }
  }
  names(random_last_m)<- c(2019:2067)
  names(random_last_w)<- c(2019:2067)
  rownames(random_last_m) <- '60세 이상'
  rownames(random_last_w) <- '60세 이상'
  #colSums(local_social)
  #colSums(random_social)
  #colSums(social_m_d + social_w_d)
  #unlist(random_last) + colSums(random_social)
  sum_social_m <- local_social_m+ social_m_d + random_social_m
  names(sum_social_m) <- c(2019:2067)
  sum_social_m<- rbind(sum_social_m, random_last_m) # 총 가입자수
  sum_social_w <- local_social_w+ social_w_d + random_social_w
  names(sum_social_w) <- c(2019:2067)
  sum_social_w <- rbind(sum_social_w, random_last_w) 
  #colSums(sum_social_w+sum_social_m) # 연도별 총 가입자수 합 출력
  pension_list <- list('남자사업장가입'=social_m_d,'여자사업장가입'=social_w_d,'남자지역가입'= local_social_m,'여자지역가입'=local_social_w,'남자임의가입'=random_social_m,'여자임의가입'=random_social_w,'남자임의계속가입'=random_last_m,'여자임의계속가입'=random_last_w,'남자총가입자'=sum_social_m,'여자총가입자'=sum_social_w,'총가입자'= sum_social_m + sum_social_w)
  return(pension_list)
}

#보험료납부금액 추계- 연령별 가입률 미적용, 유형별 간략하게
showInsur_all <- function(Path,earn , result_man, result_woman, pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , resultIncome_m , resultIncome_w, income_local_ratio){
  # Input
  ## earn : 근로소득비율
  ## pension_insur : 보험료율
  ## health_insur : 
  ## employment_insur : 
  
  ## pension_m : 국민연금가입율 - 연령별 x
  ## health_m : 건강보험가입율
  ## employ_m : 고용보험가입률
  ## pension_w
  ## health_w
  ## employ_w
  
  ## work_m : 근로자수
  ## work_w
  
  ## df_income_m : 평균 소득
  ## df_income_w
  
  # Return
  ## 보험료납부금액 추계
  social_m1_work<- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['남자사업장가입']] #국민연금 사업장가입자
  social_m1_local <- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['남자지역가입']] # 국민연금 지역가입자
  social_m2<- work_m*health_m
  social_m3<- work_m*employ_m
  
  social_w1_work<- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['여자사업장가입']] #국민연금 사업장가입자
  social_w1_local <- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['여자지역가입']] # 국민연금 지역가입자
  social_w2<- work_w*health_w
  social_w3<- work_w*employ_w
  df_showInsur_m <- data.frame('연도' = integer(), '연령'= integer(), '국민연금'= numeric(), '건강보험'= numeric(), '고용보험'= numeric())
  for(i in 1:49){
    for(j in 1:9){
      if(j==1){
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_m[j,i]) * earn # 근로소득
        pension <- social_m1_work[j,i] * income_earn * pension_insur +  social_m1_local[j,i] * income_local_ratio * income_earn * pension_insur
        health <- social_m2[j,i] * income_earn * health_insur
        employ <- social_m3[j,i] * income_earn * employment_insur
        age<-'18-19'
        df_showInsur_m <-rbind(df_showInsur_m, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
      else{
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_m[j,i]) * earn # 근로소득
        pension <- social_m1_work[j,i] * income_earn * pension_insur +  social_m1_local[j,i] * income_local_ratio * income_earn * pension_insur
        health <- social_m2[j,i] * income_earn * health_insur
        employ <- social_m3[j,i] * income_earn * employment_insur
        age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
        df_showInsur_m <-rbind(df_showInsur_m, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
    }
  }
  
  
  df_showInsur_w <- data.frame('연도' = integer(), '연령'= integer(), '국민연금'= numeric(), '건강보험'= numeric(), '고용보험'= numeric())
  for(i in 1:49){
    for(j in 1:9){
      if(j==1){
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_w[j,i]) * earn # 근로소득
        pension <- social_w1_work[j,i] * income_earn * pension_insur +  social_w1_local[j,i] * income_local_ratio * income_earn * pension_insur
        health <- social_w2[j,i] * income_earn * health_insur
        employ <- social_w3[j,i] * income_earn * employment_insur
        age<-'18-19'
        df_showInsur_w <-rbind(df_showInsur_w, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
      else{
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_w[j,i]) * earn # 근로소득
        pension <- social_w1_work[j,i] * income_earn * pension_insur +  social_w1_local[j,i] * income_local_ratio * income_earn * pension_insur
        health <- social_w2[j,i] * income_earn * health_insur
        employ <- social_w3[j,i] * income_earn * employment_insur
        age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
        df_showInsur_w <-rbind(df_showInsur_w, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
    }
  }
  my_list <- list('남자'= df_showInsur_m,'여자'= df_showInsur_w)
  return(my_list)
}
#InsurAll <- showInsur_all(earn , pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , WorkTot$남자, WorkTot$여자 , AvgIncom$남자 , AvgIncom$여자)

#가입자수 구하는 함수 - 남녀합, 보험종류별, 연령별
social_sub <- function(social_insurance , work_total , j,i,k){
  # Input 
  ## social_insurance
  ## work_total 
  
  # Return 
  # 
  social_insurance <- social_insurance[-1,-1] #필요없는 행열 제거 - 1열: 국민연금/ 2열: 건강보험/ 3열: 고용보험
  
  if(j<=3)
    return(work_total[j,i] * as.numeric(social_insurance[1,k])/100)
  else if(j>=4 & j<=5)
    return(work_total[j,i] * as.numeric(social_insurance[2,k])/100)
  else if(j>=6 & j<=7)
    return(work_total[j,i] * as.numeric(social_insurance[3,k])/100)
  else if(j>=8 & j<=9)
    return(work_total[j,i] * as.numeric(social_insurance[4,k])/100)
  else
    return(work_total[j,i] * as.numeric(social_insurance[5,k])/100)
}

showInsur_all_age <- function(social_insurance , work_total, resultIncome_s){
  # Input
  ## social_insurance
  ## work_total
  ## resultIncome_s
  
  # Return
  ## 가입자수
  
  df_showInsur <- data.frame('연도' = integer(), '연령'= integer(), '국민연금'= numeric(), '건강보험'= numeric(), '고용보험'= numeric())
  for(i in 1:49){
    for(j in 1:11){
      if(j==1){
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_s[j,i]) * earn # 근로소득
        pension <- social_sub(social_insurance , work_total ,j,i,1)* income_earn * pension_insur
        health <- social_sub(social_insurance , work_total ,j,i,2) * income_earn * health_insur
        employ <- social_sub(social_insurance , work_total ,j,i,3) * income_earn * employment_insur
        age<-'15-19'
        df_showInsur <-rbind(df_showInsur, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
      else if(j==11){
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_s[j,i]) * earn # 근로소득
        pension <-  social_sub(social_insurance , work_total ,j,i,1)* income_earn * pension_insur
        health <- social_sub(social_insurance , work_total ,j,i,2) * income_earn * health_insur
        employ <- social_sub(social_insurance , work_total ,j,i,3) * income_earn * employment_insur
        age<- '65-'
        df_showInsur <-rbind(df_showInsur, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
      else{
        year<- 2018+i
        income_earn <- as.numeric(resultIncome_s[j,i]) * earn # 근로소득
        pension <- social_sub(social_insurance , work_total ,j,i,1)* income_earn * pension_insur
        health <- social_sub(social_insurance , work_total ,j,i,2) * income_earn * health_insur
        employ <- social_sub(social_insurance , work_total ,j,i,3) * income_earn * employment_insur
        age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
        df_showInsur <-rbind(df_showInsur, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
      }
    }
  }
  return(df_showInsur)
}

#InsurAll_age <- showInsur_all_age(social_insurance , WorkTot$전체, AvgIncom$전체)



#수입지출
# 국민연금 수입지출
pension <- function(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive){
  total_receiver <-read_excel(paste(Path , "total_receiver.xlsx", sep = ''))
  income_p <- c((sum(showInsur_man %>% filter(연도==2019) %>% select(국민연금))+sum(showInsur_woman %>% filter(연도==2019) %>% select(국민연금)))* 10000 *12 ) # 단위 만원-> 원으로 변경
  cost_p <- c(as.numeric(df_disorder_receive %>%  filter(연도==2019) %>% select(장애연금수급액)) + as.numeric(df_old_receive %>%  filter(연도==2019) %>% select(노령연금수급액)) + as.numeric(df_bereaved_receive %>%  filter(연도 == 2019) %>% select(유족연금수급액)) )
  for (i in 1:48) {
    income_p <- c(income_p, (sum(showInsur_man %>% filter(연도==(2019+i)) %>% select(국민연금))+sum(showInsur_woman %>% filter(연도==(2019+i)) %>% select(국민연금)))* 10000 *12 )
    cost_p <- c(cost_p, as.numeric(df_disorder_receive %>%  filter(연도==2019+i) %>% select(장애연금수급액)) + as.numeric(df_old_receive %>%  filter(연도==2019+i) %>% select(노령연금수급액)) + as.numeric(df_bereaved_receive %>%  filter(연도 == 2019+i) %>% select(유족연금수급액)))
  }
  #total_receiver 시작 연도에따라  연도 출력 나오게
  firstYear <-as.integer(colnames(total_receiver[,2:ncol(total_receiver)]))[1]
  year <- c(firstYear:(firstYear+48))
  year
  df_pension <-data.frame('연도'= year,'국민연금수입'= income_p, '국민연금지출'= cost_p, '순수익'= (income_p- cost_p))
  
  return(df_pension)
}


#지출
#노령연금수급
old_receive <- function(Path, result_sum, pensionReceive){
  total_receiver <- read_excel(paste(Path, "total_receiver.xlsx",sep = ''))  # 국민연금 수급자 수 전망 -> 수급비율 구할 때 사용
  start_receive <-read_excel(paste(Path , "start_receive.xlsx", sep = ''))
  start_receive <- start_receive[-1,] #2019년부터 시작하게 행제거
  startAge <- as.numeric(start_receive[1,2])
  receive_2019 <- as.numeric(pensionReceive %>% filter(급여종류=='노령연금') %>% select(수급자수))
  #최근연도 수급자수/ 수급개시연령 이상 인구수
  receive_ratio_first <- receive_2019 / sum(result_sum[63:101,1])
  receive_ratio_end <- as.numeric(total_receiver[1,7])/ sum(result_sum[66:101,42]) # 60년 예상 수급자 수/65세이상(수급개시)인구
  increase <- (receive_ratio_end - receive_ratio_first)/41 # 매년 증가하는 수급비율
  receive_ratio <- receive_ratio_first
  j <- 2
  receiver <- c(receive_2019) #2019년도 수급자 먼저 추가
  old_receive <- as.numeric(pensionReceive %>% filter(급여종류 == '노령연금') %>% select(지급금액))*1000/ as.numeric(pensionReceive %>% filter(급여종류=='노령연금') %>% select(수급자수)) # 연평균 1인당 노령연급수급액 (총지급액/수급자수)
  receive_amount <- c(old_receive * receive_2019) #총수급(19년도수급자 * 19년도 수급액)먼저 추가
  toadd <-old_receive
  for(i in 1:48){
    if((2019+i)== as.numeric(start_receive[j,1]) & j<5){
      j <- j+1
      startAge <- as.numeric(start_receive[j-1,2])
    }
    receiver <- c(receiver, sum(result_sum[startAge:101,i+1])*receive_ratio )
    old_receive <- growth(old_receive, inflation, wage_increase_rate)
    toadd <- old_receive * sum(result_sum[startAge:101,i+1]) * receive_ratio
    receive_amount <- c(receive_amount, toadd)
    receive_ratio <- receive_ratio + increase  # 수급비율증가
  }
  df_cost_p <- data.frame('연도'= c(2019:2067), '수급자수'= receiver,  '노령연금수급액'= receive_amount)
  return(df_cost_p)
}


#장애연금 수급자
disorder_receive <- function(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate, pensionReceive){
  disorder_rate<-read_excel(paste(Path , "disorder_rate.xlsx", sep = ''))#장애발생률
  disorder_1519<-read_excel(paste(Path , "disorder_1519.xlsx", sep = ''))#1519 장애수급자
  sum_social_m <- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['남자총가입자']]
  sum_social_w <- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['여자총가입자']]
  #사망률->사망자수/인구수
  for(i in 1:49){
    for(j in 1:10){
      if(j==1){
        toappend_m <-c(sum(deathNum_m[16:21,i])/sum(result_man[16:21,i]))
        toappend_w <-c(sum(deathNum_w[16:21,i])/sum(result_woman[16:21,i]))
      }
      else if(j==10){
        toappend_m <- c(toappend_m, (sum(deathNum_m[61:101,i])/sum(result_man[61:101,i])))
        toappend_w <- c(toappend_w, (sum(deathNum_w[61:101,i])/sum(result_woman[61:101,i])))
      }
      else{
        toappend_m <- c(toappend_m, sum(deathNum_m[(11+5*j):(15+5*j),i])/sum(result_man[(11+5*j):(15+5*j),i]))
        toappend_w <- c(toappend_w, sum(deathNum_w[(11+5*j):(15+5*j),i])/sum(result_woman[(11+5*j):(15+5*j),i]))
      }
      
    }
    if(i==1){
      death_rate_m <- data.frame( '2019' =toappend_m)
      death_rate_w <- data.frame( '2019' =toappend_w)
    }
    else{
      death_rate_m <- cbind(death_rate_m, data.frame(x = toappend_m))
      death_rate_w <- cbind(death_rate_w, data.frame(x = toappend_w))
      colnames(death_rate_m)[i] <- 2018+i
      colnames(death_rate_w)[i] <- 2018+i
    }
  }
  rownames(death_rate_m) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60세 이상')
  rownames(death_rate_w) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60세이상')
  
  #60세 이상 인구 비율
  for(i in 1:10){
    if(i ==1){
      toappend_60_m <-sum(result_man[61:101,1])/sum(result_man[1:101,1])
      toappend_60_w <-sum(result_woman[61:101,1])/sum(result_woman[1:101,1])
    }
    else{
      toappend_60_m <- c(toappend_60_m, sum(result_man[61:101,5*i-4])/sum(result_man[1:101,5*i-4]))
      toappend_60_w<- c(toappend_60_w, sum(result_woman[61:101,5*i-4])/sum(result_woman[1:101,5*i-4]))
    }
  }
  
  rate_60<-data.frame(toappend_60_m,toappend_60_w)
  rate_60<-t(rate_60)
  
  #근로자 수 다시묶기 (잠재가입자)
  for(i in 1:49){
    for(j in 1:9){
      if(j==1){
        toappend <- c(mulEmp(sum(result_man[16:20,i]), j, 1))
        toappend2 <- c(mulEmp(sum(result_woman[16:20,i]), j, 2))
      }
      else{
        toappend <- c(toappend, mulEmp(sum(result_man[(11+5*j):(15+5*j),i]), j, 1))
        toappend2 <- c(toappend2, mulEmp(sum(result_woman[(11+5*j):(15+5*j),i]), j, 2))
      }
    }
    
    if(i==1){
      work_m_d <- data.frame( '2019' =toappend)
      work_w_d <- data.frame( '2019' =toappend2)
    }
    else{
      work_m_d <- cbind(work_m_d, data.frame(x = toappend))
      work_w_d <- cbind(work_w_d, data.frame(x = toappend2))
      colnames(work_m_d)[i] <- 2018+i
      colnames(work_w_d)[i] <- 2018+i
    }
  }
  rownames(work_m_d) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세')
  rownames(work_w_d) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세')
  
  #가입자수(잠재가입자)
  social_m_d<- work_m_d*pension_m
  social_w_d<- work_w_d*pension_w
  
  #장애연금 수급자 수 추계-중복 급여 고려x
  disorder_man  <- disorder_1519[1,3:13,drop = FALSE]*(as.numeric(disorder_rate[1,2])/100)*(1-death_rate_m[1,1])
  disorder_2019_m <- disorder_pensionReceive_age[3:11,3,drop = FALSE]
  colnames(disorder_2019_m )<-'2019'
  names(disorder_man) <- c('2019','2024','2029','2034','2039','2044','2049','2054','2059','2064','2069')
  disorder_woman  <- disorder_1519[2,3:13,drop = FALSE]*(as.numeric(disorder_rate[1,3])/100)*(1-death_rate_w[1,1])
  disorder_2019_w <- disorder_pensionReceive_age[3:11,4,drop = FALSE]
  colnames(disorder_2019_w)<-'2019'
  names(disorder_woman) <- c('2019','2024','2029','2034','2039','2044','2049','2054','2059','2064','2069')
  disorder_man  <- rbind.fill(disorder_man,disorder_2019_m)
  disorder_woman  <- rbind.fill(disorder_woman,disorder_2019_w)
  
  # sum_social_m<-sum_social_m[-10,]
  # sum_social_w<-sum_social_w[-10,]
  
  for(i in 1:10){
    disorder_man[,i] <- as.numeric(disorder_man[,i])
    disorder_woman[,i] <- as.numeric(disorder_woman[,i])
    for(j in 1:9){
      if(j==9){
        disorder_man[j+1,i+1] <-disorder_man[j,i]*(1-death_rate_m[j+1,5*i-4])*rate_60[1,i]+(disorder_man[j+1,i]+5*(sum_social_m[j,5*i-4]*(as.numeric(disorder_rate[j,2])/100))*(1-death_rate_m[j+1,5*i-4]))
        disorder_woman[j+1,i+1] <-disorder_woman[j,i]*(1-death_rate_w[j+1,5*i-4])*rate_60[2,i]+(disorder_woman[j+1, i]+5*(sum_social_w[j,5*i-4]*as.numeric(disorder_rate[j,3])/100)*(1-death_rate_w[j+1,5*i-4]))
      }
      else{
        disorder_man[j+1, i+1] <-(disorder_man[j,i]+5*(sum_social_m[j,5*i-4]*as.numeric(disorder_rate[j,2])/100))*(1-death_rate_m[j+1,5*i-4])
        disorder_woman[j+1, i+1] <-(disorder_woman[j,i]+5*(sum_social_w[j,5*i-4]*as.numeric(disorder_rate[j,3])/100))*(1-death_rate_w[j+1,5*i-4])
      }
      colnames(disorder_man)[i] <- 2014+5*i
      colnames(disorder_woman)[i] <- 2014+5*i
    }
  }
  
  rownames(disorder_man) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60세 이상')
  rownames(disorder_woman) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60세 이상')
  #colSums(disorder_man)+colSums(disorder_woman)
  
  disorder_man <- linear_approx(disorder_man) # 선형보간
  disorder_woman <- linear_approx(disorder_woman)
  disorder_man <- disorder_man[, -50] # 선형보간 하고 68년 제거
  disorder_woman <- disorder_woman[, -50]
  
  #장애유족연금
  am<-sum(disorder_bereaved_rate[9:15,2,drop= FALSE])/7 #60세 이상 유유족률 평균값
  d_bereaved_m<-rbind(disorder_bereaved_rate[1:8,2,drop= FALSE],am)
  aw<-sum(disorder_bereaved_rate[9:15,3,drop= FALSE])/7 
  d_bereaved_w<-rbind(disorder_bereaved_rate[1:8,3,drop= FALSE],aw)
  
  
  disorder_bereaved_m <- disorder_man[-1,,drop= FALSE] #15~19세 행 제거, 유유족률X
  disorder_bereaved_w <- disorder_woman[-1,,drop= FALSE]

  
  for(i in 1:10){
    for(j in 1:9){
      if(j==1){
        disorder_bereaved_m[j,i] <-disorder_man[j+1,i]*death_rate_m[j+1,i]*d_bereaved_m[j,1]/100
        disorder_bereaved_w[j,i] <-disorder_woman[j+1,i]*death_rate_w[j+1,i]*d_bereaved_w[j,1]/100
      }
      else{
        disorder_bereaved_m[j,i] <- disorder_man[j+1,i]*death_rate_m[j+1,i]*d_bereaved_m[j,1]/100
        disorder_bereaved_w[j,i] <- disorder_woman[j+1,i]*death_rate_w[j+1,i]*d_bereaved_w[j,1]/100
      }
    }
    rownames(disorder_bereaved_m) = c('20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60세이상')
    rownames(disorder_bereaved_w) = c('20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60세이상')
    #return(my_list)
  }
  disorder_money <- as.numeric(pensionReceive %>% filter(급여종류=='장애연금') %>% select(지급금액))*1000 / as.numeric(pensionReceive %>% filter(급여종류=='장애연금') %>%  select(수급자수)) # 장애연금 총지급액/수급자수 -> 1인당 연평균 장애연금 수급액
  disorder_sum <- disorder_man + disorder_woman

  for(i in 1:49){ 
    if(i==1){
      disorder_receive_mon <- c(as.numeric(colSums(disorder_sum[,i,drop= FALSE])) * disorder_money)
    }
    else{
      disorder_receive_mon <- c(disorder_receive_mon, as.numeric(colSums(disorder_sum[,i,drop= FALSE])) * disorder_money)
    }
    disorder_money <- growth(disorder_money, inflation, wage_increase_rate )
  }
  df_disorder_mon <- data.frame('연도'= c(2019:2067), '장애연금수급액'= disorder_receive_mon)
  my_list <- list('남자장애수급자'= disorder_man, '여자장애수급자'= disorder_woman, '남자장애유족' = disorder_bereaved_m, '여자장애유족'= disorder_bereaved_w, '전체장애연금수급액'= df_disorder_mon )
  return(my_list)
  
}

#선형보간 함수
linear_approx <- function(df_disorder){
  i <-1
  allyear <- df_disorder[,1 , drop= FALSE]
  for(i in 1:10){
    if(i>1)
      allyear <- cbind(allyear, df_disorder[, i, drop= FALSE])
    for(j in 1:10){
      inc <- (df_disorder[j,i+1] - df_disorder[j,i])/5
      if(j==1){
        add1 <- c(df_disorder[j,i] + inc*1)
        add2 <- c(df_disorder[j,i] + inc*2)
        add3 <- c(df_disorder[j,i] + inc*3)
        add4 <- c(df_disorder[j,i] + inc*4)
      }
      else{
        add1 <- c(add1, df_disorder[j,i] + inc*1)
        add2 <- c(add2, df_disorder[j,i] + inc*2)
        add3 <- c(add3, df_disorder[j,i] + inc*3)
        add4 <- c(add4, df_disorder[j,i] + inc*4)
      }
    }
    dataset <- data.frame(add1, add2, add3, add4)
    names(dataset) <- c(as.character(2014+5*i+1), as.character(2014+5*i+2), as.character(2014+5*i+3), as.character(2014+5*i+4))
    allyear <- cbind(allyear, dataset)
  }
  return(allyear)
}
#유족연금
bereaved_receive <- function(Path, sum_social_s, total_receiver, pensionReceive){
  be_money <- as.numeric(pensionReceive %>% filter(급여종류 == '유족연금') %>% select(지급금액))*1000/ as.numeric(pensionReceive %>% filter(급여종류=='유족연금') %>% select(수급자수))
  start_ratio <- as.numeric(total_receiver[2,2])/ sum(sum_social_s[,1])
  end_ratio <- as.numeric(total_receiver[2,7])/ sum(sum_social_s[,42])
  increase <- (end_ratio- start_ratio)/41
  ratio <- start_ratio
  total <-colSums(sum_social_s) # 연도별 총가입자
  be_receive <- c(as.numeric(total_receiver[2,2]))
  receive_mon <- c(be_money*as.numeric(total_receiver[2,2]))
  for (i in 1:48) {
    be_money <- growth(be_money, inflation, wage_increase_rate)
    be_receive <- c(be_receive, sum(sum_social_s[,i+1])* ratio )
    receive_mon <- c(receive_mon, be_money * sum(sum_social_s[,i+1])* ratio)
    ratio <- ratio + increase
  }
  return(data.frame('연도'= c(2019:2067),'유족연금수급자'= be_receive, '유족연금수급액'= receive_mon))
}

employ_insurance <- function(Path, work_s, earn, Insurance_age, resultIncome_s){
  employee_by_scale <-read_excel(paste(Path , "employee_by_scale.xlsx", sep = ''))
  resultIncome_s <- avgIncome(income_sum, inflation, 3.5) # 고용보험 재정전망에서 명목임금상승률 3.5%로 가정
  #work_2016 <- 26409000
  self_insur <- 0.02 #  자영업자 보험료율
  employee_by_scale <- employee_by_scale[-1,-c(1:2)]
  names(employee_by_scale) <- c('규모별', '총종사자수', '자영업자')
  employee_by_scale$`총종사자수` <- as.numeric(employee_by_scale$`총종사자수`) 
  employee_by_scale$`자영업자` <- as.numeric(employee_by_scale$`자영업자`) 
  company_1 <- (sum(employee_by_scale[2:6,2]) + employee_by_scale[7,2]/2 - (sum(employee_by_scale[2:6,3]) + employee_by_scale[7,3]/2)) / employee_by_scale[1,2]
  company_2 <- (sum(employee_by_scale[8:10,2]) + employee_by_scale[7,2]/2 - (sum(employee_by_scale[2:6,3]) + employee_by_scale[7,3]/2)) / employee_by_scale[1,2]
  company_3 <- (employee_by_scale[11,2] - employee_by_scale[11,3]) / employee_by_scale[1,2]
  resultIncome_s$X2019 <- as.numeric(resultIncome_s$X2019)
  company_1_self <- (sum(employee_by_scale[2:6,3]) + employee_by_scale[7,3]/2) / employee_by_scale[1,2]
  company_2_self <- (sum(employee_by_scale[8:10,3]) + employee_by_scale[7,3]/2) / employee_by_scale[1,2]
  company_3_self <- employee_by_scale[11,3] / employee_by_scale[1,2]
  
  data <- data.frame('기업규모'= numeric(), '연령'= character(), '고용보험납부액'= numeric(), '연도' = integer())
  for(i in 1:ncol(work_s)){
    for (j in 1:nrow(work_s)) {
      income_earn <- resultIncome_s[j,i] * 10000 * 12 * earn
      a <- as.numeric(work_s[j,i]* company_1 * income_earn * (employment_insur+0.0025)+  work_s[j,i]* company_1_self * income_earn * (self_insur+0.0025))
      b <- as.numeric(work_s[j,i] * company_2 * income_earn * (employment_insur+0.0065)+ work_s[j,i] * company_2_self * income_earn * (self_insur++0.0065))
      c <- as.numeric(work_s[j,i]* company_3 * income_earn * (employment_insur+0.0085)+ work_s[j,i] * company_3_self * income_earn * (self_insur++0.0085))
      year <- 2018+i
      data <- rbind(data, data.frame('기업규모'= 1, '연령' = rownames(work_s)[j], '고용보험납부액' = a, '연도' = year ))
      data <- rbind(data, data.frame('기업규모'= 2, '연령' = rownames(work_s)[j], '고용보험납부액' = b, '연도' = year ))
      data <- rbind(data, data.frame('기업규모'= 3, '연령' = rownames(work_s)[j], '고용보험납부액' = c, '연도' = year ))
    }
  }
  df_employ <- data.frame('연도'= numeric(), '고용보험료수입' = numeric())
  for(i in 1:49){
    df_employ <- rbind(df_employ,data.frame('연도'= 2018+i, '고용보험료수입' = sum(data %>% filter(연도 == (2018+i)) %>% select(고용보험납부액)) + sum(Insurance_age %>%  filter(연도== 2018+i ) %>% select(고용보험)) * 10000 * 12))
  }
  #sum(Insurance_age %>%  filter(연도== 2019) %>% select(고용보험)) * 10000 * 12
  #sum(data %>% filter(연도 == 2019) %>% select(고용보험납부액))
  return(df_employ)
}

# 함수 실행부분
# 대입 제외하고 주석처리
result_man <- result_total(birth_man , people2019_man , death_man)[[1]]
result_woman <- result_total(birth_woman , people2019_woman , death_woman)[[1]]
result_sum <- result_man + result_woman
resultIncome_m <- avgIncome(income_man, inflation, wage_increase_rate)
resultIncome_w <- avgIncome(income_woman, inflation, wage_increase_rate)
resultIncome_s <- avgIncome(income_sum, inflation, wage_increase_rate)
work_m <- work_total(employment_man, result_man)
work_w <- work_total(employment_woman, result_woman)
work_s <- work_m + work_w
#showInsur_all_age(Path)
#pension_sub(Path)
# showInsur_all(Path, earn , result_man, result_woman, pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , df_income_m , df_income_w, income_local_ratio)
showInsur_man <- showInsur_all(Path, earn , result_man, result_woman, pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , resultIncome_m , resultIncome_w, income_local_ratio)[[1]]
showInsur_woman <- showInsur_all(Path, earn , result_man, result_woman, pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , resultIncome_m , resultIncome_w , income_local_ratio)[[2]]
#old_receive(Path, result_sum, pensionReceive)
deathNum_m <- result_total(birth_man , people2019_man , death_man)[[2]]
deathNum_w <- result_total(birth_woman , people2019_woman , death_woman)[[2]]
disorder_man <- disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate,pensionReceive)[[1]]
disorder_woman <- disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate, pensionReceive)[[2]]
#linear_approx(disorder_man)+linear_approx(disorder_woman)
#disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w,disorder_bereaved_rate, pensionReceive)
df_old_receive <- old_receive(Path, result_sum, pensionReceive)
df_disorder_receive <- disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate, pensionReceive)[['전체장애연금수급액']]
sum_social_s <- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['총가입자']]
df_bereaved_receive <- bereaved_receive(Path, sum_social_s, total_receiver, pensionReceive)
#bereaved_receive(Path, sum_social_s, total_receiver, pensionReceive)
#pension(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive) # 국민연금 수입지출
Insurance_age <-showInsur_all_age(social_insurance , work_s, resultIncome_s)
#employ_insurance(Path, work_s, earn, Insurance_age, resultIncome_s)

