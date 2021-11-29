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
library(forecast)

# 색깔 정의 (ggplot 용)
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

# sapply 위한 Function들 정의
A<- function(x){as.Date(as.numeric(x), origin = "1899-12-30")} # Convert numeric to date
B<- function(x){as.numeric(x)} # convert to numeric value 
C<- function(x){as.character(as.Date(as.numeric(x), origin = "1899-12-30"))} # Convert date type to character
# 1) 시뮬레이션 기간 (t)와 시뮬레이션 횟수 (n)을 정의하고
#) 해당기간에 기대되는 증가율(수입, 지출), 수익률(국내채권, 해외채권, 국내주식, 해외주식)을 입력
#3) resampling. 방식으로 시뮬레이션
## Mean vector와 Cov vector를 주면 MVO를 만들어 주는 함수

select <- dplyr::select
options("scipen" = 100) 
#Path <-'C:/Users/Lenovo/Desktop/새 폴더/'
#Path <- '/Users/ko-eunseo/Documents/ALM system/'
#Path <- 'C:/R/'
category <- 2 # 고용보험 되나 테스트
usedata<-function(file){
  
  unzip(file,  exdir = getwd())
}

useInsurance <-function(Path, inf, wage, category){
  
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
  inflation <- as.numeric(inf)  #물가상승률
  wage_increase_rate <- as.numeric(wage)  #임금상승률
  income_local_ratio <- 0.5 # 사업장가입자 소득 대비 지역가입자 소득비율
  #인구수 관련 Input
  birth_man <-read_excel(paste(Path , "/birth_man.xlsx", sep = '')) #출생아 수
  people2019_man<-read_excel(paste(Path , "/people2019_man.xlsx", sep = '')) #2019년 인구수
  death_man <- read_excel(paste(Path , "/death_man.xlsx", sep = '')) #사망률
  birth_woman <-read_excel(paste(Path , "/birth_woman.xlsx", sep = ''))
  people2019_woman<-read_excel(paste(Path , "/people2019_woman.xlsx", sep = ''))
  death_woman <- read_excel(paste(Path , "/death_woman.xlsx", sep = ''))
  # Input Data Import 
  income_man <-read_excel(paste(Path , "/income_man.xlsx", sep = ''))
  income_woman <-read_excel(paste(Path , "/income_woman.xlsx", sep = ''))
  income_sum <-read_excel(paste(Path , "/income_sum.xlsx", sep = ''))
  
  employment_man <- read_excel(paste(Path , "/employment_man.xlsx", sep = '')) #고용률(15~65세)
  employment_woman <- read_excel(paste(Path , "/employment_woman.xlsx", sep = ''))
  social_insurance <-read_excel(paste(Path , "/social_insurance_age.xlsx", sep = ''))
  pension_compose <-read_excel(paste(Path , "/pension_compose.xlsx", sep = ''))
  old_pensionReceive_age <-read_excel(paste(Path , "/old_pensionReceive_age.xlsx", sep = ''))
  names(old_pensionReceive_age) = c('연령','합계','남자','여자')
  old_pensionReceive_age <- old_pensionReceive_age[-(1:2),]
  old_bereaved_rate <-read_excel(paste(Path , "/old_bereaved_rate.xlsx", sep = ''))
  disorder_pensionReceive_age <-read_excel(paste(Path , "/disorder_pensionReceive_age.xlsx", sep = ''))
  names(disorder_pensionReceive_age) = c('연령','합계','남자','여자')
  disorder_pensionReceive_age <- disorder_pensionReceive_age[-(1:2),]
  disorder_bereaved_rate <-read_excel(paste(Path , "/disorder_bereaved_rate.xlsx", sep = ''))
  total_receiver <- read_excel(paste(Path, "/total_receiver.xlsx",sep = ''))
  pensionReceive <- read_excel(paste(Path, "/pensionReceive.xlsx",sep = ''))
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
    #inflation <- 2
    #wage_increase_rate <- 2
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
    pension_compose <-read_excel(paste(Path , "/pension_compose.xlsx", sep = ''))
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
  
  #수입지출
  # 국민연금 수입지출
  pension <- function(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive){
    total_receiver <-read_excel(paste(Path , "/total_receiver.xlsx", sep = ''))
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
    total_receiver <- read_excel(paste(Path, "/total_receiver.xlsx",sep = ''))  # 국민연금 수급자 수 전망 -> 수급비율 구할 때 사용
    start_receive <-read_excel(paste(Path , "/start_receive.xlsx", sep = ''))
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
    disorder_rate<-read_excel(paste(Path , "/disorder_rate.xlsx", sep = ''))#장애발생률
    disorder_1519<-read_excel(paste(Path , "/disorder_1519.xlsx", sep = ''))#1519 장애수급자
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
    my_list <- list('남자장애수급자'= disorder_man, '여자장애수급자'= disorder_woman, '전체장애연금수급액'= df_disorder_mon )
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
  
  #고용보험
  employ_insurance <- function(Path, work_s, earn, Insurance_age, resultIncome_s){
    employee_by_scale <-read_excel(paste(Path , "/employee_by_scale.xlsx", sep = ''))
    resultIncome_s <- avgIncome(income_sum, inflation, 3.8) # 고용보험 재정전망에서 명목임금상승률 3.8%로 가정
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
        a <- as.numeric(work_s[j,i]* company_1 * income_earn * (employment_insur)+  work_s[j,i]* company_1_self * income_earn * (self_insur))
        b <- as.numeric(work_s[j,i] * company_2 * income_earn * (employment_insur)+ work_s[j,i] * company_2_self * income_earn * (self_insur))
        c <- as.numeric(work_s[j,i]* company_3 * income_earn * (employment_insur)+ work_s[j,i] * company_3_self * income_earn * (self_insur))
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
    return(df_employ)
  }
  unemployment <- function(Path, work_s){
    receiver <-colSums(work_s) * 0.035 * 0.366 * 3.192 # 경제활동인구 * 실업률 * 실업자 수 대비 구직급여 수급자 수 비율 * 조정계수(월평균->연평균)
    mon <-8385895020690/1527189
    receive_mon <- c(receiver[1]*mon)
    receive_mon
    for(i in 1:48){
      mon <- growth(mon,inflation,3.8)
      receive_mon <- c(receive_mon, receiver[1+i] * mon)
    }
    em <- employ_insurance(Path, work_s, earn, Insurance_age, resultIncome_s)
    em <- cbind(em, data.frame('고용보험지출' = receive_mon, '순수익' = em$고용보험료수입 - receive_mon))
    return(em)
  }
  #함수 실행부분
  result_man <- result_total(birth_man , people2019_man , death_man)[[1]]
  result_woman <- result_total(birth_woman , people2019_woman , death_woman)[[1]]
  result_sum <- result_man + result_woman
  resultIncome_m <- avgIncome(income_man, inflation, wage_increase_rate)
  resultIncome_w <- avgIncome(income_woman, inflation, wage_increase_rate)
  resultIncome_s <- avgIncome(income_sum, inflation, wage_increase_rate)
  work_m <- work_total(employment_man, result_man)
  work_w <- work_total(employment_woman, result_woman)
  work_s <- work_m + work_w
  showInsur_man <- showInsur_all(Path, earn , result_man, result_woman, pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , resultIncome_m , resultIncome_w, income_local_ratio)[[1]]
  showInsur_woman <- showInsur_all(Path, earn , result_man, result_woman, pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , resultIncome_m , resultIncome_w , income_local_ratio)[[2]]
  deathNum_m <- result_total(birth_man , people2019_man , death_man)[[2]]
  deathNum_w <- result_total(birth_woman , people2019_woman , death_woman)[[2]]
  disorder_man <- disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate,pensionReceive)[[1]]
  disorder_woman <- disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate, pensionReceive)[[2]]
  df_old_receive <- old_receive(Path, result_sum, pensionReceive)
  df_disorder_receive <- disorder_receive(Path, result_man, result_woman, pension_m, pension_w, deathNum_m, deathNum_w, disorder_bereaved_rate, pensionReceive)[['전체장애연금수급액']]
  sum_social_s <- pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)[['총가입자']]
  df_bereaved_receive <- bereaved_receive(Path, sum_social_s, total_receiver, pensionReceive)
  bereaved_receive(Path, sum_social_s, total_receiver, pensionReceive)
  Insurance_age <-showInsur_all_age(social_insurance , work_s, resultIncome_s)
  if(category == 1){ # 국민연금
    return(pension(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive))
  }
  else if(category ==2){ # 고용보험
    return(unemployment(Path, work_s))
  }
}

#적립기금추이 함수
Fund <- function(Path, inf, wage, category){
  if(as.numeric(category)==1){
    #수익률 테이블 생성
    year<-c('18-20','21-30','31-40','41-50','51-60','61-70')
    return18<-4.9/100  #18-20년도 수익률
    return21<-4.8/100  #21-30년도 수익률
    return31<-4.6/100  #31-40년도 수익률
    return41<-4.5/100  #41-50년도 수익률
    return51<-4.5/100  #51-60년도 수익률
    return61<-4.4/100  #61-70년도 수익률
    
    
    Fund_return<-c(return18,return21,return31,return41,return51,return61) #%화된거 변경
    Fund_return<-data.frame(year,Fund_return)
    colnames(Fund_return)=append('연도', '기금투자수익률')
    
    
    #18년말 적립금 테이블 생성
    asset_year<-c(2018)
    Fund_Asset<-c(638781) #2018년말 국민연금 적립금 (단위:십억원) 
    Fund_Asset<-data.frame(asset_year,Fund_Asset)
    colnames(Fund_Asset)=append('연도','적립기금')
    
    #투자수익 테이블 생성
    Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    
    #수입지출모듈
    pension<-useInsurance(Path, inf, wage, category) # 국민연금 수입지출
    
    income<-pension[,2]/1000000000 #단위 맞추기(십억원) 수입
    expenditure<-pension[,3]/1000000000 #단위 맞추기(십억원) 지출
    
    income<- data.frame('연도'= c(2019:2067),'국민연금수입'= income)
    expenditure<-data.frame('연도'= c(2019:2067),'국민연금지출'= expenditure)
    
    
    pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
    #투자자산 연도 생성
    for (i in 1:49){
      Fund_Asset[i+1,1]<-2018+i
    }
    #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
    
    i=1
    for(a in 1:49){
      if(Fund_Asset[a,1]%%10==0){
        i=i+1
      }
      Fund_Asset[a+1,2]<-(Fund_return[i,2]*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
      
      if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게
        break
      }
    }
    
    colnames(Fund_Asset)<-append("연도", "적립기금")
    
    #투자수익 확인
    #년도생성
    for (i in 1:50){
      Fund_Invest[i,1]<-2017+i
    }
    i=1
    for(a in 1:50){
      if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
        i=i+1
      }
      
      Fund_Invest[a+1,2]<-Fund_return[i,2]*(Fund_Asset[a,2])
      
      if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
        Fund_Invest[a+1,2]<-NA
        break
      }
    }
    
    colnames(Fund_Invest)<-append("연도", "투자수익")
    
    
    #총수입 계산
    Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
    for(i in 1:nrow(Fund_Invest)){
      Total_income[i,1]<-Fund_Invest[i,1]}
    for(i in 2:nrow(Fund_Invest)){
      Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
      if(is.na(Fund_Invest[i,2])){
        Total_income[i,2]<-pension[i-1,2]
      }
    }
    colnames(Total_income)<-append("연도", "총수입")
    
    #수지차 생성
    difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    for (i in 1:49){
      difference[i,1]<-2018+i
      difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
    colnames(difference)<-append("연도", "수지차")
    
    #year를 기준으로 합치기
    Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
    
    
    # max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
    # max_year<-as.numeric(max_asset_year[1])
    # max_asset<-as.numeric(max_asset_year[2])
    # 
    # p<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
    #   geom_line(color=3)+geom_point(size=1, color=4)+
    #   scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
    #   scale_y_continuous(breaks=seq(0, 2000000, 200000))+
    #   annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
    #   annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
    #   annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
    #   labs(x = '연도',
    #        y = '적립기금 (단위 : 십억원)',
    #        title = "적립기금추이")
  }
  else if(as.numeric(category)==2){
    year<-c('18-20','21-30','31-40','41-50','51-60','61-70')
    return18<-4.9/100  #18-20년도 수익률
    return21<-4.8/100  #21-30년도 수익률
    return31<-4.6/100  #31-40년도 수익률
    return41<-4.5/100  #41-50년도 수익률
    return51<-4.5/100  #51-60년도 수익률
    return61<-4.4/100  #61-70년도 수익률
    
    
    Fund_return<-c(return18,return21,return31,return41,return51,return61) #%화된거 변경
    Fund_return<-data.frame(year,Fund_return)
    colnames(Fund_return)=append('연도', '기금투자수익률')
    
    
    #18년말 적립금 테이블 생성
    asset_year<-c(2018)
    Fund_Asset<-c(97097) #2018년말 고용보험 적립금 (단위:억원) 
    Fund_Asset<-data.frame(asset_year,Fund_Asset)
    colnames(Fund_Asset)=append('연도','적립기금')
    
    #투자수익 테이블 생성
    Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    
    #수입지출모듈
    pension<-useInsurance(Path, inf, wage, category) # 고용보험 수입지출
    
    income<-pension[,2]/100000000 #단위 맞추기(억원) 수입
    expenditure<-pension[,3]/100000000 #단위 맞추기(억원) 지출
    
    income<- data.frame('연도'= c(2019:2067),'고용보험수입'= income)
    expenditure<-data.frame('연도'= c(2019:2067),'고용보험지출'= expenditure)
    
    
    pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
    #투자자산 연도 생성
    for (i in 1:49){
      Fund_Asset[i+1,1]<-2018+i
    }
    #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
    
    i=1
    for(a in 1:49){
      if(Fund_Asset[a,1]%%10==0){
        i=i+1
      }
      Fund_Asset[a+1,2]<-(Fund_return[i,2]*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
      
      if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게
        break
      }
    }
    
    colnames(Fund_Asset)<-append("연도", "적립기금")
    
    #투자수익 확인
    #년도생성
    for (i in 1:50){
      Fund_Invest[i,1]<-2017+i
    }
    i=1
    for(a in 1:50){
      if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
        i=i+1
      }
      
      Fund_Invest[a+1,2]<-Fund_return[i,2]*(Fund_Asset[a,2])
      
      if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
        Fund_Invest[a+1,2]<-NA
        break
      }
    }
    
    colnames(Fund_Invest)<-append("연도", "투자수익")
    
    
    #총수입 계산
    Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
    for(i in 1:nrow(Fund_Invest)){
      Total_income[i,1]<-Fund_Invest[i,1]}
    for(i in 2:nrow(Fund_Invest)){
      Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
      if(is.na(Fund_Invest[i,2])){
        Total_income[i,2]<-pension[i-1,2]
      }
    }
    colnames(Total_income)<-append("연도", "총수입")
    
    #수지차 생성
    difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    for (i in 1:49){
      difference[i,1]<-2018+i
      difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
    colnames(difference)<-append("연도", "수지차")
    
    #year를 기준으로 합치기
    Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
    
    
    # max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
    # max_year<-as.numeric(max_asset_year[1])
    # max_asset<-as.numeric(max_asset_year[2])
    # 
    # p<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
    #   geom_line(color=3)+geom_point(size=1, color=4)+
    #   scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
    #   scale_y_continuous(breaks=seq(0, 2000000, 200000))+
    #   annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
    #   annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
    #   annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
    #   labs(x = '연도',
    #        y = '적립기금 (단위 : 십억원)',
    #        title = "적립기금추이")
  }
  
  
  my_list<-list('재정수지전망'=Fund_Total)#,  'plot'=p)
  return(my_list)
}

#수입지출이 67년까지 밖에 없음. 
senario<-function(Path, inf, wage, sereturn, category){
  if(as.numeric(category)==1){
    year<-c('18-20','21-30','31-40','41-50','51-60','61-70')
    return18<-4.9/100  #18-20년도 수익률
    return21<-4.8/100  #21-30년도 수익률
    return31<-4.6/100  #31-40년도 수익률
    return41<-4.5/100  #41-50년도 수익률
    return51<-4.5/100  #51-60년도 수익률
    return61<-4.4/100  #61-70년도 수익률
    sereturn<-as.numeric(sereturn)
    
    
    Fund_return<-c(return18+(sereturn/100),return21+(sereturn/100),return31+(sereturn/100),return41+(sereturn/100),return51+(sereturn/100),return61+(sereturn/100)) 
    Fund_return<-data.frame(year,Fund_return)
    colnames(Fund_return)=append('연도', '기금투자수익률')
    
    #18년말 적립금 테이블 생성
    asset_year<-c(2018)
    Fund_Asset<-c(638781) #2018년말 국민연금 적립금 (단위:십억원) 
    Fund_Asset<-data.frame(asset_year,Fund_Asset)
    colnames(Fund_Asset)=append('연도','적립기금')
    
    #투자수익 테이블 생성
    Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    
    #수입지출모듈
    pension<-useInsurance(Path, inf, wage, category) # 국민연금 수입지출
    
    income<-pension[,2]/1000000000 #단위 맞추기(십억원) 수입
    expenditure<-pension[,3]/1000000000 #단위 맞추기(십억원) 지출
    
    income<- data.frame('연도'= c(2019:2067),'국민연금수입'= income)
    expenditure<-data.frame('연도'= c(2019:2067),'국민연금지출'= expenditure)
    
    
    pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
    #투자자산 연도 생성
    for (i in 1:49){
      Fund_Asset[i+1,1]<-2018+i
    }
    #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
    
    i=1
    for(a in 1:49){
      if(Fund_Asset[a,1]%%10==0){
        i=i+1
      }
      Fund_Asset[a+1,2]<-(Fund_return[i,2]*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
      if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게/기금이 전부 양수일 ???? 오류.
        break
      }
    }
    
    colnames(Fund_Asset)<-append("연도", "적립기금")
    
    #투자수익 확인
    #년도생성
    for (i in 1:50){
      Fund_Invest[i,1]<-2017+i
    }
    i=1
    for(a in 1:50){
      if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
        i=i+1
      }
      
      Fund_Invest[a+1,2]<-Fund_return[i,2]*(Fund_Asset[a,2])
      
      if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
        Fund_Invest[a+1,2]<-NA
        break
      }
    }
    
    colnames(Fund_Invest)<-append("연도", "투자수익")
    
    #총수입 계산
    Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
    for(i in 1:nrow(Fund_Invest)){
      Total_income[i,1]<-Fund_Invest[i,1]}
    for(i in 2:nrow(Fund_Invest)){
      Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
      if(is.na(Fund_Invest[i,2])){
        Total_income[i,2]<-pension[i-1,2]
      }
    }
    colnames(Total_income)<-append("연도", "총수입")
    
    #수지차 생성
    difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    for (i in 1:49){
      difference[i,1]<-2018+i
      difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
    colnames(difference)<-append("연도", "수지차")
    
    #year를 기준으로 합치기
    Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
    
    
    # max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
    # max_year<-as.numeric(max_asset_year[1])
    # max_asset<-as.numeric(max_asset_year[2])
    # 
    # p2<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
    #   geom_line(color=3)+geom_point(size=1, color=4)+
    #   scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
    #   scale_y_continuous(breaks=seq(0, 2000000, 200000))+
    #   annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
    #   annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
    #   annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
    #   labs(x = '연도',
    #        y = '적립기금 (단위 : 십억원)',
    #        title = "적립기금추이 시나리오")
  }
  else if(as.numeric(category)==2){
    year<-c('18-20','21-30','31-40','41-50','51-60','61-70')
    return18<-4.9/100  #18-20년도 수익률
    return21<-4.8/100  #21-30년도 수익률
    return31<-4.6/100  #31-40년도 수익률
    return41<-4.5/100  #41-50년도 수익률
    return51<-4.5/100  #51-60년도 수익률
    return61<-4.4/100  #61-70년도 수익률
    sereturn<-as.numeric(sereturn)
    
    
    Fund_return<-c(return18+(sereturn/100),return21+(sereturn/100),return31+(sereturn/100),return41+(sereturn/100),return51+(sereturn/100),return61+(sereturn/100)) 
    Fund_return<-data.frame(year,Fund_return)
    colnames(Fund_return)=append('연도', '기금투자수익률')
    
    #18년말 적립금 테이블 생성
    asset_year<-c(2018)
    Fund_Asset<-c(97097) #2018년말 고용보험 적립금 (단위:억원) 
    Fund_Asset<-data.frame(asset_year,Fund_Asset)
    colnames(Fund_Asset)=append('연도','적립기금')
    
    #투자수익 테이블 생성
    Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    
    #수입지출모듈
    pension<-useInsurance(Path, inf, wage, category) # 고용보험 수입지출
    
    income<-pension[,2]/100000000 #단위 맞추기(억원) 수입
    expenditure<-pension[,3]/100000000 #단위 맞추기(억원) 지출
    
    income<- data.frame('연도'= c(2019:2067),'고용보험수입'= income)
    expenditure<-data.frame('연도'= c(2019:2067),'고용보험지출'= expenditure)
    
    
    pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
    #투자자산 연도 생성
    for (i in 1:49){
      Fund_Asset[i+1,1]<-2018+i
    }
    #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
    
    i=1
    for(a in 1:49){
      if(Fund_Asset[a,1]%%10==0){
        i=i+1
      }
      Fund_Asset[a+1,2]<-(Fund_return[i,2]*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
      if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게/기금이 전부 양수일 ???? 오류.
        break
      }
    }
    
    colnames(Fund_Asset)<-append("연도", "적립기금")
    
    #투자수익 확인
    #년도생성
    for (i in 1:50){
      Fund_Invest[i,1]<-2017+i
    }
    i=1
    for(a in 1:50){
      if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
        i=i+1
      }
      
      Fund_Invest[a+1,2]<-Fund_return[i,2]*(Fund_Asset[a,2])
      
      if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
        Fund_Invest[a+1,2]<-NA
        break
      }
    }
    
    colnames(Fund_Invest)<-append("연도", "투자수익")
    
    #총수입 계산
    Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
    for(i in 1:nrow(Fund_Invest)){
      Total_income[i,1]<-Fund_Invest[i,1]}
    for(i in 2:nrow(Fund_Invest)){
      Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
      if(is.na(Fund_Invest[i,2])){
        Total_income[i,2]<-pension[i-1,2]
      }
    }
    colnames(Total_income)<-append("연도", "총수입")
    
    #수지차 생성
    difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    for (i in 1:49){
      difference[i,1]<-2018+i
      difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
    colnames(difference)<-append("연도", "수지차")
    
    #year를 기준으로 합치기
    Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
    
    
    # max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
    # max_year<-as.numeric(max_asset_year[1])
    # max_asset<-as.numeric(max_asset_year[2])
    # 
    # p2<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
    #   geom_line(color=3)+geom_point(size=1, color=4)+
    #   scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
    #   scale_y_continuous(breaks=seq(0, 2000000, 200000))+
    #   annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
    #   annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
    #   annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
    #   annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
    #   labs(x = '연도',
    #        y = '적립기금 (단위 : 십억원)',
    #        title = "적립기금추이 시나리오")
  }
  my_list<-list('재정수지전망 시나리오'=Fund_Total) # 'plot'=p2)
  return(my_list)
  
}

#기본가정과 시나리오 비교하여 볼 수 있는 테이블과 그래프
senario_plot<-function(Path, inf, wage, sereturn, category){
  sereturn<-as.numeric(sereturn)
  category<-as.numeric(category)
  
  origin<-Fund(Path, inf, wage, category)
  senario_<-senario(Path, inf, wage, sereturn, category)
  
  origin_Total<-origin$재정수지전망
  senario_Total<-senario_$`재정수지전망 시나리오`
  
  Total<-rbind(origin_Total,senario_Total)
  
  group<-data.frame(matrix(nrow=100,ncol=1))
  
  #그래프를 만들어 주기 위해 그룹을 나눠줌
  for(i in 1:100){
    if(i<=50){
      group[i,]='기본가정'
    }
    else{
      group[i,]='시나리오'
    }
  }
  
  colnames(group)<-c("구분")
  
  Total<-cbind(Total,group)
  
  max_asset_year <- Total[which.max(Total$적립기금),]
  #max_year<-as.numeric(max_asset_year[1])
  max_asset<-as.numeric(max_asset_year[2])
  
  if(as.numeric(category)==1){
    p<-ggplot(data=Total,aes(x=연도, y=적립기금, group=구분, color=구분))+
      geom_line()+#geom_point(size=1, color=4)+
      scale_x_continuous(breaks=seq(min(Total$연도), max(Total$연도), 5)) +
      scale_y_continuous(breaks=seq(0, max(max_asset), 200000))+
      geom_point(data=Total, aes(x=연도,y=적립기금,  group = 구분,color = 구분))+
      labs(x = '연도',
           y = '적립기금 (단위 : 십억원)',
           title = "국민연금 적립기금추이")
  }
  else if(as.numeric(category)==2){
    p<-ggplot(data=Total,aes(x=연도, y=적립기금, group=구분, color=구분))+
      geom_line()+#geom_point(size=1, color=4)+
      scale_x_continuous(breaks=seq(min(Total$연도), max(Total$연도), 5)) +
      scale_y_continuous(breaks=seq(0, max(max_asset), 1000000))+
      geom_point(data=Total, aes(x=연도,y=적립기금,  group = 구분,color = 구분))+
      labs(x = '연도',
           y = '적립기금 (단위 : 억원)',
           title = "고용보험 적립기금추이")
  }
  #테이블 만들기(수지적자와 기금소진이 몇년도에 일어나는지)
  for(i in 1:50){
    origin_Total[is.na(origin_Total)] <- 0
    if(origin_Total[50,7]>0){
      year_different_minus<-'2067년 이후'
      
    }
    
    else if(origin_Total[i,7]<0){
      year_different_minus<-as.numeric(origin_Total[i,1])
      break
    }
  }
  for(i in 1:50){
    if(origin_Total[50,2]>0){
      year_asset_minus<-'2067년 이후'
      
    }
    else if(origin_Total[i,2]<0){
      year_asset_minus<-as.numeric(origin_Total[i,1])
      break
    }
  }
  
  for(i in 1:50){
    senario_Total[is.na(senario_Total)] <- 0
    
    if(senario_Total[50,2]>0){
      year_asset_minus_senario<-'2067년 이후'
    }
    
    else if(senario_Total[i,2]<0){
      year_asset_minus_senario<-as.numeric(senario_Total[i,1])
      break
    }
  }
  for(i in 1:50){
    if(senario_Total[50,7]>0){
      year_different_minus_senario<-'2067년 이후'
    }
    
    else if(senario_Total[i,7]<0){
      year_different_minus_senario<-as.numeric(senario_Total[i,1])
      break
    }
  }
  table<-data.frame(matrix(ncol=2, nrow=2))
  table[1,1]=year_different_minus
  table[1,2]=year_asset_minus
  table[2,1]=year_different_minus_senario
  table[2,2]=year_asset_minus_senario
  colnames(table)<-append("수지적자(년)","기금소진(년)")
  rownames(table)<-append("기본가정", "시나리오")
  
  my_list<-list('table'=table,  'plot'=p, 'origin'=origin$재정수지전망, 'senario'=senario_$`재정수지전망 시나리오`)
  return(my_list)
}

MVOmaker_year <- function(MVO_num, RF, mean_ret , cov_mat,SeedingNum){
  suppressWarnings({
    
    set.seed(SeedingNum)
    ItemName<- colnames(cov_mat)
    
    wts <- runif(n = length(ItemName))
    wts <- wts/sum(wts)
    
    #PortFolio Return
    port_returns <- sum(wts * mean_ret)# * 252 # Mean return은 Daily Scale 로 오기 때문에 252 곱하여 Annualize
    port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts)) # Cov Matrix는 Annual scale로 옴
    
    sharpe_ratio <- (port_returns-RF/100)/port_risk
    
    # Creating Empty Vector
    num_port <- MVO_num
    
    all_wts <- matrix(nrow = num_port,ncol = length(ItemName))
    port_returns <- vector('numeric', length = num_port)
    port_risk <- vector('numeric', length = num_port)
    sharpe_ratio <- vector('numeric', length = num_port)
    
    # Simulating Process
    set.seed(SeedingNum)
    for (i in seq_along(port_returns)) {
      
      
      wts <- runif(n = length(ItemName))
      wts <- wts/sum(wts)
      
      
      all_wts[i,] <- wts
      
      # Portfolio returns
      
      port_ret <- sum(wts * mean_ret) #* 252
      
      # Storing Portfolio Returns values
      port_returns[i] <- port_ret
      
      
      # Creating and storing portfolio risk
      port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
      port_risk[i] <- port_sd
      
      # Creating and storing Portfolio Sharpe Ratios
      # Assuming 0% Risk free rate
      
      sr <- (port_ret-RF/100)/port_sd
      sharpe_ratio[i] <- sr
      
    }
    
    # Storing the values in the table
    portfolio_values <- tibble(Return = port_returns,
                               Risk = port_risk,
                               SharpeRatio = sharpe_ratio)
    
    
    # Converting matrix to a tibble and changing column names
    all_wts <- tk_tbl(all_wts)
    
    colnames(all_wts) <- colnames(cov_mat)
    
    # Combing all the values together
    portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
    
    my_list <- list("pfvalue" = portfolio_values, "mean" = mean_ret, "cov" = cov_mat)
  })
  return(my_list)
}



hedgedbenchdata<-function(Path, category){
  benchMark <-read_excel(paste(Path , "/benchMark.xlsx", sep = '')) # 수익률 벤치마크 데이터
  df_benchMark <- benchMark[grep('12-(26|28)', benchMark$대분류),]    # 선택한 날짜 포함한 행
  df_benchMark_11 <- benchMark[grep('11-(26|28)', benchMark$대분류),]
  df_benchMark <- df_benchMark[-c(4,6,8,13,15,20,22,24),] #중복 연도 제거
  df_benchMark_11 <- df_benchMark_11[-c(2,4,9,11,13,18,20),]
  df_benchMark <- df_benchMark[-(17:nrow(df_benchMark)),]  #2004년 이후로 제거
  df_benchMark_11 <- df_benchMark_11[-(17:nrow(df_benchMark_11)),]
  df_Exchange_rate <- df_benchMark[,15]
  df_Exchange_rate_11 <- df_benchMark_11[,15]
  df_benchMark <- df_benchMark[,c(1,2,9,12,14)]
  df_benchMark_11 <- df_benchMark_11[,c(1,2,9,12,14)]
  
  names(df_benchMark) <- c('연도','국내채권','국내주식','해외채권','해외주식')
  names(df_benchMark_11) <- c('연도','국내채권','국내주식','해외채권','해외주식')
  names(df_Exchange_rate)<-c("환율")
  names(df_Exchange_rate_11)<-c("환율")
  df_benchMark$국내채권 <- as.numeric(df_benchMark$국내채권)
  df_benchMark$국내주식 <- as.numeric(df_benchMark$국내주식)
  df_benchMark$해외채권 <- as.numeric(df_benchMark$해외채권)
  df_benchMark$해외주식 <- as.numeric(df_benchMark$해외주식)
  df_benchMark_11$국내채권 <- as.numeric(df_benchMark_11$국내채권)
  df_benchMark_11$국내주식 <- as.numeric(df_benchMark_11$국내주식)
  df_benchMark_11$해외채권 <- as.numeric(df_benchMark_11$해외채권)
  df_benchMark_11$해외주식 <- as.numeric(df_benchMark_11$해외주식)
  df_Exchange_rate <- as.double(df_Exchange_rate$환율)
  df_Exchange_rate_11 <- as.double(df_Exchange_rate_11$환율)
  
  df_benchMark <- df_benchMark[c(nrow(df_benchMark):1),] #2005년부터 시작하게 행순서 뒤집기
  df_benchMark_11 <- df_benchMark_11[c(nrow(df_benchMark_11):1),]
  df_Exchange_rate<-df_Exchange_rate[c(length(df_Exchange_rate):1)]
  df_Exchange_rate_11<-df_Exchange_rate_11[c(length(df_Exchange_rate_11):1)]
  for(i in 1:14){
    if(i==1){
      a <- c(as.numeric(df_benchMark[1+i,2]/ df_benchMark_11[i+1,2]))
      b <- c(as.numeric(df_benchMark[1+i,3]/ df_benchMark_11[i+1,3]))
      c <- c(as.numeric(df_benchMark[1+i,4]/ df_benchMark_11[i+1,4]))
      d <- c(as.numeric(df_benchMark[1+i,5]/ df_benchMark_11[i+1,5]))
      exchange<-c(as.numeric(df_Exchange_rate[1+i]/df_Exchange_rate_11[1+i]))
      ex_test<-c(as.numeric((df_Exchange_rate[1+i]-df_Exchange_rate_11[1+i])/df_Exchange_rate_11[1+i]))
    }
    else{
      a <- c(a, as.numeric(df_benchMark[1+i,2]/ df_benchMark_11[i+1,2]))
      b <- c(b, as.numeric(df_benchMark[1+i,3]/ df_benchMark_11[i+1,3]))
      c <- c(c, as.numeric(df_benchMark[1+i,4]/ df_benchMark_11[i+1,4]))
      d <- c(d, as.numeric(df_benchMark[1+i,5]/ df_benchMark_11[i+1,5]))
      exchange<-c(exchange,as.numeric(df_Exchange_rate[1+i]/df_Exchange_rate_11[1+i]))
      ex_test<-c(ex_test,as.numeric((df_Exchange_rate[1+i]-df_Exchange_rate_11[1+i])/df_Exchange_rate_11[1+i]))
      
    }
  }
  yoy <- data.frame('국내채권'= a,'국내주식'= b,'해외채권'= c,'해외주식'= d)
  exchange<-data.frame('환율'=exchange)
  # 상관관계분석
  if(as.numeric(category)==1){
    past_pension <-read_excel(paste(Path , "/past_pension.xls", sep = '')) # 과거국민연금 수입지출(단위-억원)
    #correlation <- function(past_pension, benchMark){
    #연도별 데이터 전처리
    year <- c(2006:2019)
    for(i in 1:14){
      if(i==1){
        income <- c(as.numeric(gsub('\\D','',past_pension[1, 2+i]))/ as.numeric(gsub('\\D','',past_pension[1, 1+i])))
        cost <- c(as.numeric(gsub('\\D','',past_pension[5, 2+i]))/ as.numeric(gsub('\\D','',past_pension[5, 1+i])))
        realcost <- c((as.numeric(gsub('\\D','',past_pension[5, 2+i]))- as.numeric(gsub('\\D','',past_pension[1, 2+i])))/ (as.numeric(gsub('\\D','',past_pension[5, 1+i])) - as.numeric(gsub('\\D','',past_pension[1, 1+i]))))
      }
      else{
        income <- c(income, as.numeric(gsub('\\D','',past_pension[1, 2+i]))/ as.numeric(gsub('\\D','',past_pension[1, 1+i])))
        cost <- c(cost, as.numeric(gsub('\\D','',past_pension[5, 2+i]))/ as.numeric(gsub('\\D','',past_pension[5, 1+i])))
        realcost <- c(realcost, (as.numeric(gsub('\\D','',past_pension[5, 2+i]))- as.numeric(gsub('\\D','',past_pension[1, 2+i])))/ (as.numeric(gsub('\\D','',past_pension[5, 1+i])) - as.numeric(gsub('\\D','',past_pension[1, 1+i]))))
      }
    }
    df_past_pension <- data.frame('연도'=year, '수입'= income, '지출'= cost)
    data <- cbind(df_past_pension[,2:ncol(df_past_pension)], yoy)
    income <- ts(income, start = c(2006,1)) #시계열 데이터 변환
    cost <- ts(cost, start = c(2006,1))
    yoy <- ts(yoy, start(2006,1))
    log_income <- log(income) #로그변환
    log_cost <- log(cost)
    diff_income <- diff(log_income) #차분
    diff_cost <- diff(log_cost) #차분
  }
  else if(as.numeric(category)==2){
    past_employment <-read_excel(paste(Path , "/past_employment.xls", sep = '')) # 과거고용보험 수입지출(단위-억원)
    #correlation <- function(past_employment, benchMark){
    year <- c(2006:2019)
    #연도별 데이터 전처리
    for(i in 1:14){
      if(i==1){
        income <- c(as.numeric(gsub('\\D','',past_employment[7, 2+i]))/ as.numeric(gsub('\\D','',past_employment[7, 1+i])))
        cost <- c(as.numeric(gsub('\\D','',past_employment[1, 2+i]))/ as.numeric(gsub('\\D','',past_employment[1, 1+i])))
        #realcost <- c((as.numeric(gsub('\\D','',past_employment[5, 2+i]))- as.numeric(gsub('\\D','',past_employment[1, 2+i])))/ (as.numeric(gsub('\\D','',past_employment[5, 1+i])) - as.numeric(gsub('\\D','',past_employment[1, 1+i]))))
      }
      else{
        income <- c(income, as.numeric(gsub('\\D','',past_employment[7, 2+i]))/ as.numeric(gsub('\\D','',past_employment[7, 1+i])))
        cost <- c(cost, as.numeric(gsub('\\D','',past_employment[1, 2+i]))/ as.numeric(gsub('\\D','',past_employment[1, 1+i])))
        #realcost <- c(realcost, (as.numeric(gsub('\\D','',past_employment[5, 2+i]))- as.numeric(gsub('\\D','',past_employment[1, 2+i])))/ (as.numeric(gsub('\\D','',past_employment[5, 1+i])) - as.numeric(gsub('\\D','',past_employment[1, 1+i]))))
      }
    }
    df_past_emp <- data.frame('연도'=year, '수입'= income, '지출'= cost)
    data <- cbind(df_past_emp[,2:ncol(df_past_emp)], yoy)
    income <- ts(income, start = c(2006,1)) #시계열 데이터 변환
    cost <- ts(cost, start = c(2006,1))
    yoy <- ts(yoy, start(2006,1))
    log_income <- log(income) #로그변환
    log_cost <- log(cost)
    log_yoy <- log(yoy)
    diff_income <- diff(log_income) #차분
    diff_cost <- diff(log_cost) #차분
  }
  log_yoy <- log(yoy)
  log_exchange <-log(exchange)
  #환율변화율 어떻게 할지.
  bond1<-data.frame()
  stock1<-data.frame()
  bond<-data.frame()
  stock<-data.frame()
  for(i in seq(from=0, to=100, by=10)){
    for(j in 1:nrow(log_yoy)){
      if(i==0){
        bond1[j,1]<-(1+log_yoy[,3][j])*(1+log_exchange[j,]*i/100)-1
        stock1[j,1]<-(1+log_yoy[,4][j])*(1+log_exchange[j,]*i/100)-1 #해외주식
      }
      else{
        
        bond[j,i/10]<-(1+log_yoy[,3][j])*(1+log_exchange[j,]*i/100)-1
        stock[j,i/10]<-(1+log_yoy[,4][j])*(1+log_exchange[j,]*i/100)-1 #해외주식
        
      }
    }
  }
  bond<-cbind(bond1,bond)
  stock<-cbind(stock1,stock)
  new_data<-list()
  for (i in 1:11){
    new_data[[i]]<-cbind(data.frame('수입'= diff_income,'지출'=diff_cost), log_yoy[2:nrow(log_yoy),1:2],"해외채권"=bond[2:nrow(log_yoy),i],"해외주식"=stock[2:nrow(log_yoy),i])
    
  }
  return(new_data)
  
}

simulation_con_hedged <- function(Path, ben_a, ben_b, ben_c, ben_d,income_inc, cost_inc, n, category){
  income_inc<-as.numeric(income_inc)
  cost_inc<-as.numeric(cost_inc)
  ben_a<-as.numeric(ben_a)
  ben_b<-as.numeric(ben_b)
  ben_c<-as.numeric(ben_c)
  ben_d<-as.numeric(ben_d)
  t<-as.numeric(5) #향후 5년
  n<-as.numeric(n)
  category<-as.numeric(category)
  
  new_data<-hedgedbenchdata(Path, category)
  hedged<-list()
  for(j in 1:11){
    cov_s <- cov(new_data[[j]])
    mean_s <- c(income_inc/100, cost_inc/100, ben_a/100, ben_b/100, ben_c/100, ben_d/100)
    df_income <- data.frame('연도' = c(1:t), '')
    mydata <- list()
    for(i in 1:n){
      set.seed(i)
      mydata[[i]] <- as.data.frame(mvrnorm(t, mean_s, cov_s))
      names(mydata[[i]]) <- c('income', 'expenditure', 'domestic.bond', 'domestic.stock', 'foreign.bond', 'foreign.stock')
      mydata[[i]] <- cbind(mydata[[i]], data.frame('year'= c(1:t)))
    }
    hedged<-append(hedged,mydata)
  }
  bind0<-hedged[1:n]
  bind1<-hedged[(n+1):(2*n)]
  bind2<-hedged[(2*n+1):(3*n)]
  bind3<-hedged[(3*n+1):(4*n)]
  bind4<-hedged[(4*n+1):(5*n)]
  bind5<-hedged[(5*n+1):(6*n)]
  bind6<-hedged[(6*n+1):(7*n)]
  bind7<-hedged[(7*n+1):(8*n)]
  bind8<-hedged[(8*n+1):(9*n)]
  bind9<-hedged[(9*n+1):(10*n)]
  bind10<-hedged[(10*n+1):(11*n)]
  
  my_list<-list(bind0,bind1,bind2,bind3,bind4,bind5,bind6,bind7,bind8,bind9,bind10)
  return(my_list)
}


random_simul_hedged<-function(Path, ben_a, ben_b, ben_c, ben_d,income_inc, cost_inc, n, MVO_num, RF, hedge_num, inf, wage, category){
  yy<-as.numeric(2020) #시작연도
  t<-as.numeric(5) #향후 몇년? 일단 5년
  ben_a<-as.numeric(ben_a)
  ben_b<-as.numeric(ben_b)
  ben_c<-as.numeric(ben_c)
  ben_d<-as.numeric(ben_d)
  income_inc<-as.numeric(income_inc)
  cost_inc<-as.numeric(cost_inc)
  n<-as.numeric(n) #시뮬레이션 횟수
  MVO_num <- as.numeric(MVO_num)
  RF<-as.numeric(RF)
  hedge_num<-as.numeric(hedge_num)
  inflation <- as.numeric(inf)  #물가상승률
  wage_increase_rate <- as.numeric(wage)  #임금상승률
  category<-as.numeric(category)
  
  mydata<-simulation_con_hedged(Path,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, n, category)
  fund_total <- Fund(Path, inf, wage, category)[['재정수지전망']]
  
  #19년부터 5년 가정
  year<-c((yy+1):(yy+t))
  if(category==1){
    cost<-fund_total%>%filter(연도>(yy-1) & 연도<(yy+t))%>% select(국민연금지출)
    income<-fund_total%>%filter(연도>(yy-1) & 연도<(yy+t)) %>% select(국민연금수입)
    
    asset<-as.numeric(785408)
  }
  
  else if(category==2){
    asset<-as.numeric(70227) #단위:억원
    cost<-fund_total%>%filter(연도>(yy-1) & 연도<(yy+t))%>% select(고용보험지출)
    income<-fund_total%>%filter(연도>(yy-1) & 연도<(yy+t)) %>% select(고용보험수입)
    
  }
  
  asset_simul<-data.frame(matrix(nrow=t+1,ncol=MVO_num))
  
  ratio_simul<-data.frame(matrix(nrow=t,ncol=n))
  asset_simul[1,]<-asset
  
  
  
  ratio_sd<-data.frame()
  weight<-data.frame()
  weight_select<-data.frame()
  p<-list()
  real_hedge_num<-as.numeric((hedge_num/10)+1)
  
  for ( h in 1:11){
    mydata_bind<-mydata[[h]]
    
    for(i in 1:n) {
      
      MeanRet <- colMeans(data.frame(mydata_bind[[i]][3:6]))
      COV_mat <- cov(data.frame(mydata_bind[[i]][3:6]))
      SeedingNum<-i
      Result<- MVOmaker_year(MVO_num,RF,MeanRet , COV_mat,SeedingNum)
      
      
      for(m in 1:MVO_num){
        for (j in 1:t){
          
          Return1<-Result$pfvalue$Return[m] #mvo_num이 들어가야함
          
          asset_simul[j+1,m]<-asset_simul[j,m]+income[j,]-cost[j,]+asset_simul[j,m]*(Return1)
          ratio_simul[j,m]<-cost[j,]/asset_simul[j,m]
          ratio_sd[m,1]<-sd(ratio_simul[,m])
          num<-which.min(as.matrix(ratio_sd))
        }
      }
      
      weight<-rbind(weight,Result$pfvalue[num,])
      if(h==real_hedge_num){
        weight_select<-rbind(weight_select,Result$pfvalue[num,])}
    }
    mean_weight <- data.frame(describe(weight))[,c("mean","min","max","sd")]
    
    meanOutput <- t(data.frame(mean_weight)['mean'])
    
    meanper <- apply(data.frame(t(meanOutput[,1:(ncol(meanOutput)-1)])),2,convert2ratio)
    
    meanper<-data.frame(t(meanper))
    
    SharpeCol <- data.frame(meanOutput[,ncol(meanOutput)])
    
    colnames(SharpeCol) <- c("Sharpe Ratio")
    
    merr <-cbind(meanper,SharpeCol)
    rownames(merr)<-c("min variance mean weight")
    
    p<-append(p,list(merr))
    select_p<-p[real_hedge_num]
  }
  my_list<-list('0% 헤지 자산배분안 평균'=p[1],
                '10% 헤지 자산배분안 평균'=p[2],
                '20% 헤지 자산배분안 평균'=p[3],
                '30% 헤지 자산배분안 평균'=p[4],
                '40% 헤지 자산배분안 평균'=p[5],
                '50% 헤지 자산배분안 평균'=p[6],
                '60% 헤지 자산배분안 평균'=p[7],
                '70% 헤지 자산배분안 평균'=p[8],
                '80% 헤지 자산배분안 평균'=p[9],
                '90% 헤지 자산배분안 평균'=p[10],
                '100% 헤지 자산배분안 평균'=p[11] , 'weight'=weight_select, 'select'=select_p)#, 'value'=q)
  return(my_list)
  
}


#' @param mean Mean return of timeseries data
#' @param sd Standard deviation value of timeseries data
#' @param VARConfidence Alpha value for confidence level신뢰구간
#' @param ManageTime Number of days to forecast VaR 관리기간
#' @param n Number of iterations 비모수일 때
#' @param VarUse 모수, 비모수 선택
#' mean -> port_return, sd-> port_risk 일간으로 
#' # 관리기간이 일간으로 입력받기 때문에
#' 
parametric <- function(Path, VarUse,mean, sd, VarConfidence, ManageTime,  Exposure) {
  VarUse<-as.numeric(VarUse)
  Exposure<-as.numeric(Exposure)  #익스포저
  VarConfidence<-as.numeric(VarConfidence)  #신뢰구간
  ManageTime<-as.numeric(ManageTime)  #관리기간
  var_n<-as.numeric(1000000) #Number of iterations -> default=1000000
  
  #pf<-random_simul_zero(yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,RF)
  #mean<-colMeans(pf$weight[5])
  #sd<-colMeans(pf$weight[6])
  if(as.numeric(VarUse) == 1){ # 모수적 방법 사용
    
    VARpercent<-qnorm(1-VarConfidence/100,mean*100,sd*100)*sd*sqrt(ManageTime)
    VARpercent<-abs(VARpercent)
    VARpercent_str<- paste(as.character(round(VARpercent*100,3)),"%")
    
    Annual.Return<-seq(from=mean*100-sd*500,to=mean*100+sd*500,length=200)
    
    Prob<-dnorm(Annual.Return,mean=mean*100,sd=sd*100)
    
    ForText <- max(Prob)*1/2
    ForProb <- max(Prob)*0.8/2
    
    p<- ggplot(data.frame(x=c(mean*100-sd*500,mean*100+sd*500)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean*100, sd=sd*100) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean*100, sd=sd*100), xlim=c(mean*100-sd*500, qnorm(1-VarConfidence/100, mean*100, sd*100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100, sd*100)-0.5, y=ForText), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100, sd*100)-0.5, y=ForProb), label=VARpercent_str)+
      ggtitle(paste("모수적 방법 VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100, mean*100, sd*100), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk(%)", y="Prob")
    
  }else if(as.numeric(VarUse) == 2) # 비모수적 방법 사용(몬테카를로)
  {
    VARpercent<-(mean*100 + sd*100*rnorm(var_n))*sqrt(ManageTime)
    
    VARpercent_use<-quantile(VARpercent, (1-VarConfidence/100))
    VARpercent<-abs(VARpercent_use)/100
    VARpercent_<-abs(VARpercent_use)
    VARpercent_str<- paste(as.character(round(VARpercent_,3)),"%")
    
    X<-seq(from=mean*100-sd*500,to=mean*100+sd*500,length=200)
    
    Prob<-dnorm(X,mean=mean*100,sd=sd*100)
    ForText <- max(Prob)*1/2
    ForProb <- max(Prob)*0.8/2
    #gvar=paste(as.character(round(VARpercent,digits=4)))
    #Pgvar=paste(as.character(round(PrinVAR,digits=3)))
    
    p<- ggplot(data.frame(x=c(mean*100-sd*500,mean*100+sd*500)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean*100, sd=sd*100) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean*100, sd=sd*100), xlim=c(mean*100-sd*500, qnorm(1-VarConfidence/100, mean=mean*100, sd=sd*100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean=mean*100, sd=sd*100)-sd*100, y=ForText), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean=mean*100, sd=sd*100)-sd*100, y=ForProb), label=VARpercent_str)+
      ggtitle(paste("비모수적 방법 VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100,mean=mean*100, sd=sd*100), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk(%)", y="Prob")
  }
  
  #어떻게 구현할 수 있을지 생각...
  else if(as.numeric(VarUse) == 3) # EWMA 방법 사용
  {
    
    lambda=0.94
    ewma.loop <- function(rets, lambda) {
      n <- length(rets)+1
      sig.s <- rep(0, n)
      for (i in 2:n) {
        sig.s[i] <- (sig.s[i-1])*lambda + (rets[i-1]^2)*(1 - lambda)
      }
      return(sqrt(tail(sig.s, n-1)))
    }
    
    #EWMA 표준편차 구하기
    data<-hedgedbenchdata(Path, category)[[1]]
    
    mean_ret<-(data[3:6])
    
    
    #자산군이 5개이므로
    rets1 <-  as.numeric(mean_ret[,1])
    rets1<-ewma.loop((rets1), lambda)
    
    rets2 <-  as.numeric(mean_ret[,2])
    rets2<-ewma.loop(rets2, lambda)
    
    rets3 <-  as.numeric(mean_ret[,3])
    rets3<-ewma.loop(rets3, lambda)
    
    rets4 <-  as.numeric(mean_ret[,4])
    rets4<-ewma.loop(rets4, lambda)
    
    
    ewmasd<-data.frame(rets1,rets2,rets3,rets4)
    
    ewmasd<-ewmasd[nrow(ewmasd),]
    
    
    
    # 3) Covariance Matrix
    #관리기간을 일별로 입력해주니까 연간으로 바꿔줄 필요가 없음.
    #cov_mat <- cor_ret *as.numeric(as.matrix(ewmasd) %*% t(as.matrix(ewmasd)))# *252
    cov_mat<-cov(data[3:6])
    
    set.seed(1)
    
    wts <- runif(n = length(ewmasd))
    wts <- wts/sum(wts)
    
    
    sd <- as.numeric(sqrt(t(wts) %*% (as.matrix(cov_mat) %*% wts)))
    
    VARpercent<-qnorm(1-VarConfidence/100,mean*100,sd*100)*sd*sqrt(ManageTime)
    VARpercent<-abs(VARpercent)
    VARpercent_str<- paste(as.character(round(VARpercent*100,3)),"%")
    
    Annual.Return<-seq(from=mean*100-sd*500,to=mean*100+sd*500,length=200)
    
    Prob<-dnorm(Annual.Return,mean=mean*100,sd=sd*100)
    
    ForText <- max(Prob)*1/2
    ForProb <- max(Prob)*0.8/2
    
    p<- ggplot(data.frame(x=c(mean*100-sd*500,mean*100+sd*500)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean*100, sd=sd*100) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean*100, sd=sd*100), xlim=c(mean*100-sd*500, qnorm(1-VarConfidence/100, mean*100, sd*100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100,sd*100)-0.1, y=ForText), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100,sd*100)-0.1, y=ForProb), label=VARpercent_str)+
      ggtitle(paste("EWMA VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100, mean*100, sd*100), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk", y="Prob")
  }
  
  
  PVAR <- VARpercent*Exposure #금액VAR
  PVAR <- round(PVAR, digits=2)
  TableVAR <- as.data.frame(c(VARpercent_str, PVAR ))
  rownames(TableVAR) <- c( "%VaR" , "금액VAR")
  colnames(TableVAR) <- c('VaR Analysis Result')
  
  my_list<-list("VaRTable"=TableVAR, "VaRPlot"=p) 
  return(my_list)
}


#어떤 자산배분안을 나타낼지..
SFRiskReturn<- function(Mean,Stdev,SFRisk,yr){
  Mean <- as.numeric(Mean)
  Stdev <- as.numeric(Stdev)
  UseMean <- (1 + Mean)**yr -1
  UseStdev <- Stdev * sqrt(yr)
  
  SV <- as.numeric(SFRisk)
  SV<- (1+SV)**yr -1
  shortfallp <- pnorm(SV, mean=UseMean*100, sd=UseStdev*100)
  
  return(shortfallp)
}
convert2ratio <- function(x){
  x<- round(x,digits=4)
  x<-paste(as.character(x*100),"%")
  return(x)
}
#원금과 CPI 넣을수있게 effresults는 샤프확률일거,,
RiskPlot_<- function(EFFResults,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit){
  Risk1Th <- as.numeric(Risk1Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk1_Limit <- as.numeric(Risk1_Limit)
  
  # EFFResults<- arrange(EFFResults, desc(sharpe))
  # UseMean<- EFFResults[1,]$Exp.Return
  # UseStdev<- EFFResults[1,]$Std.Dev
  
  EFFResults<-arrange(EFFResults, desc(SharpeRatio))
  UseMean<-EFFResults[1,]$Return
  UseStdev<-EFFResults[1,]$Risk
  
  #UseMean<-EFFResults[5]
  #UseStdev<-EFFResults[6]
  
  SV <- as.numeric(Risk1Th)
  
  UseMean <- UseMean*Risk1Yr
  UseStdev<-   UseStdev*Risk1Yr
  shortfallp <- pnorm(SV, mean=UseMean*100, sd=UseStdev*100)
  
  
  Annual.Return<-seq(from=UseMean*100-UseStdev*500,to=UseMean*100+UseStdev*500,length.out=200)
  
  Prob<-dnorm(Annual.Return,mean=UseMean*100,sd=UseStdev*100)
  
  ForText <- max(Prob)*1/2
  ForProb <- max(Prob)*0.8/2
  
  rfrisk <- paste(as.character(round(shortfallp,digits=4)*100),"%")
  
  ThresholdLine <- qnorm(Risk1_Limit/100 , mean = UseMean*100 , sd = UseStdev*100)
  
  p<- ggplot(data.frame(x=c(UseMean*100-UseStdev*500,UseMean*100+UseStdev*500)), aes(x=x))+
    stat_function(fun=dnorm, args=list(mean=UseMean*100, sd=UseStdev*100) ,size =1)+
    geom_area(stat='function', fun=dnorm, args=list(mean=UseMean*100, sd=UseStdev*100), xlim=c(UseMean*100-UseStdev*500, SV), fill='gray75') +
    geom_text(aes(x=UseMean*100 -UseStdev*300, y=ForText), label="ShortFall Risk:")+
    geom_text(aes(x=UseMean*100 -UseStdev*300, y=ForProb), label=rfrisk)+
    ggtitle(paste(Risk1Name,Risk1Yr,"Year","Shortfall Risk"))+
    geom_vline(xintercept = ThresholdLine, colour = 'red')+
    theme(panel.background=element_rect(fill=eallighttan),
          text=element_text(color=ealdark),
          plot.title=element_text(size=24, color=ealred))+
    labs(x="Annualized Return (%)", y="Prob")
  
  return(p)
}
#MVO<-MVOShortfall(Path , BenchUse, RF , ExUse, StdUse , CorrUse, MVO_num, SeedingNum,depositfix,fixedvalue)

#Rplot<-RiskPlot_(portfolio_values,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit)

# Constraint 적용
Filtering_MVO <- function(TotalDF, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit){
  Risk1Th <- as.numeric(Risk1Th)
  Risk2Th <- as.numeric(Risk2Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk2Yr <- as.numeric(Risk2Yr)
  Risk1_Limit <- as.numeric(Risk1_Limit)
  Risk2_Limit <- as.numeric(Risk2_Limit)
  #ILMax <- as.numeric(ILMax)
  WillUseDF <- data.frame()
  SimulSharpe <- data.frame()
  SimulVar <- data.frame()
  
  # for(k in 1:length(TotalDF[,"Exp.Return"])){
  #   UseMeans <- TotalDF[,"Exp.Return"][k]
  #   UseStdevs <- TotalDF[,"Std.Dev"][k]
  # 
  # 조건 걸어주는 부분
  for(k in 1:length(TotalDF$Return)){
    #UseMeans <- portfolio_values["Return"][k,]
    UseMeans<-TotalDF$Return[k]
    #UseStdevs <- portfolio_values["Risk"][k,]
    UseStdevs<-TotalDF$Risk[k]
    UseWeight <- TotalDF[k,1:(ncol(TotalDF)-3)]
    
    # 각 Random Portfolio 결과들중 해당 조건들에 맞는 부분만 추출
    # 1) Risk 1 과 Risk 2는 허용 위험 한도보다 낮을 확률을 계산하여
    # 해당 결과보다 높은 위험 확률을 주는 결과들은 제거 함.
    # 2) IL Risk 는 운용배수 관련 위험으로 랜덤 결과에서 나오는 위험도가
    # 처음에 넣는 위험보다 높은 결과 제거
    
    if(SFRiskReturn(UseMeans,UseStdevs,Risk1Th/100,Risk1Yr)< 0.01*Risk1_Limit & 
       SFRiskReturn(UseMeans,UseStdevs,Risk2Th/100,Risk2Yr)< 0.01*Risk2_Limit){
      #ILRisk_Return(Path , UseWeight,mean_ret,UseMeans, Target1L,Resampled_or_True) < 0.01 * ILMax)
      WillUseDF <- rbind(WillUseDF, TotalDF[k,])
    }
  }
  if(nrow(WillUseDF) != 0 ){
    SharpeMax <- WillUseDF[which(WillUseDF$SharpeRatio == max(WillUseDF$SharpeRatio)),]
    VarMin <- WillUseDF[which(WillUseDF$Risk == min(WillUseDF$Risk)),]
    
    SimulSharpe <- rbind(SimulSharpe, SharpeMax)
    SimulVar <- rbind(SimulVar , VarMin)
    
    my_list <- list("SharpeMAx" = SimulSharpe , "VarianceMin" = SimulVar)
    
    return(my_list)
  } else{
    return("NA")
  }
  
}

Filtered_MVO<-function(EFFs , Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit){
  
  Risk1Th <- as.numeric(Risk1Th)
  Risk2Th <- as.numeric(Risk2Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk2Yr <- as.numeric(Risk2Yr)
  Risk1_Limit <- as.numeric(Risk1_Limit)
  Risk2_Limit <- as.numeric(Risk2_Limit)
  
  x_var <- c()
  x_sh <- c()
  for_index <-c()
  Points_Sharpe_ <- data.frame()
  Points_Var_ <- data.frame()
  
  filter<-Filtering_MVO(EFFs,  Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit)
  if(length(filter)!=0){
    ShMax <- filter$SharpeMAx
    VarMin <- filter$VarianceMin
    Points_Sharpe_ <- rbind(Points_Sharpe_, ShMax)
    Points_Var_ <- rbind(Points_Var_ , VarMin)
    
    
    TotalDF <- list("SharpePoints" = Points_Sharpe_ , "VariancePoints" = Points_Var_)
    
    # 비중이 양수인 점들만 저장
    TotalDF$VariancePoints<- subset(TotalDF$VariancePoints, apply(TotalDF$VariancePoints, 1, function(x){all(x > 0)}))
    TotalDF$SharpePoints<- subset(TotalDF$SharpePoints, apply(TotalDF$SharpePoints, 1, function(x){all(x > 0)}))
    
    var_df <- data.frame(describe(TotalDF$VariancePoints))[,c("mean","min","max","sd")]
    sh_df <- data.frame(describe(TotalDF$SharpePoints))[,c("mean","min","max","sd")]
    
    x_var <- append(x_var, list(var_df))
    x_sh <- append(x_sh, list(sh_df))
    #for_index <- append(for_index,paste(toString(yr),"_",toString(THrisk)))
    
    FilteredOutput <- rbind(t(data.frame(x_sh)['mean']) , t(data.frame(x_var)['mean']))
    
    rownames(FilteredOutput) <- c("Max Sharpe Weight", "Min Variance Weight")
    
    ExceptSharpe <- apply(FilteredOutput[,(1:(ncol(FilteredOutput)-1))],2,convert2ratio)
    rownames(ExceptSharpe) <- c("Average Max Sharpe Weight", "Average Min Variance Weight")
    
    SharpeCol <- data.frame(FilteredOutput[,ncol(FilteredOutput)])
    
    colnames(SharpeCol) <- c("Sharpe Ratio")
    
    FilteredOutput <- cbind(ExceptSharpe,SharpeCol)
  }
  else {
    FilteredOutput<-"NA"
    TotalDF$SharpePoints<-"NA"
  }
  my_list <- list("WeightTable" = FilteredOutput , "ShapreMaxPoints" = TotalDF$SharpePoints, "VariancePoint"=TotalDF$VariancePoints )
  return(my_list)
}

#적립기금추이 함수
Fund_result <- function(Path, inf, wage, category, return){
  if(as.numeric(category)==1){
    #수익률 테이블 생성
    
    return<-as.numeric(return)
    
    #18년말 적립금 테이블 생성
    asset_year<-c(2018)
    Fund_Asset<-c(638781) #2018년말 국민연금 적립금 (단위:십억원) 
    Fund_Asset<-data.frame(asset_year,Fund_Asset)
    colnames(Fund_Asset)=append('연도','적립기금')
    
    #투자수익 테이블 생성
    Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    
    #수입지출모듈
    pension<-useInsurance(Path, inf, wage, category) # 국민연금 수입지출
    
    income<-pension[,2]/1000000000 #단위 맞추기(십억원) 수입
    expenditure<-pension[,3]/1000000000 #단위 맞추기(십억원) 지출
    
    income<- data.frame('연도'= c(2019:2067),'국민연금수입'= income)
    expenditure<-data.frame('연도'= c(2019:2067),'국민연금지출'= expenditure)
    
    
    pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
    #투자자산 연도 생성
    for (i in 1:49){
      Fund_Asset[i+1,1]<-2018+i
    }
    #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
    
    for(a in 1:49){
      
      Fund_Asset[a+1,2]<-(return*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
      
      if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게
        break
      }
    }
    
    colnames(Fund_Asset)<-append("연도", "적립기금")
    
    #투자수익 확인
    #년도생성
    for (i in 1:50){
      Fund_Invest[i,1]<-2017+i
    }
    # i=1
    for(a in 1:50){
      #   if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
      #     i=i+1
      #   }
      
      Fund_Invest[a+1,2]<-return*(Fund_Asset[a,2])
      
      if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
        Fund_Invest[a+1,2]<-NA
        break
      }
    }
    
    
    colnames(Fund_Invest)<-append("연도", "투자수익")
    
    
    #총수입 계산
    Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
    for(i in 1:nrow(Fund_Invest)){
      Total_income[i,1]<-Fund_Invest[i,1]}
    for(i in 2:nrow(Fund_Invest)){
      Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
      if(is.na(Fund_Invest[i,2])){
        Total_income[i,2]<-pension[i-1,2]
      }
    }
    colnames(Total_income)<-append("연도", "총수입")
    
    #수지차 생성
    difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    for (i in 1:49){
      difference[i,1]<-2018+i
      difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
    colnames(difference)<-append("연도", "수지차")
    
    #year를 기준으로 합치기
    Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
    
    
    max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
    max_year<-as.numeric(max_asset_year[1])
    max_asset<-as.numeric(max_asset_year[2])
    
    p<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
      geom_line(color=3)+geom_point(size=1, color=4)+
      scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
      scale_y_continuous(breaks=seq(0, 2000000, 200000))+
      annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
      annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
      annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
      annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
      annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
      labs(x = '연도',
           y = '적립기금 (단위 : 십억원)',
           title = "국민연금 적립기금추이")
  }
  
  else if(as.numeric(category)==2){
    
    return<-as.numeric(return)
    
    #18년말 적립금 테이블 생성
    asset_year<-c(2018)
    Fund_Asset<-c(97097) #2018년말 고용보험 적립금 (단위:억원) 
    Fund_Asset<-data.frame(asset_year,Fund_Asset)
    colnames(Fund_Asset)=append('연도','적립기금')
    
    #투자수익 테이블 생성
    Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    
    #수입지출모듈
    pension<-useInsurance(Path, inf, wage, category) # 고용보험 수입지출
    
    income<-pension[,2]/100000000 #단위 맞추기(억원) 수입
    expenditure<-pension[,3]/100000000 #단위 맞추기(억원) 지출
    
    income<- data.frame('연도'= c(2019:2067),'고용보험수입'= income)
    expenditure<-data.frame('연도'= c(2019:2067),'고용보험지출'= expenditure)
    
    
    pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
    #투자자산 연도 생성
    for (i in 1:49){
      Fund_Asset[i+1,1]<-2018+i
    }
    #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
    
    for(a in 1:49){
      
      Fund_Asset[a+1,2]<-(return*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
      
      if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게
        break
      }
    }
    
    colnames(Fund_Asset)<-append("연도", "적립기금")
    
    #투자수익 확인
    #년도생성
    for (i in 1:50){
      Fund_Invest[i,1]<-2017+i
    }
    
    for(a in 1:50){
      
      Fund_Invest[a+1,2]<-return*(Fund_Asset[a,2])
      
      if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
        Fund_Invest[a+1,2]<-NA
        break
      }
    }
    
    colnames(Fund_Invest)<-append("연도", "투자수익")
    
    
    #총수입 계산
    Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
    for(i in 1:nrow(Fund_Invest)){
      Total_income[i,1]<-Fund_Invest[i,1]}
    for(i in 2:nrow(Fund_Invest)){
      Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
      if(is.na(Fund_Invest[i,2])){
        Total_income[i,2]<-pension[i-1,2]
      }
    }
    colnames(Total_income)<-append("연도", "총수입")
    
    #수지차 생성
    difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
    for (i in 1:49){
      difference[i,1]<-2018+i
      difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
    colnames(difference)<-append("연도", "수지차")
    
    #year를 기준으로 합치기
    Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
    Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
    
    
    max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
    max_year<-as.numeric(max_asset_year[1])
    max_asset<-as.numeric(max_asset_year[2])
    
    p<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
      geom_line(color=3)+geom_point(size=1, color=4)+
      scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
      scale_y_continuous(breaks=seq(0, 2000000, 500000))+
      annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
      annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
      annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
      annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/10000, 2) , size = 4)+
      annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
      labs(x = '연도',
           y = '적립기금 (단위 : 십억원)',
           title = "고용보험 적립기금추이")
  }
  
  
  my_list<-list('table'=Fund_Total,  'plot'=p)
  return(my_list)
}
Simul_Run<- function(Path,ben_a, ben_b, ben_c, ben_d, income_inc, cost_inc, n, MVO_num, RF, hedge_num, Risk1Name, Risk2Name, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit, inf, wage, category)
{
  income_inc<-as.numeric(income_inc)
  cost_inc<-as.numeric(cost_inc)
  ben_a<-as.numeric(ben_a)
  ben_b<-as.numeric(ben_b)
  ben_c<-as.numeric(ben_c)
  ben_d<-as.numeric(ben_d)
  t<-as.numeric(5) #향후 5년
  n<-as.numeric(n)
  Risk1Th <- as.numeric(Risk1Th)
  Risk2Th <- as.numeric(Risk2Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk2Yr <- as.numeric(Risk2Yr)
  Risk1_Limit <- as.numeric(Risk1_Limit)
  Risk2_Limit <- as.numeric(Risk2_Limit)
  MVO_num <- as.numeric(MVO_num)
  RF<-as.numeric(RF)
  hedge_num<-as.numeric(hedge_num)
  inflation <- as.numeric(inf)  #물가상승률
  wage_increase_rate <- as.numeric(wage)  #임금상승률
  category<-as.numeric(category)
  
  random_simul_<-random_simul_hedged(Path, ben_a, ben_b, ben_c, ben_d,income_inc, cost_inc,n,MVO_num,RF, hedge_num, inf, wage, category)
  #pf<-random_simul_select(Path, ben_a, ben_b, ben_c, ben_d,income_inc, cost_inc, n, MVO_num,RF, hedge_num)
  
  mean_table<-random_simul_$select
  portfolio_values<-random_simul_$weight
  port_mean<-mean(portfolio_values$Return %>% as.numeric())
  port_sd<- mean(portfolio_values$Risk %>% as.numeric())
  
  #filter<-Filtering_MVO( portfolio_values,  Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit)
  
  Filteredresult_output<-Filtered_MVO(portfolio_values , Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit)
  Sharpepoint<-(Filteredresult_output$ShapreMaxPoints)
  
  FirstRiskPlot_sh <- RiskPlot_(Sharpepoint,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit)
  #FirstRiskPlot_va <- RiskPlot_(Filteredresult_output$VariancePoint,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit)
  
  SecondRiskPlot_sh<-RiskPlot_(Sharpepoint,Risk2Name,Risk2Th,Risk2Yr, Risk2_Limit)
  #SecondRiskPlot_va<-RiskPlot_(Filteredresult_output$VariancePoint,Risk2Name,Risk2Th,Risk2Yr, Risk2_Limit)
  
  mylist<-list("hedged0"=random_simul_$`0% 헤지 자산배분안 평균`,
               "hedged1"=random_simul_$`10% 헤지 자산배분안 평균`,
               "hedged2"=random_simul_$`20% 헤지 자산배분안 평균`,
               "hedged3"=random_simul_$`30% 헤지 자산배분안 평균`,
               "hedged4"=random_simul_$`40% 헤지 자산배분안 평균`,
               "hedged5"=random_simul_$`50% 헤지 자산배분안 평균`,
               "hedged6"=random_simul_$`60% 헤지 자산배분안 평균`,
               "hedged7"=random_simul_$`70% 헤지 자산배분안 평균`,
               "hedged8"=random_simul_$`80% 헤지 자산배분안 평균`,
               "hedged9"=random_simul_$`90% 헤지 자산배분안 평균`,
               "hedged10"=random_simul_$`100% 헤지 자산배분안 평균`,
               "pfweight"=portfolio_values, "port_mean"=port_mean, "port_sd"=port_sd,
               "mean_table"=mean_table, "average_weight_con"=Filteredresult_output$WeightTable, 
               
               "FirstRiskPlot"=FirstRiskPlot_sh, "SecondRiskPlot"=SecondRiskPlot_sh)
  return(mylist)
}
#Simul_Run(Path,2,2,2,2,2,2,100,10,0,0,'원금','CPI',0,5,1,5,10,20,2,2)