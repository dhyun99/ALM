# Parameter Setting

Path <- 'G:/내 드라이브/비즈니스랩_ALM/인구 자료 관련/2. 데이터/'

pension_insur = 0.09  #보험료율- 고정이라 인자로 안 넣었음
health_insur = 0.0683
employment_insur = 0.016
earn <- 0.638 # 근로소득비율 
biz <- 0.218 #사업소득비율
asset <- 0.067 #재산소득비율
transfer <- 0.077 #이전소득비율
pension_m<-0.737 #국민연금가입율 - 연령별 x
health_m<-0.796 #건강보험가입율
employ_m<-0.764  #고용보험가입률
pension_w= 0.644
health_w=0.688
employ_w=0.661

inflation_rate <- 2
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


# 남녀 인구수 추계 함수
result_total<- function(birth_man , people2019_man , death_man , birth_woman , people2019_woman , death_woman) {
  # Input 
  ## birth_man : 남자 출생아수
  ## people2019_man
  ## death_man : 남자 사망률
  ## birth_woman : 여자 출생아수
  ## people2019_woman
  ## death_woman : 여자 사망률
  
  # Return
  ## 남녀 인구수 추계
  
  people2019_man <- people2019_man[-1,]
  peo <- people2019_man[2:101,3,drop = FALSE]
  result_man <- birth_man[ , 4:52,drop = FALSE]
  result_man <- rbind.fill(result_man, peo) #인구수 추계
  deathNum_m <- data.frame(rep(1:nrow(result_man),1)) #사망자 수
  
  people2019_woman <- people2019_woman[-1,]
  peo <- people2019_woman[2:101,3,drop = FALSE]
  result_woman <- birth_woman[ , 4:52,drop = FALSE]
  result_woman <- rbind.fill(result_woman, peo)
  deathNum_w <- data.frame(rep(1:nrow(result_woman),1))
  
  for(i in 1:48){
    result_man[,i] <- as.numeric(result_man[,i])
    result_woman[,i] <- as.numeric(result_woman[,i])
    deathNum_m <- cbind(deathNum_m , result_man[,i,drop = FALSE]* death_man[,(3+i),drop = FALSE])
    deathNum_w <- cbind(deathNum_w , result_woman[,i,drop = FALSE]* death_woman[,(3+i),drop = FALSE])  
    for(j in 1:100){
      if(j==100){
        result_man[j+1,i+1] <- result_man[j+1, i] - deathNum_m[j+1,i+1] + result_man[j,i] - deathNum_m[j,i+1]
        result_woman[j+1,i+1] <- result_woman[j+1, i] - deathNum_w[j+1,i+1] + result_woman[j,i] - deathNum_w[j,i+1]
      }
      else{
        result_man[j+1, i+1] = result_man[j,i] - deathNum_m[j,i+1]
        result_woman[j+1, i+1] = result_woman[j,i] - deathNum_w[j,i+1]
      }
    }
  }
  deathNum_m <- cbind(deathNum_m , result_man[,49,drop = FALSE]* death_man[,52,drop = FALSE]) 
  deathNum_m <- deathNum_m[,2:ncol(deathNum_m)]
  deathNum_w <- cbind(deathNum_w , result_woman[,49,drop = FALSE]* death_woman[,52,drop = FALSE]) 
  deathNum_w <- deathNum_w[,2:ncol(deathNum_w)]
  
  
  my_list<-list('남자'=result_man,'여자' = result_woman) 
  return(my_list) # 남녀인구수추계
}
ResTot <- result_total(birth_man , people2019_man , death_man , birth_woman , people2019_woman , death_woman)

# 소득 관련 함수
avgIncome <- function(income_man,income_woman,income_sum, inflation){
  # Input 
  ## income_man
  ## income_woman
  ## income_sum
  ## inflation 
  
  # Return
  ## 남녀평균소득 데이터프레임 형태별
  
  # 필요 함수 정의
  growth<- function(x){
    x <- as.numeric(x)
    x <- x*(inflation/100+1)}
  
  # Input 데이터에 대한 prerprocessing
  resultIncome_s <-income_sum[2:nrow(income_sum),3:ncol(income_sum),drop = FALSE]
  resultIncome_s <- data.frame(resultIncome_s)
  resultIncome_m <-income_man[2:nrow(income_man),3:ncol(income_man),drop = FALSE]
  resultIncome_m <- data.frame(resultIncome_m)
  resultIncome_w <-income_woman[2:nrow(income_woman),3:ncol(income_woman),drop = FALSE]
  resultIncome_w <- data.frame(resultIncome_w)
  
  for(i in 1:48){
    toadd1 <- data.frame(lapply(resultIncome_m[i], growth))
    colnames(toadd1) <- 2019+i
    resultIncome_m <- cbind(resultIncome_m , toadd1)
    toadd2 <- data.frame(lapply(resultIncome_w[i], growth))
    colnames(toadd2) <- 2019+i
    resultIncome_w <- cbind(resultIncome_w , toadd2)
    toadd3 <- data.frame(lapply(resultIncome_s[i], growth))
    colnames(toadd3) <- 2019+i
    resultIncome_s <- cbind(resultIncome_s , toadd3)
  }
  
  df_income_m <- data.frame('연도'=numeric(),'평균소득'= numeric(),'연령'= integer())
  df_income_w <- data.frame('연도'=numeric(),'평균소득'= numeric(),'연령'= integer())
  for(i in 1:49){
    for(j in 1:11){
      if(j==1){
        year<- 2018+i
        income<-resultIncome_m[j,i]
        age<-'0-19'
        df_income_m <-rbind(df_income_m, data.frame('연도'=year,'평균소득'=income,'연령'=age))
        income<-resultIncome_w[j,i]
        df_income_w <-rbind(df_income_w, data.frame('연도'=year,'평균소득'=income,'연령'=age))
      }
      else if(j==11){
        year<- 2018+i
        income<-resultIncome_m[j,i]
        age<- '65-'
        df_income_m <-rbind(df_income_m, data.frame('연도'=year,'평균소득'=income,'연령'=age))
        income<-resultIncome_w[j,i]
        df_income_w <-rbind(df_income_w, data.frame('연도'=year,'평균소득'=income,'연령'=age))
      }
      else{
        year<- 2018+i
        income<-resultIncome_m[j,i]
        age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
        df_income_m <-rbind(df_income_m, data.frame('연도'=year,'평균소득'=income,'연령'=age))
        income<-resultIncome_w[j,i]
        df_income_w <-rbind(df_income_w, data.frame('연도'=year,'평균소득'=income,'연령'=age))
      }
    }
  }
  my_list<-list("남자"=df_income_m,"여자"=df_income_w,'남자'= resultIncome_m, '여자'= resultIncome_w, '전체'= resultIncome_s ) 
  return(my_list) 
}
AvgIncom <- avgIncome(income_man,income_woman,income_sum , inflation_rate)

# 근로자 수
work_total <- function(employment_man, employment_woman, result_man , result_woman){
  # Input 
  ## employment_man
  ## employment_woman
  ## result_man
  ## result_woman
  
  # Return 
  ## 남녀근로자수(연령별 5세단위)
  
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
  }
  
  
  for(i in 1:49){
    for(j in 1:11){
      if(j==11){
        toappend <- c(toappend, mulEmp(sum(result_man[66:101,i]), j, 1))
        toappend2 <- c(toappend2, mulEmp(sum(result_woman[66:101,i]), j, 2))
      }
      else if(j==1){
        toappend <- c(mulEmp(sum(result_man[(11+5*j):(15+5*j),i]), j, 1)) # 고용률 5세단위, 15~19세부터 시작
        toappend2 <- c(mulEmp(sum(result_woman[(11+5*j):(15+5*j),i]), j, 2))
      }
      else{
        toappend <- c(toappend, mulEmp(sum(result_man[(11+5*j):(15+5*j),i]), j, 1))
        toappend2 <- c(toappend2, mulEmp(sum(result_woman[(11+5*j):(15+5*j),i]), j, 2))
      }
    }
    
    if(i==1){
      work_m <- data.frame( '2019' =toappend)
      work_w <- data.frame( '2019' =toappend2)
    }
    else{
      work_m <- cbind(work_m, data.frame(x = toappend))
      work_w <- cbind(work_w, data.frame(x = toappend2))
      colnames(work_m)[i] <- 2018+i
      colnames(work_w)[i] <- 2018+i
    }
  }
  rownames(work_m) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60-64세','65세이상')
  rownames(work_w) = c('15-19세','20-24세','25-29세','30-34세','35-39세','40-44세','45-49세','50-54세','55-59세','60-64세','65세이상')
  my_list<-list("남자"= work_m,"여자"= work_w , "전체" = work_m + work_w) 
  return(my_list) #남녀근로자수(연령별 5세단위)
}
WorkTot <- work_total(employment_man, employment_woman,ResTot$남자,ResTot$여자)

#보험료납부금액 추계- 연령별 가입률 미적용, 유형별 간략하게
showInsur_all <- function(earn , pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , work_m, work_w , df_income_m , df_income_w){
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
  my_list <- list('남자'= df_showInsur_m,'여자'= df_showInsur_w)
  return(my_list)
}
InsurAll <- showInsur_all(earn , pension_insur , health_insur , employment_insur , pension_m, health_m , employ_m , pension_w , health_w , employ_w , WorkTot$남자, WorkTot$여자 , AvgIncom$남자 , AvgIncom$여자)

#가입자수 구하는 함수 - 남녀합, 보험종류별, 연령별
social_sub <- function(social_insurance , work_total , j,i,k){
  # Input 
  ## social_insurance
  ## work_total 
  
  # Return 
  # 
  social_insurance <- social_insurance[-1,-1] #필요없는 행열 제거
  
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
showInsur_all_age <- function(social_insurance , work_total, resultIncome_s ){
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

InsurAll_age <- showInsur_all_age(social_insurance , WorkTot$전체, AvgIncom$전체)


