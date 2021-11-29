disorder_rate<-read_excel(paste(Path , "disorder_rate.xlsx", sep = ''))#장애발생률
disorder_bereaved_rate<-read_excel(paste(Path , "disorder_bereaved_rate.xlsx", sep = ''))#유유족률률
disorder_2019disorder_2019<-read_excel(paste(Path , "disorder_pensionReceive_age.xlsx", sep = ''))#2019 장애수급자
disorder_1519<-read_excel(paste(Path , "disorder_1519.xlsx", sep = ''))#1519 장애수급자
total_receiver <-read_excel(paste(Path , "total_receiver.xlsx", sep = ''))

#근로자 수
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

#가입자수
pension_m<-0.737
pension_w<-0.644
social_m_d<- work_m_d*pension_m
social_w_d<- work_w_d*pension_w


#가입자수 수정(지역, 납부, 임의, 임의계속 추가)
pension_compose <-read_excel(paste(Path , "pension_compose.xlsx", sep = '')) #2019년 가입자 수 

random_last_m <- data.frame('2019'=work_m[10,1] * 0.076)#임의계속
random_last_w <- data.frame('2019'=work_w[10,1] * 0.076)
local_social_m <- social_m_d #지역
local_social_w <- social_w_d
random_social_m <- social_m_d #임의
random_social_w <- social_w_d

for(i in 1:49){
  for(j in 1:9){
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
    random_last_m <- cbind(random_last_m, data.frame('e'=work_m[10,i] * 0.076 ))
    random_last_w <- cbind(random_last_w, data.frame('e'=work_w[10,i] * 0.076 ))
  }
}
names(random_last_m)<- c(2019:2067)
names(random_last_w)<- c(2019:2067)
rownames(random_last_m) <- '60세 이상'
rownames(random_last_w) <- '60세 이상'

sum_social_m <- local_social_m+ social_m_d + random_social_m
names(sum_social_m) <- c(2019:2067)
sum_social_m<- rbind(sum_social_m, random_last_m) # 총 가입자수
sum_social_w <- local_social_w+ social_w_d + random_social_w
names(sum_social_w) <- c(2019:2067)
sum_social_w <- rbind(sum_social_w, random_last_w) 

sum_social_sum<-sum_social_m+sum_social_w 


#장애연금 수급자 수 추계-중복 급여 고려x
disorder_man  <- disorder_1519[1,3:13,drop = FALSE] *(as.numeric(disorder_rate[1,2])/100)*(1-death_rate_m[1,1])
disorder_2019_m <- disorder_pensionReceive_age[3:11,3,drop = FALSE]
colnames(disorder_2019_m )<-'2019'
names(disorder_man) <- c('2019','2024','2029','2034','2039','2044','2049','2054','2059','2064','2069')
disorder_woman  <- disorder_1519[2,3:13,drop = FALSE]*(as.numeric(disorder_rate[1,3])/100)*(1-death_rate_w[1,1])
disorder_2019_w <- disorder_pensionReceive_age[3:11,4,drop = FALSE]
names(disorder_woman) <- c('2019','2024','2029','2034','2039','2044','2049','2054','2059','2064','2069')
colnames(disorder_2019_w)<-'2019'

disorder_man  <- rbind.fill(disorder_man,disorder_2019_m)
disorder_woman  <- rbind.fill(disorder_woman,disorder_2019_w)

# sum_social_m<-sum_social_m[-10,]
# sum_social_w<-sum_social_w[-10,]
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
    toappend_60_m <-sum(result_man[61:101,6])/sum(result_man[1:101,6])
    toappend_60_w <-sum(result_woman[61:101,6])/sum(result_woman[1:101,6])
  }
  else{
    toappend_60_m <- c(toappend_60_m, sum(result_man[61:101,5*i-4])/sum(result_man[1:101,5*i-4]))
    toappend_60_w<- c(toappend_60_w, sum(result_woman[61:101,5*i-4])/sum(result_woman[1:101,5*i-4]))
  }
}

rate_60<-data.frame(toappend_60_m,toappend_60_w)
rate_60<-t(rate_60)


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

colSums(disorder_man)+colSums(disorder_woman)

disorder_man <- linear_approx(disorder_man) # 선형보간
disorder_woman <- linear_approx(disorder_woman)
disorder_man <- disorder_man[, -50] # 선형보간 하고 68년 제거
disorder_woman <- disorder_woman[, -50]

subscriber_bereaved_rate <-read_excel(paste(Path , "subscriber_bereaved_rate.xlsx", sep = ''))
subscriber_pensionReceive_age <-read_excel(paste(Path , "subscriber_pensionReceive_age.xlsx", sep = ''))

#유족연금
#가입자 및 가입자이었던 자
sub_receive_ratio_first <-  as.numeric(pensionReceive_num[4,3]) / sum(result_sum[,1])
sub_receive_ratio_end <- as.numeric(total_receiver[2,7])/ sum(result_sum[,42]) # 60년 예상 수급자 수/65세이상(수급개시)인구
sub_increase <- (sub_receive_ratio_end - sub_receive_ratio_first)/41 # 매년 증가하는 수급비율
sub_receive_ratio <- sub_receive_ratio_first

names(subscriber_pensionReceive_age) = c('급여','연령','합계','남자','여자')
subscriber_pensionReceive_age <- subscriber_pensionReceive_age[-(1:2),]
sub_bereaved_m <- data.frame()

bereaved_man<-data.frame(disorder_man)
bereaved_woman<-data.frame(disorder_woman)
sub_bereaved_m <- bereaved_man
sub_bereaved_w <- bereaved_woman
sub_bereaved_m[,1] <- subscriber_pensionReceive_age[2:11,4,drop = FALSE]
sub_bereaved_w[,1] <- subscriber_pensionReceive_age[2:11,5,drop = FALSE]
for(i in 1:48){
  for(j in 1:10){
    bereaved_man[j,i] <-(sum_social_m[j,i]-disorder_man[j,i])*death_rate_m[j,i]*as.numeric(subscriber_bereaved_rate[5*j-3,2])/100
    bereaved_woman[j,i] <-(sum_social_w[j,i]-disorder_woman[j,i])*death_rate_w[j,i]*as.numeric(subscriber_bereaved_rate[5*j-3,3])/100
    sub_bereaved_m[j,i+1] <- (as.numeric(sub_bereaved_m[j,i]) + as.numeric(bereaved_man[j,i]))#*(1+sub_receive_ratio)
    sub_bereaved_w[j,i+1] <- (as.numeric(sub_bereaved_w[j,i]) + as.numeric(bereaved_woman[j,i]))#*(1+sub_receive_ratio)
    
    sub_receive_ratio <- sub_receive_ratio + sub_increase  # 수급비율증가
    #bereaved_woman[j,i]<-bereaved_woman[j,i]*death_rate_w[j,i]*subscriber_bereaved_rate[5*j-3,3]/100
  }
  sub_bereaved_m[,i]<-as.numeric(sub_bereaved_m[,i])
  sub_bereaved_w[,i]<-as.numeric(sub_bereaved_w[,i])
}

colSums(sub_bereaved_m)+colSums(sub_bereaved_w)

#sub_bereaved_m<-data.frame(toappend_sub_m)
#names(sub_bereaved_m)<- c(2019:2067)

# bereaved_man<-bereaved_man[-10,]
# bereaved_woman<-bereaved_woman[-10,] #60세 이상 제거
# 
# colSums(bereaved_man)+colSums(bereaved_woman)

total_receiver <- read_excel(paste(Path, "total_receiver.xlsx",sep = ''))  # 국민연금 수급자 수 전망 -> 수급비율 구할 때 사용
start_receive <-read_excel(paste(Path , "start_receive.xlsx", sep = ''))
start_receive <- start_receive[-1,] #2019년부터 시작하게 행제거
startAge <- as.numeric(start_receive[1,2])
receive_2019 <- as.numeric(pensionReceive_num[1,3])
#최근연도 수급자수/ 수급개시연령 이상 인구수
receive_ratio_first <- receive_2019 / sum(result_sum[63:101,1])
receive_ratio_end <- as.numeric(total_receiver[1,7])/ sum(result_sum[66:101,42]) # 60년 예상 수급자 수/65세이상(수급개시)인구
increase <- (receive_ratio_end - receive_ratio_first)/41 # 매년 증가하는 수급비율
receive_ratio <- receive_ratio_first
j <- 2
receiver <- c(receive_2019) #2019년도 수급자 먼저 추가
old_receive <- as.numeric(pensionReceive_money[1,2])* 12 # 연평균노령연급수급액
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

#유족연금수급 - 노령만
#d_receive <- function(Path, old_pensionReceive_age, result_man, result_woman, pensionReceive_num, old_bereaved_rate, death_man, death_woman){
  start_receive <-read_excel(paste(Path , "start_receive.xlsx", sep = ''))
  receive_2019_m <- as.numeric(pensionReceive_num[1,4])
  receive_2019_w <- as.numeric(pensionReceive_num[1,5])
  receive_age_m <- as.numeric(unlist(old_pensionReceive_age[9:27,3]))  
  receive_age_w <- as.numeric(unlist(old_pensionReceive_age[9:27,4])) 
  df_receive_m <- data.frame('2019' = receive_age_m )  # 수급자수(연령별) 테이블에 2019년 먼저 추가
  df_receive_w <- data.frame('2019' = receive_age_w )
  receive_ratio_m <- receive_2019_m / sum(result_man[61:101,1])
  receive_ratio_w <- receive_2019_w / sum(result_woman[61:101,1])
  for(i in 1:48){
    toadd_m <- result_man[63:80,i+1] * receive_ratio_m #62~80세
    toadd_m <- c(toadd_m, sum(result_man[81:101,i+1])* receive_ratio_m) # 80T 한 행 추가
    toadd_w <- result_woman[63:80,i+1] * receive_ratio_w
    toadd_w <- c(toadd_w, sum(result_woman[81:101,i+1])* receive_ratio_w) 
    df_receive_m <- cbind(df_receive_m, data.frame('o'= toadd_m))  # 수급자수 추계 테이블 (열이름 지정이 동적으로 안되어서 일단 o로지정)
    df_receive_w <- cbind(df_receive_w, data.frame('o'= toadd_w))
    receive_ratio_m <- receive_ratio_m + increase
    receive_ratio_w <- receive_ratio_w + increase
  }
  names(df_receive_m) <- c(2019:2067)
  row.names(df_receive_m) <- c(62:80) #열이름 62~80(세이상)으로 변경
  names(df_receive_w) <- c(2019:2067)
  row.names(df_receive_w) <- c(62:80)
  
  start <- 0
  k <- 1
  for(i in 1:49){ #유족인구
    if((2018+i) == as.numeric(start_receive[k,1])){
      start <- start + 1
      if(k==4)
        k <- k
      else
        k <- k+1
    }
    for(j in 1:19){
      if(j==19){
        df_receive_m[j,i] <- df_receive_m[j,i] * (as.numeric(death_man[62+j,3+i])+ as.numeric(death_man[101,3+i]))/2 * (as.numeric(old_bereaved_rate[5+j,2])+as.numeric(old_bereaved_rate[44,2]))/100 # 80세 이상 사망률은 80세~100세이상 사망률 평균으로 적용
        df_receive_w[j,i] <- df_receive_w[j,i] * (as.numeric(death_woman[62+j,3+i])+ as.numeric(death_woman[101,3+i]))/2 * (as.numeric(old_bereaved_rate[5+j,3])+as.numeric(old_bereaved_rate[44,3]))/100
      }
      else {
        df_receive_m[j,i] <- df_receive_m[j,i] * as.numeric(death_man[62+j,3+i]) * as.numeric(old_bereaved_rate[5+j,2])/100 # 수급자 *  사망률 * 유유족률
        df_receive_w[j,i] <- df_receive_w[j,i] * as.numeric(death_woman[62+j,3+i]) * as.numeric(old_bereaved_rate[5+j,3])/100
      }
    }
    if(i==1){
      bereaved_m <- c(sum(df_receive_m[start:19,i])) 
      bereaved_w <- c(sum(df_receive_w[start:19,i])) 
    }
    else{
      bereaved_m <- c(bereaved_m, sum(df_receive_m[start:19,i])) #수급개시연령~80세이상 유족인구 합
      bereaved_w <- c(bereaved_w, sum(df_receive_w[start:19,i]))
    }
  }
  df_d <- data.frame('연도'= c(2019:2067),'남자유족인구'=bereaved_m,'여자유족인구'= bereaved_w,'남녀 합'=bereaved_m+bereaved_w )
  return(df_d)
  

  
  am<-sum(disorder_bereaved_rate[9:15,2,drop= FALSE])/7 #60세 이상 유유족률 평균값
  d_bereaved_m<-rbind(disorder_bereaved_rate[1:8,2,drop= FALSE],am)
  aw<-sum(disorder_bereaved_rate[9:15,3,drop= FALSE])/7 
  d_bereaved_w<-rbind(disorder_bereaved_rate[1:8,3,drop= FALSE],aw)
  
  
  disorder_bereaved_m <- disorder_man[-1,,drop= FALSE] #15~19세 행 제거, 유유족률X
  disorder_bereaved_w <- disorder_woman[-1,,drop= FALSE]
  
  
  for(i in 1:49){
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
  
  for(i in 1:49){
    for(j in 1:9){
      if(j==1){
        toappend_total_m <-c(sum(bereaved_man[j,i])+sum(sub_bereaved_m[j,i]))
        toappend_total_w <-c(sum(bereaved_woman[j,i])+sum(sub_bereaved_w[j,i]))
      }
      else{
        toappend_total_m <- c(toappend_total_m, sum(bereaved_man[j,i])+sum(sub_bereaved_m[j,i]))
        toappend_total_w <- c(toappend_total_w, sum(bereaved_woman[j,i])+sum(sub_bereaved_w[j,i]))
      }
      
    }
    if(i==1){
      total_bereaved_m <- data.frame( '2019' =toappend_total_m)
      total_bereaved_w<- data.frame( '2019' = toappend_total_w)
    }
    else{
      total_bereaved_m <- cbind(total_bereaved_m, data.frame(x = toappend_total_m ))
      total_bereaved_w <- cbind(total_bereaved_w, data.frame(x = toappend_total_w))
      colnames(total_bereaved_m)[i] <- 2018+i
      colnames(total_bereaved_w)[i] <- 2018+i
    }
  }
  total_m<-colSums(bereaved_man)+colSums(sub_bereaved_m)+colSums(disorder_bereaved_m)
  total_w<-colSums(bereaved_woman)+colSums(sub_bereaved_w)+colSums(disorder_bereaved_w)
  df_total_bereaved<-data.frame('연도'= c(2019:2067),'남자유족인구'=total_m,'여자유족인구'=total_w,'전체유족인구'=total_m+total_w)
  