options("scipen" = 100)
pensionReceive_num <-read_excel(paste(Path , "pensionReceive_num.xlsx", sep = ''))
pensionReceive_money <-read_excel(paste(Path , "pensionReceive_money.xlsx", sep = ''))
start_receive <-read_excel(paste(Path , "start_receive.xlsx", sep = ''))
old_pensionReceive_age <-read_excel(paste(Path , "old_pensionReceive_age.xlsx", sep = ''))
old_bereaved_rate <-read_excel(paste(Path , "old_bereaved_rate.xlsx", sep = '')) # 유유족률
disorder_rate<-read_excel(paste(Path , "disorder_rate.xlsx", sep = ''))#장애발생률

names(pensionReceive_num) = c('급여종류','연령','전체','남자','여자')
pensionReceive_num <- pensionReceive_num[-(1:2),]
names(old_pensionReceive_age) = c('연령','합계','남자','여자')
old_pensionReceive_age <- old_pensionReceive_age[-(1:2),]
receive_2019 <- as.numeric(pensionReceive_num[1,3])
receive_2019_m <- as.numeric(pensionReceive_num[1,4]) 
receive_2019_w <- as.numeric(pensionReceive_num[1,5])

for(i in 1:49){
  if(i == 1){
    subscriber <- c(sum(social_sub(1:11, 1, 1)))
  }
  else{
    subscriber <- c(subscriber,sum(social_sub(1:11, i, 1)))
  }
}

#지출
#최근연도 수급자수/ 수급개시연령 이상 인구수
receive_ratio <- receive_2019 / sum(result_sum[61:101,1])
receive_ratio_m <- receive_2019_m / sum(result_man[61:101,1])
receive_ratio_w <- receive_2019_w / sum(result_woman[61:101,1])

j <- 3
startAge <- 62
receiver <- c(receive_2019) #2019년도 수급자 먼저 추가
old_receive <- as.numeric(pensionReceive_money[1,2])* 12 # 연평균노령연급수급액
receive_amount <- c(old_receive * receive_2019) #총수급(19년도수급자 * 19년도 수급액)먼저 추가
toadd <-old_receive
for(i in 1:48){
  if((2019+i)== as.numeric(start_receive[j,1]) & j<6){
    j <- j+1
    startAge <- as.numeric(start_receive[j-1,2])
  }
  print(startAge)
  receiver <- c(receiver, sum(result_sum[startAge:101,i+1])*receive_ratio )
  old_receive <- B(old_receive)
  toadd <- old_receive * sum(result_sum[startAge:101,i+1])*receive_ratio
  receive_amount <- c(receive_amount, toadd)
}
df_cost_p <- data.frame('연도'= c(2019:2067),'수급자수'= receiver, '국민연금지출'= receive_amount)

#수입
select <- dplyr::select
income_p <- c(sum(df_showInsur %>% filter(연도==2019) %>% select(국민연금))* 10000 *12 ) # 단위 만원-> 원으로 변경
income_p
for (i in 1:48) {
  income_p <- c(income_p,sum(df_showInsur %>% filter(연도==(2019+i)) %>% select(국민연금))*10000 *12)
}

# 유족인구추계
receive_age_m <- as.numeric(unlist(old_pensionReceive_age[7:27,3]))  
receive_age_w <- as.numeric(unlist(old_pensionReceive_age[7:27,4])) 
df_receive_m <- data.frame('2019' = receive_age_m )  # 수급자수(연령별) 테이블에 2019년 먼저 추가
df_receive_w <- data.frame('2019' = receive_age_w )
for(i in 1:48){
  toadd_m <- result_man[61:80,i+1] * receive_ratio_m
  toadd_m <- c(toadd_m, sum(result_man[81:101,i+1])* receive_ratio_m) # 80T 한 행 추가
  toadd_w <- result_woman[61:80,i+1] * receive_ratio_w
  toadd_w <- c(toadd_w, sum(result_woman[81:101,i+1])* receive_ratio_w) 
  df_receive_m <- cbind(df_receive_m, data.frame('o'= toadd_m))  # 수급자수 추계 테이블 (열이름 지정이 동적으로 안되어서 일단 o로지정)
  df_receive_w <- cbind(df_receive_w, data.frame('o'= toadd_w))
}
names(df_receive_m) <- c(2019:2067)
row.names(df_receive_m) <- c(60:80) #열이름 60~80(세)으로 변경
names(df_receive_w) <- c(2019:2067)
row.names(df_receive_w) <- c(60:80)

start <- 2 
k <- 2
for(i in 1:49){ #유족인구
  if((2018+i) == as.numeric(start_receive[k,1])){
    start <- start + 1
    if(k==5)
      k <- k
    else
      k <- k+1
  }
  for(j in 1:21){
    df_receive_m[j,i] <- df_receive_m[j,i] * as.numeric(death_man[60+j,3+i]) * as.numeric(old_bereaved_rate[3+j,2])/100 # 수급자 *  사망률 * 유유족률
    df_receive_w[j,i] <- df_receive_w[j,i] * as.numeric(death_woman[60+j,3+i]) * as.numeric(old_bereaved_rate[3+j,3])/100
  }
  if(i==1){
    bereaved_m <- c(sum(df_receive_m[start:21,i])) 
    bereaved_w <- c(sum(df_receive_w[start:21,i])) 
  }
  else{
    bereaved_m <- c(bereaved_m, sum(df_receive_m[start:21,i])) #수급개시연령~80세이상 유족인구 합
    bereaved_w <- c(bereaved_w, sum(df_receive_w[start:21,i]))
  }
}

#장애연금
disorder_pensionReceive_age <-read_excel(paste(Path , "disorder_pensionReceive_age.xlsx", sep = ''))
names(disorder_pensionReceive_age) = c('연령','합계','남자','여자')
disorder_pensionReceive_age <- disorder_pensionReceive_age[-(1:2),]
disorder_receive_2019<-as.numeric(disorder_pensionReceive_age[1,2])
disorder_receiver <- c(disorder_receive_2019)
d_receive_ratio <- disorder_receive_2019 / sum(result_sum[19:101,1])
for(i in 1:48){
  disorder_receiver <- c(disorder_receiver, sum(result_sum[19:101,i+1])*d_receive_ratio)
}
df_cost_test <- data.frame('연도'= c(2019:2067),'노령연금수급자수'= receiver,'장애연금수급자수'= disorder_receiver)
disorder_age_m <- as.numeric(unlist(disorder_pensionReceive_age[3:13,3])) 
disorder_age_w <- as.numeric(unlist(disorder_pensionReceive_age[3:13,4])) 
df_disorder_m <- data.frame('2019' = disorder_age_m )  # 수급자수(연령별) 테이블에 2019년 먼저 추가
df_disorder_w <- data.frame('2019' = disorder_age_w )

toappend
for(i in 1:48){
  for(j in 1:11){
    if(j==1){
      toappend <- c(sum(result_man[(16+5*j):(20+5*j),i+1])*d_receive_ratio)
    }
    else if(j==11){
      toappend<- c(toappend, sum(result_man[(16+5*j):101,i+1])*d_receive_ratio)
    }
    else{
      toappend<- c(toappend, sum(result_man[(16+5*j):(20+5*j),i+1])*d_receive_ratio)
    }
  }
  print(toappend)
  df_disorder_m <- cbind(df_disorder_m, data.frame('o'= toappend))  # 수급자수 추계 테이블 (열이름 지정이 동적으로 안되어서 일단 o로지정)
}
names(df_disorder_m) <- c(2019:2067)
row.names(df_disorder_m) <- c('20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70세 이상')

start <- 0
k <- 1
for(i in 1:49){ #유족인구
  if((2018+i) == as.numeric(start_receive[k,2])){
    start <- start + 1
    if(k==6)
      k <- k
    else
      k <- k+1
  }
  for(j in 1:21){
    df_receive_m[j,i] <- df_receive_m[j,i] * as.numeric(death_man[60+j,3+i]) * as.numeric(old_bereaved_rate[3+j,2]) # 수급자 *  사망률 * 유유족률
    df_receive_w[j,i] <- df_receive_w[j,i] * as.numeric(death_woman[60+j,3+i]) * as.numeric(old_bereaved_rate[3+j,3])
  }
  if(i==1){
    bereaved_m <- c(sum(df_receive_m[start:21,i])) 
    bereaved_w <- c(sum(df_receive_w[start:21,i])) 
  }
  else{
    bereaved_m <- c(bereaved_m, sum(df_receive_m[start:21,i])) #수급개시연령~80세이상 유족인구 합
    bereaved_w <- c(bereaved_w, sum(df_receive_w[start:21,i]))
  }
}


df_income_p <- data.frame('연도'= c(2019:2067), '국민연금수입'= income_p)
df_pension <- data.frame('연도'= c(2019:2067),'가입자'=subscriber, '노령 수급자'= receiver, '국민연금수입'= income_p,'국민연금지출'= receive_amount,'순수익'= income_p - receive_amount)
df_test <- data.frame('연도'=c(2019:2067),'유족인구'= bereaved_m + bereaved_w )

#장애연금 수정
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


#가입자수
pension_compose <-read_excel(paste(Path , "pension_compose.xlsx", sep = ''))

random_last_m <- data.frame('2019'=work_m[10,1] * 0.076)
random_last_w <- data.frame('2019'=work_w[10,1] * 0.076)
local_social_m <- social_m_d
local_social_w <- social_w_d
random_social_m <- social_m_d
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
colSums(sum_social_w+sum_social_m) # 연도별 총 가입자수 합 출력

#random_last_w
sum_social_sum<-sum_social_m+sum_social_w
am<-sum(df_disorder_m[9:11,1,drop= FALSE])
bm<-rbind(df_disorder_m[1:8,1,drop= FALSE],am)

aw<-sum(df_disorder_m[9:11,1,drop= FALSE])
bw<-rbind(df_disorder_m[1:8,1,drop= FALSE],aw)

testttt_m<-bm[1:9,1,drop= FALSE]/sum_social_m[2:10,1,drop=FALSE]
testttt_w<-bw[1:9,1,drop= FALSE]/sum_social_w[2:10,1,drop=FALSE]


#장애연금 수급자
df_disorder_test <- data.frame('연도' = integer(), '연령'= integer(), '장애연금수급자수'= numeric(),'테스트'= numeric())
for(i in 1:49){
  for(j in 1:10){
    if(j==1){
      year<- 2018+i
      disorder_pension_m <- sum_social_m[j,i] * 0
      disorder_pension_w <- sum_social_w[j,i] * 0
      disorder_pension_sum<-sum_social_sum[j,i]*d_receive_ratio
      age<-'18-19'
      df_disorder_test  <-rbind(df_disorder_test , data.frame('연도' = year, '연령'= age, '장애연금수급자수'= disorder_pension_m+disorder_pension_w, '테스트'=disorder_pension_sum))
    }
    else{
      year<- 2018+i
      disorder_pension_m <- sum_social_m[j,i] * testttt_m[j-1,1]
      disorder_pension_w <- sum_social_w[j,i] * testttt_w[j-1,1]
      disorder_pension_sum<-sum_social_sum[j,i]*d_receive_ratio
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_disorder_test  <-rbind(df_disorder_test, data.frame('연도' = year, '연령'= age, '장애연금수급자수'=  disorder_pension_m+disorder_pension_w,'테스트'=disorder_pension_sum))
    }
  }
}


#df_disorder_m_test
for(i in 1:49){
  if(i == 1){
    subscriber_d <- c(sum(df_disorder_test[1:9,3]))
    subscriber_sum<-c(sum(df_disorder_test[1:9,4]))
    
  }
  else{
    subscriber_d <- c(subscriber_d, sum(df_disorder_test[(9*i-8):(9*i),3]))
    subscriber_sum <- c(subscriber_sum, sum(df_disorder_test[(9*i-8):(9*i),4]))
  }
}

df_ddddddddd<-data.frame('연도'= c(2019:2067), '장애연금수급자'=subscriber_d,'테스트'=subscriber_sum )

