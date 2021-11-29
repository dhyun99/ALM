disorder_rate<-read_excel(paste(Path , "disorder_rate.xlsx", sep = ''))#장애발생률
disorder_2019<-read_excel(paste(Path , "disorder_pensionReceive_age.xlsx", sep = ''))#2019 장애수급자
disorder_1519<-read_excel(paste(Path , "disorder_1519.xlsx", sep = ''))#1519 장애수급자

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
disorder_man  <- disorder_1519[1,3:13,drop = FALSE]*(as.numeric(disorder_rate[j,2])/100)*(1-death_rate_m[1,1])
disorder_2019_m <- disorder_pensionReceive_age[3:11,3,drop = FALSE]
colnames(disorder_2019_m )<-'2019'
disorder_woman  <- disorder_1519[2,3:13,drop = FALSE]*(as.numeric(disorder_rate[j,3])/100)*(1-death_rate_w[1,1])
disorder_2019_w <- disorder_pensionReceive_age[3:11,4,drop = FALSE]
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

