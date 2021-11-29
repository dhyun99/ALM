unemp_peo <- read_excel(paste(Path , "unemp_peo.xlsx", sep = ''))
unemp_peo <-data.frame(t(unemp_peo)) # 행렬전환
unemp_peo <- unemp_peo[-1,-1]
colnames(unemp_peo) <- c('합계', '남', '여')
unemp_peo.ts <- ts(unemp_peo[1:(nrow(unemp_peo)-12),] %>% select(합계) %>%  unlist %>%  as.numeric, start = c(2010,1), frequency = 12) # 2020년 빼고 시계열 데이터

# 경제활동인구감소 반영 함수
work_decrease <- function(temp){
  k <- 0
  for(i in 1:(length(temp)-12)){
    if(i %% 12 == 1){
      k <- k + 1
      dec <- colSums(work_s[k+1])/ colSums(work_s[k])
    }
    temp[i+12] <-temp[i] * dec 
  }
  return(temp)
}

# 실업급여 수급자 시계열분석 예측
# 실업급여 수급자 수 
unemployment_p <- function(unemp_peo){
  unemp_f <- forecast(unemp_peo.ts, h = 576) %>%  as.data.frame()
  unemp_2020 <- unemp_peo[(nrow(unemp_peo)-11) : nrow(unemp_peo),1] %>% as.numeric()
  temp <- c(unemp_2020, unemp_f[13:nrow(unemp_f),1]) # 2020년 데이터 + 2021년부터 예측결과
  temp <- work_decrease(temp)
  #my_list <- list('plot' = plot(ts(temp, start = c(2020, 1), frequency = 12)) , 'time series' = ts(temp, start = c(2020, 1), frequency = 12))
  return(temp)
}
#plot(forecast(unemp_peo.ts, h = 576)) # 2010~2019데이터로 2020~2030 예측
unemployment_p(unemp_peo)

# 금액
unemployment_m <- function(Path){
  unemp <- read_excel(paste(Path , "unemp.xlsx", sep = ''))
  unemp <- data.frame(t(unemp)) # 행렬전환
  unemp <- unemp[,-c(1:3)]
  colnames(unemp) <- c('년월별', '유형', '합계', '임금근로자', '자영업자')
  unemp <- unemp[-1,]
  for(i in 1:nrow(unemp)){ # 연월 빠진부분 채워넣기
    if(i %% 3 ==1 ){
      date <- unemp[i,1]
    }
    else{
      unemp[i,1] <- date
    }
  }
  unemp <- unemp[nrow(unemp):1,] # 날짜 순서바꾸기 - 2020년 1월부터
  unemp$임금근로자 <- as.numeric(unemp$임금근로자)
  unemp$자영업자 <- as.numeric(unemp$자영업자)
  unemp$합계 <- as.numeric(unemp$합계)
  for(i in 1:12){
    if(i==1){
      money <- unemp[(3*i-2),5]  / unemp[3*i, 5]  # 월평균실업급여
    }
    else{
      money <- c(money, unemp[(3*i-2),5]  / unemp[3*i, 5])
    }
  }
  df_unemp_mon <- data.frame('2020' = money)
  df_unemp_peo <- data.frame('2020' = temp[1:12])
  
  for(i in 1:47){
    toadd <- data.frame(growth(unlist(df_unemp_mon[i]), inflation, wage_increase_rate))
    colnames(toadd) <- 2020+i
    df_unemp_mon <- cbind(df_unemp_mon , toadd)
    toadd2 <- data.frame(temp[(12*i+1): (12*(i+1))])
    colnames(toadd2) <- 2020+i
    df_unemp_peo <- cbind(df_unemp_peo, toadd2)
  }
  my_list <- list( plot = plot(colSums(df_unemp_mon * df_unemp_peo), type = 'l'), data = colSums(df_unemp_mon * df_unemp_peo) )
  return(my_list)
}
# 함수 실행부분
unemployment_m(Path)

Unemployment <- function(Path){
  unemp_peo <- read_excel(paste(Path , "unemp_peo.xlsx", sep = ''))
  unemp_peo <-data.frame(t(unemp_peo)) # 행렬전환
  unemp_peo <- unemp_peo[-1,-1]
  colnames(unemp_peo) <- c('합계', '남', '여')
  unemp_peo.ts <- ts(unemp_peo[1:(nrow(unemp_peo)-12),] %>% select(합계) %>%  unlist %>%  as.numeric, start = c(2010,1), frequency = 12) # 2020년 빼고 시계열 데이터
  # 경제활동인구감소 반영 함수
  work_decrease <- function(temp){
    k <- 0
    for(i in 1:(length(temp)-12)){
      if(i %% 12 == 1){
        k <- k + 1
        dec <- colSums(work_s[k+1])/ colSums(work_s[k])
      }
      temp[i+12] <-temp[i] * dec 
    }
    return(temp)
  }
  # 실업급여 수급자 수 
  unemp_f <- forecast(unemp_peo.ts, h = 576) %>%  as.data.frame()
  unemp_2020 <- unemp_peo[(nrow(unemp_peo)-11) : nrow(unemp_peo),1] %>% as.numeric()
  temp <- c(unemp_2020, unemp_f[13:nrow(unemp_f),1]) # 2020년 데이터 + 2021년부터 예측결과
  temp <- work_decrease(temp)
  # 금액
  unemp <- read_excel(paste(Path , "unemp.xlsx", sep = ''))
  unemp <- data.frame(t(unemp)) # 행렬전환
  unemp <- unemp[,-c(1:3)]
  colnames(unemp) <- c('년월별', '유형', '합계', '임금근로자', '자영업자')
  unemp <- unemp[-1,]
  for(i in 1:nrow(unemp)){ # 연월 빠진부분 채워넣기
    if(i %% 3 ==1 ){
      date <- unemp[i,1]
    }
    else{
      unemp[i,1] <- date
    }
  }
  unemp <- unemp[nrow(unemp):1,] # 날짜 순서바꾸기 - 2020년 1월부터
  unemp$임금근로자 <- as.numeric(unemp$임금근로자)
  unemp$자영업자 <- as.numeric(unemp$자영업자)
  unemp$합계 <- as.numeric(unemp$합계)
  for(i in 1:12){
    if(i==1){
      money <- unemp[(3*i-2),5]  / unemp[3*i, 5]  # 월평균실업급여
    }
    else{
      money <- c(money, unemp[(3*i-2),5]  / unemp[3*i, 5])
    }
  }
  df_unemp_mon <- data.frame('2020' = money)
  df_unemp_peo <- data.frame('2020' = temp[1:12])
  
  for(i in 1:47){
    toadd <- data.frame(growth(unlist(df_unemp_mon[i]), inflation, wage_increase_rate))
    colnames(toadd) <- 2020+i
    df_unemp_mon <- cbind(df_unemp_mon , toadd)
    toadd2 <- data.frame(temp[(12*i+1): (12*(i+1))])
    colnames(toadd2) <- 2020+i
    df_unemp_peo <- cbind(df_unemp_peo, toadd2)
  }
  t <- c(9000000000000, colSums(df_unemp_mon * df_unemp_peo))
  em <- employ_insurance(Path, work_s, earn, Insurance_age, resultIncome_s)
  em <- cbind(em, data.frame('고용보험지출' = t, '순수익' = d$고용보험료수입 - t))
  
  return(em)
}
Unemployment(Path)


