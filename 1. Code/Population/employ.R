unemp_benefit <- read_excel(paste(Path , "4-4.xlsx", sep = ''))
names(unemp_benefit) <- c('구분', '성별', '인원', '건수', '금액')
unemp_benefit$인원 <- as.numeric(gsub('\\D','',unemp_benefit$인원)) # 숫자에 , 없애주기
unemp_benefit$건수 <- as.numeric(gsub('\\D','',unemp_benefit$건수))
unemp_benefit$금액 <- as.numeric(gsub('\\D','',unemp_benefit$금액))
unemp_peo.ts <- ts(unemp_benefit %>% filter(성별=='계') %>%select(인원), start = c(2016,1), frequency = 12)
unemp_mon.ts <- ts(unemp_benefit %>% filter(성별=='계') %>%select(금액), start = c(2016,1), frequency = 12)
unemp_peo.ts # 실업급여 지급인원
#plot(unemp_peo.ts)
unemp_mon.ts # 실업급여 지급금액
emp_stability <- read_excel(paste(Path , "5-1.xlsx", sep = ''))
emp_stability$계 <- as.numeric(gsub('\\D','',emp_stability$계))
stab_peo.ts <-  ts(emp_stability %>% filter(...2=='인원') %>%select(계), start = c(2016,1), frequency = 12)
stab_mon.ts <-  ts(emp_stability %>% filter(...2=='금액') %>%select(계), start = c(2016,1), frequency = 12)
stab_peo.ts # 고용안정 지급인원
stab_mon.ts # 고용안정 지급금액
#plot(stab_peo.ts)
decompose.unemp_peo <-decompose(unemp_peo.ts)
decompose.unemp_mon <-decompose(unemp_mon.ts)
decompose.unemp_peo$seasonal
decompose.unemp_mon$seasonal

#직업능력개발
na_delete <- function(data){ # 결측치 처리- 남은연도 당월 평균값
  data <- unlist(data)
  for (i in 1:60) {
    if(is.na(data[i])==TRUE){
      data[i] <- mean(data[i-12], data[i+12], data[i+24], data[i+36])
    }
  }
  return(data)
}
unemp_job <- read_excel(paste(Path , "6-1.xlsx", sep = ''))
unemp_job$계 <- as.numeric(gsub('\\D','',unemp_job$계))

job_peo.ts <-  ts(na_delete(unemp_job%>% filter(...2=='인원') %>%select(계)), start = c(2016,1), frequency = 12)
job_mon.ts <-  ts(na_delete(unemp_job%>% filter(...2=='금액') %>%select(계)), start = c(2016,1), frequency = 12)
job_peo.ts # 직업능력개발 지급인원
job_mon.ts # 직업능력개발 지급금액

#모성보호1
unemp_birth <- read_excel(paste(Path , "7-1.xlsx", sep = ''))
unemp_birth$계 <- as.numeric(gsub('\\D','',unemp_birth$계))
birth_peo.ts <-  ts(unemp_birth%>% filter(...3=='수급자') %>%select(계), start = c(2016,1), frequency = 12)
birth_mon.ts <-  ts(unemp_birth%>% filter(...3=='수급 금액') %>%select(계), start = c(2016,1), frequency = 12)
birth_peo.ts # 출산전후휴가 지급인원
birth_mon.ts # 출산전후휴가 지급금액

#모성보호2
unemp_parenting <- read_excel(paste(Path , "7-3.xlsx", sep = ''))
unemp_parenting$계 <- as.numeric(gsub('\\D','',unemp_parenting$계))
#unemp_parent <- unemp_parenting[1:3,,drop= FALSE]
for(i in 0:59){
  if(i == 0){
    unemp_parent <- unemp_parenting[1:3,,drop= FALSE]
  }
  else{
    unemp_parent <- bind_rows(unemp_parent,unemp_parenting[(1+9*i):(3+9*i),,drop= FALSE])
  }
}
parenting_peo.ts <-  ts(unemp_parent%>% filter(...4=='수급자') %>%select(계),  start = c(2016,1), frequency = 12)
parenting_mon.ts <-  ts(unemp_parent%>% filter(...4=='수급 금액') %>%select(계),  start = c(2016,1), frequency = 12)
parenting_peo.ts # 육아휴직 지급인원
parenting_mon.ts # 육아휴직 지급금액
job_peo.ts[2,1]
plot(decompose(unemp_peo.ts))
plot(decompose(unemp_mon.ts))
plot(decompose(stab_peo.ts))
plot(decompose(stab_mon.ts))
plot(decompose(job_peo.ts))
plot(decompose(job_mon.ts))
plot(decompose(birth_peo.ts ))
plot(decompose(birth_mon.ts ))
plot(decompose(parenting_peo.ts))
plot(decompose(parenting_mon.ts))

# 실업급여 수급자 시계열분석 예측
# unemp_p <-unemp_peo.ts - decompose(unemp_peo.ts)$seasonal # 계절성 제거
# plot(unemp_p)
# unemp_peo.ts
ndiffs(unemp_peo.ts) # 차분횟수 얼마나 해야하는지 
#d_unemp_p <-diff(diff(unemp_peo.ts, lag = 12)) # 계절성 차분 안 해도 정상성 있길래 주석 처리했어
d_unemp_p <-diff(unemp_peo.ts)
plot(d_unemp_p)
kpss.test(d_unemp_p, null="Trend")
auto.arima(d_unemp_p)
ari <- auto.arima(d_unemp_p)
unemp_p.forecast <- forecast(ari)
plot(unemp_p.forecast)
unemp_f <-as.data.frame(unemp_p.forecast)
data_unemp <- c(606226, unemp_f[,1]) # 역차분 위해 추계 시작값 넣어줌//20년 12월로 데이터 변경
data_unemp # 실업급여 수급자 
cumsum(data_unemp) #역차분- 누적
plot(cumsum(data_unemp),type = 'o')


#고용안정
ndiffs(stab_peo.ts)

stab_peo_diff1 <- diff(stab_peo.ts, differences = 1)
stab_peo_diff2 <- diff(stab_peo.ts, differences = 2)
stab_peo_diff3 <- diff(stab_peo.ts, differences = 3)

plot.ts(stab_peo.ts)
plot.ts(stab_peo_diff1)    # 1차 차분만 해도 어느정도 정상화 패턴을 보임
plot.ts(stab_peo_diff2)
plot.ts(stab_peo_diff3)

acf(stab_peo_diff3, lag.max = 1000000)
pacf(stab_peo_diff3, lag.max = 1000000)
auto.arima(stab_peo_diff3) 
stab_peo_arima <- arima(stab_peo_diff3, order = c(5,0,0))    # 차분통해 확인한 값 적용
stab_peo_arima

stab_peo_fcast <-forecast(stab_peo_arima, h =120) #h가 1개월단위
stab_peo_fcast
plot(stab_peo_fcast)
stab_f <-as.data.frame(stab_peo_fcast)
data_stab <- c(303947, stab_f[,1]) 
data_stab 
cumsum(data_stab)
plot(cumsum(data_stab),type = 'o')


#위 실업급여와 같은 방법, 근데 추계 미래로 갈 수록 이상해서 위의 방법으로 추계함
d_stab_p <-diff(diff(stab_peo.ts, lag = 12))
plot(d_stab_p)
kpss.test(d_stab_p, null="Trend")
auto.arima(d_stab_p)
ari_1 <- auto.arima(d_stab_p)
stab_p.forecast <- forecast(ari_1)
plot(stab_p.forecast)#zero mean이 나옴
stab_f <-as.data.frame(stab_p.forecast)
data_stab <- c(303947, stab_f[,1]) 
data_stab 
cumsum(data_stab)
plot(cumsum(data_stab),type = 'o')

