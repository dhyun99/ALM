# 적립배율 = 연초 적립기금/ 해당연도 총지출
fund_total <- Fund(return18,return21,return31,return41,return51,return61,showInsur_man, showInsur_woman)[['재정수지전망']]

Accum_rate <- function(fund_total){
  accum_rate <- fund_total %>% filter(연도 < 2067) %>%  select(적립기금)/ fund_total %>% filter(연도 >2018 & 연도 < 2068) %>%  select(총지출)
  names(accum_rate) <- '적립배율'
  year <- c(2019:2067)
  df_accum_rate <- data.frame('연도'= year, '적립배율'= accum_rate)
  graph <- ggplot(data = df_accum_rate, aes(x= 연도, y = 적립배율))+ geom_line(color = 'red')+
    scale_x_continuous(breaks=seq(min(df_accum_rate$연도), max(df_accum_rate$연도), 5)) +
    scale_y_continuous(breaks=seq(0, 35, 5))+
    labs(x = '연도',
         y = '적립배율',
         title = "적립배율추이")
  my_list <- list(df_accum_rate, graph)
  return(my_list)
}
Accum_rate(fund_total)


# 상관관계분석
past_pension <-read_excel(paste(Path , "past_pension.xls", sep = '')) # 과거국민연금 수입지출(단위-억원)
benchMark <-read_excel(paste(Path , "benchMark.xlsx", sep = '')) # 수익률 벤치마크 데이터
past_pension_monthly <-read_excel(paste(Path , "past_pension_monthly.xlsx", sep = '')) # 과거 월별 국민연금 수입지출(단위-백만원)


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
  df_benchMark <- benchMark[grep('12-(26|28)', benchMark$대분류),]    # 선택한 날짜 포함한 행
  df_benchMark_11 <- benchMark[grep('11-(26|28)', benchMark$대분류),]
  df_benchMark <- df_benchMark[-c(4,6,8,13,15,20,22,24),] #중복 연도 제거
  df_benchMark_11 <- df_benchMark_11[-c(2,4,9,11,13,18,20),]
  df_benchMark <- df_benchMark[-(17:nrow(df_benchMark)),]  #2004년 이후로 제거
  df_benchMark_11 <- df_benchMark_11[-(17:nrow(df_benchMark_11)),]
  df_benchMark <- df_benchMark[,c(1,2,9,11,14)]
  df_benchMark_11 <- df_benchMark_11[,c(1,2,9,11,14)]
  names(df_benchMark) <- c('연도','국내채권','국내주식','해외채권','해외주식')
  names(df_benchMark_11) <- c('연도','국내채권','국내주식','해외채권','해외주식')
  
  df_benchMark$국내채권 <- as.numeric(df_benchMark$국내채권)
  df_benchMark$국내주식 <- as.numeric(df_benchMark$국내주식)
  df_benchMark$해외채권 <- as.numeric(df_benchMark$해외채권)
  df_benchMark$해외주식 <- as.numeric(df_benchMark$해외주식)
  df_benchMark_11$국내채권 <- as.numeric(df_benchMark_11$국내채권)
  df_benchMark_11$국내주식 <- as.numeric(df_benchMark_11$국내주식)
  df_benchMark_11$해외채권 <- as.numeric(df_benchMark_11$해외채권)
  df_benchMark_11$해외주식 <- as.numeric(df_benchMark_11$해외주식)
  df_benchMark <- df_benchMark[c(nrow(df_benchMark):1),] #2005년부터 시작하게 행순서 뒤집기
  df_benchMark_11 <- df_benchMark_11[c(nrow(df_benchMark_11):1),]
  for(i in 1:14){
    if(i==1){
      a <- c(as.numeric(df_benchMark[1+i,2]/ df_benchMark_11[i+1,2]))
      b <- c(as.numeric(df_benchMark[1+i,3]/ df_benchMark_11[i+1,3]))
      c <- c(as.numeric(df_benchMark[1+i,4]/ df_benchMark_11[i+1,4]))
      d <- c(as.numeric(df_benchMark[1+i,5]/ df_benchMark_11[i+1,5]))
    }
    else{
      a <- c(a, as.numeric(df_benchMark[1+i,2]/ df_benchMark_11[i+1,2]))
      b <- c(b, as.numeric(df_benchMark[1+i,3]/ df_benchMark_11[i+1,3]))
      c <- c(c, as.numeric(df_benchMark[1+i,4]/ df_benchMark_11[i+1,4]))
      d <- c(d, as.numeric(df_benchMark[1+i,5]/ df_benchMark_11[i+1,5]))
    }
  }
  yoy <- data.frame('국내채권'= a,'국내주식'= b,'해외채권'= c,'해외주식'= d)
  #data <- cbind(df_past_pension[,2:ncol(df_past_pension)], df_benchMark[(1:nrow(df_past_pension)),(2:ncol(df_benchMark))])
  data <- cbind(df_past_pension[,2:ncol(df_past_pension)], yoy)

  # 분석
  corNum <- cor(data) # 상관계수
  corPlot <- plot(data) # 산점도
  #my_list <- list(corNum, corPlot)
  #return(my_list)
  
  #월별 데이터 전처리
  df_past_pension_m<- data.frame('연도'=numeric(),'월'= numeric(),'수입'= numeric(),'지출'= numeric())
  k <- 2
  #year<-c(1:95)
  for(i in 1:8){
    for(j in 1:12){
      if(i==1 & j==1){
        year<- c(2011+i)
        month <- c(j)
        income_m <- c(as.numeric(gsub('\\D','',past_pension_monthly[2, k+1]))/as.numeric(gsub('\\D','',past_pension_monthly[2, k])))
        cost_m <-  c(as.numeric(gsub('\\D','',past_pension_monthly[9, k+1]))/as.numeric(gsub('\\D','',past_pension_monthly[9, k])))
        realcost_m <- c((as.numeric(gsub('\\D','',past_pension_monthly[9, k+1]))- as.numeric(gsub('\\D','',past_pension_monthly[2, k+1])))/ (as.numeric(gsub('\\D','',past_pension_monthly[9, k])) - as.numeric(gsub('\\D','',past_pension_monthly[2, k]))))
        k <- k+1
      }
      else if(k<97){
        year<- c(year, 2011+i)
        month <- c(month, j)
        income_m <- c(income_m, as.numeric(gsub('\\D','',past_pension_monthly[2, k+1]))/ as.numeric(gsub('\\D','',past_pension_monthly[2, k])))
        cost_m<-  c(cost_m, as.numeric(gsub('\\D','',past_pension_monthly[9, k+1]))/ as.numeric(gsub('\\D','',past_pension_monthly[9, k])))
        realcost_m <- c(realcost_m,(as.numeric(gsub('\\D','',past_pension_monthly[9, k+1]))- as.numeric(gsub('\\D','',past_pension_monthly[2, k+1])))/ (as.numeric(gsub('\\D','',past_pension_monthly[9, k])) - as.numeric(gsub('\\D','',past_pension_monthly[2, k]))))
        k <- k+1
      }
    }
  }
  #df_past_pension_m <- data.frame('연도'= year, '월'= month,'수입'= income_m, '지출'= cost_m)
  df_past_pension_m <- data.frame('수입'= income_m, '지출'= cost_m,'순지출'=realcost_m)
  
  df_benchMark_m <- benchMark[grep('(1|2|3|4|5|6|7|8|9|10|11|12)-(27|28|29|30|31)', benchMark$대분류),]    # 선택한 날짜 포함한 행
  df_benchMark_m <- df_benchMark_m[-(329:nrow(df_benchMark_m)),]  #2004년 이후로 제거
  df_benchMark_m <- df_benchMark_m[-(1:39),]  #2020년 이후로 제거
  df_benchMark_m <- df_benchMark_m[c(1,3,6,10,12,16,19,21,26,28,31,33,37,39,43,46,48,53,56,59,63,65,69,71,74,76,80,83,86,90,93,97,100,102, 107, 109 ,110 ,113 ,116 ,119, 123 ,126 ,129, 133, 136, 139,143 ,144
                                     , 147, 150 ,152 ,156 ,157 ,160, 165, 167 ,170 ,174 ,177 ,178, 182 ,184 ,186 ,191 ,193, 196 ,200, 202 ,206 ,209 ,212 ,214,
                                     217 ,219, 222, 226 ,228, 232, 235 ,237, 242, 244 ,247, 249, 253 ,255, 259 ,262 ,264 ,269 ,272,275 ,278 ,280, 284),] #원하는 행 뽑기
  df_benchMark_m <- df_benchMark_m[,c(1,2,9,11,14)]
  names(df_benchMark_m) <- c('연도','국내채권','국내주식','해외채권','해외주식')
  df_benchMark_m$국내채권 <- as.numeric(df_benchMark_m$국내채권)
  df_benchMark_m$국내주식 <- as.numeric(df_benchMark_m$국내주식)
  df_benchMark_m$해외채권 <- as.numeric(df_benchMark_m$해외채권)
  df_benchMark_m$해외주식 <- as.numeric(df_benchMark_m$해외주식)
  df_benchMark_m <- df_benchMark_m[c(nrow(df_benchMark_m):1),] #2012년부터 시작하게 행순서 뒤집기
  for(i in 1:95){
    if(i==1){
      a <- c(as.numeric(df_benchMark_m[1+i,2]/df_benchMark_m[i,2]))
      b <- c(as.numeric(df_benchMark_m[1+i,3]/df_benchMark_m[i,3]))
      c <- c(as.numeric(df_benchMark_m[1+i,4]/df_benchMark_m[i,4]))
      d <- c(as.numeric(df_benchMark_m[1+i,5]/df_benchMark_m[i,5]))
      e<-c(as.numeric(df_benchMark_m[i,1]))
    }
    else{
      a <- c(a, as.numeric(df_benchMark_m[1+i,2]/df_benchMark_m[i,2]))
      b <- c(b, as.numeric(df_benchMark_m[1+i,3]/df_benchMark_m[i,3]))
      c <- c(c, as.numeric(df_benchMark_m[1+i,4]/df_benchMark_m[i,4]))
      d <- c(d, as.numeric(df_benchMark_m[1+i,5]/df_benchMark_m[i,5]))
      e<-c(as.numeric(df_benchMark_m[i,1]))
    }
  }
  yoy_m <- data.frame('국내채권'= a,'국내주식'= b,'해외채권'= c,'해외주식'= d)
  #data <- cbind(df_past_pension_m[,2:ncol(df_past_pension_m)], df_benchMark_m[(1:nrow(df_past_pension_m)),(2:ncol(df_benchMark_m))])
  data_mm <- cbind(df_past_pension_m, yoy_m,df_benchMark_m[,1])#연도 추가
  data_m <- cbind(df_past_pension_m, yoy_m)
  
  # 분석
  corNum <- cor(data_m) # 상관계수
  corPlot <- plot(data_m) # 산점도
  #my_list <- list(corNum, corPlot)
  #return(my_list)
#}
#correlation(past_pension, benchMark)

  #연도별

income <- ts(income, start = c(2006,1)) #시계열 데이터 변환
cost <- ts(cost, start = c(2006,1))
realcost <-ts(realcost, start = c(2006,1))
yoy <- ts(yoy, start(2006,1))
plot.ts(income)
plot.ts(cost)
log_income <- log(income) #로그변환
log_cost <- log(cost)
log_yoy <- log(yoy)
log_realcost <- log(realcost)

diff_income <- diff(log_income) #차분
diff_cost <- diff(log_cost) #차분
diff_realcost <- diff(log_realcost)
plot(diff_cost) # 차분한 그래프
plot(diff_income)
#adf.test(income, alternative = 'stationary', k=0)
new_data <- cbind(data.frame('수입'= diff_income,'지출'=diff_cost), log_yoy[2:nrow(log_yoy),])  #증감+차분 해서 2년 후인 2007년부터 시작
cor(new_data) # 차분한 데이터 상관계수
new_data_realcost <- cbind(data.frame('수입'= diff_income,'지출'=diff_cost,'순지출'= diff_realcost), log_yoy[2:nrow(log_yoy),]) 
cor(new_data_realcost)
plot(new_data_realcost)

#월별

income_m <- ts(income_m, start = c(2012,1), frequency = 12)#시계열 데이터 변환
cost_m <- ts(cost_m, start = c(2012,1),frequency = 12)
yoy_m <- ts(yoy_m, start(2012,1),frequency = 12)
realcost_m <- ts(realcost_m, start(2012,1),frequency = 12)
plot.ts(income_m)
plot.ts(cost_m)

log_income_m <- log(income_m) #로그화
log_cost_m <- log(cost_m) 
log_yoy_m <- log(yoy_m)
log_realcost_m <- log(realcost_m)

diff_income_m <- diff(log_income_m) #차분
diff_cost_m <- diff(log_cost_m) 
diff_realcost_m <- diff(log_realcost_m)
plot(diff_cost_m) # 차분한 그래프
plot(diff_income_m)

new_data_m <- cbind(data.frame('수입'= log_income_m,'지출'=log_cost_m,'순지출'=log_realcost_m), log_yoy_m)
new_data_m<-new_data_m[-95,]
new_data_mm <- cbind(data.frame('수입'= log_income_m,'지출'=log_cost_m,'순지출'=log_realcost_m,'연도'=df_benchMark_m[,1]), log_yoy_m)
plot(new_data_m)
pairs(new_data_m, panel=panel.smooth) #추세선 추가
chart.Correlation(new_data_m, histogram=TRUE, pch=19)
cor<-cor(new_data_m) # 차분한 데이터 상관계수
corrplot(cor)

#kpss 검정
kpss.test(diff_income, null="Level") #추세 고려x
##p-value greater than printed p-value
kpss.test(diff_income, null="Trend") #추세 고려
kpss.test(diff_cost, null="Level")
#kpss.test(yoy[,1], null="Trend")
#kpss.test(yoy[,4], null="Level") # 자산수익률 정상성 확인
#plot(yoy)
#kpss.test(diff_cost, null="Trend")
##p-value greater than printed p-value

#ACF, PACF 검정
par(mfrow=c(1,1))
acf(diff_income, main="ACF") #lag1 선 초과->MA 1개
pacf(diff_income, main="PACF") #초과X

par(mfrow=c(1,1))
acf(diff_cost, main="ACF") #lag1 선 초과->MA 1개
pacf(diff_cost, main="PACF") #초과X

#rolling, 12개월로 이동상관관계분석
# install.packages("roll")
# new_data_roll<-rollapply(new_data,width=12, function(x) cor(x[,5],x[,6]) , by.column=FALSE)
# cor(new_data_roll)
monthly_correlation <- function(x, width) {
  rollapply(new_data_m, width=12, function(x) cor(x[,2],x[,3]), by.column=FALSE)
  return(new_data_m)
}
plot(new_data_m) #똑같은 거 같은데...


roll_data<-xts(data_mm,order.by = data_mm$연도) #연도 월별 각 항목 시각화
dygraph(roll_data)
roll_new_data<-xts(new_data_mm,order.by = new_data_mm$연도) #연도 월별 각 항목 시각화(로그, 차분 적용)
#dygraph(roll_new_data)

#시뮬레이션

auto.arima(diff_income) #ARIMA(2,0,0)
auto.arima(diff_cost) #ARIMA(0,1,0)

# 1) 시뮬레이션 기간 (t)와 시뮬레이션 횟수 (n)을 정의하고
#) 해당기간에 기대되는 증가율(수입, 지출), 수익률(국내채권, 해외채권, 국내주식, 해외주식)을 입력
#3) resampling. 방식으로 시뮬레이션
a[1] <- data.frame('d'=c(1,2))
a[2] <- list(data.frame('o'= c(2,3)))
a[[1]]
a[[2]]

#np.random.multivariate_normal(성장률&수익률, 공분산행렬, 기간 t )
simulation <- function(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n){
  cov_s <- cov(new_data)
  mean_s <- c(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d)
  df_income <- data.frame('연도' = c(1:t), '')
  mydata <- list()
  for(i in 1:n){
    set.seed(i)
    mydata[[i]] <- as.data.frame(mvrnorm(t, mean_s, cov_s))
    names(mydata[[i]]) <- c('income', 'expenditure', 'domestic.bond', 'domestic.stock', 'foreign.bond', 'foreign.stock')
    mydata[[i]] <- cbind(mydata[[i]], data.frame('year'= c(1:t)))
    if(i==1){
      plot_income <- ggplot() + 
        geom_line(mapping = aes(x = year, y= income) ,data= mydata[[i]], col= i)
      plot_expend <- ggplot() + 
        geom_line(mapping = aes(x = year, y= expenditure) ,data= mydata[[i]], col= i)
      plot_dom_bond <- ggplot() + 
        geom_line(mapping = aes(x = year, y= domestic.bond) ,data= mydata[[i]], col= i)
      plot_dom_stock <- ggplot() + 
        geom_line(mapping = aes(x = year, y= domestic.stock) ,data= mydata[[i]], col= i)
      plot_for_bond <- ggplot() + 
        geom_line(mapping = aes(x = year, y= foreign.bond) ,data= mydata[[i]], col= i)
      plot_for_stock <- ggplot() + 
        geom_line(mapping = aes(x = year, y= foreign.stock) ,data= mydata[[i]], col= i)
    }
    else{
      plot_income <- plot_income + 
        geom_line(mapping = aes(x = year, y= income) ,data= mydata[[i]], col= i)
      plot_expend <- plot_expend + 
        geom_line(mapping = aes(x = year, y= expenditure) ,data= mydata[[i]], col= i)
      plot_dom_bond <- plot_dom_bond + 
        geom_line(mapping = aes(x = year, y= domestic.bond) ,data= mydata[[i]], col= i)
      plot_dom_stock <- plot_dom_stock + 
        geom_line(mapping = aes(x = year, y= domestic.stock) ,data= mydata[[i]], col= i)
      plot_for_bond <- plot_for_bond + 
        geom_line(mapping = aes(x = year, y= foreign.bond) ,data= mydata[[i]], col= i)
      plot_for_stock <- plot_for_stock + 
        geom_line(mapping = aes(x = year, y= foreign.stock) ,data= mydata[[i]], col= i)
    }
  }
  combined_data <- mydata[[1]]
  for(i in 2:n){
    combined_data <- rbind(combined_data, mydata[[i]])
  }
  avgMinMax_income <- data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(income)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(income)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(income)))), 'year'= 1)
  avgMinMax_expend <- data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(expenditure)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(expenditure)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(expenditure)))), 'year'= 1)
  avgMinMax_domestic.bond <- data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(domestic.bond)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(domestic.bond)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(domestic.bond)))), 'year'= 1)
  avgMinMax_domestic.stock <- data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(domestic.stock)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(domestic.stock)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(domestic.stock)))), 'year'= 1)
  avgMinMax_foreign.bond <- data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.bond)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(foreign.bond)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(foreign.bond)))), 'year'= 1)
  avgMinMax_foreign.stock <- data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.stock)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(foreign.stock)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==1) %>% select(foreign.stock)))), 'year'= 1)
  
  for(i in 1:t){
    avgMinMax_income <- rbind(avgMinMax_income, data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(income)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(income)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(income)))), 'year'= i))
    avgMinMax_expend <- rbind(avgMinMax_expend, data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(expenditure)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(expenditure)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(expenditure)))), 'year'= i))
    avgMinMax_domestic.bond <- rbind(avgMinMax_domestic.bond, data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(domestic.bond)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(domestic.bond)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(domestic.bond)))), 'year'= i))
    avgMinMax_domestic.stock <- rbind(avgMinMax_domestic.stock, data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(domestic.stock)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(domestic.stock)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(domestic.stock)))), 'year'= i))
    avgMinMax_foreign.bond <- rbind(avgMinMax_foreign.bond, data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.bond)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.bond)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.bond)))), 'year'= i))
    avgMinMax_foreign.stock <- rbind(avgMinMax_foreign.stock, data.frame('avg'= mean(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.stock)))), 'max_'= max(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.stock)))), 'min_' = min(as.numeric(unlist(combined_data %>% filter(year==i) %>% select(foreign.stock)))), 'year'= i))
  }
  mylist <- list(avgMinMax_income, avgMinMax_expend, avgMinMax_domestic.bond, avgMinMax_domestic.stock, avgMinMax_foreign.bond, avgMinMax_foreign.stock)
  new_plot <- list()
  for(i in 1:6){
    new_plot[[i]] <- ggplot(data = mylist[[i]]) +
      geom_line(mapping = aes(x = year, y= avg *100)) +
      geom_ribbon(aes(x= year, ymin = min_ *100 , ymax = max_ *100 ),fill = 'royalblue' , alpha = 0.2)
  }
  grid.arrange(plot_income, plot_expend, plot_dom_bond, plot_dom_stock, plot_for_bond, plot_for_stock, nrow=3, ncol=2)
  grid.arrange(new_plot[[1]], new_plot[[2]], new_plot[[3]], new_plot[[4]], new_plot[[5]], new_plot[[6]], nrow=3, ncol=2)
}
simulation(1,1,1,1,1,1,15,100)

