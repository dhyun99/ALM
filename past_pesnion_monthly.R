past_pension_monthly <-read_excel(paste(Path , "past_pension_monthly.xlsx", sep = '')) # 과거 월별 국민연금 수입지출(단위-백만원)
benchMark <-read_excel(paste(Path , "benchMark.xlsx", sep = '')) # 수익률 벤치마크 데이터

#correlation <- function(past_pension_monthly, benchMark){
  #데이터 전처리
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
        k <- k+1
      }
      else if(k<97){
        year<- c(year, 2011+i)
        month <- c(month, j)
        income_m <- c(income_m, as.numeric(gsub('\\D','',past_pension_monthly[2, k+1]))/ as.numeric(gsub('\\D','',past_pension_monthly[2, k])))
        cost_m<-  c(cost_m, as.numeric(gsub('\\D','',past_pension_monthly[9, k+1]))/ as.numeric(gsub('\\D','',past_pension_monthly[9, k])))
        k <- k+1
      }
    }
  }
  #df_past_pension_m <- data.frame('연도'= year, '월'= month,'수입'= income_m, '지출'= cost_m)
  df_past_pension_m <- data.frame('수입'= income_m, '지출'= cost_m)
 
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
  data_m <- cbind(df_past_pension_m, yoy_m,df_benchMark_m[,1])#연도 추가
  data <- cbind(df_past_pension_m[,2:ncol(df_past_pension_m)], yoy_m)
  
  # 분석
  corNum <- cor(data) # 상관계수
  corPlot <- plot(data) # 산점도
  #my_list <- list(corNum, corPlot)
  #return(my_list)
#}
correlation(past_pension_monthly, benchMark)



library(tseries)
library(corrplot)
library(PerformanceAnalytics)

income_m <- ts(income_m, start = c(2012,1), frequency = 12)#시계열 데이터 변환
cost_m <- ts(cost_m, start = c(2012,1),frequency = 12)
yoy_m <- ts(yoy_m, start(2012,1),frequency = 12)
plot.ts(income_m)
plot.ts(cost_m)
# income_m.decompose<-decompose(income_m)
# income_m.decompose$seasonal
# plot(income_m.decompose)
# income_m.decompose.adj <- income_m - income_m.decompose$seasonal
# plot(income_m.decompose.adj)


log_income_m <- log(income_m) #로그화
log_cost_m <- log(cost_m) 
log_yoy_m <- log(yoy_m)

#decompose
log_income_m_de<-decompose(log_income_m,type="multiplicative")
plot(log_income_m_de)
log_cost_m_de<-decompose(log_cost_m,type="multiplicative")
plot(log_cost_m_de)
log_yoy_m_de<-decompose(log_yoy_m,type="multiplicative")
plot(log_yoy_m_de)
#diff_income_m <- diff(income_m) #차분
#diff_cost_m <- diff(cost_m) #차분
#diff_yoy <- diff(yoy)
# plot(diff_cost_m) # 차분한 그래프
# plot(diff_income_m)
#adf.test(income_m, alternative = 'stationary', k=0)
#diff_income_m
new_data <- cbind(data.frame('수입'= log_income_m,'지출'=log_cost_m), log_yoy_m)
new_data<-new_data[-95,]
plot(new_data)
pairs(new_data, panel=panel.smooth) #추세선 추가
chart.Correlation(new_data, histogram=TRUE, pch=19)
cor<-cor(new_data) # 차분한 데이터 상관계수
corrplot(cor)

#rolling, 12개월로 이동상관관계분석
library(zoo)
library(roll)
library(ggplot2)
library(tseries)
# install.packages("roll")
# new_data_roll<-rollapply(new_data,width=12, function(x) cor(x[,5],x[,6]) , by.column=FALSE)
# cor(new_data_roll)
monthly_correlation <- function(x, width) {
  rollapply(new_data, width=12, function(x) cor(x[,2],x[,3]), by.column=FALSE)
  return(new_data)
}
plot(new_data) #똑같은 거 같은데...

#rolling 12단위씩 누적하면서 
log_income_m_roll<-rollapply(log_income_m, FUN="mean", width=12)
log_cost_m_roll<-rollapply(log_cost_m, FUN="mean", width=12)
log_yoy_m_roll<-rollapply(log_yoy_m, FUN="mean", width=12)


# new_data_roll <- cbind(data.frame('수입'= log_income_m_roll,'지출'=log_cost_m_roll), log_yoy_m_roll)  
# cor(new_data_roll) # 차분한 데이터 상관계수
# plot(new_data_roll)
# pairs(new_data_roll, panel=panel.smooth)

library(xts)
library(dygraphs)
cor(data_m)

roll_data<-xts(data_m,order.by = data_m$연도) #연도 월별 각 항목 시각화
dygraph(roll_data)

str(new_data)


