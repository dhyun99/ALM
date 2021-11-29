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

benchdata<-function(Path){
  # 상관관계분석
  past_pension <-read_excel(paste(Path , "past_pension.xls", sep = '')) # 과거국민연금 수입지출(단위-억원)
  benchMark <-read_excel(paste(Path , "benchMark.xlsx", sep = '')) # 수익률 벤치마크 데이터
  #past_pension_monthly <-read_excel(paste(Path , "past_pension_monthly.xlsx", sep = '')) # 과거 월별 국민연금 수입지출(단위-백만원)
  
  
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
  
  #연도별
  
  income <- ts(income, start = c(2006,1)) #시계열 데이터 변환
  cost <- ts(cost, start = c(2006,1))
  realcost <-ts(realcost, start = c(2006,1))
  yoy <- ts(yoy, start(2006,1))
  #plot.ts(income)
  #plot.ts(cost)
  log_income <- log(income) #로그변환
  log_cost <- log(cost)
  log_yoy <- log(yoy)
  log_realcost <- log(realcost)
  
  diff_income <- diff(log_income) #차분
  diff_cost <- diff(log_cost) #차분
  diff_realcost <- diff(log_realcost)
  #plot(diff_cost) # 차분한 그래프
  #plot(diff_income)
  #adf.test(income, alternative = 'stationary', k=0)
  new_data <- cbind(data.frame('수입'= diff_income,'지출'=diff_cost), log_yoy[2:nrow(log_yoy),])  #증감+차분 해서 2년 후인 2007년부터 시작
  cor(new_data) # 차분한 데이터 상관계수
  new_data_realcost <- cbind(data.frame('수입'= diff_income,'지출'=diff_cost,'순지출'= diff_realcost), log_yoy[2:nrow(log_yoy),]) 
  cor(new_data_realcost)
  #plot(new_data_realcost)
  
  return(new_data)
}

hedgedbenchdata<-function(Path){
  # 상관관계분석
  past_pension <-read_excel(paste(Path , "past_pension.xls", sep = '')) # 과거국민연금 수입지출(단위-억원)
  benchMark <-read_excel(paste(Path , "benchMark.xlsx", sep = '')) # 수익률 벤치마크 데이터
  #past_pension_monthly <-read_excel(paste(Path , "past_pension_monthly.xlsx", sep = '')) # 과거 월별 국민연금 수입지출(단위-백만원)
  
  
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
 
 
  data <- cbind(df_past_pension[,2:ncol(df_past_pension)], yoy)
  
  #연도별
  
  income <- ts(income, start = c(2006,1)) #시계열 데이터 변환
  cost <- ts(cost, start = c(2006,1))
  realcost <-ts(realcost, start = c(2006,1))
  yoy <- ts(yoy, start(2006,1))
 
  log_income <- log(income) #로그변환
  log_cost <- log(cost)
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
  
  diff_income <- diff(log_income) #차분
  diff_cost <- diff(log_cost) #차분
  diff_realcost <- diff(log_realcost)
   new_data<-list()
  for (i in 1:11){
    new_data[[i]]<-cbind(data.frame('수입'= diff_income,'지출'=diff_cost), log_yoy[2:nrow(log_yoy),1:2],"해외채권"=bond[2:nrow(log_yoy),i],"해외주식"=stock[2:nrow(log_yoy),i])
    
  }
  return(new_data)
  
}

# simulation_con <- function(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n){
#   income_inc<-as.numeric(income_inc)
#   cost_inc<-as.numeric(cost_inc)
#   ben_a<-as.numeric(ben_a)
#   ben_b<-as.numeric(ben_b)
#   ben_c<-as.numeric(ben_c)
#   ben_d<-as.numeric(ben_d)
#   t<-as.numeric(5) #향후 5년
#   n<-as.numeric(n)
#   
#   new_data<-benchdata()
#   cov_s <- cov(new_data)
#   mean_s <- c(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d)
#   df_income <- data.frame('연도' = c(1:t), '')
#   mydata <- list()
#   for(i in 1:n){
#     set.seed(i)
#     mydata[[i]] <- as.data.frame(mvrnorm(t, mean_s, cov_s))
#     names(mydata[[i]]) <- c('income', 'expenditure', 'domestic.bond', 'domestic.stock', 'foreign.bond', 'foreign.stock')
#     mydata[[i]] <- cbind(mydata[[i]], data.frame('year'= c(1:t)))
#     
#   }
#   return(mydata)
# }
simulation_con_hedged <- function(Path,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n){
  income_inc<-as.numeric(income_inc)
  cost_inc<-as.numeric(cost_inc)
  ben_a<-as.numeric(ben_a)
  ben_b<-as.numeric(ben_b)
  ben_c<-as.numeric(ben_c)
  ben_d<-as.numeric(ben_d)
  t<-as.numeric(5) #향후 5년
  n<-as.numeric(n)
  
  new_data<-hedgedbenchdata(Path)
  hedged<-list()
  for(j in 1:11){
    cov_s <- cov(new_data[[j]])
    mean_s <- c(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d)
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

# random_simul<-function(yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,RF){
#   yy<-as.numeric(yy) #시작연도
#   t<-as.numeric(5) #향후 몇년? 일단 5년
#   n<-as.numeric(n) #시뮬레이션 횟수
#   income_inc<-as.numeric(income_inc)
#   cost_inc<-as.numeric(cost_inc)
#   ben_a<-as.numeric(ben_a)
#   ben_b<-as.numeric(ben_b)
#   ben_c<-as.numeric(ben_c)
#   ben_d<-as.numeric(ben_d)
#   MVO_num <- as.numeric(MVO_num)
#   RF<-as.numeric(RF)
#   
#   mydata<-simulation_con_hedged(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n)
#   fund_total <- Fund()[['재정수지전망']]
#   #19년부터 5년 가정
#   year<-c((yy+1):(yy+t))
#   cost<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(총지출)
#   income<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(국민연금수입)
#   
#   asset<-filter(fund_total, 연도==yy)%>%select(적립기금)
#   asset_simul<-data.frame(matrix(nrow=t+1,ncol=MVO_num))
#   
#   ratio_simul<-data.frame(matrix(nrow=t,ncol=n))
#   
#   asset_simul[1,]<-asset
#   
#   ratio_sd<-data.frame()
#   weight<-data.frame()
#   
#   mydata<-mydata[[hedge_num]]
#   for(i in 1:n) {
#     
#     MeanRet <- colMeans(data.frame(mydata[[i]][3:6]))
#     COV_mat <- cov(data.frame(mydata[[i]][3:6]))
#     SeedingNum<-i
#     Result<- MVOmaker_year(MVO_num,RF,MeanRet , COV_mat,SeedingNum)
#       
#     for(m in 1:MVO_num){
#       for (j in 1:t){
#         Return1<-Result$pfvalue$Return[m]
#         
#         asset_simul[j+1,m]<-asset_simul[j,m]+income[j,]-cost[j,]+asset_simul[j,m]*(Return1)
#         ratio_simul[j,m]<-cost[j,]/asset_simul[j,m]
#         ratio_sd[m,1]<-sd(ratio_simul[,m])
#         num<-which.min(as.matrix(ratio_sd))
#       }   
#     }
#     weight<-rbind(weight,Result$pfvalue[num,])
#     
#   }
#   mean_weight <- data.frame(describe(weight))[,c("mean","min","max","sd")]
#   
#   meanOutput <- t(data.frame(mean_weight)['mean'])
#   
#   meanper <- apply(data.frame(t(meanOutput[,1:(ncol(meanOutput)-1)])),2,convert2ratio)
#   
#   meanper<-data.frame(t(meanper))
#   
#   SharpeCol <- data.frame(meanOutput[,ncol(meanOutput)])
#   
#   colnames(SharpeCol) <- c("Sharpe Ratio")
#   
#   merr <-cbind(meanper,SharpeCol)
#   rownames(merr)<-c("최소 변동성 자산배분안 평균")
#   
#   my_list<-list('table'=merr, 'weight'=weight)
#   return(my_list)
# }

random_simul_select<-function(Path,yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,RF, hedge_num,showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive){
  yy<-as.numeric(yy) #시작연도
  t<-as.numeric(5) #향후 몇년? 일단 5년
  n<-as.numeric(n) #시뮬레이션 횟수
  income_inc<-as.numeric(income_inc)
  cost_inc<-as.numeric(cost_inc)
  ben_a<-as.numeric(ben_a)
  ben_b<-as.numeric(ben_b)
  ben_c<-as.numeric(ben_c)
  ben_d<-as.numeric(ben_d)
  MVO_num <- as.numeric(MVO_num)
  RF<-as.numeric(RF)
  hedge_num<-as.numeric(hedge_num)
  
  mydata<-simulation_con_hedged(Path,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n)
  fund_total <- Fund(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive)[['재정수지전망']]
  #19년부터 5년 가정
  year<-c((yy+1):(yy+t))
  cost<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(총지출)
  income<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(국민연금수입)
  
  asset<-filter(fund_total, 연도==yy)%>%select(적립기금)
  asset_simul<-data.frame(matrix(nrow=t+1,ncol=MVO_num))
  
  ratio_simul<-data.frame(matrix(nrow=t,ncol=n))
  
  asset_simul[1,]<-asset
  
  ratio_sd<-data.frame()
  weight<-data.frame()
  
  
  real_hedge_num<-(hedge_num/10)+1
  
  mydata<-mydata[[real_hedge_num]]
  
  for(i in 1:n) {
    
    MeanRet <- colMeans(data.frame(mydata[[i]][3:6]))
    COV_mat <- cov(data.frame(mydata[[i]][3:6]))
    SeedingNum<-i
    Result<- MVOmaker_year(MVO_num,RF,MeanRet , COV_mat,SeedingNum)
    
    for(m in 1:MVO_num){
      for (j in 1:t){
        Return1<-Result$pfvalue$Return[m]
        
        asset_simul[j+1,m]<-asset_simul[j,m]+income[j,]-cost[j,]+asset_simul[j,m]*(Return1)
        ratio_simul[j,m]<-cost[j,]/asset_simul[j,m]
        ratio_sd[m,1]<-sd(ratio_simul[,m])
        num<-which.min(as.matrix(ratio_sd))
      }   
    }
    weight<-rbind(weight,Result$pfvalue[num,])
    
  }
  mean_weight <- data.frame(describe(weight))[,c("mean","min","max","sd")]
  
  meanOutput <- t(data.frame(mean_weight)['mean'])
  
  meanper <- apply(data.frame(t(meanOutput[,1:(ncol(meanOutput)-1)])),2,convert2ratio)
  
  meanper<-data.frame(t(meanper))
  
  SharpeCol <- data.frame(meanOutput[,ncol(meanOutput)])
  
  colnames(SharpeCol) <- c("Sharpe Ratio")
  
  merr <-cbind(meanper,SharpeCol)
  rownames(merr)<-c("최소 변동성 자산배분안 평균")
  
  my_list<-list('table'=merr, 'weight'=weight)
  return(my_list)
}

random_simul_hedged<-function(Path,yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,RF,showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive){
  yy<-as.numeric(yy) #시작연도
  t<-as.numeric(5) #향후 몇년? 일단 5년
  n<-as.numeric(n) #시뮬레이션 횟수
  income_inc<-as.numeric(income_inc)
  cost_inc<-as.numeric(cost_inc)
  ben_a<-as.numeric(ben_a)
  ben_b<-as.numeric(ben_b)
  ben_c<-as.numeric(ben_c)
  ben_d<-as.numeric(ben_d)
  MVO_num <- as.numeric(MVO_num)
  RF<-as.numeric(RF)
  
  mydata<-simulation_con_hedged(Path,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n)
  fund_total <- Fund(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive)[['재정수지전망']]
  #19년부터 5년 가정
  year<-c((yy+1):(yy+t))
  
  cost<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(총지출)
  income<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(국민연금수입)
  asset<-filter(fund_total, 연도==yy)%>%select(적립기금)
  asset_simul<-data.frame(matrix(nrow=t+1,ncol=MVO_num))
  
  ratio_simul<-data.frame(matrix(nrow=t,ncol=n))
  asset_simul[1,]<-asset
  
  ratio_sd<-data.frame()
  weight<-data.frame()
  p<-list()
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
    }
    mean_weight <- data.frame(describe(weight))[,c("mean","min","max","sd")]
    
    meanOutput <- t(data.frame(mean_weight)['mean'])
    
    meanper <- apply(data.frame(t(meanOutput[,1:(ncol(meanOutput)-1)])),2,convert2ratio)
    
    meanper<-data.frame(t(meanper))
    
    SharpeCol <- data.frame(meanOutput[,ncol(meanOutput)])
    
    colnames(SharpeCol) <- c("Sharpe Ratio")
    
    merr <-cbind(meanper,SharpeCol)
    rownames(merr)<-c("최소 변동성 자산배분안 평균")
    
    p<-append(p,list(merr))
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
                '100% 헤지 자산배분안 평균'=p[11] )#, 'weight'=weight, 'value'=q)
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
parametric <- function(VarConfidence, ManageTime, VarUse, Exposure, mean, sd, var_n) {
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
    
    p<- ggplot(data.frame(x=c(mean*100-sd*500,mean*100+sd*500)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean*100, sd=sd*100) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean*100, sd=sd*100), xlim=c(mean*100-sd*500, qnorm(1-VarConfidence/100, mean*100, sd*100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100, sd*100)-0.5, y=0.33), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100, sd*100)-0.5, y=0.3), label=VARpercent_str)+
      ggtitle(paste("모수적 방법 VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100, mean*100, sd*100), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk(%)", y="Prob")
    
  }else if(as.numeric(VarUse) == 2) # 비모수적 방법 사용(몬테카를로)
  {
    VARpercent<-(mean*100 + sd*100*rnorm(var_n))*sqrt(ManageTime)
    
    VARpercent<-quantile(VARpercent, (1-VarConfidence/100))
    VARpercent<-abs(VARpercent)
    VARpercent_str<- paste(as.character(round(VARpercent,3)),"%")
    
    X<-seq(mean*100-sd*100,mean*100+sd*100,length=200)
    Prob<-dnorm(X,mean=mean*100,sd=sd*100)
    
    #gvar=paste(as.character(round(VARpercent,digits=4)))
    #Pgvar=paste(as.character(round(PrinVAR,digits=3)))
    
    p<- ggplot(data.frame(x=c(mean*100-sd*500,mean*100+sd*500)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean*100, sd=sd*100) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean*100, sd=sd*100), xlim=c(mean-sd*10, qnorm(1-VarConfidence/100, mean=mean*100, sd=sd*100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean=mean*100, sd=sd*100)-sd*100, y=max(Prob)*1/2), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean=mean*100, sd=sd*100)-sd*100, y=max(Prob)*0.8/2), label=VARpercent_str)+
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
     data<-hedgedbenchdata()[[1]]
      
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
    
   
    p<- ggplot(data.frame(x=c(mean*100-sd*500,mean*100+sd*500)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean*100, sd=sd*100) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean*100, sd=sd*100), xlim=c(mean*100-sd*500, qnorm(1-VarConfidence/100, mean*100, sd*100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100,sd*100)-0.1, y=0.33), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean*100,sd*100)-0.1, y=0.3), label=VARpercent_str)+
      ggtitle(paste("EWMA VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100, mean*100, sd*100), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk", y="Prob")
  }
  
  
  PVAR <- VARpercent*Exposure #금액VAR
  
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
  shortfallp <- pnorm(SV*100, mean=UseMean*100, sd=UseStdev*100)
  
  
  Annual.Return<-seq(UseMean*100-UseStdev*500,UseMean*100+UseStdev*500,length=200)
  Prob<-dnorm(Annual.Return,mean=UseMean*100,sd=UseStdev*100)
  
  ForText <- max(Prob)*1/2
  ForProb <- max(Prob)*0.8/2
  
  rfrisk <- paste(as.character(round(shortfallp,digits=4)*100),"%")
  
  ThresholdLine <- qnorm(Risk1_Limit , mean = UseMean*100 , sd = UseStdev*100)
  
  p<- ggplot(data.frame(x=c(UseMean*100-UseStdev*500,UseMean*100+UseStdev*500)), aes(x=x))+
    stat_function(fun=dnorm, args=list(mean=UseMean*100, sd=UseStdev*100) ,size =1)+
    geom_area(stat='function', fun=dnorm, args=list(mean=UseMean*100, sd=UseStdev*100), xlim=c(UseMean*100-UseStdev*500, SV*100), fill='gray75') +
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
Filtering_MVO <- function( TotalDF, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2Max){
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
    if(SFRiskReturn(UseMeans,UseStdevs,Risk1Th,Risk1Yr)<0.01 * Risk1_Limit/100 & 
       SFRiskReturn(UseMeans,UseStdevs,Risk2Th,Risk2Yr)<0.01 * Risk2_Limit/100){
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
  if(filter!="NA"){
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
    
    ExceptSharpe <- apply(FilteredOutput[,1:(ncol(FilteredOutput)-1)],2,convert2ratio)
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

Simul_Run<- function(MVO_num, RF, income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit,VarConfidence, ManageTime, VarUse, Exposure)
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
  
  
  random_simul_<-random_simul_hedged(Path,yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,RF,showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive)
  pf<-random_simul_select(Path,yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,RF, hedge_num,showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive)
  
  mean_table<-pf$table
  portfolio_values<-pf$weight
  port_mean<-colMeans(portfolio_values[5])
  port_sd<-colMeans(portfolio_values[6])
  
  filter<-Filtering_MVO( portfolio_values,  Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit)
  
  Filteredresult_output<-Filtered_MVO(portfolio_values , Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1_Limit,Risk2_Limit )
  Sharpepoint<-(Filteredresult_output$ShapreMaxPoints)
  
  FirstRiskPlot_sh <- RiskPlot_(filtered$ShapreMaxPoints,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit)
  FirstRiskPlot_va <- RiskPlot_(filtered$VariancePoint,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit)
  
  SecondRiskPlot_sh<-RiskPlot_(filtered$ShapreMaxPoints,Risk2Name,Risk2Th,Risk2Yr, Risk2_Limit)
  SecondRiskPlot_va<-RiskPlot_(filtered$VariancePoint,Risk2Name,Risk2Th,Risk2Yr, Risk2_Limit)
  
  mylist<-list("hedged"=random_simul_, "pfweight"=portfolio_values, "port_mean"=port_mean, "port_sd"=port_sd, "mean_table"=mean_table, "average_weight_con"=Filteredresult_output$WeightTable, "FirstRiskPlot"=FirstRiskPlot_sh, "SecondRiskPlot"=SecondRiskPlot_sh)
  return(mylist)
  }
