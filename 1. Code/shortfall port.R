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

## MVO 를 그리는게 아닌 PF Value를 Return 해주는 함수
MVOmaker_2 <- function(Path,BenchUse,RF,ExUse,StdUse,CorrUse,MVO_num,SeedingNum,depositfix,fixedvalue){
  
  #UsingParamter
  #Path , BenchUse, RF , EXUse, StdUse , CorrUse, MVO_num, SeedingNum
  # 벤치마크 사용여부로 Asset Name 지정 (Excel Sheet 불러오기 위함)
  if(as.numeric(BenchUse) == 1){ # 벤치마크 사용
    AssetName <- "벤치마크"
  }else if(as.numeric(BenchUse) == 2){ # 벤치마크 미사용
    AssetName <- "자산"
  }
  # 어떻게 Mean Vector 를 구성할지 정함.
  if(as.numeric(ExUse) == 1){ # Calculated Mean
    
    DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
    
    ItemName <- unname(unlist(DF[1,-1]))
    price_data <- DF[-1,]
    
    price_data <- cbind(lapply(price_data[1], A), price_data[2:(length(ItemName)+1)])
    colnames(price_data) <- append(ItemName, 'Dates', after = 0)
    
    price_data <- data.frame(price_data, row.names = 1)
    price_data <- data.frame(apply(price_data, 2, B), row.names = row.names(price_data))
    
    mean_ret <- colMeans(price_data,na.rm = TRUE)/21
    
  }else if(as.numeric(ExUse) == 2){ # Calculated Median
    DF <-excel(Path,sheet = paste(AssetName,"Return"))
    ItemName <- unname(unlist(DF[1,-1]))
    
    price_data <- DF[-1,]
    
    price_data <- cbind(lapply(price_data[1], A), price_data[2:(length(ItemName)+1)])
    colnames(price_data) <- append(ItemName, 'Dates', after = 0)
    
    price_data <- data.frame(price_data, row.names = 1)
    price_data <- data.frame(apply(price_data, 2, B), row.names = row.names(price_data))
    
    mean_ret <- colMedians(data.matrix(price_data),na.rm = TRUE)
    mean_ret <- t(data.frame(mean_ret))
    colnames(mean_ret) = ItemName
    mean_ret <- mean_ret[1,]/21
    
  }else if(as.numeric(ExUse)==3){ # Given Mean
    MeanInfo <- data.frame(read_excel(Path,sheet = paste(AssetName,"평균,변동성")))
    ItemName <- MeanInfo[,1]
    mean_ret <- MeanInfo[,"기대수익률"]/25200
    names(mean_ret) <- ItemName
  }
  
  # 어떻게 Correlation Matrix 구성할지 정함.
  # 1) 변동성 
  if(as.numeric(StdUse) == 1){
    STD_ret <- sapply(na.omit(price_data),sd)/sqrt(21)
  }else if(as.numeric(StdUse) == 2){
    StdInfo <- data.frame(read_excel(Path,sheet = paste(AssetName,"평균,변동성")))
    ItemName <- StdInfo[,1]
    STD_ret <- StdInfo[,"표준편차"]/(100 * sqrt(252))
    names(STD_ret) <- ItemName
  }
  
  # 2) Corrleation
  if(as.numeric(CorrUse) == 1){
    cor_ret <- cor(na.omit(price_data))
  }else if(as.numeric(CorrUse) == 2){
    cor_ret <- data.frame(read_excel(Path,sheet = paste(AssetName,"상관관계")))
    rownames(cor_ret)<- cor_ret$...1
    cor_ret$...1<-NULL
  }
  # 3) Covariance Matrix
  cov_mat <- as.matrix(cor_ret *(as.matrix(STD_ret) %*% t(as.matrix(STD_ret))) * 252)
  
  ## set seed number
  set.seed(SeedingNum)
  
  if(depositfix == TRUE){
    wts <- runif(n = length(ItemName)-1)
    wts <- wts/sum(wts)*(1-fixedvalue)
    wts<- append(fixedvalue,wts)
  }else{
    wts <- runif(n = length(ItemName))
    wts <- wts/sum(wts)
  }
  
  #PortFolio Return
  port_returns <- sum(wts * mean_ret)* 252
  port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
  
  sharpe_ratio <- (port_returns-RF/100)/port_risk
  
  # Creating Empty Vector
  num_port <- MVO_num
  
  all_wts <- matrix(nrow = num_port,ncol = length(ItemName))
  port_returns <- vector('numeric', length = num_port)
  port_risk <- vector('numeric', length = num_port)
  sharpe_ratio <- vector('numeric', length = num_port)
  
  # Simulating Process
  set.seed(2020)
  for (i in seq_along(port_returns)) {
    
    if(depositfix == TRUE){
      wts <- runif(n = length(ItemName)-1)
      wts <- wts/sum(wts)*(1-fixedvalue)
      wts<- append(fixedvalue,wts)
    }else{
      wts <- runif(n = length(ItemName))
      wts <- wts/sum(wts)
    }
    
    all_wts[i,] <- wts
    
    # Portfolio returns
    
    port_ret <- sum(wts * mean_ret)* 252
    
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
  return(my_list)
}



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
  
  #UseMean<- as.numeric(Mean)/100 #기대수익률 
  #UseStdev<- as.numeric(Stdev)/100 #변동성 
  SV <- as.numeric(Risk1Th)
  
  UseMean <- UseMean*Risk1Yr
  UseStdev<-   UseStdev*Risk1Yr
  shortfallp <- pnorm(SV, mean=UseMean*100, sd=UseStdev*100)
  
  Annual.Return<-seq(UseMean*100-UseStdev*500,UseMean*100+UseStdev*500,length=200)
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
Filtering_MVO <- function(Path , TotalDF, mean_ret, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max, Target1L , ILMax,Resampled_or_True){
  Risk1Th <- as.numeric(Risk1Th)
  Risk2Th <- as.numeric(Risk2Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk2Yr <- as.numeric(Risk2Yr)
  Risk1Max <- as.numeric(Risk1Max)
  Risk2Max <- as.numeric(Risk2Max)
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
    #UseWeight <- portfolio_values[k,1:(ncol(portfolio_values)-3)]
    
    # 각 Random Portfolio 결과들중 해당 조건들에 맞는 부분만 추출
    # 1) Risk 1 과 Risk 2는 허용 위험 한도보다 낮을 확률을 계산하여
    # 해당 결과보다 높은 위험 확률을 주는 결과들은 제거 함.
    # 2) IL Risk 는 운용배수 관련 위험으로 랜덤 결과에서 나오는 위험도가
    # 처음에 넣는 위험보다 높은 결과 제거
    if(SFRiskReturn(UseMeans,UseStdevs,Risk1Th/100,Risk1Yr)<0.01 * Risk1Max & 
       SFRiskReturn(UseMeans,UseStdevs,Risk2Th/100,Risk2Yr)<0.01 * Risk2Max ){
       #ILRisk_Return(Path , UseWeight,mean_ret,UseMeans, Target1L,Resampled_or_True) < 0.01 * ILMax){
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

Filtered_MVO<-function(EFFs,Path , BenchUse , ExUse , NeedRet, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max){
 
NeedRet <- as.numeric(NeedRet)
Risk1Th <- as.numeric(Risk1Th)
Risk2Th <- as.numeric(Risk2Th)
Risk1Yr <- as.numeric(Risk1Yr)
Risk2Yr <- as.numeric(Risk2Yr)
Risk1Max <- as.numeric(Risk1Max)
Risk2Max <- as.numeric(Risk2Max)

x_var <- c()
x_sh <- c()
for_index <-c()
Points_Sharpe_ <- data.frame()
Points_Var_ <- data.frame()

filter<-Filtering(Path , portfolio_values, mean_ret, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max)
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
my_list <- list("WeightTable" = FilteredOutput , "ShapreMaxPoints" = TotalDF$SharpePoints )
return(my_list)
}

pf<-MVOmaker_2(Path,BenchUse,RF,ExUse,StdUse,CorrUse,MVO_num,SeedingNum,depositfix,fixedvalue)
portfolio_values<-pf$pfvalue

filter<-Filtering_MVO(Path , portfolio_values, mean_ret, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max)

Filteredresult_output<-Filtered_MVO(portfolio_values,Path , BenchUse , ExUse , NeedRet, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max )
Sharpepoint<-(Filteredresult_output$ShapreMaxPoints)

FirstRiskPlot <- RiskPlot_(Sharpepoint,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit)
SecondRiskPlot<-RiskPlot_(Sharpepoint,Risk2Name,Risk2Th,Risk2Yr, Risk2_Limit)
