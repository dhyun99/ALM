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

# Making MVO by simulation method 
MVOmaker <- function(Path , BenchUse, RF , ExUse, StdUse , CorrUse, MVO_num, SeedingNum,depositfix,fixedvalue){
  
  # Parameter type convert character to numeric
  RF <- as.numeric(RF)
  BenchUse <- as.numeric(BenchUse)
  ExUse <- as.numeric(ExUse)
  StdUse <- as.numeric(StdUse)
  CorrUse <- as.numeric(CorrUse)
  MVO_num <- as.numeric(MVO_num)
  SeedingNum <- as.numeric(SeedingNum)
  depositfix <- as.numeric(depositfix)
  fixedvalue <- as.numeric(fixedvalue)
  
  # 1. 필요 Stat 계산 및 변수로 저장
  ## if fixed deposit ratio fixed, else deposit ratio are not fixed
  if(depositfix == 1){
    depositfix <- TRUE
  }else{depositfix <- FALSE}
  
  ## 벤치마크 사용여부로 Asset Name 지정 (Excel Sheet 불러오기 위함)
  if(as.numeric(BenchUse) == 1){ # 벤치마크 사용
    AssetName <- "벤치마크"
  }else if(as.numeric(BenchUse) == 2){ # 벤치마크 미사용
    AssetName <- "자산"
  }
  
  ## 어떻게 Mean Vector 를 구성할지 정함.
  if(as.numeric(ExUse) == 1){ # Calculated Mean
    
    DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
    
    ItemName <- unname(unlist(DF[1,-1]))
    price_data <- DF[-1,]
    
    price_data <- cbind(lapply(price_data[1], A), price_data[2:(length(ItemName)+1)])
    colnames(price_data) <- append(ItemName, 'Dates', after = 0)
    
    price_data <- data.frame(price_data, row.names = 1)
    price_data <- data.frame(apply(price_data, 2, B), row.names = row.names(price_data))
    
    mean_ret <- colMeans(price_data,na.rm = TRUE)/21 # Divied by 21 as it is monthly return
    
  }else if(as.numeric(ExUse) == 2){
    # Calculated Median
    DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
    
    ItemName <- unname(unlist(DF[1,-1]))
    
    price_data <- DF[-1,]
    
    price_data <- cbind(lapply(price_data[1], A), price_data[2:(length(ItemName)+1)])
    colnames(price_data) <- append(ItemName, 'Dates', after = 0)
    
    price_data <- data.frame(price_data, row.names = 1)
    price_data <- data.frame(apply(price_data, 2, B), row.names = row.names(price_data))
    
    mean_ret <- colMedians(data.matrix(price_data),na.rm = TRUE)
    mean_ret <- t(data.frame(mean_ret))
    colnames(mean_ret) = ItemName
    mean_ret <- mean_ret[1,]
    
    mean_ret <- mean_ret/21
  }else if(as.numeric(ExUse)==3){ # Given Mean
    
    MeanInfo <- data.frame(read_excel(Path,sheet = paste(AssetName,"평균,변동성")))
    
    ItemName <- MeanInfo[,1]
    mean_ret <- MeanInfo[,"기대수익률"]/25200
    names(mean_ret) <- ItemName
  }
  
  ## 어떻게 Correlation Matrix 구성할지 정함.
  ## 1) 변동성 
  if(as.numeric(StdUse) == 1){
    STD_ret <- sapply(na.omit(price_data),sd)
    STD_ret <- STD_ret/sqrt(21)
    
  }else if(as.numeric(StdUse) == 2){
    
    StdInfo <- data.frame(read_excel(Path,sheet = paste(AssetName,"평균,변동성")))
    
    ItemName <- StdInfo[,1]
    STD_ret <- StdInfo[,"표준편차"]/(100 * sqrt(252))
    names(STD_ret) <- ItemName
  }
  
  ## 2) Corrleation
  if(as.numeric(CorrUse) == 1){
    cor_ret <- cor(na.omit(price_data))
  }else if(as.numeric(CorrUse) == 2){
    
    cor_ret <- data.frame(read_excel(Path,sheet = paste(AssetName,"상관관계")))
    
    rownames(cor_ret)<- cor_ret$...1
    cor_ret$...1<-NULL
  }
  
  ## 3) Covariance Matrix
  cov_mat <- cor_ret *(as.matrix(STD_ret) %*% t(as.matrix(STD_ret))) * 252
  
  # 2. Simulation Process
  
  ## Set seed
  set.seed(SeedingNum)
  
  ## weight control by deposit fix option
  if(depositfix == TRUE){
    wts <- runif(n = length(ItemName)-1)
    wts <- wts/sum(wts)*(1-fixedvalue)
    wts<- append(fixedvalue,wts)
  }else{
    wts <- runif(n = length(ItemName))
    wts <- wts/sum(wts)
  }
  
  ## PortFolio Return
  port_returns <- sum(wts * mean_ret)* 252
  port_risk <- sqrt(t(wts) %*% (as.matrix(cov_mat) %*% wts))
  
  sharpe_ratio <- (port_returns-RF/100)/port_risk
  
  ## Creating Empty Vector
  num_port <- MVO_num
  all_wts <- matrix(nrow = num_port,ncol = length(ItemName))
  port_returns <- vector('numeric', length = num_port)
  port_risk <- vector('numeric', length = num_port)
  sharpe_ratio <- vector('numeric', length = num_port)
  
  # Simulating Process
  
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
    port_sd <- sqrt(t(wts) %*% (as.matrix(cov_mat)  %*% wts)) # cov_mat 은 annualized 되어 있어서 252 안곱함.
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
  
  colnames(all_wts) <- ItemName
  
  # Combing all the values together
  portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))
  
  min_var <- portfolio_values[which.min(portfolio_values$Risk),]
  max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
  
  #Ploting
  p <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = "Portfolio Optimization & Efficient Frontier") +
    geom_point(aes(x = Risk,
                   y = Return), data = min_var, color = 'red') +
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'red') + 
    annotate('text', x = max_sr$Risk + 0.015, y = max_sr$Return + 0.01, label = "Tangency Portfolio" , size = 4.5) +
    annotate('text', x = min_var$Risk+0.015, y = min_var$Return - 0.01, label = "Minimum variance portfolio",size = 4.5) +
    annotate("segment", x=max_sr$Risk + 0.015, xend=max_sr$Risk, y=max_sr$Return+0.01, yend=max_sr$Return    , size=1, colour="red", arrow=arrow())+
    annotate("segment", x=min_var$Risk + 0.015, xend=min_var$Risk, y=min_var$Return-0.01, yend=min_var$Return    , size=1, colour="red", arrow=arrow())
  
  return(p)
}

# Step1. Mean Return 과 Cov 를 통해 무작위 수익률을 만들어냄
ReturnSimulator <- function(Path , BenchUse,ExUse,StdUse,CorrUse){
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
    
    DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
    
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
  cov_mat <- cor_ret *(as.matrix(STD_ret) %*% t(as.matrix(STD_ret))) # Daily Scale
  
  SimulatedRet <- mvtnorm::rmvnorm(252*5,mean = mean_ret, sigma = as.matrix(cov_mat)) # Daily Scale의 mean_ret,cov_mat 넣어서 Simulation
  
  colnames(SimulatedRet) <- colnames(cov_mat)
  return(SimulatedRet)
}

## Random Mean을 정확하게 Sample mean에 맞추기 위해 아래 함수를 통해 그에 맞는 Return 생성
RealreturnSimul <- function(Path , BenchUse,ExUse,StdUse,CorrUse){
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
    
    DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
    
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
  cov_mat <- cor_ret *(as.matrix(STD_ret) %*% t(as.matrix(STD_ret)))
  SimulatedRet <- mvrnorm(n=252.0*5, as.numeric(mean_ret), cov_mat, empirical = TRUE) # 5년치 수익률 생성 
  
  colnames(SimulatedRet) <- colnames(cov_mat)
  
  return(SimulatedRet)
}

## Random 생성된 Return을 담는 List 생성
ResamplingReturn <- function(Path , BenchUse,ExUse,StdUse,CorrUse,Resamplesim_num,SeedingNum){
  # Set Seed
  set.seed(SeedingNum)
  TotSimul<-c()
  for(i in 1:Resamplesim_num){
    # Make Random Return
    ResampleRet_raw <- ReturnSimulator(Path , BenchUse,ExUse,StdUse,CorrUse)
    # Save Random Return to list
    TotSimul <- append(TotSimul,list(ResampleRet_raw))
  }
  return(TotSimul)
}

# Step 2. 각 Random Return 별 EFF 생성
## EFF frontier 계산하는 함수 
eff.frontier <- function (returns, short="no", max.allocation=NULL,
                          risk.premium.up=.5, risk.increment=.005){
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short
  # selling prohibited)max.allocation is the maximum % allowed for any one
  # security (reduces concentration) risk.premium.up is the upper limit of the
  # risk premium modeled (see for loop below) and risk.increment is the
  # increment (by) value used in the for loop
  
  covariance <- cov(returns, use="complete.obs") * 252
  n <- ncol(covariance)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Calculate the number of loops
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    
    dvec <- colMeans(returns,na.rm = TRUE)*252.0 * i # This moves the solution along the EF
    sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns,na.rm = TRUE) * 252.0)
    eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
}

# 각 Random Return별 EFF 결과 저장
EFFsMaker_Fixed<- function(TotSimul){
  Points_Sharpe <- data.frame()
  Points_Var <- data.frame()
  EFFResult <- c()
  TotalEFFs <- c()
  
  # Calculate True return EFF by eff.frontier
  eff_real <- eff.frontier(returns=TrueRet, short="no", max.allocation=.99,
                           risk.premium.up=1, risk.increment=.001)
  
  # 음수를 포함하는 Weight 모두 제거
  eff_real <- subset(eff_real , "국내채권" >0 &  "국내주식" >0 & "해외채권" >0 & "해외주식" >0)
  eff_real <- subset(eff_real, apply(eff_real, 1, function(x){all(x > 0)}))
  
  for(j in 1:length(TotSimul)){
    
    SampledRet <- data.frame(TotSimul[j])
    
    # Calculate EFF points by eff.frontier function
    
    eff <- eff.frontier(returns=SampledRet, short="no", max.allocation=.99,
                        risk.premium.up=1, risk.increment=.001)
    
    
    # 음수를 포함하는 Weight 모두 제거
    eff <- subset(eff , "국내채권" >0 &  "국내주식" >0 & "해외채권" >0 & "해외주식" >0)
    eff <- subset(eff, apply(eff, 1, function(x){all(x > 0)}))
    
    
    TotalEFFs <- append(TotalEFFs, list(eff))}
  
  my_list <- list("TotalEFFs" = TotalEFFs , "RealEFF" = eff_real)
  return(my_list)
}

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
  set.seed(2021)
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

## Mean vector와 Cov vector를 주면 MVO를 만들어 주는 함수
MVOmaker_3 <- function(depositfix ,fixedvalue ,MVO_num, RF, mean_ret , cov_mat,SeedingNum){
  suppressWarnings({
    
    set.seed(SeedingNum)
    ItemName<- colnames(cov_mat)
    if(depositfix == TRUE){
      wts <- runif(n = length(ItemName)-1)
      wts <- wts/sum(wts)*(1-fixedvalue)
      wts<- append(fixedvalue,wts)
    }else{
      wts <- runif(n = length(ItemName))
      wts <- wts/sum(wts)
    }
    
    
    #PortFolio Return
    port_returns <- sum(wts * mean_ret)* 252 # Mean return은 Daily Scale 로 오기 때문에 252 곱하여 Annualize
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
  })
  return(my_list)
}

## Bin을 만들어주는 함수
MakeBin<- function(TotalDF,Result , RealRet){
  PFret<- Result$pfvalue
  mean_ret<- Result$mean
  cov_mat<- Result$cov
  
  # Calculate Real Eff point
  eff_real <- eff.frontier(returns=RealRet, short="no", max.allocation=.90,
                           risk.premium.up=1, risk.increment=.001)
  
  # Find the optimal portfolio (Sharpe Max)
  eff_real.optimal.point <- eff_real[eff_real$sharpe==max(eff_real$sharpe),]
  
  # 구간 100개의 Bin 으로 나눔.
  divnum <- 100
  Bin<- seq(from=min(PFret$Risk), to=max(PFret$Risk), length.out = divnum)
  ColsVec <- colnames(eff_real)
  ColsVec <- ColsVec[1:(length(ColsVec)-3)]
  
  AverageW_Total<- data.frame()
  
  for(i in 1:(length(Bin)-1)){
    # Define Bin's range
    lb = Bin[i]
    ub = Bin[i+1]
    
    Average <- colMeans(filter(TotalDF, Std.Dev >= lb,Std.Dev < ub))
    MeanW<- Average[ColsVec] # ColsVec: 사용하는 자산 이름
    MeanW <- MeanW/sum(MeanW) # Make wum of weight 1 
    MeanW<- data.frame(MeanW) # Convert to dataframe type
    colnames(MeanW) <- i
    AverageW_Total<- append(AverageW_Total , MeanW)
    
  }
  
  ResampledPF_mean <- c()
  ResampledPF_stdev <- c()
  ResampledPF_weight <- data.frame()
  
  for(i in 1:length(AverageW_Total)){
    # Weight Vector = ExWeight
    ExWeight<-data.frame(AverageW_Total[i])
    ExWeight<- data.frame(t(ExWeight))
    colnames(ExWeight) <- ColsVec
    
    # Weight에 음수 포함하면 Pass
    if(nrow(ExWeight[ExWeight<0,]) != 0){
      next
    }
    
    # Calculate Portfolio Stats by Weight
    ResampledPF_weight <- rbind(ResampledPF_weight , ExWeight)
    ResampledPF_mean<- append(ResampledPF_mean , c(sum(ExWeight * mean_ret)* 252))
    
    ExWeight<- as.vector(t(ExWeight))
    PFstdev<- sqrt(t(ExWeight) %*% (cov_mat  %*% ExWeight))
    ResampledPF_stdev<-append(ResampledPF_stdev, PFstdev[1,1])
  }
  
  my_list <- list("weight" = ResampledPF_weight, "mean" = ResampledPF_mean, "stdev" = ResampledPF_stdev)
  
  return(my_list)
}

## Random Simulation 결과에서 EFF를 만들어 주는 함수(eff.frontier와는 다르게
## simulation point 에서 가장 왼쪽 점들 정보만 저장)
## 직접 계산하는게 아니라 Simulation 결과에서 뽑아내는 함수임.
EFF_extract <- function(Result){
  PFs <- Result$pfvalue
  NextDF<- copy(PFs)
  EFF_re_stdev <- c()
  EFF_re_mu <- c()
  EFFweight <- data.frame()
  
  # Simulation point들 중 EFF에 맞는 점이 없어 질 때까지 Random 반복
  while(1){
    MinStdev <- min(NextDF$Risk)
    MinMean <- filter(NextDF , Risk == MinStdev)$Return
    EFF_re_stdev <- append(EFF_re_stdev , MinStdev)
    EFF_re_mu <- append(EFF_re_mu , MinMean)
    Minweight<- data.frame(filter(NextDF , Risk == MinStdev)[1:(length(colnames(NextDF))-3)])
    EFFweight <- rbind(EFFweight, Minweight)
    if(MinMean == max(NextDF$Return)){
      break
    }
    NextDF<- filter(NextDF, Return > MinMean , Risk>MinStdev)
    if(nrow(NextDF) == 0){
      break
    }
  }
  EFFReal <- data.frame(cbind(EFF_re_stdev , EFF_re_mu))
  colnames(EFFReal) <- c("Std.Dev","Exp.Return")
  my_list <- list("EFFReal" = EFFReal , "Weight" = EFFweight)
}

## EFF 모음 Plot 만드는 함수
PlotEFFs <- function(TotalDF, ResampledPF_weight,ResampledPF_mean , ResampledPF_stdev) {
  ResampledPF <- data.frame(ResampledPF_mean,ResampledPF_stdev)
  min_index <- which.min(ResampledPF_stdev)
  max_index<-which.max(ResampledPF_mean)
  
  colnames(ResampledPF)<-c("Mean","Stdev")
  ResampledPF<- data.frame(drop_na(ResampledPF))
  
  EFF_re_stdev <- c()
  EFF_re_mu <- c()
  EFFweight <- data.frame()
  Tot <- cbind(ResampledPF_weight , ResampledPF_mean , ResampledPF_stdev)
  
  NextDF<- copy(ResampledPF)
  while(1){
    MinMean <- as.numeric(NextDF[min_index,]["Mean"])
    
    MinStdev <- as.numeric(NextDF[min_index,]["Stdev"])
    
    Minweight <- drop_na(Tot[Tot$ResampledPF_mean ==MinMean,])
    colnames(Minweight) <- as.vector(colnames(Tot))
    EFF_re_stdev <- append(EFF_re_stdev , MinStdev)
    EFF_re_mu <- append(EFF_re_mu , MinMean)
    EFFweight <- rbind(EFFweight, Minweight)
    if(MinMean == as.numeric(ResampledPF[max_index, ]["Mean"])){
      break
    }
    NextDF<- filter(ResampledPF, Mean > MinMean , Stdev>MinStdev)
    if(nrow(NextDF) == 0){
      break
    }
    min_index <- which.min(as.vector(NextDF$Stdev))
  }
  
  FORGraph <- data.frame(EFF_re_mu , EFF_re_stdev)
  colnames(FORGraph) <- c("Exp.Return" , "Std.Dev")
  
  p2<-ggplot(TotalDF, aes(x=Std.Dev, y=Exp.Return,type = "l")) + geom_point(alpha=.1, color=ealdark) +
    ggtitle("Efficient Frontier and Optimal Portfolio") +
    labs(x="Risk (standard deviation of portfolio)", y="Return") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    theme(panel.background=element_rect(fill=eallighttan),
          text=element_text(color=ealdark),
          plot.title=element_text(size=24, color=ealred)) +
    geom_line(data =FORGraph, aes(y = Exp.Return, x = Std.Dev),
              stat="identity",color = "blue",size=3)
  
  my_list <- list("EFFplot" = p2, "EFFweight" = EFFweight)
  return(my_list)
}

## Weight 함수
### STDEV 기준(x 축이)
WeightPlot<-function(EFFweight, ResampledPF_stdev){
  # create data
  itemnum<- (length(colnames(EFFweight))-2)
  itemname <- colnames(EFFweight)[1:itemnum]
  EFFweight<- arrange(EFFweight, ResampledPF_stdev)
  EFFweight<- EFFweight[!duplicated(EFFweight),]
  
  stdevrep <- as.numeric(rep(EFFweight$ResampledPF_stdev,each=itemnum))
  group <- rep(itemname,times=length(stdevrep)/itemnum)  
  value <- c()
  for(i in 1:length(EFFweight$ResampledPF_stdev)){
    value<- append(value, as.numeric(EFFweight[i,][itemname]))
  }
  data <- data.frame(stdevrep, value*100, group)
  colnames(data) = c("Standard.Deviation", "Weight" , "Group")
  # Note: compute percentages without dplyr:
  my_fun <- function(vec){ 
    as.numeric(vec[2]) / sum(data$value[data$stdevrep==vec[1]]) *100 
  }
  data$percentage <- apply(data , 1 , my_fun)
  
  p2 <- ggplot(data, aes(x=Standard.Deviation, y=Weight, fill=group)) + 
    geom_area()+
    scale_x_continuous(labels = scales::percent) +
    theme(panel.background=element_rect(fill=eallighttan),
          text=element_text(color=ealdark),
          plot.title=element_text(size=24, color=ealred))+
    ggtitle("Portfolio Weight")
}
### Mean 기준(x 축이)
WeightPlot2<-function(EFFweight, ResampledPF_mean ){
  # create data
  itemnum<- (length(colnames(EFFweight))-2)
  itemname <- colnames(EFFweight)[1:itemnum]
  EFFweight<- arrange(EFFweight, ResampledPF_mean )
  EFFweight<- EFFweight[!duplicated(EFFweight),]
  
  stdevrep <- as.numeric(rep(EFFweight$ResampledPF_mean ,each=itemnum))
  group <- rep(itemname,times=length(stdevrep)/itemnum)  
  value <- c()
  for(i in 1:length(EFFweight$ResampledPF_mean )){
    value<- append(value, as.numeric(EFFweight[i,][itemname]))
  }
  data <- data.frame(stdevrep, value*100, group)
  colnames(data) = c("Expected.Retrun", "Weight" , "Group")
  # Note: compute percentages without dplyr:
  my_fun <- function(vec){ 
    as.numeric(vec[2]) / sum(data$value[data$stdevrep==vec[1]]) *100 
  }
  data$percentage <- apply(data , 1 , my_fun)
  
  p2 <- ggplot(data, aes(x=Expected.Retrun, y=Weight, fill=group)) + 
    geom_area()+
    scale_x_continuous(labels = scales::percent) +
    theme(panel.background=element_rect(fill=eallighttan),
          text=element_text(color=ealdark),
          plot.title=element_text(size=24, color=ealred))+
    ggtitle("Portfolio Weight")
}

# return mean vector
mean_ret_maker<- function(Path , BenchUse,ExUse){
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
  }else if(as.numeric(ExUse) == 2)
  {
    DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
    
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
  return(mean_ret)
}

# EFF 결과를 조건 제약을 걸어주는 함수.
FilteredResult<-function(EFFs,Path , BenchUse , ExUse , NeedRet, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max , Target1L , ILMax , Resampled_or_True){
  
  # 변수 저장 
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
  
  TrueMean <- mean_ret_maker(Path ,BenchUse,ExUse)
  for(i in 1:length(EFFs)){
    print(i/length(EFFs))
    
    if(nrow(data.frame(EFFs[i])) != 0){
      eff<- data.frame(EFFs[i])
      # 요 최소 수익률 
      eff<- subset(eff, Exp.Return > (NeedRet/100)) # 무조건(unconstraint)일시 주석처리하고 돌리면 됨
      
      if(nrow(eff) != 0){
        FilteredDF <- Filtering(Path , eff, TrueMean , Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max ,Target1L, ILMax , Resampled_or_True)
        
        if(FilteredDF != "NA"){
          ShMax <- FilteredDF$SharpeMAx
          VarMin <- FilteredDF$VarianceMin
          
          Points_Sharpe_ <- rbind(Points_Sharpe_, ShMax)
          Points_Var_ <- rbind(Points_Var_ , VarMin)
        } else{
        }
      } else{}
    }
  }
  
  if(nrow(Points_Sharpe_) != 0){
    TotalDF <- list("SharpePoints" = Points_Sharpe_ , "VariancePoints" = Points_Var_)
    
    # 비중이 양수인 점들만 저장
    TotalDF$VariancePoints<- subset(TotalDF$VariancePoints, apply(TotalDF$VariancePoints, 1, function(x){all(x > 0)}))
    TotalDF$SharpePoints<- subset(TotalDF$SharpePoints, apply(TotalDF$SharpePoints, 1, function(x){all(x > 0)}))
    
    var_df <- data.frame(describe(TotalDF$VariancePoints))[,c("mean","min","max","sd")]
    sh_df <- data.frame(describe(TotalDF$SharpePoints))[,c("mean","min","max","sd")]
    
    x_var <- append(x_var, list(var_df))
    x_sh <- append(x_sh, list(sh_df))
    #for_index <- append(for_index,paste(toString(yr),"_",toString(THrisk)))
  } else {}
  FilteredOutput <- rbind(t(data.frame(x_sh)['mean']) , t(data.frame(x_var)['mean']))
  
  rownames(FilteredOutput) <- c("Max Sharpe Weight", "Min Variance Weight")
  
  ExceptSharpe <- apply(FilteredOutput[,1:(ncol(FilteredOutput)-1)],2,convert2ratio)
  rownames(ExceptSharpe) <- c("Average Max Sharpe Weight", "Average Min Variance Weight")
  SharpeCol <- data.frame(FilteredOutput[,ncol(FilteredOutput)])
  colnames(SharpeCol) <- c("Sharpe Ratio")
  FilteredOutput <- cbind(ExceptSharpe,SharpeCol)
  
  my_list <- list("WeightTable" = FilteredOutput , "ShapreMaxPoints" = TotalDF$SharpePoints )
  return(my_list)
}
# Constraint 적용
Filtering <- function(Path , TotalDF, mean_ret, Risk1Th, Risk2Th, Risk1Yr,Risk2Yr,Risk1Max,Risk2Max, Target1L , ILMax,Resampled_or_True){
  Risk1Th <- as.numeric(Risk1Th)
  Risk2Th <- as.numeric(Risk2Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk2Yr <- as.numeric(Risk2Yr)
  Risk1Max <- as.numeric(Risk1Max)
  Risk2Max <- as.numeric(Risk2Max)
  ILMax <- as.numeric(ILMax)
  WillUseDF <- data.frame()
  SimulSharpe <- data.frame()
  SimulVar <- data.frame()
  
  
  # 조건 걸어주는 부분
  for(k in 1:length(TotalDF[,"Exp.Return"])){
    UseMeans <- TotalDF[,"Exp.Return"][k]
    UseStdevs <- TotalDF[,"Std.Dev"][k]
    UseWeight <- TotalDF[k,1:(ncol(TotalDF)-3)]
    
    # 각 Random Portfolio 결과들중 해당 조건들에 맞는 부분만 추출
    # 1) Risk 1 과 Risk 2는 허용 위험 한도보다 낮을 확률을 계산하여
    # 해당 결과보다 높은 위험 확률을 주는 결과들은 제거 함.
    # 2) IL Risk 는 운용배수 관련 위험으로 랜덤 결과에서 나오는 위험도가
    # 처음에 넣는 위험보다 높은 결과 제거
    if(SFRiskReturn(UseMeans,UseStdevs,Risk1Th/100,Risk1Yr)<0.01 * Risk1Max & 
       SFRiskReturn(UseMeans,UseStdevs,Risk2Th/100,Risk2Yr)<0.01 * Risk2Max &
       ILRisk_Return(Path , UseWeight,mean_ret,UseMeans, Target1L,Resampled_or_True) < 0.01 * ILMax){
      WillUseDF <- rbind(WillUseDF, TotalDF[k,])
    }
  }
  if(nrow(WillUseDF) != 0 ){
    SharpeMax <- WillUseDF[which(WillUseDF$sharpe == max(WillUseDF$sharpe)),]
    VarMin <- WillUseDF[which(WillUseDF$Std.Dev == min(WillUseDF$Std.Dev)),]
    
    SimulSharpe <- rbind(SimulSharpe, SharpeMax)
    SimulVar <- rbind(SimulVar , VarMin)
    
    my_list <- list("SharpeMAx" = SimulSharpe , "VarianceMin" = SimulVar)
    
    return(my_list)
  } else{
    return("NA")
  }
  
}

# 운용 배수 분석 관련 함수
# 1L = IL 을 의미 실수로 1로 변수를 저장한 것.
## 운용배수 고려한 Risk Probablity return
ILRisk_Return <- function(Path , weight_vec,mean_ret,ResampledPFmean, Target1L , Resampled_or_True){
  # Data Import
  Target1L <- as.numeric(Target1L)
  
  fund <- data.frame(read_xlsx(Path,sheet = "보증잔액성장률"))
  #fund <- cbind(fund[1],data.frame((fund[2]/lag(fund[2])-1))) # 보증잔액 증가율
  fund_accident <- data.frame(read_xlsx(Path,sheet = "기금_사고율"))
  fund_return <- data.frame(read_xlsx(Path,sheet = "기금_수익률"))
  
  
  FundInfo <- merge(fund, fund_return,by='Date', all=TRUE)
  FundInfo <- merge(FundInfo, fund_accident,by='Date', all=TRUE)
  FundInfo <- cbind(lapply(FundInfo['Date'], function(x){as.character(x)}),FundInfo[2:length(FundInfo)])
  DF <- read_excel(Path,sheet = "자산 Return")
  
  ItemName <- unname(unlist(DF[1,-1]))
  
  price_data <- DF[-1,]
  
  price_data <- cbind(lapply(price_data[1], C), price_data[2:(length(ItemName)+1)])
  colnames(price_data) <- append(ItemName, 'Date', after = 0)
  Total <- merge(FundInfo,price_data , by='Date', all=TRUE)
  
  rownames(Total) <- Total$Date
  Total <- Total[2:ncol(Total)]
  UseTotal <- data.frame(lapply(Total, function(x){as.numeric(x)}))
  rownames(UseTotal)<- row.names(Total)
  
  
  Asset_Stat  <- data.frame(read_xlsx(Path,sheet = "자산 평균,변동성"))
  rownames(Asset_Stat) <- Asset_Stat$...1
  Asset_Stat <- Asset_Stat[,2:ncol(Asset_Stat)]
  
  #Weight_Ex <- data.frame(c(0.2,0.678,0.03,0.0313,0.0607))
  #rownames(Weight_Ex) <- rownames(Asset_Stat)
  #colnames(Weight_Ex) = "Weight"
  # Statistics
  
  ## 1) Correlation Matrix 
  CorrMat <- cor(UseTotal, use="complete.obs")
  
  ## 2) Covariance Matrix
  ## Replicating 하는 Excel 에서 sqrt(12) 를 써서 sqrt(12) 로 적용 
  ## 12 그냥 곱하는게 아닌지.. 후에 확인 필요
  CovMat <- cov(UseTotal, use="complete.obs")* sqrt(12) 
  
  ## 3) Mean Vector
  mean_vec <- colMeans(UseTotal,na.rm = TRUE)
  mean_vec <- data.frame(as.numeric(mean_vec))
  rownames(mean_vec) <- colnames(UseTotal)
  mean_vec <-t(mean_vec) * 12 
  ## 3) SD Vector
  std_vec <- sapply(na.omit(UseTotal[,1:ncol(UseTotal)]), sd)
  std_vec <- data.frame(as.numeric(std_vec))
  rownames(std_vec) <- colnames(UseTotal)
  std_vec <-t(std_vec) * sqrt(12)
  
  ## 4) 평균 보증 수익률 
  avg_backed_ret <- mean_vec[,"보증수익률"] 
  
  ## 5) 보증잔액 성장률 관련
  avg_backed_resid_growth <- mean_vec[,"보증잔액성장률"] 
  std_backed_resid_growth <- std_vec[,"보증잔액성장률"] 
  
  ## 6) 사고율 관련 
  avg_accident <- mean_vec[,"사고율"] 
  std_accident <- std_vec[,"사고율"]
  
  # ShortFall Risk 관련
  ## 1) Covar(A-g)percent 단위
  covar_A_g <-sum(Asset_Stat[,"표준편차",drop=FALSE] * t(weight_vec)*(CorrMat["보증잔액성장률",as.vector(colnames(weight_vec))] * std_backed_resid_growth))
  ## 2) Covar(A-p)percent 단위
  covar_A_p <-sum(Asset_Stat[,"표준편차",drop=FALSE] * t(weight_vec)*(CorrMat["사고율",as.vector(colnames(weight_vec))] * std_accident))
  
  if(Resampled_or_True == 1 ){
    GivenMean <- as.numeric(sum(weight_vec * mean_ret*252))
  }else if(Resampled_or_True == 2 ){
    GivenMean <- as.numeric(ResampledPFmean)
  }
  # 현재는 
  
  Mean1L <- (1/Target1L*(1+ GivenMean- avg_backed_resid_growth) + avg_backed_ret - avg_accident) *100
  
  Std1L <- sqrt((1/Target1L)^2 * ((GivenMean/100)^2 + std_backed_resid_growth^2 - 2*covar_A_g/100) + std_accident^2 - 2*(1/Target1L) * covar_A_p/100 + 2 * (1/Target1L) * CovMat["보증잔액성장률","사고율"])*100
  
  # Replicating 하는 Excel 참고
  RiskProb <- pnorm(Mean1L -0.7 , Mean1L, Std1L )
  
  return(RiskProb)
}
## 운용배수 관련 분석 중 필요한 Mean,Std 정보를 return 해줌 
MultipleAnalysis <- function(Path , Target1L , NeedRet , Weight_Ex , IL_Limit){
  
  # Data Import
  Target1L <- as.numeric(Target1L)
  NeedRet <- as.numeric(NeedRet)
  
  fund <- data.frame(read_xlsx(Path,sheet = "보증잔액성장률"))
  #fund <- cbind(fund[1],data.frame((fund[2]/lag(fund[2])-1))) # 보증잔액 증가율
  fund_accident <- data.frame(read_xlsx(Path,sheet = "기금_사고율"))
  fund_return <- data.frame(read_xlsx(Path,sheet = "기금_수익률"))
  
  
  FundInfo <- merge(fund, fund_return,by='Date', all=TRUE)
  FundInfo <- merge(FundInfo, fund_accident,by='Date', all=TRUE)
  FundInfo <- cbind(lapply(FundInfo['Date'], function(x){as.character(x)}),FundInfo[2:length(FundInfo)])
  DF <- read_excel(Path,sheet = "자산 Return")
  
  ItemName <- unname(unlist(DF[1,-1]))
  
  price_data <- DF[-1,]
  
  price_data <- cbind(lapply(price_data[1], C), price_data[2:(length(ItemName)+1)])
  colnames(price_data) <- append(ItemName, 'Date', after = 0)
  
  Total <- merge(FundInfo,price_data , by='Date', all=TRUE)
  
  rownames(Total) <- Total$Date
  Total <- Total[2:ncol(Total)]
  UseTotal <- data.frame(lapply(Total, function(x){as.numeric(x)}))
  rownames(UseTotal)<- row.names(Total)
  
  Asset_Stat  <- data.frame(read_xlsx(Path,sheet = "자산 평균,변동성"))
  rownames(Asset_Stat) <- Asset_Stat$...1
  Asset_Stat <- Asset_Stat[,2:ncol(Asset_Stat)]
  
  # Statistics
  
  ## 1) Correlation Matrix 
  CorrMat <- cor(UseTotal, use="complete.obs")
  ## 2) Covariance Matrix
  CovMat <- cov(UseTotal, use="complete.obs")* sqrt(12) # Excel 에서 sqrt(12) 를 써서 sqrt(12) 로 적용 
  ## 3) Mean Vector
  mean_vec <- colMeans(UseTotal,na.rm = TRUE)
  mean_vec <- data.frame(as.numeric(mean_vec))
  rownames(mean_vec) <- colnames(UseTotal)
  mean_vec <-t(mean_vec) * 12 
  ## 3) SD Vector
  std_vec <- sapply(na.omit(UseTotal[,1:ncol(UseTotal)]), sd)
  std_vec <- data.frame(as.numeric(std_vec))
  rownames(std_vec) <- colnames(UseTotal)
  std_vec <-t(std_vec) * sqrt(12)
  
  
  ## 4) 평균 보증 수익률 
  avg_backed_ret <- mean_vec[,"보증수익률"] 
  
  ## 5) 보증잔액 성장률 관련
  avg_backed_resid_growth <- mean_vec[,"보증잔액성장률"] 
  std_backed_resid_growth <- std_vec[,"보증잔액성장률"] 
  
  ## 6) 사고율 관련 
  avg_accident <- mean_vec[,"사고율"] 
  std_accident <- std_vec[,"사고율"] 
  
  # ShortFall Risk 관련
  ## 1) Covar(A-g)percent 단위
  covar_A_g <-sum(Asset_Stat[,"표준편차",drop=FALSE] * Weight_Ex*(CorrMat["보증잔액성장률",as.vector(rownames(Weight_Ex))] * std_backed_resid_growth))
  ## 2) Covar(A-p)percent 단위
  covar_A_p <-sum(Asset_Stat[,"표준편차",drop=FALSE] * Weight_Ex*(CorrMat["사고율",as.vector(rownames(Weight_Ex))] * std_accident))
  
  # 결과 도출
  ## 1) Mean 1L (percentage)
  Mean1L <- (1/Target1L*(1+NeedRet/100 - avg_backed_resid_growth) + avg_backed_ret - avg_accident) *100
  
  ## 2) Std 1L (percentage)
  Std1L <- sqrt((1/Target1L)^2 * ((NeedRet/100)^2 + std_backed_resid_growth^2 - 2*covar_A_g/100) + std_accident^2 - 2*(1/Target1L) * covar_A_p/100 + 2 * (1/Target1L) * CovMat["보증잔액성장률","사고율"])*100
  
  
  # RiskProb
  RiskProb <- pnorm(Mean1L -0.7 , Mean1L, Std1L )
  MultipleInfoDT <- data.frame(c(Mean1L , Std1L , RiskProb*100))
  # 만약 나온 위험 확률이 우리가 사전에 주는 허용 위험한도보다 높으면 Std1L 값을 엄청 크게 주어서
  # 후에 Min Std1L 최적화시 최적 point로 안나오게 임의의 큰 값을 집어 넣음
  if(RiskProb*100 > IL_Limit){
    Std1L <- 1000000
  }else{
    
  }
  MultipleInfoDT <- apply(MultipleInfoDT/100, 2, convert2ratio)
  rownames(MultipleInfoDT) <- c("Mean_IL", "STD.DEV_IL","IL ShortFall Risk")
  colnames(MultipleInfoDT) <- "운용배수 Stat"
  
  my_list <- list("Mean1L" = Mean1L , "Std1L" = Std1L , "MultipleInfoDT" = MultipleInfoDT)
  return(my_list)
}
## Stedv 최적화 과정
Optimize.stdev <- function(FilterResult , TrueMean , Resampled_or_True , Target1L , Path , ILMax){
  WT <- FilterResult$WeightTable
  SharpePoints <- FilterResult$ShapreMaxPoints
  
  SharpeWeightInfo <- arrange(SharpePoints, desc(sharpe))
  SharpeWeightInfo_Total <- arrange(SharpePoints, desc(sharpe))
  SharpeWeightInfo<- t(SharpeWeightInfo[1,1:(ncol(SharpeWeightInfo)-3)])
  
  # 수많은 랜덤속에 같은 variance min or max sharpe인 점들이 많은 (weight가 동일한경우)
  # 이에 이런 점들은 제거 하고 중복이 없는 weight들을 우선 뽑아냄
  SharpeWeightInfo_Total <- distinct(SharpeWeightInfo_Total , Domestic.Bond, .keep_all = TRUE)
  
  STDEV.RISK_list <- c()
  for(i in 1:nrow(SharpeWeightInfo_Total)){
    
    UseWeight <- t(SharpeWeightInfo_Total[i,1:(ncol(SharpeWeightInfo_Total)-3)])
    
    # UseMean 은 랜덤 return 결과에서 계산되는 mean vector 를 이용하여
    # portfolio 평균 구함
    # UseMean_2 는 우리가 사전에 parameter로 주는 mean_vector를 이용하여
    # portfolio 평균 구함
    UseMean <- t(SharpeWeightInfo_Total[i,(ncol(SharpeWeightInfo_Total)-1)])
    UseMean_2 <- sum(UseWeight * TrueMean * 252) 
    
    # 1 이면 true, 2면 랜덤 결과에서 나온 값 이용하여 Mean, Std1L , risk info를 저장
    if(Resampled_or_True == 1){
      MultipleResult_ForUse <- MultipleAnalysis(Path , Target1L,UseMean_2*100, UseWeight,ILMax)
    }else if(Resampled_or_True == 2){
      MultipleResult_ForUse <- MultipleAnalysis(Path , Target1L,UseMean*100, UseWeight,ILMax)
    }
    
    STDEV.RISK_list <- append( STDEV.RISK_list, as.numeric(MultipleResult_ForUse$Std1L))
  }
  
  # 최소 STD1L 기준 재측정(Optimization)
  
  MinIndex <- match(min(STDEV.RISK_list),STDEV.RISK_list)
  
  PointInfo <- SharpeWeightInfo_Total[MinIndex,]
  min.stdev_weight <- t(SharpeWeightInfo_Total[MinIndex , 1:(ncol(SharpeWeightInfo_Total)-3)])
  min.mean <- t(SharpeWeightInfo_Total[MinIndex , (ncol(SharpeWeightInfo_Total)-1)])
  min.mean_2 <- sum(min.stdev_weight * TrueMean * 252)
  for_save_in_WT <- t(SharpeWeightInfo_Total[MinIndex ,])
  
  if(Resampled_or_True == 1){
    MultipleResult <- MultipleAnalysis(Path , Target1L,min.mean_2*100 , min.stdev_weight,ILMax)
  }else if(Resampled_or_True == 2){
    MultipleResult <- MultipleAnalysis(Path , Target1L,min.mean*100 , min.stdev_weight,ILMax)
  }
  
  
  ThridRiskPlot<- RiskPlot2_(MultipleResult)
  
  colnames(for_save_in_WT ) <- "IL considered Min STDEV Weight"
  for_save_in_WT <- t(for_save_in_WT)
  
  sharpeforsave <- t(data.frame(round(for_save_in_WT[1,ncol(for_save_in_WT)],2)))
  rownames(sharpeforsave ) <- "IL considered Min STDEV Weight"
  colnames(sharpeforsave) <- "Sharpe Ratio"
  
  for_save_in_WT<- t(data.frame(for_save_in_WT[,1:(ncol(for_save_in_WT)-1)]))
  
  
  for_save_in_WT<- t(data.frame(apply(for_save_in_WT,2,convert2ratio)))
  rownames(for_save_in_WT ) <- "IL considered Min STDEV Weight"
  
  for_save_in_WT <- cbind(for_save_in_WT , sharpeforsave)
  colnames(for_save_in_WT) <- colnames(WT)
  
  WT <- rbind(WT,for_save_in_WT)
  
  my_list <- list("WeightTable" = WT , "ThridPlot" = ThridRiskPlot , "OptimizedResult" = PointInfo, "MultipleInfo" = MultipleResult$MultipleInfoDT)
  return(my_list)
}

# RMVO 총 run 함수
RMVORun <- function(Path,BenchUse,RF,ExUse,StdUse,CorrUse,MVO_num,RMVO_num,SeedingNum,depositfix,fixedvalue,NeedRet,Risk1_Name,Risk2_Name,Risk1_Threshold, Risk2_Threshold, Risk1_Year,Risk2_Year ,Risk1_Limit,Risk2_Limit , Target1L , IL_allowance , Resampled_or_True){
  # 기본 Paramter 저장
  RF <- as.numeric(RF)
  BenchUse <- as.numeric(BenchUse)
  ExUse <- as.numeric(ExUse)
  StdUse <- as.numeric(StdUse)
  CorrUse <- as.numeric(CorrUse)
  MVO_num <- as.numeric(MVO_num)
  RMVO_num <- as.numeric(RMVO_num)
  SeedingNum <- as.numeric(SeedingNum)
  depositfix <- as.numeric(depositfix)
  fixedvalue <- as.numeric(fixedvalue)
  Target1L <- as.numeric(Target1L)
  IL_allowance <- as.numeric(IL_allowance)
  Resampled_or_True <- as.numeric(Resampled_or_True)
  
  # 예금 비중 정 여부
  if(depositfix == 1){
    depositfix <- TRUE
  }else{depositfix <- FALSE}
  suppressMessages(
    # 1) Random Return들을 만들고 Simulation 정보 저장
    TotSimul<- ResamplingReturn(Path , BenchUse,ExUse,StdUse,CorrUse,RMVO_num,SeedingNum)
  )
  
  # 2) 정확하게 Sample Return이 나오는 Random Return 만들고저장 
  TrueRet<- RealreturnSimul(Path , BenchUse,ExUse,StdUse,CorrUse)
  # 3) 각 Random Return 별 EFF 생성
  EFFTotal<- data.frame()
  EFFWeightTotal <- data.frame()
  Effgrouping <- c()
  for(i in 1:length(TotSimul)){
    
    MeanRet_Toadd <- colMeans(data.frame(TotSimul[i]))
    COV_mat <- cov(data.frame(TotSimul[i]))*252
    MvoEx<- MVOmaker_3(depositfix,fixedvalue,MVO_num,RF,MeanRet_Toadd , COV_mat,SeedingNum)
    
    # MVO Simulation Point를 Input으로 넣어서 EFF 에 해당하는 점들만 뽑아냄.
    EFFInfo<-EFF_extract(MvoEx)
    
    # EFF mean, stdev( Porfolio 관련 그래프상의 좌표정보) 저장
    EFFTotal <- rbind(EFFTotal ,EFFInfo$EFFReal)
    # 해당 좌표의 Weight 정보 저장
    EFFWeightTotal <- rbind(EFFWeightTotal ,EFFInfo$Weight)
    #Sharpe 계산
    sharpe <- data.frame((EFFInfo$EFFReal$Exp.Return - RF)/ EFFInfo$EFFReal$Std.Dev)
    colnames(sharpe) <- c("sharpe")
    # Weight + PF mean,stdev + sharpe column 인 DataFrame 만듬
    Effgrouping<- append(Effgrouping, list(cbind(EFFInfo$Weight, EFFInfo$EFFReal, sharpe)))
    
  }
  
  # Total EFF weight 저장 ( Weight + PF mean,stdev 가 column)
  TotalEffs <- cbind(EFFWeightTotal,EFFTotal)
  
  print("TotalEFF Making")
  # Weight + Mean + STDEV 결과를 주는 MVOmaker_2 사용 하여 Resampled 결과 산출
  Result <- MVOmaker_2(Path,BenchUse,RF,ExUse,StdUse,CorrUse,MVO_num,SeedingNum,depositfix,fixedvalue)
  ResampledResult <- MakeBin(TotalEffs,Result,TrueRet)
  ResampledPF_weight <- ResampledResult$weight
  ResampledPF_mean <- ResampledResult$mean
  ResampledPF_stdev <- ResampledResult$stdev
  
  print("EFF plot Making")
  EFFRes<- PlotEFFs(TotalEffs , ResampledPF_weight,ResampledPF_mean , ResampledPF_stdev)
  EFFweight <- EFFRes$EFFweight
  print("WeightPlot Making")
  wp <- WeightPlot(EFFweight, ResampledPF_stdev)
  wp2 <- WeightPlot2(EFFweight, ResampledPF_mean)
  
  TrueMean <- mean_ret_maker(Path ,BenchUse,ExUse)
  # 허용 위험 한도 바탕 제약 조건 적용
  FilteredResult_Output <- FilteredResult(Effgrouping,Path , BenchUse , ExUse,  NeedRet, Risk1_Threshold, Risk2_Threshold, Risk1_Year,Risk2_Year ,Risk1_Limit,Risk2_Limit,Target1L , IL_allowance, Resampled_or_True)
  # 결과들중 IL Std1L 최소화 결과 산출
  FilteredResult_Output <- Optimize.stdev(FilteredResult_Output, TrueMean , Resampled_or_True, Target1L , Path , IL_allowance)
  WT <- FilteredResult_Output$WeightTable
  SharpePoints <- FilteredResult_Output$ShapreMaxPoints
  
  
  FirstRiskPlot <- RiskPlot_(FilteredResult_Output$OptimizedResult,Risk1_Name,Risk1_Threshold,Risk1_Year , Risk1_Limit)
  SecondRiskPlot <- RiskPlot_(FilteredResult_Output$OptimizedResult,Risk2_Name,Risk2_Threshold,Risk2_Year, Risk2_Limit)
  ThridRiskPlot<- FilteredResult_Output$ThridPlot
  
  my_list <- list("EFFGroup" = EFFRes$EFFplot , "STD.Weight" = wp, "Mean.Weight" = wp2,"WeightTable" = WT , "FirstRiskplt" = FirstRiskPlot , "SecondRiskplt" = SecondRiskPlot, "ThridRiskplt" = ThridRiskPlot, "RiskTable" = FilteredResult_Output$MultipleInfo)
  
  return(my_list)
}
SFRiskReturn<- function(Mean,Stdev,SFRisk,yr){
  Mean <- as.numeric(Mean)
  Stdev <- as.numeric(Stdev)
  UseMean <- (1 + Mean)**yr - 1
  UseStdev <- Stdev * sqrt(yr)
  
  SV <- as.numeric(SFRisk)
  SV<- (1+SV)**yr - 1 
  shortfallp <- pnorm(SV, mean=UseMean*100, sd=UseStdev*100)
  
  return(shortfallp)
}
convert2ratio <- function(x){
  x<- round(x,digits=4)
  x<-paste(as.character(x*100),"%")
  return(x)
}

RiskPlot_<- function(EFFResults,Risk1Name,Risk1Th,Risk1Yr , Risk1_Limit){
  Risk1Th <- as.numeric(Risk1Th)
  Risk1Yr <- as.numeric(Risk1Yr)
  Risk1_Limit <- as.numeric(Risk1_Limit)
  
  EFFResults<- arrange(EFFResults, desc(sharpe))
  UseMean<- EFFResults[1,]$Exp.Return
  UseStdev<- EFFResults[1,]$Std.Dev
  
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
RiskPlot2_<- function(MultipleResult){
  Mean1L_use <- MultipleResult$Mean1L
  Std1L_use <- MultipleResult$Std1L
  
  
  UseMean <- Mean1L_use/100
  UseStdev <- Std1L_use/100
  
  SV <- Mean1L_use -0.7
  shortfallp <- pnorm(SV, mean=UseMean*100, sd=UseStdev*100)
  
  Annual.Return<-seq(UseMean*100-UseStdev*500,UseMean*100+UseStdev*500,length=200)
  Prob<-dnorm(Annual.Return,mean=UseMean*100,sd=UseStdev*100)
  
  ForText <- max(Prob)*1/2
  ForProb <- max(Prob)*0.8/2
  
  rfrisk <- paste(as.character(round(shortfallp,digits=4)*100),"%")
  p<- ggplot(data.frame(x=c(UseMean*100-UseStdev*500,UseMean*100+UseStdev*500)), aes(x=x))+
    stat_function(fun=dnorm, args=list(mean=UseMean*100, sd=UseStdev*100) ,size =1)+
    geom_area(stat='function', fun=dnorm, args=list(mean=UseMean*100, sd=UseStdev*100), xlim=c(UseMean*100-UseStdev*500, SV), fill='gray75') +
    geom_text(aes(x=UseMean*100 -UseStdev*300, y=ForText), label="ShortFall Risk:")+
    geom_text(aes(x=UseMean*100 -UseStdev*300, y=ForProb), label=rfrisk)+
    ggtitle(paste("운용배수","1","Year","Shortfall Risk"))+
    theme(panel.background=element_rect(fill=eallighttan),
          text=element_text(color=ealdark),
          plot.title=element_text(size=24, color=ealred))+
    labs(x="Annualized Return (%)", y="Prob")
  
  return(p)
}
#Var분석 관련 함수
VARAnalysis <- function(ER, EV , RiskInfo , Exposure ,VARConfidenc , ManageTime){
  ER <- as.numeric(ER)
  EV<- as.numeric(EV)
  
  Exposure<-as.numeric(Exposure)
  VARConfidenc<-as.numeric(VARConfidenc)
  ManageTime<-as.numeric(ManageTime)
  
  SFRet <- RiskInfo$RiskTH
  SFYr<- RiskInfo$RiskYr
  SFAllow<- RiskInfo$RiskLimit
  
  #허용가능 연간 변동성 한도
  #=((((1+수익률)^원금Shortfall통제기간)-1)-원금손실조건의임계수익률)/NORMSINV(1-ShortfallRisk의허용위험한도)
  YearlyVol <- (((((1+ER/100)^SFYr)-1)-SFRet/100)/qnorm(1-SFAllow/100)) * 100
  YearlyVol_str<- paste(as.character(round(YearlyVol,3)),"%")
  
  #%VaR
  #허용가능연간변동성한도/SQRT(252)*NORMSINV(VaR신뢰수준)*SQRT(관리기간)
  PVAR <- (YearlyVol/100)/SQRT(252)*qnorm(99/100)*SQRT(ManageTime)*100
  PVAR_str<- paste(as.character(round(PVAR,3)),"%")
  
  #금액VAR 
  #=퍼센트VaR*익스포져
  PrinVAR <- PVAR/100*Exposure
  
  TableRes <- as.data.frame(c(YearlyVol_str , PVAR_str , PrinVAR))
  rownames(TableRes) <- c("허용가능 연간 변동성 한도", "%VaR" , "금액VAR ")
  colnames(TableRes) <- c('VaR Analysis Result')
  
  return(TableRes)
}

#Name이 원금인것 분류(VAR 분석에 사용 )
PrinDefine <- function(Risk1_Name , Risk2_Name, Risk1_Threshold, Risk2_Threshold, Risk1_Year, Risk2_Year ,Risk1_Limit,Risk2_Limit){
  Risk1_Threshold <- as.numeric(Risk1_Threshold)
  Risk2_Threshold <- as.numeric(Risk2_Threshold)
  Risk1_Year <- as.numeric(Risk1_Year)
  Risk2_Year <- as.numeric(Risk2_Year)
  Risk1_Limit <- as.numeric(Risk1_Limit)
  Risk2_Limit <- as.numeric(Risk2_Limit)
  if(Risk2_Name == "원금"){
    my_list <- list("RiskTH" = Risk2_Threshold , "RiskYr"= Risk2_Year, "RiskLimit" = Risk2_Limit)
    return(my_list)
  } else{
    my_list <- list("RiskTH" = Risk1_Threshold , "RiskYr"= Risk1_Year, "RiskLimit" = Risk1_Limit)
    
    return(my_list)
  }
}




#' Parametric MEAN하고 SD를 어떤걸로 받을지 생각 -> 모수, 비모수
#' @param mean Mean return of timeseries data
#' @param sd Standard deviation value of timeseries data
#' @param VARConfidence Alpha value for confidence level신뢰구간
#' @param ManageTime Number of days to forecast VaR 관리기간
#' @param n Number of iterations 비모수일 때
#' @param VarUse 모수, 비모수 선택
#' mean -> port_return, sd-> port_risk 일간으로 
#' # 관리기간이 일간으로 입력받기 때문에
#' 
parametric <- function(VarConfidence, ManageTime, VarUse, Exposure) {
  
  Exposure<-as.numeric(Exposure)  #익스포저
  VarConfidence<-as.numeric(VarConfidence)  #신뢰구간
  ManageTime<-as.numeric(ManageTime)  #관리기간
  n<-as.numeric(1000000) #Number of iterations -> default=1000000
  
  if(as.numeric(VarUse) == 1){ # 모수적 방법 사용
    
    VARpercent<-qnorm(1-VarConfidence/100,0,1)*sd*sqrt(ManageTime)
    VARpercent<-abs(VARpercent)*100
    VARpercent_str<- paste(as.character(round(VARpercent,3)),"%")
    
    p<- ggplot(data.frame(x=c(-4,4)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=0, sd=1) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=0, sd=1), xlim=c(-4, qnorm(1-VarConfidence/100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100)-0.5, y=0.33), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100)-0.5, y=0.3), label=VARpercent_str)+
      ggtitle(paste("모수적 방법 VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk", y="Prob")
    
  }else if(as.numeric(VarUse) == 2) # 비모수적 방법 사용(몬테카를로)
  {
    VARpercent<-(mean + sd*rnorm(n))*sqrt(ManageTime)
    
    VARpercent<-quantile(VARpercent, (1-VarConfidence/100))
    VARpercent=abs(VARpercent)*100
    VARpercent_str<- paste(as.character(round(VARpercent,3)),"%")
    
    X<-seq(mean-sd*10,mean+sd*10,length=200)
    Prob<-dnorm(X,mean=mean,sd=sd)
    
    #gvar=paste(as.character(round(VARpercent,digits=4)))
    #Pgvar=paste(as.character(round(PrinVAR,digits=3)))
    
    p<- ggplot(data.frame(x=c(mean-sd*10,mean+sd*10)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=mean, sd=sd) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=mean, sd=sd), xlim=c(mean-sd*10, qnorm(1-VarConfidence/100, mean=mean, sd=sd)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean=mean, sd=sd)-sd, y=max(Prob)*1/2), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100,mean=mean, sd=sd)-sd, y=max(Prob)*0.8/2), label=VARpercent_str)+
      ggtitle(paste("비모수적 방법 VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100,mean=mean, sd=sd), colour = 'red')+
      theme(panel.background=element_rect(fill=eallighttan),
            text=element_text(color=ealdark),
            plot.title=element_text(size=24, color=ealred))+
      labs(x="Value at Risk", y="Prob")
  }
  
  else if(as.numeric(VarUse) == 3) # EWMA 방법 사용
  {
    # 벤치마크 사용여부로 Asset Name 지정 (Excel Sheet 불러오기 위함)
    if(as.numeric(BenchUse) == 1){ # 벤치마크 사용
      AssetName <- "벤치마크"
    }else if(as.numeric(BenchUse) == 2){ # 벤치마크 미사용
      AssetName <- "자산"
    }
    
    #EWMA 표준편차 구하기
    
    if(as.numeric(ExUse)==1 | as.numeric(ExUse)==2){# (timeseries일 때)
      DF <- read_excel(Path,sheet = paste(AssetName,"Return"))
      
      ItemName <- unname(unlist(DF[1,-1]))
      price_data <- DF[-1,]
      
      price_data <- cbind(lapply(price_data[1], A), price_data[2:(length(ItemName)+1)])
      colnames(price_data) <- append(ItemName, 'Dates', after = 0)
      
      price_data <- data.frame(price_data, row.names = 1)
      price_data <- data.frame(apply(price_data, 2, B), row.names = row.names(price_data))
      
      
      ewma.loop <- function(rets, lambda) {
        n <- length(rets)+1
        sig.s <- rep(0, n)
        for (i in 2:n) {
          sig.s[i] <- (sig.s[i-1])*lambda + (rets[i-1]^2)*(1 - lambda)
        }
        return(sqrt(tail(sig.s, n-1)))
      }
      
      lambda=0.94
      #자산군이 5개이므로
      rets1 <-  as.numeric(price_data[,1])
      rets1<-ewma.loop(rets1, lambda)
      
      rets2 <-  as.numeric(price_data[,2])
      rets2<-ewma.loop(rets2, lambda)
      
      rets3 <-  as.numeric(price_data[,3])
      rets3<-ewma.loop(rets3, lambda)
      
      rets4 <-  as.numeric(price_data[,4])
      rets4<-ewma.loop(rets4, lambda)
      
      rets5 <-  as.numeric(price_data[,5])
      rets5<-ewma.loop(rets5, lambda)
      
      
      ewmasd<-data.frame(rets1,rets2,rets3,rets4,rets5)
      colnames(ewmasd)<-ItemName
      ewmasd<-ewmasd[nrow(ewmasd),]/sqrt(21)
    }
    
    else if(as.numeric(ExUse)==3){ # Given Mean일 때(timeseries가 아닌 사용자가 입력할 때)
      
      MeanInfo <- data.frame(read_excel(Path,sheet = paste(AssetName,"평균,변동성")))
      
      ItemName <- MeanInfo[,1]
      mean_ret <- MeanInfo[,"기대수익률"]/100
      names(mean_ret) <- ItemName
      
      ewmasd<-mean_ret^2/sqrt(252)
      
      #lambda=0.94
      
      lambda=0.94/1.94
      lambda1=1/1.94
      ewmasd<-lambda*ewmasd+lambda1*ewmasd
      
      
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
    #관리기간을 일별로 입력해주니까 연간으로 바꿔줄 필요가 없음.
    cov_mat <- cor_ret *as.numeric(as.matrix(ewmasd) %*% t(as.matrix(ewmasd))) 
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
    #port_returns <- sum(wts * mean_ret)* 252
    sd <- as.numeric(sqrt(t(wts) %*% (as.matrix(cov_mat) %*% wts)))
    
    VARpercent<-qnorm(1-VarConfidence/100,0,1)*sd*sqrt(ManageTime)
    VARpercent<-abs(VARpercent)*100
    VARpercent_str<- paste(as.character(round(VARpercent,3)),"%")
    
    p<- ggplot(data.frame(x=c(-4,4)), aes(x=x))+
      stat_function(fun=dnorm, args=list(mean=0, sd=1) ,size =1)+
      geom_area(stat='function', fun=dnorm, args=list(mean=0, sd=1), xlim=c(-4, qnorm(1-VarConfidence/100)), fill='gray75') +
      geom_text(aes(x=qnorm(1-VarConfidence/100)-0.5, y=0.33), label="%VaR:")+
      geom_text(aes(x=qnorm(1-VarConfidence/100)-0.5, y=0.3), label=VARpercent_str)+
      ggtitle(paste("EWMA VaR Analysis Result"))+
      geom_vline(xintercept = qnorm(1-VarConfidence/100), colour = 'red')+
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