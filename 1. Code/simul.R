

Fund_<-Fund(return18,return21,return31,return41,return51,return61,showInsur_man, showInsur_woman)
aaa<-Fund_$재정수지전망
year<-c(2019:2067)
new<-data.frame(year)
anew<-data.frame(year)
for(n in 2:50){
new[n-1,2]<-aaa$총지출[n]/aaa$적립기금[n-1]
anew[n-1,2]<-aaa$적립기금[n-1]/aaa$총지출[n]
}

bb<-new[,2]
Accum<-Accum_rate(fund_total)
# 2. Simulation Process
fund_total <- Fund(return18,return21,return31,return41,return51,return61,showInsur_man, showInsur_woman)[['재정수지전망']]
# 1) 시뮬레이션 기간 (t)와 시뮬레이션 횟수 (n)을 정의하고
#) 해당기간에 기대되는 증가율(수입, 지출), 수익률(국내채권, 해외채권, 국내주식, 해외주식)을 입력
#3) resampling. 방식으로 시뮬레이션
## Mean vector와 Cov vector를 주면 MVO를 만들어 주는 함수

MVOmaker_year <- function(depositfix ,fixedvalue ,MVO_num, RF, mean_ret , cov_mat,SeedingNum){
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

simulation_con <- function(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n){
  
  cov_s <- cov(new_data)
  mean_s <- c(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d)
  df_income <- data.frame('연도' = c(1:t), '')
  mydata <- list()
  for(i in 1:n){
    set.seed(i)
    mydata[[i]] <- as.data.frame(mvrnorm(t, mean_s, cov_s))
    names(mydata[[i]]) <- c('income', 'expenditure', 'domestic.bond', 'domestic.stock', 'foreign.bond', 'foreign.stock')
    mydata[[i]] <- cbind(mydata[[i]], data.frame('year'= c(1:t)))
  
  }
  return(mydata)
}

random_simul<-function(showInsur_man, showInsur_woman,yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,depositfix,fixedvalue,RF,SeedingNum){
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
  SeedingNum <- as.numeric(SeedingNum)
  depositfix <- as.numeric(depositfix)
  fixedvalue <- as.numeric(fixedvalue)
  
  mydata<-simulation_con(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d, t, n)
  fund_total <- Fund(showInsur_man, showInsur_woman)[['재정수지전망']]
 # cov_s <- cov(new_data)
#  mean_s <- c(income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d)
  #19년부터 5년 가정
  year<-c((yy+1):(yy+t))
  #cost<-filter(fund_total, 연도==yy)%>%select(총지출)
  cost<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(총지출)
  income<-fund_total%>%filter(연도>yy-1 & 연도<yy+t)%>% select(국민연금수입)
  #income<-as.numeric(filter(fund_total, 연도==yy)%>%select(국민연금수입))
  asset<-filter(fund_total, 연도==yy)%>%select(적립기금)
  #income_fund<-filter(fund_total, yy<=연도&연도<yy+t+1)%>%select(투자수익)
  #accum[i]<-filter(fund_total, 연도==yy+i)%>%select(총지출)/filter(fund_total, 연도==yy+i)%>%select(적립기금)
  #income_simul <- data.frame(matrix(nrow=t+1,ncol=n))
  #cost_simul<-data.frame(matrix(nrow=t+1,ncol=n))
  asset_simul<-data.frame(matrix(nrow=t+1,ncol=MVO_num))
 
  ratio_simul<-data.frame(matrix(nrow=t,ncol=n))
 #income_simul[1,]<-income
 #cost_simul[1,]<-cost
 asset_simul[1,]<-asset

 ssss<-data.frame()
 weight<-data.frame()
 
 
  for(i in 1:n) {
    
     
     #mydata_income<-(mydata[[i]][1])
    #  mydata_cost<-mydata[[i]][2]
     # mydata_dom_bond<-mydata[[i]][3][j,]
    #  mydata_dom_stock<-mydata[[i]][4][j,]
     # mydata_for_bond<-mydata[[i]][5][j,]
    #  mydata_for_stock<-mydata[[i]][6][j,]
      
      
     # random<-random_simul()
      
        MeanRet <- colMeans(data.frame(mydata[[i]][3:6]))
        COV_mat <- cov(data.frame(mydata[[i]][3:6]))
        SeedingNum<-i
        Result<- MVOmaker_year(depositfix,fixedvalue,MVO_num,RF,MeanRet , COV_mat,SeedingNum)
  
        
       # income_simul[j+1,i]<-(income_simul[j,i]*exp((mydata_income[j,])))#+income_simul[j,n]
        
       # cost_simul[j+1,i]<-cost_simul[j,i]*exp(mydata_cost[j,])
        
        for(m in 1:MVO_num){
          for (j in 1:t){
        Return1<-Result$pfvalue$Return[m] #mvo_num이 들어가야함
        
       
      asset_simul[j+1,m]<-asset_simul[j,m]+income[j,]-cost[j,]+asset_simul[j,m]*(Return1)
      ratio_simul[j,m]<-cost[j,]/asset_simul[j,m]
      ssss[m,1]<-sd(ratio_simul[,m])
      mm<-which.min(as.matrix(ssss))
      
     
          }
          
         # ssss<-cbind(ssss,num)
          
        }
        weight<-rbind(weight,Result$pfvalue[mm,])
      #수익률에 따라 경우의 수 나오게 계산하기
      # asset_invest<-list()
      # for(m in 1:MVO_num){
      #   return<-Return[m]
      # asset_invest[[m]]<-asset_simul[j,i]+income_simul[j+1,i]-cost_simul[j+1,i]+(asset[j,i]*(1+return))
      # }
      # ratio_simul[j,i]<-cost_simul[j+1,i]/asset_simul[j,i]
      # real_ratio_simul<-(ratio_simul[j+1,i]-ratio_simul[j,i])/ratio_simul[j,i]
      # std[i,1]<-sapply(ratio_simul[i],sd)
      # 
      # }
    # mm<-data.frame(cbind(std,num))
    # 
    # mm<-mm[which.min(as.matrix(std)),]
    # mm1<-mm[,2]
    # weight<-Result$pfvalue[mm1,]
        
        
  }
 #mweight<-colMeans(weight[1:4])
 
 mean_weight <- data.frame(describe(weight))[,c("mean","min","max","sd")]
 
 meanOutput <- t(data.frame(mean_weight)['mean'])
 
 #rownames(meanOutput) <- c("Average Min Variance Weight")
 
 meanper <- apply(data.frame(t(meanOutput[,1:(ncol(meanOutput)-1)])),2,convert2ratio)
 #rownames(meanper) <- c( "Average Min Variance Weight")
 
 meanper<-data.frame(t(meanper))
 
 SharpeCol <- data.frame(meanOutput[,ncol(meanOutput)])
 
 colnames(SharpeCol) <- c("Sharpe Ratio")
 
 merr <-cbind(meanper,SharpeCol)
 rownames(merr)<-c("최소 변동성 자산배분안 평균")
 
 my_list<-list('table'=merr, 'weight'=weight)
 return(my_list)
}
aa<-random_simul(showInsur_man, showInsur_woman,yy,t,n,income_inc, cost_inc, ben_a, ben_b, ben_c, ben_d,MVO_num,depositfix,fixedvalue,RF,SeedingNum)
 #mean_s<-colMeans(new_data)
 
 EFFTotal<- data.frame()
 EFFWeightTotal <- data.frame()
 Effgrouping <- c()
 
 for(i in 1:n){
   MeanRet_Toadd <- colMeans(data.frame(mydata[[i]][3:6]))
   COV_mat <- cov(data.frame(mydata[[i]][3:6]))
   MvoEx<- MVOmaker_year(depositfix,fixedvalue,MVO_num,RF,MeanRet_Toadd , COV_mat,SeedingNum)
   
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
 
 # for(i in n){
 # TrueRet<-mydata[[i]][1:6]
 # }
 for(i in 1:n){
   set.seed(i)
   mydata[[i]] <- as.data.frame(mvrnorm(t, mean_s, cov_s))
   names(mydata[[i]]) <- c('income', 'expenditure', 'domestic.bond', 'domestic.stock', 'foreign.bond', 'foreign.stock')
   TrueRet <- (mydata[[i]][3:6])
   
 }
# Result <- MVOmaker_2(Path,BenchUse,RF,ExUse,StdUse,CorrUse,MVO_num,SeedingNum,depositfix,fixedvalue)
 
 
 ResampledResult <- MakeBin(TotalEffs,Result,TrueRet)
 ResampledPF_weight <- ResampledResult$weight
 ResampledPF_mean <- ResampledResult$mean
 ResampledPF_stdev <- ResampledResult$stdev

 EFFRes<- PlotEFFs(TotalEffs , ResampledPF_weight,ResampledPF_mean , ResampledPF_stdev)
 EFFweight <- EFFRes$EFFweight
 
 wp <- WeightPlot(EFFweight, ResampledPF_stdev)
 wp2 <- WeightPlot2(EFFweight, ResampledPF_mean)
 
 }
 asset_simul_invest<-data.frame()
 aa<-data.frame(matrix(nrow=5,ncol=50))
 
 for(i in n){
   for(m in 1:MVO_num){
   for(j in t){
     invest_simul<-portfolio_values$Return
     #aa[]<-(asset_simul[j+1,i]*(1+invest_simul[m]))
     aa[j,i]<-asset_simul[j,i]*(invest_simul[m])
   #asset_simul_invest[[m]]<-data.frame(asset_simul[j+1,i]*(1+invest_simul[m]))
   #asset_simul_invest[[m]]<-asset_simul[j+1,i]
   }
   }
 }
}
 ratio_simul<-cbind(ratio_simul,year)
 ratio_var<-data.frame(matrix(nrow=n,ncol=1))
 num<-c(1:n)
 for(i in 1:n){
   ratio_var[i,]<-var(ratio_simul[i])
 }
 ratio_var<-cbind(ratio_var,num)
 colnames(ratio_var)<-c('var','num')
 ratio_min<-(ratio_var)[which.min(as.matrix(ratio_var)),]
}



  names(income_simul[[i]]) <- c('연도','수입')


  simulincome<-income[j]*mydata[[i]]$income[j]
  simulcost[2,i]<-cost*mydata$expenditure[i]
  df_accum_rate <- data.frame('연도'= year, '적립배율'= accum_rate)
  simulincome[2,i]<-filter(fund_total, 연도=yy+i)%>%select(총수입)*mydata$income[i]
  simulcost[2,i]<-filter(fund_total, 연도=yy+i)%>%select(총지출)*mydata$expenditure[i]

  

  ## Set seed
set.seed(SeedingNum)

www<-runif(n=5)
## weight control by deposit fix option
if(depositfix == TRUE){
  wts <- runif(n = length(ItemName)-1)
  wts <- wts/sum(wts)*(1-fixedvalue)
  wts<- append(fixedvalue,wts)
}else{
  wts <- runif(n = length(ItemName))
  wts <- wts/sum(wts)
}
