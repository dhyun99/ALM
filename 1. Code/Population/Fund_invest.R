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


#적립기금추이 함수
Fund <- function(showInsur_man, showInsur_woman){
  #수익률 테이블 생성
  year<-c('18-20','21-30','31-40','41-50','51-60','61-70')
  return18<-4.9/100  #18-20년도 수익률
  return21<-4.8/100  #21-30년도 수익률
  return31<-4.6/100  #31-40년도 수익률
  return41<-4.5/100  #41-50년도 수익률
  return51<-4.5/100  #51-60년도 수익률
  return61<-4.4/100  #61-70년도 수익률
  
 
  Fund_return<-c(return18,return21,return31,return41,return51,return61) #%화된거 변경
  Fund_return<-data.frame(year,Fund_return)
  colnames(Fund_return)=append('연도', '기금투자수익률')
  
  
  #18년말 적립금 테이블 생성
  asset_year<-c(2018)
  Fund_Asset<-c(638781) #2018년말 국민연금 적립금 (단위:십억원) 
  Fund_Asset<-data.frame(asset_year,Fund_Asset)
  colnames(Fund_Asset)=append('연도','적립기금')
  
  #투자수익 테이블 생성
  Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
  
  #수입지출모듈
  pension<-pension(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive) # 국민연금 수입지출

  income<-pension[,2]/1000000000 #단위 맞추기(십억원) 수입
  expenditure<-pension[,3]/1000000000 #단위 맞추기(십억원) 지출
  
  income<- data.frame('연도'= c(2019:2067),'국민연금수입'= income)
  expenditure<-data.frame('연도'= c(2019:2067),'총지출'= expenditure)
  
  
  pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
  #투자자산 연도 생성
  for (i in 1:49){
    Fund_Asset[i+1,1]<-2018+i
  }
  #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
  
  i=1
  for(a in 1:49){
    if(Fund_Asset[a,1]%%10==0){
      i=i+1
    }
    Fund_Asset[a+1,2]<-(Fund_return[i,2]*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
    
     if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게
      break
    }
    }
  
      colnames(Fund_Asset)<-append("연도", "적립기금")
  
  #투자수익 확인
  #년도생성
   for (i in 1:50){
     Fund_Invest[i,1]<-2017+i
   }
    i=1
    for(a in 1:50){
      if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
        i=i+1
      }
      
      Fund_Invest[a+1,2]<-Fund_return[i,2]*(Fund_Asset[a,2])
      
       if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
         Fund_Invest[a+1,2]<-NA
         break
       }
    }
    
    colnames(Fund_Invest)<-append("연도", "투자수익")

  
  #총수입 계산
  Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
  for(i in 1:nrow(Fund_Invest)){
  Total_income[i,1]<-Fund_Invest[i,1]}
  for(i in 2:nrow(Fund_Invest)){
  Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
  if(is.na(Fund_Invest[i,2])){
    Total_income[i,2]<-pension[i-1,2]
  }
  }
  colnames(Total_income)<-append("연도", "총수입")

#수지차 생성
difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
for (i in 1:49){
difference[i,1]<-2018+i
difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
colnames(difference)<-append("연도", "수지차")

#year를 기준으로 합치기
Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)


# max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
# max_year<-as.numeric(max_asset_year[1])
# max_asset<-as.numeric(max_asset_year[2])
# 
# p<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
#   geom_line(color=3)+geom_point(size=1, color=4)+
#   scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
#   scale_y_continuous(breaks=seq(0, 2000000, 200000))+
#   annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
#   annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
#   annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
#   annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
#   annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
#   labs(x = '연도',
#        y = '적립기금 (단위 : 십억원)',
#        title = "적립기금추이")
  
my_list<-list('재정수지전망'=Fund_Total) #,  'plot'=p)
return(my_list)
}

#수입지출이 67년까지 밖에 없음. 
senario<-function(showInsur_man, showInsur_woman, sereturn){
  year<-c('18-20','21-30','31-40','41-50','51-60','61-70')
  return18<-4.9/100  #18-20년도 수익률
  return21<-4.8/100  #21-30년도 수익률
  return31<-4.6/100  #31-40년도 수익률
  return41<-4.5/100  #41-50년도 수익률
  return51<-4.5/100  #51-60년도 수익률
  return61<-4.4/100  #61-70년도 수익률
  #return71<-4.4/100  #71-80년도 수익률
  #return81<-4.3/100  #81-88년도 수익률
  
  sereturn<-as.numeric(sereturn)/100
  
  
  Fund_return<-c(return18+sereturn,return21+sereturn,return31+sereturn,return41+sereturn,return51+sereturn,return61+sereturn) 
  Fund_return<-data.frame(year,Fund_return)
  colnames(Fund_return)=append('연도', '기금투자수익률')
  
  #18년말 적립금 테이블 생성
  asset_year<-c(2018)
  Fund_Asset<-c(638781) #2018년말 국민연금 적립금 (단위:십억원) 
  Fund_Asset<-data.frame(asset_year,Fund_Asset)
  colnames(Fund_Asset)=append('연도','적립기금')
  
  #투자수익 테이블 생성
  Fund_Invest <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
  
  #수입지출모듈
  pension<-pension(showInsur_man, showInsur_woman, df_disorder_receive, df_old_receive, df_bereaved_receive) # 국민연금 수입지출
  
  income<-pension[,2]/1000000000 #단위 맞추기(십억원) 수입
  expenditure<-pension[,3]/1000000000 #단위 맞추기(십억원) 지출
  
  income<- data.frame('연도'= c(2019:2067),'국민연금수입'= income)
  expenditure<-data.frame('연도'= c(2019:2067),'총지출'= expenditure)
  
  
  pension<-merge(income, expenditure, by="연도", all.x=TRUE) #수입지출 합친 테이블
  #투자자산 연도 생성
  for (i in 1:49){
    Fund_Asset[i+1,1]<-2018+i
  }
  #투자자산*수익률+투자자산+수입-지출 생성->다음년도 투자자산
  
  i=1
  for(a in 1:49){
    if(Fund_Asset[a,1]%%10==0){
      i=i+1
    }
    Fund_Asset[a+1,2]<-(Fund_return[i,2]*Fund_Asset[a,2])+pension[a,2]-pension[a,3]+Fund_Asset[a,2]
     if(Fund_Asset[a+1,2]<0){ #기금이 음수가 나오면 멈추게/기금이 전부 양수일 떄 오류.
       break
     }
  }

  colnames(Fund_Asset)<-append("연도", "적립기금")
  
  #투자수익 확인
  #년도생성
  for (i in 1:50){
    Fund_Invest[i,1]<-2018+i
  }
  i=1
  for(a in 1:50){
    if(Fund_Invest[a,1]%%10==0){ #연도/10이 1이면 더해지게..?
      i=i+1
    }
    
    Fund_Invest[a+1,2]<-Fund_return[i,2]*(Fund_Asset[a,2])
    
    if (Fund_Asset[a,2]<0){ #기금이 음수가 나오면 멈추게 
      Fund_Invest[a+1,2]<-NA
      break
    }
  }
  
  colnames(Fund_Invest)<-append("연도", "투자수익")
  
  #총수입 계산
  Total_income<-data.frame(matrix(nrow=nrow(Fund_Invest),ncol=2))
  for(i in 1:nrow(Fund_Invest)){
    Total_income[i,1]<-Fund_Invest[i,1]}
  for(i in 2:nrow(Fund_Invest)){
    Total_income[i,2]<-pension[i-1,2]+Fund_Invest[i,2]
    if(is.na(Fund_Invest[i,2])){
      Total_income[i,2]<-pension[i-1,2]
    }
  }
  colnames(Total_income)<-append("연도", "총수입")
  
  #수지차 생성
  difference <- data.frame(matrix(nrow=nrow(Fund_Asset),ncol=2))
  for (i in 1:49){
    difference[i,1]<-2018+i
    difference[i,2]<- Total_income[i+1,2]-expenditure[i,2]}
  colnames(difference)<-append("연도", "수지차")
  
  #year를 기준으로 합치기
  Fund_Total<-merge(Fund_Asset, Total_income, by="연도", all.x=TRUE)
  Fund_Total<-merge(Fund_Total,income ,  by="연도", all.x=TRUE)
  Fund_Total<-merge(Fund_Total,Fund_Invest ,  by="연도", all.x=TRUE)
  Fund_Total<-merge(Fund_Total,expenditure ,  by="연도", all.x=TRUE)
  Fund_Total<-merge(Fund_Total,difference ,  by="연도", all.x=TRUE)
  
  
  # max_asset_year <- Fund_Total[which.max(Fund_Total$적립기금),]
  # max_year<-as.numeric(max_asset_year[1])
  # max_asset<-as.numeric(max_asset_year[2])
  # 
  # p2<-ggplot(data=Fund_Asset,aes(x=연도, y=적립기금))+
  #   geom_line(color=3)+geom_point(size=1, color=4)+
  #   scale_x_continuous(breaks=seq(min(Fund_Total$연도), max(Fund_Total$연도), 5)) +
  #   scale_y_continuous(breaks=seq(0, 2000000, 200000))+
  #   annotate('text', x = max_year, y = max_asset + 500000, label = "최대적립기금" , size = 4)+
  #   annotate('text', x = max_year, y = max_asset + 350000, label =(max_year) , size = 4)+
  #   annotate('text', x = max_year+3.5, y = max_asset + 350000, label ='년' , size = 4)+
  #   annotate('text', x = max_year, y = max_asset + 200000, label =round(max_asset/1000, 2) , size = 4)+
  #   annotate('text', x = max_year+5, y = max_asset + 200000, label ='조원' , size = 4)+
  #   labs(x = '연도',
  #        y = '적립기금 (단위 : 십억원)',
  #        title = "적립기금추이 시나리오")
  
  my_list<-list('재정수지전망 시나리오'=Fund_Total) # 'plot'=p2)
  return(my_list)
  
}

#기본가정과 시나리오 비교하여 볼 수 있는 테이블과 그래프
scenario_plot<-function(showInsur_man, showInsur_woman, sereturn){
  
  origin<-Fund(showInsur_man, showInsur_woman)
  senario<-senario(showInsur_man, showInsur_woman,sereturn)
  
  origin_Total<-origin$재정수지전망
  senario_Total<-senario$`재정수지전망 시나리오`
  
  Total<-rbind(origin_Total,senario_Total)
  
  group<-data.frame(matrix(nrow=100,ncol=1))
  
  #그래프를 만들어 주기 위해 그룹을 나눠줌
  for(i in 1:100){
    if(i<=50){
      group[i,]='기본가정'
    }
    else{
      group[i,]='시나리오'
    }
  }
  
  colnames(group)<-c("구분")
  
  Total<-cbind(Total,group)
 
  max_asset_year <- Total[which.max(Total$적립기금),]
  #max_year<-as.numeric(max_asset_year[1])
  max_asset<-as.numeric(max_asset_year[2])
  
   p<-ggplot(data=Total,aes(x=연도, y=적립기금, group=구분, color=구분))+
    geom_line()+#geom_point(size=1, color=4)+
    scale_x_continuous(breaks=seq(min(Total$연도), max(Total$연도), 5)) +
    scale_y_continuous(breaks=seq(0, max(max_asset), 200000))+
    geom_point(data=Total, aes(x=연도,y=적립기금,  group = 구분,color = 구분))+
   labs(x = '연도',
         y = '적립기금 (단위 : 십억원)',
         title = "적립기금추이")
   
   #테이블 만들기(수지적자와 기금소진이 몇년도에 일어나는지)
   for(i in 1:50){
     origin_Total[is.na(origin_Total)] <- 0
     if(origin_Total[i,7]<0){
       year_different_minus<-as.numeric(origin_Total[i,1])
       break
     }
   }
     for(i in 1:50){
     if(origin_Total[i,2]<0){
       year_asset_minus<-as.numeric(origin_Total[i,1])
       break
     }
   }
   
   for(i in 1:50){
     senario_Total[is.na(senario_Total)] <- 0
     
     if(senario_Total[50,2]>0){
       year_asset_minus_senario<-'2067년 이후'
     }
    
     else if(senario_Total[i,2]<0){
       year_asset_minus_senario<-as.numeric(senario_Total[i,1])
       break
     }
   }
     for(i in 1:50){
       if(senario_Total[50,7]>0){
         year_asset_minus_senario<-'2067년 이후'
       }
       
     else if(senario_Total[i,7]<0){
       year_different_minus_senario<-as.numeric(senario_Total[i,1])
       break
     }
     }
     table<-data.frame(matrix(ncol=2, nrow=2))
     table[1,1]=year_different_minus
     table[1,2]=year_asset_minus
     table[2,1]=year_different_minus_senario
     table[2,2]=year_asset_minus_senario
     colnames(table)<-append("수지적자(년)","기금소진(년)")
     rownames(table)<-append("기본가정", "시나리오")
   
     my_list<-list('결과비교'=table,  'plot'=p)
     return(my_list)
   }


test<-Fund(showInsur_man, showInsur_woman)
setest<-senario(showInsur_man, showInsur_woman, sereturn)
plot<-scenario_plot(showInsur_man, showInsur_woman, sereturn)
