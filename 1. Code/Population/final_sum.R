result_sum <- result_man + result_woman
work_s <- work_m + work_w
income_sum <-read_excel(paste(Path , "income_sum.xlsx", sep = ''))
resultIncome_s <-income_sum[2:nrow(income_sum),3:ncol(income_sum),drop = FALSE]
resultIncome_s <- data.frame(resultIncome_s)
for(i in 1:48){
  toadd <- data.frame(lapply(resultIncome_s[i], B))
  colnames(toadd) <- 2019+i
  resultIncome_s <- cbind(resultIncome_s , toadd)
}

social_insurance <-read_excel(paste(Path , "social_insurance_age.xlsx", sep = ''))

social_insurance <- social_insurance[-1,-1] #필요없는 행열 제거
as.numeric(social_insurance[,])
#가입자수 구하는 함수
social_sub <- function(j,i,k){
  if(j<=3)
    return(work_s[j,i] * as.numeric(social_insurance[1,k])/100)
  else if(j>=4 & j<=5)
    return(work_s[j,i] * as.numeric(social_insurance[2,k])/100)
  else if(j>=6 & j<=7)
    return(work_s[j,i] * as.numeric(social_insurance[3,k])/100)
  else if(j>=8 & j<=9)
    return(work_s[j,i] * as.numeric(social_insurance[4,k])/100)
  else
    return(work_s[j,i] * as.numeric(social_insurance[5,k])/100)
}
  


df_showInsur <- data.frame('연도' = integer(), '연령'= integer(), '국민연금'= numeric(), '건강보험'= numeric(), '고용보험'= numeric())
for(i in 1:49){
  for(j in 1:11){
    if(j==1){
      year<- 2018+i
      income_earn <- as.numeric(resultIncome_s[j,i]) * earn # 근로소득
      pension <- social_sub(j,i,1)* income_earn * pension_insur
      health <- social_sub(j,i,2) * income_earn * health_insur
      employ <- social_sub(j,i,3) * income_earn * employment_insur
      age<-'15-19'
      df_showInsur <-rbind(df_showInsur, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
    else if(j==11){
      year<- 2018+i
      income_earn <- as.numeric(resultIncome_s[j,i]) * earn # 근로소득
      k <- k+1
      pension <-  social_sub(j,i,1)* income_earn * pension_insur
      health <- social_sub(j,i,2) * income_earn * health_insur
      employ <- social_sub(j,i,3) * income_earn * employment_insur
      age<- '65-'
      df_showInsur <-rbind(df_showInsur, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
    else{
      year<- 2018+i
      income_earn <- as.numeric(resultIncome_s[j,i]) * earn # 근로소득
      k <- k+1
      pension <- social_sub(j,i,1)* income_earn * pension_insur
      health <- social_sub(j,i,2) * income_earn * health_insur
      employ <- social_sub(j,i,3) * income_earn * employment_insur
      age <- as.character(paste0(as.character(10+5*j),'-',as.character(14+5*j)))
      df_showInsur <-rbind(df_showInsur, data.frame('연도' = year, '연령'= age, '국민연금'= pension, '건강보험'= health, '고용보험'= employ))
    }
  }
}

