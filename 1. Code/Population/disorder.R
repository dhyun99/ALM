disorder_rate<-read_excel(paste(Path , "disorder_rate.xlsx", sep = ''))#장애발생률
disorder_2019<-read_excel(paste(Path , "disorder_2019.xlsx", sep = ''))#2019 장애수급자
disorder_2019 <- disorder_2019[-(1:2),]
disorder_2019 <- disorder_2019[,-1]
names(disorder_2019) <- c('연령','남자','여자')
disorder_man <- disorder_2019[,2,drop= FALSE]
rownames(disorder_man)<- c('20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80세이상')
sub_man <- as.data.frame(pension_sub(Path, result_man, result_woman, pension_m, health_m, employ_m, pension_w, health_w, employ_w)['남자총가입자'])
for(i in 1:9){
  for(j in 1:9){
    toadd <-sum(sub_man[j,(5*(i-1)+1):(5*(i-1)+6)])* disorder_rate[j,2]/100
    disorder_man[j,i+1] <- toadd
  }
  for(j in 1:13){
    if(j==13){
      disorder_man[j,i+1]<- as.numeric(disorder_man[j, i+1])
      disorder_man[j,i+1] <- as.numeric(disorder_man[j,i])
    }
    else{
      disorder_man[j+1,i+1]<- as.numeric(disorder_man[j+1, i+1])
      disorder_man[j+1,i+1]<- as.numeric(disorder_man[j,i]) + as.numeric(disorder_man[j+1,i+1])
    }

  }
}
ncol(disorder_man)
disorder_receive <- function(Path){
  

}
