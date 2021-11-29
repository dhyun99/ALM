source('1. Code/Shiny/Global.R')

server <- function(input, output) {
  # Submit 버튼클릭시 변수 받아지는 곳.
  v <- reactiveValues(data = NULL)
  exceldata <- reactiveValues(data = NULL)
  Result<- reactiveValues(data = NULL)
  ##  벤치마크 사용 유무
  observeEvent(input$action, {
    v$BenchType<-input$BenchUse
  })
  ##  기대 수익률 관련 Control
  observeEvent(input$action, {
    v$Expected_Ret<-input$Expected_Ret
  })
  ##  변동성 관련 Control
  observeEvent(input$action, {
    v$Stdev<-input$Stdev
  })
  ##  상관관계 Control
  observeEvent(input$action, {
    v$Correlation<-input$Correlation
  })
  ##  무위험 수익률 Control
  observeEvent(input$action, {
    v$RiskFree_Rate<-input$RiskFree_Rate
  })
  ##  목표 수익률 Control
  observeEvent(input$action, {
    v$NeedRet<-input$NeedRet
  })
  ##  목표 변동성 Control
  observeEvent(input$action, {
    v$NeedVol<-input$NeedVol
  })
  ## 예금 Fixed 여부
  observeEvent(input$action, {
    v$DepositFixed<-input$DepositFixed
  })
  ## Fixed Ratio Define
  observeEvent(input$action, {
    v$FixedValue<-input$FixedValue
  })
  
  ##  익스포져 Control
  observeEvent(input$action, {
    v$Exposure<-input$Exposure
  })
  ## Var 신뢰구간 Control
  observeEvent(input$action, {
    v$VarConfidence<-input$VarConfidence
  })
  ## 관리기간 Control
  observeEvent(input$action, {
    v$ManageTime<-input$ManageTime
  })
  
  ##  위험 허용한도 1 Control _ 변수이름
  observeEvent(input$action, {
    v$Risk1_Name<-input$Risk1_Name
  })
  ##  위험 허용한도 1 Control _ 기준
  observeEvent(input$action, {
    v$Risk1_Threshold<-input$Risk1_Threshold
  })
  ##  위험 허용한도 1 Control _ 기간
  observeEvent(input$action, {
    v$Risk1_Year<-input$Risk1_Year
  })
  ##  위험 허용한도 1 Control _Max
  observeEvent(input$action, {
    v$Risk1_Limit<-input$Risk1_Limit
  })
  
  ##  위험 허용한도 2 Control _ 변수이름
  observeEvent(input$action, {
    v$Risk2_Name<-input$Risk2_Name
  })
  ##  위험 허용한도 2 Control _ 기준
  observeEvent(input$action, {
    v$Risk2_Threshold<-input$Risk2_Threshold
  })
  ##  위험 허용한도 2 Control _ 기간
  observeEvent(input$action, {
    v$Risk2_Year<-input$Risk2_Year
  })
  ##  위험 허용한도 2 Control _ Max
  observeEvent(input$action, {
    v$Risk2_Limit<-input$Risk2_Limit
  })
  
  ##  위험 허용한도 3 Control _ 운용배수
  observeEvent(input$action, {
    v$Risk3_multiple<-input$Risk3_multiple
  })
  
  ## IL_allowance
  observeEvent(input$action, {
    v$IL_allowance<-input$IL_allowance
  })
  
  ## IL 어떤 Mean Vector 이용할지 여부
  observeEvent(input$action, {
    v$Resampled_or_True<-input$Resampled_or_True
  })
  
  
  ##  MVO Simulation number Control
  observeEvent(input$action, {
    v$MVO_num<-input$MVO_num
  })
  ##  RMVO Simulation number Control
  observeEvent(input$action, {
    v$RMVO_num<-input$RMVO_num
  })
  ##  Random Seed
  observeEvent(input$action, {
    v$RandomSeed<-input$RandomSeed
  })
  
  
  
  ##  Imported Data manage
  
  #observeEvent(input$action, {
  #exceldata$StockRet <-read_excel(paste(input$file$datapath, sep=""), sheet = "주식 Return")
  #exceldata$BenchRet <-read_excel(paste(input$file$datapath, sep=""), sheet = "벤치마크 Return")
  #})
  
  observeEvent(input$action, {
    print("MVO make")
    Result$MVO <- MVOmaker(paste(input$file$datapath, sep="") , v$BenchType , v$RiskFree_Rate, v$Expected_Ret ,v$Stdev, v$Correlation ,v$MVO_num , v$RandomSeed , v$DepositFixed, v$FixedValue)  
    print("MVO Done")
    print("RMVO make")
    Result$RMVOplot<- RMVORun(paste(input$file$datapath, sep=""), v$BenchType ,v$RiskFree_Rate, v$Expected_Ret , v$Stdev,v$Correlation ,v$MVO_num,v$RMVO_num,v$RandomSeed,v$DepositFixed,v$FixedValue,v$NeedRet, v$Risk1_Name , v$Risk2_Name, v$Risk1_Threshold, v$Risk2_Threshold, v$Risk1_Year,v$Risk2_Year ,v$Risk1_Limit,v$Risk2_Limit,v$Risk3_multiple, v$IL_allowance , v$Resampled_or_True)
    print("RMOV Done")
    Result$PrincipalInfo <- PrinDefine(v$Risk1_Name , v$Risk2_Name, v$Risk1_Threshold, v$Risk2_Threshold, v$Risk1_Year,v$Risk2_Year ,v$Risk1_Limit,v$Risk2_Limit )
    Result$VarTable <- VARAnalysis(v$NeedRet, v$NeedVol , Result$PrincipalInfo , v$Exposure ,v$VarConfidence , v$ManageTime)
    print("VaR Analysis Done")
  })
  
  #observeEvent(input$action, {
  #  Result$OMPlot<- OMRiskPlot(v$OM_mean , v$OM_stdev , v$OM_want)
  #}
  #)
  
  output$plot2<-renderPlotly({
    print(ggplotly(Result$MVO))})
  
  output$plot3<-renderPlot({
    print(Result$RMVOplot$EFFGroup)
  })
  output$plot4<-renderPlot({
    print(Result$RMVOplot$STD.Weight)
  })
  output$table <- renderTable(
    Result$RMVOplot$WeightTable,rownames = TRUE
  )
  output$plot5<-renderPlot({
    print(Result$RMVOplot$FirstRiskplt)
  })
  output$plot6<-renderPlot({
    print(Result$RMVOplot$SecondRiskplt)
  })
  output$table_VaR <- renderTable(
    Result$VarTable,rownames = TRUE
  )
  output$plot7<-renderPlot({
    print(Result$RMVOplot$ThridRiskplt)
  })
  output$table_Risk <- renderTable(
    Result$RMVOplot$RiskTable,rownames = TRUE
  )
  
  
  
}