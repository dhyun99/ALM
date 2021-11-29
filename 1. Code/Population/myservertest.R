source('hedged_new.R')

server <- function(input, output) {
  # Submit 버튼클릭시 변수 받아지는 곳.
  v <- reactiveValues(data = NULL)
  exceldata <- reactiveValues(data = NULL)
  Result<- reactiveValues(data = NULL)
  
  #경로
  observeEvent(input$action, {
    v$Path <- input$Path
  })
  #사회보험 선택
  observeEvent(input$action, {
    v$category<-input$category
  })
  #물가상승률
  observeEvent(input$action, {
    v$inf <- input$inf
  })
  #임금상승률
  observeEvent(input$action, {
    v$wage <- input$wage
  })
  #시나리오 수익률 조정값
  observeEvent(input$action, {
    v$sereturn <- input$sereturn
  })
  #국내채권 기대수익률
  observeEvent(input$action, {
    v$ben_a<-input$ben_a
  })
  #국내주식 기대수익률
  observeEvent(input$action, {
    v$ben_b<-input$ben_b
  })
  #해외채권 기대수익률
  observeEvent(input$action, {
    v$ben_c<-input$ben_c
  })
  #해외주식 기대수익률
  observeEvent(input$action, {
    v$ben_d<-input$ben_d
  })
  #수입 증감률
  observeEvent(input$action, {
    v$income_inc<-input$income_inc
  })
  #지출 증감률
  observeEvent(input$action, {
    v$cost_inc<-input$cost_inc
  })
  #헤지비율
  observeEvent(input$action, {
    v$hedge_num<-input$hedge_num
  })
  ##  무위험 수익률 Control
  observeEvent(input$action, {
    v$RF<-input$RF
  })
  
  
  #시뮬레이션 횟수
  observeEvent(input$action, {
    v$n<-input$n
  })
  #MVO횟수
  observeEvent(input$action, {
    v$MVO_num<-input$MVO_num
  })
  
  #var 방법 선택
  observeEvent(input$action, {
    v$VarUse<-input$VarUse
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
    v$Risk1_Th<-input$Risk1_Th
  })
  ##  위험 허용한도 1 Control _ 기간
  observeEvent(input$action, {
    v$Risk1_Yr<-input$Risk1_Yr
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
    v$Risk2_Th<-input$Risk2_Th
  })
  ##  위험 허용한도 2 Control _ 기간
  observeEvent(input$action, {
    v$Risk2_Yr<-input$Risk2_Yr
  })
  ##  위험 허용한도 2 Control _ Max
  observeEvent(input$action, {
    v$Risk2_Limit<-input$Risk2_Limit
  })
  
  
  
  ##  Imported Data manage
  
  #observeEvent(input$action, {
  #exceldata$StockRet <-read_excel(paste(input$file$datapath, sep=""), sheet = "주식 Return")
  #exceldata$BenchRet <-read_excel(paste(input$file$datapath, sep=""), sheet = "벤치마크 Return")
  #})
  
  observeEvent(input$action, {
    print("senario make")
    Result$senario_plot <- senario_plot(v$Path, v$inf, v$wage, v$sereturn, v$category)
    print("weight make")
    Result$Simul_Plot<-Simul_Run(v$Path,v$ben_a, v$ben_b, v$ben_c, v$ben_d, v$income_inc, v$cost_inc, v$n, v$MVO_num, v$RF, v$hedge_num, v$Risk1_Name, v$Risk2_Name, v$Risk1_Th, v$Risk2_Th, v$Risk1_Yr,v$Risk2_Yr,v$Risk1_Limit,v$Risk2_Limit, v$inf, v$wage, v$category)
    print("weight Done")
    Result$Fund_result<-Fund_result(v$Path, v$inf, v$wage, v$category, Result$Simul_Plot$port_mean)
    Result$VaR<-parametric(v$Path, v$VarUse,Result$Simul_Plot$port_mean, Result$Simul_Plot$port_sd , v$VarConfidence, v$ManageTime,  v$Exposure)
    print("VaR Done")
  })
  #observeEvent(input$action, {
  #  Result$OMPlot<- OMRiskPlot(v$OM_mean , v$OM_stdev , v$OM_want)
  #}
  #)
  
  output$seplot<-renderPlot({
    print(Result$senario_plot$plot)
  })
  output$setable <- renderTable(
    Result$senario_plot$table,rownames = TRUE
  )
  output$origin <- renderTable(
    Result$senario_plot$origin,rownames = TRUE
  )
  output$senario <- renderTable(
    Result$senario_plot$senario,rownames = TRUE
  )
  output$basic_txt <- renderText({
    "basic assumption"
  })
  output$senario_txt <- renderText({
    "senario assumption"
  })
  output$txtmean <- renderText({
    "select hedge mean weight"
  })
  
  output$table_mean <- renderTable(
    Result$Simul_Plot$mean_table,rownames = TRUE
  )
  output$txt0 <- renderText({
    "0% hedge"
  })
  
  output$table_hedged0 <- renderTable(
    Result$Simul_Plot$hedged0,rownames = TRUE
  )
  output$txt1 <- renderText({
    "10% hedge"
  })
  output$table_hedged1 <- renderTable(
    Result$Simul_Plot$hedged1,rownames = TRUE
  )
  output$txt2 <- renderText({
    "20% hedge"
  })
  output$table_hedged2 <- renderTable(
    Result$Simul_Plot$hedged2,rownames = TRUE
  )
  output$txt3 <- renderText({
    "30% hedge"
  })
  output$table_hedged3 <- renderTable(
    Result$Simul_Plot$hedged3,rownames = TRUE
  )
  output$txt4 <- renderText({
    "40% hedge"
  })
  output$table_hedged4 <- renderTable(
    Result$Simul_Plot$hedged4,rownames = TRUE
  )
  output$txt5 <- renderText({
    "50% hedge"
  })
  output$table_hedged5 <- renderTable(
    Result$Simul_Plot$hedged5,rownames = TRUE
  )
  output$txt6 <- renderText({
    "60% hedge"
  })
  output$table_hedged6 <- renderTable(
    Result$Simul_Plot$hedged6,rownames = TRUE
  )
  output$txt7 <- renderText({
    "70% hedge"
  })
  output$table_hedged7 <- renderTable(
    Result$Simul_Plot$hedged7,rownames = TRUE
  )
  output$txt8 <- renderText({
    "80% hedge"
  })
  output$table_hedged8 <- renderTable(
    Result$Simul_Plot$hedged8,rownames = TRUE
  )
  output$txt9 <- renderText({
    "90% hedge"
  })
  output$table_hedged9 <- renderTable(
    Result$Simul_Plot$hedged9,rownames = TRUE
  )
  output$txt10 <- renderText({
    "100% hedge"
  })
  output$table_hedged10 <- renderTable(
    Result$Simul_Plot$hedged10,rownames = TRUE
  )
  output$Fund_table <- renderTable(
    Result$Fund_result$table,rownames = TRUE
  )
  output$Fund_plot<-renderPlot({
    print(Result$Fund_result$plot)
  })
  output$table_avg_weight <- renderTable(
    Result$Simul_Plot$average_weight_con,rownames = TRUE
  )
  output$plot5<-renderPlot({
    print(Result$Simul_Plot$FirstRiskPlot)
  })
  output$plot6<-renderPlot({
    print(Result$Simul_Plot$SecondRiskPlot)
  })
  output$VaRTable <- renderTable(
    Result$VaR$VaRTable,rownames = TRUE
  )
  output$plot7<-renderPlot({
    print(Result$VaR$VaRPlot)
  })
  
  
  
  
}