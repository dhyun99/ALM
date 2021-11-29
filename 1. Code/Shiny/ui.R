ui <- dashboardPage(
  dashboardHeader(title = "HF ALM System"
  ), ## header
  
  dashboardSidebar(## Sidebar
    sidebarMenu(
      menuItem("Intro", tabName = "Intro", icon = icon("th")
      ),
      
      menuItem("Input", tabName = "input", icon = icon("th")
      ),
      menuItem("Output_MVO", tabName = "output_mvo", icon = icon("dashboard")
      ),
      menuItem("Output_ResampledMVO", tabName = "output_rmvo", icon = icon("dashboard")
      ),
      menuItem("Output_Constraint_Result", tabName = "output_constraint_result", icon = icon("dashboard")
      )
    )
  ),
  
  dashboardBody(## Body
    tabItems(
      # First tab content
      tabItem(tabName = "Intro"),
      
      tabItem(tabName = "input",
              fluidRow(
                
                column(10,
                       fileInput("file", h3("자산군관련 데이터"),accept = c(".xlsx"))
                       
                ),
                
                column(5,
                       radioButtons("BenchUse", 
                                    h3("벤치마크 사용여부"), 
                                    choices = list("벤치마크 사용" = 1, 
                                                   "벤치마크 미사용" = 2
                                                   
                                    ),
                                    selected = 1
                       )
                ),
                column(4, 
                       textInput("RiskFree_Rate", h3("무위험 수익률"), 
                                 value = "% 단위로 입력해주세요")   
                )
              ),fluidRow(
                
                column(4, 
                       radioButtons("Expected_Ret", 
                                    h3("기대수익률"), 
                                    choices = list("Mean 사용" = 1, 
                                                   "Median 사용" = 2, 
                                                   "임의의 값 입력" = 3
                                    ),
                                    selected = 1
                       )
                ),
                
                column(4, 
                       radioButtons("Stdev", 
                                    h3("변동성"), 
                                    choices = list("계산되는 표준편차 사용" = 1, 
                                                   "임의의 값 입력" = 2 
                                    ),
                                    selected = 1
                       )
                       
                ),
                column(4,
                       radioButtons("Correlation", h3("상관관계"),
                                    choices = list("계산되는 상관관계 사용" = 1,
                                                   "임의의 값 입력" = 2
                                    ),
                                    selected = 1
                       )
                )
              ),fluidRow(column(3, 
                                textInput("MVO_num", h3("MVO 시뮬레이션 수"), 
                                          value = "예) 5000,10000 등")),
                         column(3, 
                                textInput("RMVO_num", h3("Random Return 생성 수"), 
                                          value = "예) 200,1000 등")),
                         column(3, 
                                textInput("RandomSeed", h3("랜덤 Seed 입력"), 
                                          value = "예) 2020,2021 등"))
                         
              ),fluidRow(column(3, 
                                textInput("NeedRet", h3("목표 수익률"), 
                                          value = "예) 1%,5% 등")),
                         column(3, 
                                textInput("NeedVol", h3("목표 변동성"), 
                                          value = "예) 1%,5% 등")),
                         column(3, 
                                radioButtons("DepositFixed", h3("예금 고정 여부"), 
                                             choices = list("Fixed" = 1,
                                                            "Not Fixed" = 2
                                             ),selected = 1)),
                         column(3, 
                                textInput("FixedValue", h3("고정 비율"), 
                                          value = "예) 0.2,0.3 등"))),
              
              fluidRow(column(3, 
                              textInput("Exposure", h3("익스포져"), 
                                        value = "예) 70000,100000 등")),
                       column(3, 
                              textInput("VarConfidence", h3("VaR 신뢰수준"), 
                                        value = "예) 95%,99% 등")),
                       column(3, 
                              textInput("ManageTime", h3("관리기간(日)"), 
                                        value = "예) 5,10 등")))
              
              ,fluidRow(column(3, 
                               textInput("Risk1_Name", h3("허용위험한도(1) 명칭"), 
                                         value = "예) 원금,CPI 등")   
              ),
              column(3, 
                     textInput("Risk1_Threshold", h3("허용위험한도(1) 임계수익률"), 
                               value = "예) 0%, 5% 등")   
              ),
              
              column(3, 
                     textInput("Risk1_Year", h3("허용위험한도(1) 통제기간"), 
                               value = "예) 1,3,5 등")   
              ),
              
              column(3, 
                     textInput("Risk1_Limit", h3("허용위험한도(1) 한도"), 
                               value = "예) 0%, 5% 등")   
              )
              )
              ,fluidRow(column(3, 
                               textInput("Risk2_Name", h3("허용위험한도(2) 명칭"), 
                                         value = "예) 원금,CPI 등")   
              ),
              column(3, 
                     textInput("Risk2_Threshold", h3("허용위험한도(2) 임계수익률"), 
                               value = "예) 0%, 5% 등")   
              ),
              
              column(3,
                     textInput("Risk2_Year", h3("허용위험한도(2) 통제기간"), 
                               value = "예) 1,3,5 등")   
              ),
              
              column(3, 
                     textInput("Risk2_Limit", h3("허용위험한도(2) 한도"), 
                               value = "예) 0%, 5% 등")   
              )
              ),
              fluidRow(column(3, 
                              textInput("Risk3_multiple", h3("허용위험한도(3) 배수"), 
                                        value = "예) 5,10,12")   
              ),
              
              column(3, 
                     textInput("IL_allowance", h3("허용위험한도(3) 한도"), 
                               value = "예) 0%, 5% 등")
              ),
              column(3,
                     radioButtons("Resampled_or_True", 
                                  h3("Mean Vector Type"), 
                                  choices = list("Given" = 1, 
                                                 "Resampled" = 2 
                                  ),
                                  selected = 1)
              )
              ),
              fluidRow(column(3,offset = 10,
                              br(),
                              br(), 
                              actionButton("action","제출"))  
              )
      ),
      # Second tab content
      tabItem(tabName = "output_mvo",
              fluidRow(
                column(5, 
                       plotlyOutput("plot2",width = "250%")   
                )
                
              )),
      # Third tab content
      tabItem(tabName = "output_rmvo",
              fluidRow(
                column(5, 
                       plotOutput("plot3",width = "125%")   
                )),
              fluidRow(
                column(5, 
                       plotOutput("plot4",width = "125%")   
                )
              )
      ),
      tabItem(tabName = "output_constraint_result",
              fluidRow(
                column(12,
                       tableOutput('table')
                )
              ),
              fluidRow(
                column(3,
                       tableOutput('table_VaR')
                ),column(3,
                         tableOutput('table_Risk')
                         
                )),
              
              fluidRow(
                column(4, 
                       plotOutput("plot5",width = "90%")   
                ),
                column(4, 
                       plotOutput("plot6",width = "90%")   
                ),
                column(4, 
                       plotOutput("plot7",width = "90%")   
                )
              )
      )
    )
  ) 
)