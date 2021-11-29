library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "ALM System"
  ), ## header
  
  dashboardSidebar(## Sidebar
    sidebarMenu(
      # menuItem("Intro", tabName = "Intro", icon = icon("th")
      # ),
      
      menuItem("Input", tabName = "input", icon = icon("th")
      ),
      menuItem("Output_basic", tabName = "Output_basic", icon = icon("dashboard")
      ),
      menuItem("Output_result", tabName = "Output_result", icon = icon("dashboard")
      ),
      menuItem("Output_hedge", tabName = "output_hedge", icon = icon("dashboard")
      ),
      menuItem("Output_VaR", tabName = "output_VaR", icon = icon("dashboard")
      ),
      menuItem("Output_Constraint_Result", tabName = "output_constraint_result", icon = icon("dashboard")
      )
    )
  ),
  
  dashboardBody(## Body
    tabItems(
      # First tab content
      # tabItem(tabName = "Intro"),
      
      tabItem(tabName = "input",
              fluidRow(
                
                column(10,
                       fileInput("file", h3("엑셀 데이터"),accept = c(".zip"))
                       
                ),
                column(3,radioButtons("category", 
                                      h3("보험종류 선택"), 
                                      choices = list("국민연금" = 1, 
                                                     "고용보험" = 2),
                                      selected = 1)),
              ),
              fluidRow(
                column(4,
                       textInput("inf", h3("물가상승률"),
                                 value = "%단위로 입력해주세요.")
                       
                ),
                column(4,
                       textInput("wage", h3("임금상승률"),
                                 value = "%단위로 입력해주세요.")
                       
                ),
                column(4,
                       textInput("sereturn", h3("기본가정 수익률 조정값"),
                                 value = "% 단위로 입력해주세요.")
                       
                )
                
              ),fluidRow(
                
                column(3, 
                       textInput("ben_a", h3("국내채권 기대수익률"), 
                                 value = "% 단위로 입력해주세요")   
                ),
                column(3, 
                       textInput("ben_b", h3("국내주식 기대수익률"), 
                                 value = "% 단위로 입력해주세요")   
                ),
                column(3, 
                       textInput("ben_c", h3("해외채권 기대수익률"), 
                                 value = "% 단위로 입력해주세요")   
                ),
                column(3, 
                       textInput("ben_d", h3("해외주식 기대수익률"), 
                                 value = "% 단위로 입력해주세요")   
                )
              ),fluidRow(
                
                column(4, 
                       textInput("income_inc", h3("사회보험 수입 기대증감율"), 
                                 value = "% 단위로 입력해주세요")   
                ),
                
                column(4, 
                       textInput("cost_inc", h3("사회보험 지출 기대증감율"), 
                                 value = "% 단위로 입력해주세요")   
                ),
                column(4, 
                       textInput("hedge_num", h3("헤지 비율"), 
                                 value = "0~100까지 10단위로 입력해주세요")   
                )
              ),fluidRow(column(4, 
                                textInput("MVO_num", h3("MVO 시뮬레이션 수"), 
                                          value = "예) 5000,10000 등")),
                         column(4, 
                                textInput("n", h3("Random Return 생성 수"), 
                                          value = "예) 100,500 등")),
                         column(4, 
                                textInput("RF", h3("무위험 수익률"), 
                                          value = "예) 1%,5% 등"))
                         
                         
              ),
              
              fluidRow(column(3,
                              radioButtons("VarUse", 
                                           h3("VaR 측정 방법 선택"), 
                                           choices = list("모수적 방법" = 1, 
                                                          "비모수적 방법" = 2,
                                                          "EWMA 방법" = 3
                                           ),
                                           selected = 1)
              ),
              column(3, 
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
                     textInput("Risk1_Th", h3("허용위험한도(1) 임계수익률"), 
                               value = "예) 0%, 5% 등")   
              ),
              
              column(3, 
                     textInput("Risk1_Yr", h3("허용위험한도(1) 통제기간"), 
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
                     textInput("Risk2_Th", h3("허용위험한도(2) 임계수익률"), 
                               value = "예) 0%, 5% 등")   
              ),
              
              column(3,
                     textInput("Risk2_Yr", h3("허용위험한도(2) 통제기간"), 
                               value = "예) 1,3,5 등")   
              ),
              
              column(3, 
                     textInput("Risk2_Limit", h3("허용위험한도(2) 한도"), 
                               value = "예) 0%, 5% 등")   
              )
              ),
              
              fluidRow(column(3,offset = 10,
                              br(),
                              br(), 
                              actionButton("action","제출"))  
              )
      ),
      # Second tab content
      tabItem(tabName = "Output_basic",
              fluidRow(
                column(3, 
                       tableOutput("setable")   
                ),
                
                column(4, 
                       plotOutput("seplot",width = "200%")   
                )
              ),
              fluidRow(
                column(1, 
                       textOutput("basic_txt") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("origin") 
                )),
              fluidRow(
                column(1, 
                       textOutput("senario_txt") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("senario") 
                ))
      ),
      tabItem(tabName = "Output_result",
              
              fluidRow(
                column(4, 
                       plotOutput("Fund_plot",width = "200%")   
                       
                )),
              fluidRow(
                column(3, 
                       tableOutput("Fund_table")   
                ))
      ),
      
      tabItem(tabName = "output_hedge",
              fluidRow(
                column(1, 
                       textOutput("txt0") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged0") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt1") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged1") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt2") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged2") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt3") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged3") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt4") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged4") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt5") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged5") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt6") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged6") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt7") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged7") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt8") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged8") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt9") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged9") 
                )),
              fluidRow(
                column(1, 
                       textOutput("txt10") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_hedged10") 
                ))),
      # Third tab content
      tabItem(tabName = "output_VaR",
              fluidRow(
                column(1, 
                       textOutput("txtmean") 
                )),
              fluidRow(
                column(3, 
                       tableOutput("table_mean")   
                )
                
              ),fluidRow(
                column(3, 
                       tableOutput("VaRTable")   
                ),
                
                column(4, 
                       plotOutput("plot7",width = "200%")   
                )
              )
      ),
      tabItem(tabName = "output_constraint_result",
              fluidRow(
                column(12,
                       tableOutput('table_avg_weight')
                )
              ),
              
              
              fluidRow(
                column(4, 
                       plotOutput("plot5",width = "125%")   
                ),
                column(4, 
                       plotOutput("plot6",width = "125%")   
                ),
                
              )
      )
    )
  ) 
)