  library(shiny)
  library(shinydashboard)
  library(data.table)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(gridExtra)
  library(DT)
  library(highcharter)
  library(outliers)
  library(sdcMicro)
  library(diffpriv)
  library(shinycssloaders)
  library(jpeg)
  library(png)
  library(imager)
  library(reshape2)
  library(httr)
  
  options(shiny.maxRequestSize=30000*1024^2) # 메모리 세팅
  options(scipen=999) #지수로 표기할 자리수 세팅 
  options(shiny.port=8802)
  
  ##### UI ######
  ui <- dashboardPage(
    skin="black",
    dashboardHeader(title ="가명처리 솔루션"),
    
    #### 사이드 바 UI ####
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        
        menuItem("사전준비", tabName = "dashboard", icon = icon("clipboard-check")),
        
        menuItem(
          "정형데이터", icon = icon("th"), startExpanded = TRUE,
          menuSubItem("대상선정", tabName = "subitem1"),
          menuSubItem("위험도 측정", tabName = "subitem2"),
          menuSubItem("가명처리 수준정의", tabName = "subitem3"),
          menuSubItem("보고서 생성", tabName = "subitem4")
        ),
        
        menuItem(
          "이미지데이터", icon = icon("bar-chart-o"), startExpanded = TRUE,
          menuSubItem("이미지 불러오기", tabName = "subitem5"),
          menuSubItem("이미지 정보", tabName = "subitem6"),
          menuSubItem("표현", tabName = "subitem7")
        ),
        
        menuItem("적정성검토 및 추가 가명처리", tabName="check", icon = icon("user-check")),
        
        menuItem("활용 및 사후관리", tabName="last", icon = icon("receipt"))
      )
    ),
    #######################
    
    #### 각 항목별 UI ####
    dashboardBody(
      tabItems(
        tabItem(tabName= "dashboard", 
                fluidPage(
                  plotOutput("plot3")
                ),
        ),
        
        #### Item1 ####
        tabItem(
          tabName = "subitem1", h2("대상선정"),
          fluidPage(
            fluidRow(
              column(
                4,
                box(
                  fileInput('file1', 'Choose CSV File', accept = c('.csv','.tsv')),
                  tags$script('$("#file1").on("click", function(){this.value = null;});'),
                  width = 12,
                ),
                box(
                  # strong("summary"),
                  h4(uiOutput("input_summary")),
                  width = 12,
                ),
                box(
                  actionButton("button_1to2", "대상 선정 완료",),
                  width = 12,
                )
              ),
              column(
                8,
                box(
                  title = "데이터 테이블", #status = "primary", solidHeader = TRUE, #,collapsible = TRUE
                  DT::dataTableOutput("print_original_input"),
                  width = 12
                ),
              )
            ),
          )
        ),
        ###############
        
        #### Item2 ####
        tabItem(
          tabName = "subitem2", h2("위험도 측정"),
          fluidPage(
            fluidRow(
              column(
                5,
                box(
                  uiOutput("selectinput_datacols"),
                  width = 12
                ),
                tabBox(
                  width = 12,
                  id = "tabset2-1",
                  height = "450px",
                  selected = "처리 환경",
                  tabPanel(
                    title = "처리 환경",
                    box(
                      selectInput('selectinput_danger_env1', '이용 및 활용 시스템',
                                  choices = c("내부 시스템"='1', "내부 및 외부 시스템" = '2', "외부 시스템"='3')),
                      selectInput('selectinput_danger_env2', '활용 목적', 
                                  choices = c("내부활용"='1', "외부제공" = '2',"외부결합" = '3')),
                      selectInput('selectinput_danger_env3', '개인정보 보호수준', 
                                  choices = c("개인정보보호 인증"='1', "정보보호 인증" = "2", "인증 없음"="3")),
                      selectInput('selectinput_danger_env4', '물리적 환경',  
                                  choices = c("통제공간"='1', "제한공간" = "2","접견공간"="3")),
                      width = 12,
                    ),
                    ##
                  ),
                  tabPanel(
                    title = "개인정보 속성", 
                    box(
                      fileInput('file2', '개인정보 속성 파일 불러오기', accept = c('.csv','.tsv')),
                      actionButton("button_load_idx", "불러온 파일 적용"),
                      width = 12
                    ),
                    box(
                      radioButtons('radiobutton_select_idx', '개인정보 속성 선택', choices = c("ID", "QI", "SA", "NSA"), inline = TRUE, select = "ID"),
                      actionButton("button_idx", "적용"),
                      actionButton("button_reset_idx", "초기화"),
                      actionButton("button_all_idx", "일괄적용"),
                      width = 12
                    ),
                    box(
                      actionButton("button_risk_analysis", "위험도 분석")
                    )
                  ),
                  tabPanel(
                    title = "데이터 타입/유형",
                    box(
                      radioButtons('radiobutton_data_type', '데이터 타입 변경', choices = c("숫자", "문자", "바이너리", "날짜"), inline = TRUE, select = "숫자"),
                      actionButton("button_data_type", "적용"),
                      width = 12
                    ),
                    box(
                      radioButtons('radiobutton_data_cate', '데이터 유형 변경', choices = c("범주형", "연속형"), inline = TRUE, select = "범주형"),
                      actionButton("button_data_cate", "적용"),
                      width = 12
                    )
                  ),
                  tabPanel(
                    title = "그래프",
                    highchartOutput("show_chart"),
                  ),
                  tabPanel(
                    title = "K값",
                    div(
                      style = 'min-height: 350px; max-height: 350px; overflow-y: scroll; position:relative',
                      uiOutput("checkbox_idx_qi"),
                    ),
                    splitLayout(
                      cellWidths = c("33%", "33%", "33%"),
                      actionButton("button_check_k", "유일성 분석", class = "button blue", width = 120),
                      actionButton("button_delete_k", "K값 초기화", width = 120),
                      actionButton("button_decide_k", "K값으로 지정", width = 120)
                    )
                  )
                ),
                box(
                  actionButton("button_2to3", "위험도 분석 완료"),
                  width = 12
                )
              ),
              column(
                7,
                tabBox(
                  width = 12,
                  id = "tabset2-2",
                  selected = "개인정보 속성",
                  tabPanel(
                    "개인정보 속성", 
                    DT::dataTableOutput("print_idx_table"),
                    width = 12
                  ),
                  tabPanel(
                    "항목별 위험도",
                    DT::dataTableOutput("print_risk_analysis"),
                    width = 12
                  ),
                  tabPanel(
                    "총 위험도",
                    DT::dataTableOutput("print_risk_test"),
                    width = 12
                  ),
                  tabPanel(
                    "유일성 분석", 
                    DT::dataTableOutput("print_k_danger_table"),
                    width = 12
                  ),
                  tabPanel(
                    "K값", 
                    DT::dataTableOutput("print_k_table"),
                    width = 12
                  )
                )
              )
            ),
          )
        ),
        ###############
        
        #### Item3 ####
        tabItem(
          tabName = "subitem3", h2("가명처리 수준정의"),
          fluidPage(
            fluidRow(
              column(
                width = 4,
                tabBox(
                  width = 12,
                  height = "550px",
                  selected = "가명처리 수준정의",
                  tabPanel(
                    title = "가명처리 수준정의", 
                    selectInput('selectinput_idx_3', '항목 선택', choices = c("ID", "QI", "SA", "NSA")),
                    uiOutput("selectinput_datacols_item3"),
                    uiOutput("selectinput_process"),
                    uiOutput("process_support"),
                    actionButton("button_decide_process", "적용"),
                    width = 12
                  ),
                  tabPanel(
                    title = "K값",
                    DT::dataTableOutput("print_final_k"),
                    numericInput("k_level", "제거할 K값 수준:", 0, min = 0, max = 20),
                    width = 12
                  )
                ),
                box(
                  actionButton("button_3to4", "가명처리"),
                  width = 12
                )
              ),
              column(
                width = 8,
                fluidRow(
                  box(
                    title = "가명 처리 기준 정의",
                    DT::dataTableOutput("print_process"),
                    width = 12
                  ),
                )
              )
            ),
          )
        ),
        ###############
        
        #### Item4 ####
        tabItem(
          tabName = "subitem4",  h2("보고서 생성"),
          fluidPage(
            fluidRow(
              tabBox(
                width = 12,
                tabPanel(
                  title = "보고서",
                  fluidRow(
                    box(downloadButton("downloadData", "Download")),
                    box(
                      title = "가명 처리 데이터",
                      shinycssloaders::withSpinner(DT::dataTableOutput("print_output")),
                      width = 12
                    ),
                    box(
                      title = "가명처리 검토 결과 보고서",
                      dataTableOutput("report_user_info"),
                      br(),
                      dataTableOutput("report_danger_input"),
                      br(),
                      dataTableOutput("report_danger_output"),
                      br(),
                      dataTableOutput("report_summary_input"),
                      br(),
                      dataTableOutput("report_summary_output"),
                      br(),
                      dataTableOutput("print_total_summary"),
                      width = 12
                    ),
                    width = 12
                  ),
                  width = 12
                ),
                tabPanel(
                  title = "그래프",
                  div(
                    style = 'min-height: 1000px; max-height: 15000px; overflow-y: scroll; position:relative',
                    plotOutput("all_chart", width = "100%"),
                  ),
                  # plotOutput("all_chart", width = "100%"),
                  width = 12
                  
                )
              )
            ),
          )
        ),
        ###############
        
        #### Item5 ####
        tabItem(
          tabName = "subitem5",
          fluidPage(
            box(
              fileInput(
                inputId = "choose_files",
                label = "Input files",
                accept = c('image/png', 'image/jpeg','image/jpg')
              ),
              tags$hr()),
          ),
          
          box(
            shinycssloaders::withSpinner(plotOutput("plot_image"))
          ),
        ),
        ###############
        
        #### Item6 ####
        tabItem(tabName = "subitem6",
                
                fluidPage(
                  fluidRow(
                    column(
                      width = 4,
                      tabBox(
                        width = 12,
                        height = "110px",
                        selected = "인식된 사람 수",
                        tabPanel(
                          title = "인식된 사람 수", 
                          box(
                            uiOutput("imgtest2")
                          )
                        ),
                        tabPanel(
                          title = "이미지 픽셀", 
                          box(
                            uiOutput("imgtest3")
                          )
                        )
                      )
                    )
                  )
                ),
                fluidPage(
                  box(
                    DT::dataTableOutput("imgtest"),
                    width = 12
                  ),
                ),
                # box(
                #   shinycssloaders::withSpinner(plotOutput("dp_image"))
                # ),
        ),
        ###############
        
        #### Item7 ####
        tabItem(
          tabName = "subitem7",
          
          fluidPage(
            box(
              selectInput('selectinput_image', 'image 처리 방법 선택', choices = c("GRAY", "ALL", "GROUP")),
            ),
          ),
          fluidPage(
            box(
              conditionalPanel(
                condition = "input.selectinput_image == 'GRAY'",
                shinycssloaders::withSpinner(plotOutput("plot_image2"))
              ),
              conditionalPanel(
                condition = "input.selectinput_image == 'ALL'",
                selectInput('dp_choice', '입실론 값 선택', choices = c("0.2", "0.4", "0.6", "0.8", "1.0")),
                actionButton("dp_button", "적용하기"),
                shinycssloaders::withSpinner(plotOutput("dp_image"))
              ),
              conditionalPanel(
                condition = "input.selectinput_image == 'GROUP'",
                actionButton("dp_button2", "적용하기"),
                shinycssloaders::withSpinner(plotOutput("dp_image2"))
              )
            ),
          )
        ),
        ###############
        
        tabItem(tabName= "check", 
                fluidPage(
                  plotOutput("plot4")
                ),
        ),

        tabItem(tabName= "last", 
                fluidPage(
                  plotOutput("plot5")
                ),
        )
      )
    )
    ####################
  )
  
  
  
  ##### 서버 #####
  server <- function(input, output, session) {
    
    #### 테이블 생성 ####
    
    ## 식별자 분류 테이블 생성
    values <- reactiveValues(
      process_level = 0,
      
      # Item 1
      original = NULL,
      
      # Item 2
      idx_table = NULL, 
      risk_table = NULL,
      k_values = NULL,
      risk_env = NULL,
      
      
      # Item 3
      k_danger_table = NULL,
      process_table = NULL,
      process_support_table_1 = NULL,
      process_support_table_2 = NULL,
      
      # Item 4
      output_table = NULL,
      outlier_table = NULL,
      output_idx_table = NULL,
      chart_table = NULL,
      chart_size = 1000,
      
      # Image
      img_output = NULL,
      
      stringsAsFactors = F
    )
    
    ## CSV 파일 불러오기
    tableData <- reactive({
      inFile <- input$file1
      if (!is.null(inFile)) {
        guessEncoding <-  guess_encoding(inFile$datapath, n_max = 1000)
        if(guessEncoding[1,1] =="ASCII") 
        { guessEncoding[1,1] <- "UTF-8" }
        fread(inFile$datapath, encoding = as.character(guessEncoding[1,1]))
        # fread(inFile$datapath, encoding = "UTF-8")
      }
    })
    
    ## 개인정보 속성 CSV 파일 불러오기
    tempdata <- reactive({
      inFile <- input$file2
      if (!is.null(inFile)) {
        guessEncoding <-  guess_encoding(inFile$datapath, n_max = 1000)
        if(guessEncoding[1,1] =="ASCII") 
        { guessEncoding[1,1] <- "UTF-8" }
        fread(inFile$datapath, encoding = as.character(guessEncoding[1,1]))
        #fread(inFile$datapath, encoding = "UTF-8")
      }
    })
    
    #####################
    
    
    #### 테이블 출력 ####
    
    ## 원본 데이터 출력 - Item1
    output$print_original_input <- DT::renderDataTable({
      DT::datatable(
        tableData(), 
        filter ="top", 
        options = list(pageLength = 15, scrollX=TRUE),
        #options = list(pageLength = 15, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), scrollX=TRUE, dom ='Bfrtip'),
        extensions = c('Buttons'))
    })
    
    ## 식별자 분류 테이블 출력 - Item2
    output$print_idx_table<- renderDataTable({
      data <- values$idx_table
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), stringsAsFactors=FALSE) 
      names(data) <- c("column명", "개인정보 속성", "데이터 타입", "데이터 유형", "유일한 값 수", "특이정보 가능성")
      datatable(
        data,
        options = list(
          # lengthMenu = list(c(15, 30, -1), c('15', '30', 'All')),
          columnDefs = list(list(className = 'dt-center', targets = c(2,6))),
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        extensions = 'Buttons'
      ) %>% formatStyle(columns = "개인정보 속성", backgroundColor = styleEqual("NA", "red"))
    })
    
    ## 위험도 분석 출력 - Item2
    output$print_risk_analysis <- renderDataTable({
      data <- values$risk_table
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), stringsAsFactors=FALSE)
      names(data) <- c("column명", "레코드 수 1000개 이상", "식별가능성 높음", "특이정보 가능성", "위험도 수준")
      datatable(
        data,
        options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 15),
        extensions = 'Buttons'
      )%>% formatStyle(columns = "위험도 수준", backgroundColor = styleEqual(c("하", "중", "상"), c("silver", "Azure", "Red")))
    })
    
    
    ## 위험도 분석 출력 - Item3(check@)
    output$print_risk_test <- renderDataTable({
      
      data1 <- mean(values$risk_env)
      data4 <- sum(values$risk_data)
      data2 <- (mean(values$risk_data))
      
      if ( ((data1/4)+data2) >4){
        data3<- '상'
      }
      else if ( 2 < ((data1/4)+data2) &&  ((data1/4)+data2) <= 4  ){
        data3<- '중'
      }
      else
      {
        data3<- '하'
      }
      
      data <- cbind(data1,data4,data3)
      
      datatable(
        data,
        colnames = c("처리(제공)환경 위험도", "항목별 위험도", "위험도 수준"),
        options = list(dom = 't', pageLength = 15)
      )
    })
    
    
    ## 유일성 분석 출력 - Item2
    output$print_k_danger_table <- DT::renderDataTable({
      data <-values$k_danger_table
      if(is.null(data))
        data <- data.frame(except_column=character(), column_name=character(), unique_count=character(), unique_risk=character(), stringsAsFactors = F)
      names(data) <- c("미포함 column", "K값", "K = 1의 개수", "K = 1 퍼센트")
      DT::datatable(
        data, 
        options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 10), 
        extensions = 'Buttons'
      ) %>% formatStyle(
        'K = 1 퍼센트',
        background = styleColorBar(as.numeric(data[, 4]), 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center')
    })
    
    ## K 익명성 출력 - Item2
    output$print_k_table <- renderDataTable({
      k_list <- values$k_values
      data <- values$idx_table
      data <- data[data$tableName %in% k_list, ]
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), stringsAsFactors=FALSE)
      names(data) <- c("column명", "개인정보 속성", "데이터 타입", "데이터 유형", "유일한 값", "특이치 유무")
      datatable(
        data,
        options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 10),
        extensions = 'Buttons'
      )
    })
    
    ## 지정된 K값 출력 - Item3
    output$print_final_k <- DT::renderDataTable({
      # data <- values$k_danger_table[1, -c(1)]
      data <- values$k_values
      # output_idx_tableName <- input$checkbox_idx_qi
      tableDataDT <- data.table(values$original)
      tableCnt <- nrow(tableDataDT)
      resultTbl <- data.frame(column_name=NA, unique_count=NA, unique_risk=NA, stringsAsFactors = F)
      
      # 모든 데이터 위험성
      groupByCntTbl <- tableDataDT[, .N, by=data]
      uniqueCnt <- nrow(filter(groupByCntTbl, N==1))
      uniqueRate <- uniqueCnt/tableCnt
      resultTbl[1, ] <- c(paste(data, collapse = '\t'), uniqueCnt, uniqueRate)
      
      
      DT::datatable(
        resultTbl,
        options = list(
          lengthMenu = list(c(1), c('1')),
          pageLength = 1, 
          scrollX=TRUE)
      )
    })
    
    ## 가명 처리 수준 테이블 출력 - Item3
    output$print_process<- renderDataTable({
      data <- values$process_table
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), stringsAsFactors=FALSE)
      names(data) <- c("column명", "개인정보 속성", "데이터 타입", "데이터 유형", "특이치 유무", "처리수준")
      datatable(
        data = data,
        options = list(
          # lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        extensions = 'Buttons'
      ) %>% formatStyle(columns = "처리수준", backgroundColor = styleEqual(c("삭제", "차등 정보 처리", "라운딩", "일반화", "코드화"), c("silver", "Azure", "Azure", "Azure", "Azure")))
    })
    
    ## 결과 데이터 출력 - Item4
    output$print_output <- DT::renderDataTable({
      DT::datatable(
        values$output_table, filter ="top", 
        options = list(pageLength = 15, scrollX=TRUE),
        #options = list(pageLength = 15, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), scrollX=TRUE, dom ='Bfrtip'),
        extensions = c('Buttons'))
    })
    
    ####################
    
    
    #### UI 출력 ####
    
    output$input_summary <- renderText({
      summary <- tableData()
      # print(inFile[1])  
      paste("Column :", ncol(summary), br(), br(), "Row :", nrow(summary))
    })
    
    output$outlierNumColName <- renderUI({
      numColIdx <- sapply(values$original,is.numeric)
      selectInput("outlierNumColName", "Column 선택", c(names(values$original)))
    })
    
    output$selectinput_datacols <- renderUI({
      selectInput("selectinput_datacols", "Column 선택", c(names(values$original)))
    })
    
    output$checkbox_idx_qi <- renderUI({
      checkboxGroupInput("checkbox_idx_qi", "Column 선택", c(values$idx_table[values$idx_table[2]=='QI', 1]))
    })
    
    output$selectinput_datacols_item3 <- renderUI({
      idx <- input$selectinput_idx_3
      selectInput("selectinput_datacols_item3", "Column 선택", c(values$idx_table[values$idx_table[ ,2] == idx, 1]))
    })
    
    output$selectinput_process <- renderUI({
      name <- input$selectinput_datacols_item3
      op <- input$selectinput_idx_3
      type <- values$idx_table[values$idx_table[, 1] == name, 3]
      
      if(op == 'ID' | type == '문자')
        selectInput("selectinput_decide_process", "가명처리 수준", c("삭제", "보존"))
      else if(type == '숫자') 
        selectInput("selectinput_decide_process", "가명처리 수준", c("삭제", "차등 정보 처리", "라운딩", "보존"))
      else if(type == '바이너리') 
        selectInput("selectinput_decide_process", "가명처리 수준", c("삭제", "코드화", "보존"))
      else
        selectInput("selectinput_decide_process", "가명처리 수준", c("삭제", "보존"))
    })
    
    output$process_support <- renderUI({
      process <- input$selectinput_decide_process
      
      if(process == '라운딩')
        numericInput("rounding", "자리 수 : ", 1, min = -5, max = 50)
      else if(process == '일반화') 
        numericInput("generalize", "단위 : ", 0, min = 0, max = 100000000000)
      else if(process == '차등 정보 처리')
        sliderInput("epsilon", "Epsilon : ", min = 0.2, max = 2, value = 1.0, step = 0.1)
      else
        ""
      
    })
    
    #################
    
    
    #### 버튼 이벤트 Item1 ####
    
    ## 데이터 속성 테이블 생성 - Item1
    observeEvent(
      input$button_1to2,
      {
        values$original <- tableData()
        
        if(is.null(values$original)) {
          showModal(
            modalDialog(
              title = "파일 ERROR!",
              "데이터 파일이 비어있습니다.",
              br(),
              "데이터 파일을 불러와주세요.",
              easyClose = TRUE
            )
          )
        }
        else {
          df <- values$original
          values$idx_table <- NULL
          
          for(i in 1:ncol(df)) {
            tableName <- colnames(df)[i]
            
            data_freq <- as.data.frame(table(df[, i, with=FALSE]))
            data_freq <- data_freq[order(data_freq$Freq),]
            outliers = ""
            # print(data_freq)
            # 데이터 타입
            d_type <- class(df[[tableName]])[1]
            if (nrow(data_freq) == 2) { 
              data_type <- "바이너리" 
              data_cate <- "범주형"
            }
            else if(d_type == "IDate") { 
              data_type <- "날짜" 
              data_cate <- "범주형"
            }
            else if(d_type == "numeric" | d_type == "integer") { 
              data_type <- "숫자"
              data_cate <- "연속형" 
              
              line<- df[[i]]
              # 1분위수 계산
              Q1 <- quantile(line, probs = c(0.25), na.rm = TRUE) 
              # 3분위수 계산
              Q3 <- quantile(line, probs = c(0.75), na.rm = TRUE)
              LC <- Q1 - 1.5 * (Q3 - Q1) # 아래 울타리
              UC <- Q3 + 1.5 * (Q3 - Q1) # 위 울타리
              
              #3분위 값에 1.5IQR을 더한 상위 극단치보다 큰 값이 있는지 확인합니다. 
              tmpUOutlier <- which(line > UC)
              
              #1분위 값에 1.5IQR을 뺀 하위 극단치보다 작은 값이 있는지 확인합니다. 
              tmpLOutlier <- which(line < LC)
              
              # print(paste(tableName, length(tmpUOutlier), length(tmpLOutlier)))
              if (length(tmpUOutlier) > 0 || length(tmpLOutlier) > 0)
                outliers = "O" 
            }
            else { 
              data_type <- "문자"
              data_cate <- "범주형" 
              
              if (nrow(data_freq[data_freq$Freq < 4, ]) > 0)
                outliers = "O" 
            }
            
            tableDataDT <- data.table(df)
            groupByCntTbl <- tableDataDT[, .N, by=tableName]
            uniqueCnt <- nrow(filter(groupByCntTbl, N==1))
            
            new_row <- data.frame(tableName, "NA", data_type, data_cate, uniqueCnt, outliers, stringsAsFactors = F)
            values$idx_table <- rbind(values$idx_table, new_row)
          }
          updateTabItems(session, "tabs", "subitem2")
          values$process_level = 1
        }
      }
    )
    
    ###########################
    
    
    #### 버튼 이벤트 Item2 ####
    
    ## 유일성 분석 - Item2
    observeEvent(
      input$button_check_k, 
      {
        values$k_danger_table = NULL
        
        output_idx_tableName <- input$checkbox_idx_qi
        tableDataDT <- data.table(values$original)
        tableCnt <- nrow(tableDataDT)
        resultTbl <- data.frame(except_column=NA, column_name=NA, unique_count=NA, unique_risk=NA, stringsAsFactors = F)
        
        # 모든 데이터 위험성
        groupByCntTbl <- tableDataDT[, .N, by=output_idx_tableName]
        uniqueCnt <- nrow(filter(groupByCntTbl, N==1))
        uniqueRate <- uniqueCnt/tableCnt
        resultTbl[1, ] <- c(NA, paste(output_idx_tableName, collapse = '\t'), uniqueCnt, uniqueRate)
        
        # 하나 제거 위험성
        for(i in output_idx_tableName){
          idx <- output_idx_tableName %in% i
          groupByNames <- output_idx_tableName[!idx]
          groupByCntTbl <- tableDataDT[, .N, by=groupByNames]
          uniqueCnt <- nrow(filter(groupByCntTbl, N==1))
          uniqueRate <- uniqueCnt/tableCnt
          resultTbl[which(idx==T)+1, ] <- c(i, paste(groupByNames, collapse = '\t'), uniqueCnt, uniqueRate)
        }
        updateTabItems(session, "tabset2-2", "유일성 분석")
        values$k_danger_table <- resultTbl
      }
    )
    
    ## K값 초기화 - Item2
    observeEvent(
      input$button_delete_k,
      {
        values$k_values = NULL
      }
    )
    
    ## K값 지정 - Item2
    observeEvent(
      input$button_decide_k,
      {
        values$k_values = input$checkbox_idx_qi
        updateTabItems(session, "tabset2-2", "K값")
      }
    )
    
    ## 개인정보 속성 파일 적용 - Item2
    observeEvent(
      input$button_load_idx,
      {
        temp <- tempdata()
        
        if(is.null(temp)) {
          showModal(
            modalDialog(
              title = "파일 ERROR!",
              "데이터 파일이 비어있습니다.",
              br(),
              "데이터 파일을 불러와주세요.",
              easyClose = TRUE
            )
          )
        }
        else {
          values$idx_table[, 2] <- temp
        }
      }
    )
    
    ## 개인정보 속성 변경 - Item2
    observeEvent(
      input$button_idx,
      {
        data1 <- input$selectinput_datacols
        data2 <- input$radiobutton_select_idx
        values$idx_table[values$idx_table[, 1] == input$selectinput_datacols, 2] <- data2
      }
    )
    
    ## 개인정보 속성 초기화 - Item2
    observeEvent(
      input$button_reset_idx,
      {
        values$idx_table[, 2] <- "NA"
      }
    )
    
    ## 개인정보 속성 일괄적용 - Item2
    observeEvent(
      input$button_all_idx,
      {
        data <- input$radiobutton_select_idx
        values$idx_table[values$idx_table[, 2] == "NA", 2] <- data
      }
    )
    
    ## 위험도 분석 - Item2
    observeEvent(
      input$button_risk_analysis,
      {
        if(values$process_level < 1) {
          showModal(
            modalDialog(
              title = "건너뛰기 불가능",
              "대상선정 단계를 완료해주세요",
              easyClose = TRUE
            )
          )
          return()
        }
        else if(nrow(values$idx_table[values$idx_table[ ,2] == "NA", ]) != 0) {
          showModal(
            modalDialog(
              title = "개인정보 속성 에러",
              "개인정보 속성이 정해지지않은 데이터가 존재합니다.",
              br(),
              "개인정보 속성을 모두 지정해주세요.",
              easyClose = TRUE
            )
          )
          return()
        }
        
        values$risk_table = NULL
        df <- values$idx_table
        
        
        ## 환경 RISK 측정 (check@)
        risk_env1 = 0
        risk_env2 = 0
        risk_env3 = 0
        risk_env4 = 0
        
        risk_env1 <- as.numeric(input$selectinput_danger_env1)
        risk_env2 <- as.numeric(input$selectinput_danger_env2)
        risk_env3 <- as.numeric(input$selectinput_danger_env3)
        risk_env4 <- as.numeric(input$selectinput_danger_env4)
        
        values$risk_env = risk_env1 + risk_env2 + risk_env3 + risk_env4
        
        ##
        
        risk1_rowover = 0
        if(nrow(values$original > 1000))
          risk1_rowover = 1
        
        for(i in 1:nrow(df)) {
          tableName <- df[i, 1]
          
          risk2_qiunique = 0
          if(df[i, 2] == "QI" & df[i, 5] != 0)
            risk2_qiunique = 1
          
          risk3_haveoutlier = 0
          if(df[i, 6] == "O")
            risk3_haveoutlier = 1
          
          total = risk1_rowover + risk2_qiunique + risk3_haveoutlier
          values$risk_data[i] <- risk1_rowover + risk2_qiunique + risk3_haveoutlier
          
          if(risk1_rowover == 1)
            r1 = "O"
          else
            r1 = ""
          
          if(risk2_qiunique == 1)
            r2 = "O"
          else
            r2 = ""
          
          if(risk3_haveoutlier == 1)
            r3 = "O"
          else
            r3 = ""
          
          if(total == 3)
            total_out <- "상"
          else if(total == 2)
            total_out <- "중"
          else
            total_out <- "하"
          
          new_row <- data.frame(tableName, r1, r2, r3, total_out, stringsAsFactors = F)
          values$risk_table <- rbind(values$risk_table, new_row)
        }
        
        updateTabItems(session, "tabset2-2", "항목별 위험도")
        
      }
    )
    
    ## 데이터 타입 변경 - Item2
    observeEvent(
      input$button_data_type,
      {
        data1 <- input$selectinput_datacols
        data2 <- input$radiobutton_data_type
        values$idx_table[values$idx_table[, 1] == input$selectinput_datacols, 3] <- data2
      }
    )
    
    ## 데이터 유형 변경 - Item2
    observeEvent(
      input$button_data_cate,
      {
        data1 <- input$selectinput_datacols
        data2 <- input$radiobutton_data_cate
        values$idx_table[values$idx_table[, 1] == input$selectinput_datacols, 4] <- data2
      }
    )
    
    ## 가명 처리 수준 정의 테이블 생성 - Item2
    observeEvent(
      input$button_2to3,
      {
        if(values$process_level < 1) {
          showModal(
            modalDialog(
              title = "건너뛰기 불가능",
              "대상선정 단계를 완료해주세요",
              easyClose = TRUE
            )
          )
          return()
        }
        
        cnt_na = 0
        for (i in 1:nrow(values$idx_table)){
          if (values$idx_table[i, 2] == "NA")
          { cnt_na = cnt_na + 1 }
        }
        
        if (cnt_na != 0) {
          showModal(modalDialog(
            title = "식별자 ERROR!",
            paste0(cnt_na,"개의 식별자 속성이 입력되지 않았습니다!"),
            easyClose = TRUE))
        }
        else {
          
          values$process_table <- NULL
          data <- values$idx_table
          values$process_support_table_1 <- rep(0, nrow(data))
          values$process_support_table_2 <- rep(0, nrow(data))
          
          for(i in 1:nrow(data)) {
            names <- data[i, 1]
            idx <- data[i, 2]
            data_type <- data[i,3]
            data_cate <- data[i,4]
            outliers <- data[i, 6]
            
            new_row <- data.frame(names, idx, data_type, data_cate, outliers, "보존", stringsAsFactors = F)
            values$process_table <- rbind(values$process_table, new_row)
          }
          for (i in 1: nrow(values$process_table)){
            if(values$process_table[i,2] == 'ID')
            { values$process_table[i,6] <- '삭제' }
          }
          updateTabItems(session, "tabs", "subitem3")
          values$process_level = 2
        }
      }
    ) 
    
    ###########################
    
    
    #### 버튼 이벤트 Item3 ####
    
    ## 가명처리 수준 변경 이벤트 - Item3
    observeEvent(
      input$button_decide_process,
      {
        name <- input$selectinput_datacols_item3
        process <- input$selectinput_decide_process
        
        values$process_table[values$process_table[, 1] == name, 6] <- process
        index <- which(values$process_table[, 1] == name)
        
        if(process == '라운딩')
          values$process_support_table_1[index] <- input$rounding
        else if(process == '일반화')
          values$process_support_table_1[index] <- input$generalize
        else if(process == '코드화') 
          values$process_support_table_1[index] <- c(input$epsilon, 0)
        else if(process == '차등 정보 처리')
          values$process_support_table_1[index] <- input$epsilon
      }
    )
    
    ## *dp*
    dp_mean_std <- function(xs, epsilon) {
      target <- function(X) mean(X)
      
      n <- length(xs)
      mech <- DPMechLaplace(target = target, sensitivity = 1/n, dims = 1)
      
      r <- releaseResponse(mech, privacyParams = DPParamsEps(epsilon = epsilon), X = xs)
      r$response
    }
    
    ## 가명 처리 테이블 - Item3
    observeEvent(
      input$button_3to4,
      {
        if(values$process_level < 2) {
          showModal(
            modalDialog(
              title = "건너뛰기 불가능",
              "위험도 분석 단계를 완료해주세요",
              easyClose = TRUE
            )
          )
          return()
        }
        else
          updateTabItems(session, "tabs", "subitem4")
        
        values$output_table <- values$original
        values$outlier_table <- NULL
        
        data <- values$output_table
        pr_table <- values$process_table
        
        process_del <- c(which(pr_table[ ,6] == "삭제"))
        process_dpi <- c(which(pr_table[ ,6] == "차등 정보 처리"))
        process_rnd <- c(which(pr_table[ ,6] == "라운딩"))
        process_cod <- c(which(pr_table[ ,6] == "코드화"))
        process_gen <- c(which(pr_table[ ,6] == "일반화"))
        
        # K 처리
        k_TOBE <- input$k_level
        
        # 지정된 K값
        qiList <- values$k_values
        
        if (k_TOBE > 0 & length(qiList) > 1) {
          
          #준식별자 동질그룹의 최소값 구하기 -> K익명성 값
          dt <- as.data.table(data)
          dt_u <- dt %>% select(qiList) %>% unique()
          dt_u$id <- 1:nrow(dt_u)
          setkeyv(dt, qiList)
          setkeyv(dt_u, qiList)
          
          tmp1 <- dt[dt_u]
          tmp2 <- tmp1 %>% group_by(id) %>% summarise(k=n())
          tmp3 <- as.data.table(tmp2)
          tmp4 <- tmp3[k == min(k), list(id)]
          
          #현재 K값
          k_ASIS <- min(tmp3$k)
          
          #K값을 만족하면 처리하지 않음.
          if(k_ASIS <= k_TOBE){
            tmp5 <- tmp1 %>% filter(!id %in% tmp4$id) %>% select(-c(id))
            values$output_table <- tmp5
          }
        }
        
        # DP
        if (length(process_dpi) > 0) {
          for (i in 1:length(process_dpi)) {
            index <- process_dpi[i]
            line <- as.vector(values$output_table[[index]])
            epsilon = values$process_support_table_1[index]
            
            for(j in 1:length(line))
              line[j] <- as.integer(dp_mean_std(as.numeric(line[j]), epsilon))
            
            values$output_table[[index]] <- line
          }
        }
        
        # 라운딩
        if (length(process_rnd) > 0) {
          for (i in 1:length(process_rnd)) {
            index <- process_rnd[i]
            line <- as.vector(values$output_table[[index]])
            rounding = values$process_support_table_1[index]
            
            for(j in 1:length(line)) {
              num <- line[j]
              if (is.null(num)) {
                line[j] <- 0
                next
              }
              len <- nchar(num)
              if (len > rounding)
                line[j] <- round(num, -rounding)
              else
                line[j] <- 0
            }
            values$output_table[[index]] <- line
          }
        }
        
        # 일반화
        if (length(process_gen) > 0) {
          for (i in 1:length(process_gen)) {
            index <- process_gen[i]
            line <- as.vector(values$output_table[[index]])
            generalize = values$process_support_table_1[index]
            
            for(j in 1:length(line)) {
              num <- line[j]
              if (is.null(num)) {
                line[j] <- 0
                next
              }
              len <- nchar(num)
              if (len > rounding)
                line[j] <- round(num, -(len - rounding))
              else
                line[j] <- 0
            }
            values$output_table[[index]] <- line
          }
        }
        
        # 코드화
        if (length(process_cod) > 0) {
          for (i in 1:length(process_cod)) {
            index <- process_cod[i]
            line <- as.vector(values$output_table[[index]])
            
            for(j in 1:length(line)) {
              if(line[j] == line[1])
                line[j] <- sample(c(1,2,3,4,5), 1, replace = TRUE)
              else
                line[j] <- sample(c(6,7,8,9,10), 1, replace = TRUE)
            }
            values$output_table[[index]] <- line
          }
        }
        
        # 이상치 처리
        idx_table <- values$idx_table
        
        cha_out = NULL
        for(i in 1:ncol(values$output_table)) {
          if(idx_table[i, 6] == "O" & pr_table[i, 6] != "삭제") {
            # 연속형 특이치 처리
            if(idx_table[i, 4] == "연속형") {
              s_len = 0
              # Outlier인 데이터 범위 정의
              # 1분위수 계산
              Q1 <- quantile(values$output_table[[i]], probs = c(0.25), na.rm = TRUE) 
              # 3분위수 계산
              Q3 <- quantile(values$output_table[[i]], probs = c(0.75), na.rm = TRUE)
              
              LC <- Q1 - 1.5 * (Q3 - Q1) # 아래 울타리
              UC <- Q3 + 1.5 * (Q3 - Q1) # 위 울타리
              
              s_len = length(which(values$output_table[[i]] > UC)) + length(which(values$output_table[[i]] < LC))
              
              #조건에 해당하는 행들을 상하한 값으로 대체처리
              values$output_table[which(values$output_table[[i]] > UC), i] <- UC
              values$output_table[which(values$output_table[[i]] < LC), i] <- LC
              
              values$outlier_table[i] <- c(paste(s_len, " / 0"))
            }
            # 범주형 특이치 처리
            else {
              data_freq <- as.data.frame(table(values$output_table[, i, with=FALSE]))
              data_freq <- data_freq[order(data_freq$Freq),]
              
              for(j in 1:nrow(data_freq)) 
                if(data_freq[j, 2] <= 3) {
                  values$outlier_table[i] <- c(paste(length(which(values$output_table[[i]] == data_freq[j, 1])), " / 0"))
                  cha_out <- c(cha_out, which(values$output_table[[i]] == data_freq[j, 1]))
                }
            }
          }
          else
            values$outlier_table[i] <- c("")
        }
        if(length(cha_out) > 0) {
          cha_out <- sort(unique(cha_out))
          values$output_table <- values$output_table[-c(cha_out)]
        }
        
        # 삭제
        values$output_table <- subset(values$output_table, select = -c(process_del))
        
        values$output_idx_table <- NULL
        df <- values$output_table
        o_df <- values$idx_table
        r_df <-  values$risk_table
        for(i in 1:ncol(df)) {
          tableName <- colnames(df)[i]
          tableDataDT <- data.table(values$output_table)
          tableCnt <- nrow(tableDataDT)
          
          # 데이터 범주 저장
          tmpDf <- as.data.frame(table(df[, i, with=FALSE]))
          tmpDf <- tmpDf[order(tmpDf$Freq),]
          names(tmpDf)[1] <- c(tableName)
          
          groupByCntTbl <- tableDataDT[, .N, by=tableName]
          uniqueCnt <- nrow(filter(groupByCntTbl, N==1))
          
          new_row <- data.frame(tableName, o_df[o_df$tableName == tableName, 2], o_df[o_df$tableName == tableName, 3], o_df[o_df$tableName == tableName, 4], uniqueCnt, "", r_df[r_df$tableName == tableName ,5], stringsAsFactors = F)
          values$output_idx_table <- rbind(values$output_idx_table, new_row)
          
        }
        values$process_level = 3
      }
    )
    
    ###########################
    
    
    #### 가명처리 보고서 Item4 #####
    
    ## 다운로드
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("가명처리결과", ".csv")
      },
      content = function(file) {
        write.csv(values$output_table, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    ## 사용자 정보
    output$report_user_info<- DT::renderDataTable({
      data <- data.frame("입력해주세요.","입력해주세요")
      names(data) <- c("소속","이름(직책)")
      DT::datatable(
        data,
        class = "cell-border",
        caption =htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',"1. 가명정보 처리자"),
        rownames = F,
        editable =T,
        options = list(dom = 't')
      )
    })
    
    ## 원본 위험도 분석
    output$report_danger_input<- renderDataTable({
      data <- cbind(values$idx_table, values$risk_table[, 5])
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), i7=character(), stringsAsFactors=FALSE)
      names(data) <- c("column명", "개인정보 속성", "데이터 타입", "데이터 유형", "유일한 값 개수", "특이치 유무", "위험도 수준")
      datatable(
        data,
        class = "cell-border",
        caption =htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:150% ;',"원본 데이터 항목별 위험도 분석"),
        options = list(
          lengthMenu = list(c(10, 30, -1), c('10', '30', 'All')),
          pageLength = 100,
          dom = 't',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        extensions = 'Buttons'
      )
    })
    
    ## 가명처리 데이터 위험도 분석
    output$report_danger_output<- renderDataTable({
      # data <- values$output_idx_table
      data <- values$output_idx_table
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), i7=character(), stringsAsFactors=FALSE)
      names(data) <- c("column명", "개인정보 속성", "데이터 타입", "데이터 유형", "유일한 값 개수", "특이치 유무", "위험도 수준")
      datatable(
        data,
        class = "cell-border",
        caption =htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:150% ;',"가명처리 데이터 항목별 위험도 분석"),
        options = list(
          lengthMenu = list(c(10, 30, -1), c('10', '30', 'All')),
          pageLength = 100,
          dom = 't',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        extensions = 'Buttons'
      )
    })
    
    ## 원본데이터 요약
    output$report_summary_input<- DT::renderDataTable({
      data <- values$idx_table
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), stringsAsFactors=FALSE)
      else {
        column_num <- nrow(data)
        id_column <- nrow(data[data[,2]=="ID",])
        outlier_num <- nrow(data[data[,6]=="O",])
        record <- nrow(values$original)
        qi_column <- nrow(data[data[,2]=="QI",])
        encoding <- "UTF8"
        data <- data.frame(column_num, id_column, outlier_num, record, qi_column, encoding)
      }
      names(data) <- c("칼럼 수","식별자 칼럼 수","특이정보 존재 칼럼 수","레코드 수", "준식별자 칼럼 수", "문자 인코딩 정보")
      DT::datatable(
        data,
        class = "cell-border",
        caption =htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',"2-1. 원본데이터 명세 요약"),
        rownames = F,
        options = list(dom = 't')
      )
    })
    
    ## 가명처리 데이터 요약
    output$report_summary_output<- DT::renderDataTable({
      data <- values$output_idx_table
      if(is.null(data))
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), stringsAsFactors=FALSE)
      else {
        column_num <- nrow(data)
        id_column <- nrow(data[data[,2]=="ID",])
        outlier_num <- nrow(data[data[,6]=="O",])
        record <- nrow(values$output_table)
        # qi_column <- nrow(data[data[,2]=="QI",])
        encoding <- "UTF8"
        
        if(input$k_level > 0)
          qi_column <- paste0(nrow(data[data[,2]=="QI",]), "(", length(values$k_values), ")")
        else
          qi_column <- paste0(nrow(data[data[,2]=="QI",]), "(0)")
        
        data <- data.frame(column_num, id_column, outlier_num, record, qi_column, encoding)
      }
      names(data) <- c("칼럼 수","식별자 칼럼 수","특이정보 존재 칼럼 수","레코드 수", "준식별자 칼럼 수 (K값)", "문자 인코딩 정보")
      DT::datatable(
        data,
        class = "cell-border",
        caption =htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',"2-2. 가명처리데이터 명세 요약"),
        rownames = F,
        options = list(dom = 't')
      )
    })
    
    ## 전체 컬럼 요약
    output$print_total_summary<- DT::renderDataTable({
      data <- values$process_table
      if(values$process_level < 3)
        data <- data.frame(i1=character(), i2=character(), i3=character(), i4=character(), i5=character(), i6=character(), stringsAsFactors=FALSE)
      else {
        data <- data[,-c(5)]
        data <- cbind(data, values$outlier_table)
        rownames(data) <- NULL
      }
      
      names(data) <- c("column명", "개인정보 속성", "데이터 타입", "데이터 유형", "가명처리","특이정보 수 전/후")
      DT::datatable(
        data,
        caption =htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',"3. 가명처리 결과"),
        class = "cell-border",
        options = list(
          lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
          pageLength = 100,
          dom = 't',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
        extensions = 'Buttons'
      )
      
    })
    
    ##############################
    
    
    #### 그래프 ####
    
    # 최종보고서
    output$all_chart <- renderPlot({
      
      df_pro <- values$process_table
      df_ori <- values$original
      df_out <- values$output_table
      
      pp <- df_pro[df_pro[,3] == "숫자", 1]
      
      size <- length(df_pro[df_pro[,3] == "숫자", 1])
      par(mfrow = c(size, 2))
      for (i in 1:nrow(df_pro)) {
        if(df_pro[i, 6] != "삭제") {
          colname <- df_pro[i, 1]
          d_type <- df_pro[i, 3]
          d_cate <- df_pro[i, 4]
          
          if(d_type == "숫자" & d_cate == "연속형") {
            p_ori <- boxplot(df_ori[[colname]], main = paste0(colname,"(처리 전)"), cex.main = 3)
            p_out <- boxplot(df_out[[colname]], main = paste0(colname,"(처리 후)"), cex.main = 3)
          }
          else if(d_type == "숫자" & d_cate == "범주형") {
            p_ori <- hist(df_ori[[colname]], main = paste0(colname,"(처리 전)"), cex.main = 3)
            p_out <- hist(df_out[[colname]], main = paste0(colname,"(처리 후)"), cex.main = 3)
          }
        }
      }
    }, width = 1200, height = 2500)
    
    
    hcharts <- function(df, data_cate, name) {
      if(data_cate == "연속형") {
        boxdata <- df[[name]]
        hc <- hcboxplot(
          x = boxdata,
          var = c(name),
          outliers = F,
          name = "Value",
          color = "#2980b9"
        ) %>% hc_chart(type = "column")
      }
      else {
        hc <- hchart(
          as.character(df[[name]]), 
          color = "#2980b9",
          name = "Value",
          var = c(name),
          showInLegend = F
        )
      }
      return(hc)
    }
    
    # 그래프 출력 - Item 2
    output$show_chart <- renderHighchart({
      df <- values$original
      data_cate <- values$idx_table[values$idx_table[,1] == input$selectinput_datacols, 4]
      hc <- hcharts(df, data_cate, input$selectinput_datacols)
      hc
    })
    
    #################
    
    
    #### 이미지 처리 #####
    
    ## 이미지 파일 불러오기
    ## 이미지 파일 불러오기
    rawimage <- reactive({
      
      req(input$choose_files)
      #input the image
      inp <- input$choose_files
      
      # read and strore the image
      img <- readJPEG(inp$datapath)
      
    })
    
    rawimage2 <- reactive({
      
      req(input$choose_files)
      #input the image
      inp <- input$choose_files
      
      # read and strore the image
      img <- load.image(inp$datapath)
      
    })
    
    img_data <- reactive({
      
      req(input$choose_files)
      #input the image
      inp <- input$choose_files
      
      face_api_url <- "https://koreacentral.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceLandmarks=TRUE"
      
      api_key <- "400671baf6c84c20a19060a426fd71c2"
      # Create an empty dataframe
      
      # Get the photo with photo list
      body_image <- upload_file(inp$datapath)
      class(body_image)
      # Use POST methord to get result from API
      result <- POST(face_api_url,
                     body = body_image,
                     add_headers(.headers = c("Content-Type" = "application/octet-stream",
                                              "Ocp-Apim-Subscription-Key" = api_key)))
      
      
      # Store result of the image to temp in data frame
      temp <- as.data.frame(content(result))
      
      a <- data.frame()
      b <- data.frame()
      
      
      for (i in 1:ncol(temp)){
        num1 = i %/% 59
        num2 = i %% 59
        
        if (num1 == 0)
          a[1,i] <- temp[1,i]
        else if (i == 59){
          a[1,i] <- temp[1,i]
          names(a) <- names(temp[1,1:59])
        }
        else if (num1 != 0)
          if (num2 == 1){
            b <- NULL
            b <- temp[1,i:(i+58)]
            names(b) <- names(a)
            a <- rbind(a,b)
          }
      }
      
      a
    })
    
    ## 이미지 DP처리
    # dp_process <- eventReactive(
    observeEvent (
      input$dp_button, {
        
        # read and strore the image
        img <- rawimage()
        
        #이미지 RGB 값 정의
        mm <- melt(img, varnames=c("X","Y","Color"))
        mm$Color <- factor(mm$Color, levels=1:3, labels=c("Red","Blue","Green"))
        xx <- dcast(mm, X+Y~Color)
        xx <- xx %>% mutate(rgb.val=rgb(Red,Green,Blue))
        
        # 이미지 변경 : RGB의 값을 조정하는 테스트도 진행해주세요.
        e <- input$dp_button
        
        # print(e)
        for (row in 1:nrow(xx)){
          print(row)
          a <- xx[row,3]
          b <- xx[row,4]
          c <- xx[row,5]
          am <- dp_mean_std(a, e) #앱실론만 변경
          if(am < 0){
            am <- 0.0000001
          }
          if(am > 1){
            am <- 0.9999999
          }
          
          bm <- dp_mean_std(b, e)  #앱실론만 변경
          if(bm < 0){
            bm <- 0.0000001
          }
          if(bm > 1){
            bm <- 0.9999999
          }
          
          cm <- dp_mean_std(c, e)  #앱실론만 변경
          if(cm < 0){
            cm <- 0.0000001
          }
          if(cm > 1){
            cm <- 0.9999999
          }
          #RGB 값 변경
          xx[row,6] <- rgb(am,bm,cm)
        }
        
        #이미지를 저장
        #p <- ggplot(xx,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
        p <- ggplot(xx,aes(Y,X))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
        p <- p+scale_y_reverse()
        #png(filename=("car_mod_7.png"), width=600, height=600, unit="px", bg="transparent")
        # jpeg(filename=(paste0("car_720_mod_",e,".jpg")), width=720, height=540, unit="px", bg="transparent")
        # dev.off() 
        values$img_output <- p
        
      })
    
    observeEvent (
      input$dp_button, {
        
        # read and strore the image
        img <- rawimage()
        
        #이미지 RGB 값 정의
        mm <- melt(img, varnames=c("X","Y","Color"))
        mm$Color <- factor(mm$Color, levels=1:3, labels=c("Red","Blue","Green"))
        xx <- dcast(mm, X+Y~Color)
        xx <- xx %>% mutate(rgb.val=rgb(Red,Green,Blue))
        
        # 이미지 변경 : RGB의 값을 조정하는 테스트도 진행해주세요.
        e <- input$dp_button
        
        # print(e)
        for (row in 1:nrow(xx)){
          print(row)
          a <- xx[row,3]
          b <- xx[row,4]
          c <- xx[row,5]
          am <- dp_mean_std(a, e) #앱실론만 변경
          if(am < 0){
            am <- 0.0000001
          }
          if(am > 1){
            am <- 0.9999999
          }
          
          bm <- dp_mean_std(b, e)  #앱실론만 변경
          if(bm < 0){
            bm <- 0.0000001
          }
          if(bm > 1){
            bm <- 0.9999999
          }
          
          cm <- dp_mean_std(c, e)  #앱실론만 변경
          if(cm < 0){
            cm <- 0.0000001
          }
          if(cm > 1){
            cm <- 0.9999999
          }
          #RGB 값 변경
          xx[row,6] <- rgb(am,bm,cm)
        }
        
        #이미지를 저장
        #p <- ggplot(xx,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
        p <- ggplot(xx,aes(Y,X))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
        p <- p+scale_y_reverse()
        #png(filename=("car_mod_7.png"), width=600, height=600, unit="px", bg="transparent")
        # jpeg(filename=(paste0("car_720_mod_",e,".jpg")), width=720, height=540, unit="px", bg="transparent")
        # dev.off() 
        values$img_output <- p
        
      })
    
    observeEvent (
      input$dp_button2, {
        
        # read and strore the image
        img <- rawimage()
        test <- img_data()
        
        #이미지 RGB 값 정의
        mm <- melt(img, varnames=c("X","Y","Color"))
        mm2 <- melt(a, varnames=c("X","Y","Color"))
        str(mm)
        mm$Color <- factor(mm$Color, levels=1:3, labels=c("Red","Blue","Green"))
        mm$Color2 <- factor(mm$Color, levels=1:3, labels=c("Red","Blue","Green"))
        xx <- dcast(mm, X+Y~Color)
        xx <- xx %>% mutate(rgb.val=rgb(Red,Green,Blue))
        xxtmp <- as.data.frame(t(col2rgb(xx$rgb.val)))
        xx$Red2 <- xxtmp$red
        xx$Green2 <- xxtmp$green
        xx$Blue2 <- xxtmp$blue
        
        other <- 0.1
        face <- 3.0
        # eye1 <- 3.0
        # eye2 <- 3.0
        # nose <- 3.0
        # mouth <- 3.0
        
        for( i in 1:nrow(test)){
          print((test$faceRectangle.top[i]+test$faceRectangle.width[i]))
        }
        
        for (row in 1:nrow(xx)){
          #row <- 250000
          a <- xx[row,3]
          b <- xx[row,4]
          c <- xx[row,5]
          h <- xx[row,1]
          w <- xx[row,2]
          
          e<- 0
          
          for( i in 1:nrow(test)){
            if(w>=test$faceRectangle.left[i] && w<=(test$faceRectangle.left[i]+test$faceRectangle.width[i])){
              if(h>=test$faceRectangle.top[i] && h<=(test$faceRectangle.top[i]+test$faceRectangle.height[i])){
                e <- face
              }
            }
            
          }
          
          if (e ==0){
            e<-other
          }
          
          print(e)
          print((row/nrow(xx))*100)
          
          am <- dp_mean_std(a, e)
          bm <- dp_mean_std(b, e)
          cm <- dp_mean_std(c, e)
          
          if(am < 0){
            am <- 0
          }
          if(am > 1){
            am <- 1
          }
          
          if(bm < 0){
            bm <- 0
          }
          if(bm > 1){
            bm <- 1
          }
          
          if(cm < 0){
            cm <- 0
          }
          if(cm > 1){
            cm <- 1
          }
          
          #RGB 값 변경
          #xx[row,6] <- rgb(am,bm,cm, maxColorValue=255)
          xx[row,6] <- rgb(am,bm,cm)
        }
        #head(xx,10)
        
        #p <- ggplot(xx,aes(x,y))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
        p <- ggplot(xx,aes(Y,X))+geom_raster(aes(fill=rgb.val))+scale_fill_identity()
        p <- p+scale_y_reverse()
        values$img_output2 <- p
        
      })
    
    
    ## 이미지 파일 출력
    output$plot_image <- renderPlot({
      
      # read and strore the image
      my_photo <- rawimage2()
      
      plot(my_photo, main = "Raw Data")
      
    })
    
    ##그레이처리
    output$plot_image2 <- renderPlot({
      my_photo <- rawimage2()
      plot(grayscale(my_photo), main = "My photo in a grayscale")
      
    })
    
    output$dp_image <- renderPlot({
      values$img_output
    })
    
    output$dp_image2 <- renderPlot({
      values$img_output2
    })
    
    
    output$imgtest <- DT::renderDataTable({
      data <- img_data()
      DT::datatable(
        data, filter ="top", 
        options = list(pageLength = 15, scrollX=TRUE, dom ='Bfrtip'),
        # options = list(pageLength = 15, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), scrollX=TRUE, dom ='Bfrtip'),
        extensions = c('Buttons'))
    })
    
    
    
    ## 통합
    output$imgtest2<- renderText({
      data <- img_data()
      paste(nrow(data), "명" )
      #사람 수 몇 이상 ?
    })
    
    output$imgtest3<- renderText({
      data <- rawimage()
      paste("X :", dim(data)[1], "Y :" , dim(data)[2])
      #해상도 몇 이상 ?
    })
    
    output$plot3 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./',
                                          paste('img1', input$n, '.png', sep='')))
      
      # Return a list containing the filename
      list(src = filename)
    })
  
    output$plot4 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./',
                                          paste('img3', input$n, '.png', sep='')))
      
      # Return a list containing the filename
      list(src = filename)
    })
    
    output$plot5 <- renderImage({
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./',
                                          paste('img4', input$n, '.png', sep='')))
      
      # Return a list containing the filename
      list(src = filename)
    })
    ##########################
    
  }
  
  shinyApp(ui, server)