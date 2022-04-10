rm(list=ls(all=TRUE))               # 清除所有變數
#An Interactive Web Application of Data Science with R using Shiny
#====================================== 安裝套件 ===========================================
{packages_name=c("MASS","rstudioapi","lubridate","dplyr","cpm","ecp","data.table","RColorBrewer",
                 "shiny","shinydashboard","shinyWidgets","DT","shinyjs","V8","ggplot2",
                 "plotly","maps","rworldmap")
if (length(setdiff(packages_name, rownames(installed.packages()))) > 0) {
  # install packages if needed
  install.packages(setdiff(packages_name , rownames(installed.packages())))  }
sapply(packages_name, require, character.only = TRUE)}                    # Loading packages
#===========================================================================================

#====================================== 設定路徑 ===========================================
dir_path =dirname(rstudioapi::getSourceEditorContext()$path)        #直接讀取R檔案存放的路徑
setwd(dir_path) ; getwd()
#===========================================================================================

#===========================================================================================
name=c('澳洲','日本','南韓','中國','香港','新加坡','馬來西亞','印度','沙烏地阿拉伯','土耳其',
       '南非','法國','英國','俄羅斯','德國','巴西','美國','加拿大','墨西哥','阿根廷')
eng_name=c('Australia','Japan','South Korea','China','Hong Kong','Singapore','Malaysia','India','Saudi Arabia','Turkey',
           'South Africa','France','UK','Russia','Germany','Brazil','USA','Canada','Mexico','Argentina')
Year = c(2019:2009)
fee  = c(0.1, 0.25, 0.5, 1)
rang = c(40, 20, 10)
fit    = read.csv(paste0(dir_path,'/欣翰/MSE預測/澳洲2019預測結果.csv'))
forecast = read.csv(paste0(dir_path,'/欣翰/預測結果/澳洲預測值.csv'))

logrt=fread(paste0(dir_path,'/欣翰/market_2020/原始檔/澳洲.csv'),select=c('Date','logrt_100'))
logrt=as.matrix(logrt)
qua=quantile(as.numeric(as.character(logrt[1:(nrow(logrt)),2])),c(0.4,0.6))
#===========================================================================================

#===========================================================================================
header <- dashboardHeader(title = "HSIN HAN", titleWidth = 260 )  # titleWidth=標題欄寬度
#===========================================================================================

#===========================================================================================
sidebar <- dashboardSidebar(width = 260, 
                            collapsed = F,  # collapsed 是否縮小
                            sidebarMenu(    # 分頁的名稱跟內容設定
                              tags$hr(),    # tags$hr() 可以在分頁中間加入一條橫線
                              menuItem("Website", icon = icon("send",lib='glyphicon'), href = "http://www.stat.nuk.edu.tw/"),
                              tags$hr(),
                              menuItem("Contry",                tabName = "C", icon = icon("line")),
                              tags$hr(),
                              menuItem("result",                tabName = "R2", icon = icon("instagram")),
                              tags$hr(),
                              menuItem("result",                tabName = "R1", icon = icon("telegram-plane")),
                              tags$hr(),
                              menuItem("result",                tabName = "R3", icon = icon("comment-dollar")),
                              tags$hr(),
                              menuItem("result",                tabName = "R4", icon = icon("google")),
                              tags$hr(),
                              menuItem("map",    tabName = "map", icon = icon("map")),
                              tags$hr()
                            )
)
#===========================================================================================

#===========================================================================================
body <- dashboardBody(
  
  # tags$head裡面的指令是為了改變標題字型
  tags$head(tags$style(HTML('.main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;}'))),
  #如果有多個分頁，必須加tabItems()這個function
  
  tabItems( 
    tabItem(tabName = "map",
            h3("以下為「一期網絡相關性強度」，相關性以皮爾森相關進行檢定"),
            h3("以 (1 - p-value) 代表相關性強度，數字越大相關性越強"),
            fluidRow(
              box(width = 12,solidHeader = TRUE, status = "info",
                  title = "adjacency", dataTableOutput(outputId = "adjcacency"))
              ),
              
            h3("下方呈現的是對於您預測之國家「有影響力的金融市場」。"),
            fluidRow(
              plotOutput("colormap", height="660px", width="1260px")
            )
    ),
    
    tabItem(tabName = "C",
            h2(strong("ARMA-GARCH-NAR模型對於金融數據之預測")),
            h3("先指定欲預測的「國家」，再指定欲預測的「日期」"),
            h3("下方將呈現250天的配適效果、預測之狀態以及該國家各年份的預測效果"),
            fluidRow(
              # 國家選單
              box(
                width=4, height = 200, background = 'purple', 
                selectInput("contry", "choose a contry :", list(`contry` = name)), textOutput("contryname"),
                tags$head(tags$style("#contryname{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"))),
              # 日期選單
              box(
                width=4, height = 200, background = 'purple', 
                dateInput("date", h3("Date input"), value = "2019-12-31"), textOutput("datechoose"),
                tags$head(tags$style("#datechoose{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }")))
              ),
            
            h3("以下分別為4種方法預測之「對數收益值」"),
            fluidRow(
              valueBoxOutput("vbox_RL" , width = 2),
              valueBoxOutput("vbox_NAR", width = 2),
              valueBoxOutput("vbox_VAR", width = 2),
              valueBoxOutput("vbox_AM" , width = 2),
              valueBoxOutput("vbox_AR" , width = 2))
            
            # ,
            # 
            # h3("以下為所選日期前250天之「配適圖」"),
            # fluidRow(
            #   box(title = '所選日期前250天之配適圖',
            #       width =8, height = 540,
            #       solidHeader = T,
            #       collapsible = T,
            #       collapsed = T,
            #       status = 'warning',imageOutput("picture1")))

    ),
    
    tabItem(tabName = "R1",
            h3("以下為所選國家所有年份預測情況之「Pearson correlation」"),
            fluidRow(
              box(title = 'Pearson correlation',
                  width =8, height = 540,
                  solidHeader = T,
                  collapsed = F,
                  status = 'warning',imageOutput("picture2"))),
            
            h3("以下為所選國家所有年份預測情況之「Kendall correlation」"),
            fluidRow(
              box(title = 'Kendall correlation',
                  width =8, height = 540,
                  solidHeader = T,
                  collapsed = F,
                  status = 'primary',imageOutput("picture3"))),
            
            h3("以下為所選國家所有年份預測情況之「Spearman correlation」"),
            fluidRow(
              box(title = 'Spearman correlation',
                  width =8, height = 540,
                  solidHeader = T,
                  collapsed = F,
                  status = 'success',imageOutput("picture4")))
    ),
    
    tabItem(tabName = "R2",
            h3("以下為陽性陰性預測值計算方法"),
            h3("大漲為超過歷史資料60百分位數"),
            h3("大跌為低於歷史資料40百分位數"),
            fluidRow(
              imageOutput("picture5")),
            
            fluidRow(
              # 國家選單
              box(
                width=4, height = 200, background = 'purple', 
                selectInput("contry2", "choose a contry :", list(`contry` = name)), textOutput("contryname2"),
                tags$head(tags$style("#contryname2{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"))),
              # 年份選單
              box(
                width=4, height = 200, background = 'purple', 
                selectInput("year", "choose a year :", list(`year` = Year)), textOutput("yearchoose"),
                tags$head(tags$style("#yearchoose{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }")))
              ),
            
            h3("上排為4種方法之「陽性預測值」，下排為「陰性預測值」(大漲大跌)"),
            fluidRow(
              valueBoxOutput("ppv_NAR", width = 3),
              valueBoxOutput("ppv_VAR", width = 3),
              valueBoxOutput("ppv_AM" , width = 3),
              valueBoxOutput("ppv_AR" , width = 3)),
            fluidRow(
              valueBoxOutput("npv_NAR", width = 3),
              valueBoxOutput("npv_VAR", width = 3),
              valueBoxOutput("npv_AM" , width = 3),
              valueBoxOutput("npv_AR" , width = 3)),
            
            h3("上排為4種方法之「陽性預測值」，下排為「陰性預測值」(漲跌)"),
            fluidRow(
              valueBoxOutput("ppv_NAR2", width = 3),
              valueBoxOutput("ppv_VAR2", width = 3),
              valueBoxOutput("ppv_AM2" , width = 3),
              valueBoxOutput("ppv_AR2" , width = 3)),
            fluidRow(
              valueBoxOutput("npv_NAR2", width = 3),
              valueBoxOutput("npv_VAR2", width = 3),
              valueBoxOutput("npv_AM2" , width = 3),
              valueBoxOutput("npv_AR2" , width = 3)),
            
            fluidRow( # 散佈圖
              box(width = 2,background = "yellow", 
                  selectInput("type1", "choose a method :", list(`method` = colnames(forecast)[3:6])),
                  textOutput("typename1")
              ),
              box(title = 'scatter plot',
                  width=8,
                  height = 500,
                  solidHeader = T,
                  collapsed = F,
                  status = 'info',
                  plotOutput('scatter')
              )
            )
            
            # ,
            # 
            # h3("以下分別為4種方法之「配適指標」"),
            # fluidRow(
            #   box(width = 10,solidHeader = TRUE, status = "info",
            #       title = "Model fit", dataTableOutput(outputId = "modelfit"))
            # )
            
    ),
    
    tabItem(tabName = "R3",
            h3("以下為所選國家各方法「年度總報酬」之對比"),
            fluidRow(
              # 國家選單
              box(
                width=4, height = 150, background = 'purple', 
                selectInput("contry3", "選擇國家 :", list(`contry` = name)), textOutput("contryname3"),
                tags$head(tags$style("#contryname3{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"))),
              box(
                width=4, height = 150, background = 'purple', 
                selectInput("range", "選擇漲跌百分比界線 :", list(`percentile` = rang)), textOutput("percentile"),
                tags$head(tags$style("#percentile{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"))),
              box(
                width=4, height = 150, background = 'purple', 
                selectInput("feee", "選擇手續費 :", list(`fee` = fee)), textOutput("feecost"),
                tags$head(tags$style("#feecost{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }")))
            ),
            fluidRow(
              imageOutput("picture6"))
            
            
            
    ),
    
    tabItem(tabName = "R4",
            fluidRow(
              # 國家選單
              box(
                width=4, height = 150, background = 'purple', 
                selectInput("contry4", "選擇國家 :", list(`contry` = name)), textOutput("contryname4"),
                tags$head(tags$style("#contryname4{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"))),
              box(
                width=4, height = 150, background = 'purple', 
                selectInput("range2", "選擇漲跌百分比界線 :", list(`percentile` = rang)), textOutput("percentile2"),
                tags$head(tags$style("#percentile2{color: pink;
                                 font-size: 30px;
                                 font-style: italic;
                                 }")))
              
            ),
            fluidRow(
              imageOutput("picture7")) 
            
    )
  )
)

#===========================================================================================

#===========================================================================================
# ui 中的 header、sidebar、body設置在上方
ui <- dashboardPage(skin = "black", header, sidebar, body)
#===========================================================================================


#===========================================================================================
server <- function(input, output, session){
  
  # 第2頁
  #----------------------------------------------------------------------------------
  # 顯示是選擇哪個國家
  output$contryname <- renderText({
    # input$state的state是在ui部分建立的資料
    paste0('您選擇了第  ', which(input$contry == name),'  個國家 --- ', input$contry)})
  
  # 顯示選擇哪個日期
  output$datechoose <- renderText({
    # input$state的state是在ui部分建立的資料
    paste0('您選擇了  ', input$date)})
  #----------------------------------------------------------------------------------
  # 讀取所選國家的預測值
  fore  <- reactive({read.csv(paste0(dir_path,'/欣翰/預測結果/',input$contry,'預測值.csv'))})
  
  # Real的預測值
  output$vbox_RL <- renderValueBox({
    valueBox(value = tags$p(round(fore()[which(as.Date(fore()[,1])==input$date),2],3), style = "font-size: 150%;"),color='purple',
             subtitle = tags$p(paste(' REAL',sep=" "), style = "font-size: 150%;"),icon = icon("dollar-sign"))})
  
  # NAR的預測值
  output$vbox_NAR <- renderValueBox({
    valueBox(value = tags$p(round(fore()[which(as.Date(fore()[,1])==input$date),6],3), style = "font-size: 150%;"),
             subtitle = tags$p(paste(' NAR-GARCH',sep=" "), style = "font-size: 150%;"),icon = icon("dollar-sign"))})
  
  # VAR的預測值
  output$vbox_VAR <- renderValueBox({
    valueBox(value = tags$p(round(fore()[which(as.Date(fore()[,1])==input$date),5],3), style = "font-size: 150%;"),
             subtitle = tags$p(paste(' VAR-GARCH',sep=" "), style = "font-size: 150%;"),icon = icon("coins"))})
  
  # ARMA-GARCH的預測值
  output$vbox_AM <- renderValueBox({
    valueBox(value = tags$p(round(fore()[which(as.Date(fore()[,1])==input$date),4],3), style = "font-size: 150%;"),
             subtitle = tags$p(paste(' ARMA-GARCH',sep=" "), style = "font-size: 150%;"),icon = icon("hand-holding-usd"))})
  
  # AR(1)的預測值
  output$vbox_AR <- renderValueBox({
    valueBox(value = tags$p(round(fore()[which(as.Date(fore()[,1])==input$date),3],3), style = "font-size: 150%;"),
             subtitle = tags$p(paste(' AR(1)',sep=" "), style = "font-size: 150%;"),icon = icon("search-dollar"))})
  #----------------------------------------------------------------------------------
  # 前250天的配適圖
  # output$picture1 <- renderImage({
  #   list(src = paste0(input$contry,'  ',input$date,' 過去250天配適情況 (1).png'),contentType = 'image/png')}, deleteFile = FALSE)
  #----------------------------------------------------------------------------------
  
  
  # 第3頁
  #----------------------------------------------------------------------------------
  # 陽、陰性預測值解釋圖
  output$picture5 <- renderImage({
    list(src = '陽陰性預測值.png',contentType = 'image/png')}, deleteFile = FALSE)
  #----------------------------------------------------------------------------------
  # 顯示是選擇哪個國家
  output$contryname2 <- renderText({
    # input$state的state是在ui部分建立的資料
    paste0('您選擇了第  ', which(input$contry2 == name),'  個國家 --- ', input$contry2)})
  
  # 顯示選擇哪個年份
  output$yearchoose <- renderText({
    # input$state的state是在ui部分建立的資料
    paste0('您選擇了  ', input$year)})
  #----------------------------------------------------------------------------------
  
  
  # 預測大漲真實大漲
  #----------------------------------------------------------------------------------
  # 讀取所選國家 所選年份的陽陰性預測值
  confusion  <- reactive({read.csv(paste0(dir_path,'/欣翰/預測結果/',input$contry2,input$year,'陽陰性準確率.csv'))})

  # NAR的陽性準確率
  output$ppv_NAR <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[1,5],1), "%"), style = "font-size: 150%;"),color='purple',
             subtitle = tags$p(paste0(confusion()[2,5],'/',confusion()[3,5],' NAR-GARCH'), style = "font-size: 180%;"),icon = icon("dollar-sign"))})
  
  # VAR的陽性準確率
  output$ppv_VAR <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[1,4],1), "%"), style = "font-size: 150%;"),color='light-blue',
             subtitle = tags$p(paste0(confusion()[2,4],'/',confusion()[3,4],' VAR-GARCH'), style = "font-size: 180%;"),icon = icon("coins"))})
  
  # ARMA-GARCH的陽性準確率
  output$ppv_AM <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[1,3],1), "%"), style = "font-size: 150%;"),color='red',
             subtitle = tags$p(paste0(confusion()[2,3],'/',confusion()[3,3],' ARMA-GARCH'), style = "font-size: 180%;"),icon = icon("hand-holding-usd"))})
  
  # AR(1)的陽性準確率
  output$ppv_AR <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[1,2],1), "%"), style = "font-size: 150%;"),color='yellow',
             subtitle = tags$p(paste0(confusion()[2,2],'/',confusion()[3,2],' AR(1)'), style = "font-size: 180%;"),icon = icon("search-dollar"))})
  #----------------------------------------------------------------------------------
  # NAR的陰性準確率
  output$npv_NAR <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[7,5],1), "%"), style = "font-size: 150%;"),color='purple',
             subtitle = tags$p(paste0(confusion()[8,5],'/',confusion()[9,5]), style = "font-size: 180%;"),icon = icon("dollar-sign"))})
  
  # VAR的陰性準確率
  output$npv_VAR <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[7,4],1), "%"), style = "font-size: 150%;"),color='light-blue',
             subtitle = tags$p(paste0(confusion()[8,4],'/',confusion()[9,4]), style = "font-size: 180%;"),icon = icon("coins"))})
  
  # ARMA-GARCH的陰性準確率
  output$npv_AM <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[7,3],1), "%"), style = "font-size: 150%;"),color='red',
             subtitle = tags$p(paste0(confusion()[8,3],'/',confusion()[9,3]), style = "font-size: 180%;"),icon = icon("hand-holding-usd"))})
  
  # AR(1)的陰性準確率
  output$npv_AR <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[7,2],1), "%"), style = "font-size: 150%;"),color='yellow',
             subtitle = tags$p(paste0(confusion()[8,2],'/',confusion()[9,2]), style = "font-size: 180%;"),icon = icon("search-dollar"))})
  #----------------------------------------------------------------------------------
  
  # 預測大漲真實漲
  #----------------------------------------------------------------------------------
  # NAR的陽性準確率
  output$ppv_NAR2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[4,5],1), "%"), style = "font-size: 150%;"),color='purple',
             subtitle = tags$p(paste0(confusion()[5,5],'/',confusion()[6,5],' NAR-GARCH'), style = "font-size: 180%;"),icon = icon("dollar-sign"))})
  
  # VAR的陽性準確率
  output$ppv_VAR2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[4,4],1), "%"), style = "font-size: 150%;"),color='light-blue',
             subtitle = tags$p(paste0(confusion()[5,4],'/',confusion()[6,4],' VAR-GARCH'), style = "font-size: 180%;"),icon = icon("coins"))})
  
  # ARMA-GARCH的陽性準確率
  output$ppv_AM2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[4,3],1), "%"), style = "font-size: 150%;"),color='red',
             subtitle = tags$p(paste0(confusion()[5,3],'/',confusion()[6,3],' ARMA-GARCH'), style = "font-size: 180%;"),icon = icon("hand-holding-usd"))})
  
  # AR(1)的陽性準確率
  output$ppv_AR2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[4,2],1), "%"), style = "font-size: 150%;"),color='yellow',
             subtitle = tags$p(paste0(confusion()[5,2],'/',confusion()[6,2],' AR(1)'), style = "font-size: 180%;"),icon = icon("search-dollar"))})
  #----------------------------------------------------------------------------------
  # NAR的陰性準確率
  output$npv_NAR2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[10,5],1), "%"), style = "font-size: 150%;"),color='purple',
             subtitle = tags$p(paste0(confusion()[11,5],'/',confusion()[12,5]), style = "font-size: 180%;"),icon = icon("dollar-sign"))})
  
  # VAR的陰性準確率
  output$npv_VAR2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[10,4],1), "%"), style = "font-size: 150%;"),color='light-blue',
             subtitle = tags$p(paste0(confusion()[11,4],'/',confusion()[12,4]), style = "font-size: 180%;"),icon = icon("coins"))})
  
  # ARMA-GARCH的陰性準確率
  output$npv_AM2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[10,3],1), "%"), style = "font-size: 150%;"),color='red',
             subtitle = tags$p(paste0(confusion()[11,3],'/',confusion()[12,3]), style = "font-size: 180%;"),icon = icon("hand-holding-usd"))})
  
  # AR(1)的陰性準確率
  output$npv_AR2 <- renderValueBox({
    valueBox(value = tags$p(paste0(round(100*confusion()[10,2],1), "%"), style = "font-size: 150%;"),color='yellow',
             subtitle = tags$p(paste0(confusion()[11,2],'/',confusion()[12,2]), style = "font-size: 180%;"),icon = icon("search-dollar"))})
  #----------------------------------------------------------------------------------

  #----------------------------------------------------------------------------------
  # 顯示是選擇哪個type
  output$typename1 <- renderText({
    paste0('您選擇了  ', input$type1)})
  
  # 散佈圖
  output$scatter <- renderPlot(
    ggplot(forecast, aes(x = REAL, y = eval(parse(text=input$type1))))+
      geom_point(shape = 1)+
      geom_hline(yintercept=qua[1], color='coral', size=1)+
      geom_hline(yintercept=qua[2], color='coral', size=1)+
      geom_vline(xintercept=0     , size=1)
  )
  #----------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------
  # 配適結果的 DT
  # output$modelfit <- renderDataTable(fit, extensions = c('Buttons','FixedColumns','FixedHeader'),
  #                                    options = list(dom = 'Blfrtip',
  #                                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print' , 'colvis'),
  #                                                   scrollX = TRUE,
  #                                                   fixedColumns = TRUE,
  #                                                   pageLength = nrow(a), fixedHeader = TRUE))
  #----------------------------------------------------------------------------------
  
  
  # 第4頁
  #----------------------------------------------------------------------------------
  # 預測值的三種相關性結果(所有年分)
  output$picture2 <- renderImage({
    list(src = paste0(input$contry,' 預測值 Pearson correlation (1).png'),contentType = 'image/png')}, deleteFile = FALSE)
  output$picture3 <- renderImage({
    list(src = paste0(input$contry,' 預測值 Kendall correlation (1).png'),contentType = 'image/png')}, deleteFile = FALSE)
  output$picture4 <- renderImage({
    list(src = paste0(input$contry,' 預測值 Spearman correlation (1).png'),contentType = 'image/png')}, deleteFile = FALSE)
  #----------------------------------------------------------------------------------
  
  
  # 第5頁
  #----------------------------------------------------------------------------------
  # 顯示是選擇哪個國家
  output$contryname3 <- renderText({
    paste0('您選擇了第  ', which(input$contry3 == name),'  個國家 --- ', input$contry3)})
  
  # 顯示選擇的百分比
  output$percentile <- renderText({
    paste0('您選擇的百分位數界線為  ', input$range,'%')})
  
  # 顯示選擇的手續費
  output$feecost <- renderText({
    paste0('您選擇的手續費為  ', input$feee,'%')})
  
  # 年度總報酬率(含手續費)
  output$picture6 <- renderImage({
    list(src = paste0(input$contry3,'總報酬率_',input$range,'_',input$feee,'.png'),contentType = 'image/png')}, deleteFile = FALSE)
  #----------------------------------------------------------------------------------
  
  # 顯示是選擇哪個國家
  output$contryname4 <- renderText({
    paste0('您選擇了第  ', which(input$contry4 == name),'  個國家 --- ', input$contry4)})
  
  # 顯示選擇的百分比
  output$percentile2 <- renderText({
    paste0('您選擇的百分位數界線為  ', input$range2,'%')})
  
  # 年度總報酬率(含手續費)
  output$picture7 <- renderImage({
    list(src = paste0(input$contry4,'總報酬率_',input$range2,'.png'),contentType = 'image/png')}, deleteFile = FALSE)
  
  #----------------------------------------------------------------------------------
  

  # 第6頁
  #----------------------------------------------------------------------------------
  # 用1減去一期網絡的p-value
  adj <- read.csv(paste0(dir_path,'/欣翰/關聯性/澳洲2020一期網絡.csv'))
  adj=adj[60,2:21]
  for(i in 1:20){
    if(adj[,i]!=0){
      adj[,i]=round(1-adj[,i], digits = 5)
    }
  }
  rownames(adj)=c('2019-12-30')
  
  # 將eng_name中的國家名稱轉換成rwmGetISO3的國家代碼型式
  country_iso3 = unlist(lapply(eng_name, rwmGetISO3))
  # 將數據和rworldmap中的國家代碼合併成data.frame
  mmap = data.frame(country = country_iso3, Value = as.numeric(adj))
  # 將data.frame轉換為地圖可輸出的格式
  mmaps = joinCountryData2Map(mmap, joinCode = "ISO3", nameJoinColumn = "country")
  # 輸出地圖
  output$colormap <- renderPlot({
    mapCountryData(mmaps, nameColumnToPlot="Value", colourPalette="terrain", mapTitle = "world map", lwd = 1)
  })
  
  # 一期網絡的p-value
  output$adjcacency <- renderDataTable(adj, extensions = c('Buttons','FixedColumns','FixedHeader'),
                                     options = list(dom = 'Blfrtip',
                                                    #buttons = c('copy', 'csv', 'excel', 'pdf', 'print' , 'colvis'),
                                                    scrollX = TRUE,
                                                    fixedColumns = TRUE,
                                                    pageLength = nrow(a), fixedHeader = TRUE))
  #----------------------------------------------------------------------------------

}
#===========================================================================================

# Create Shiny app
shinyApp(ui, server)









