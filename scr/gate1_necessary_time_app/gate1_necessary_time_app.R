library(shiny)
library(lubridate)
library(dplyr)

shinyApp(
        ui <- pageWithSidebar(
                headerPanel("gate visit count"),
                
                
                sidebarPanel(
                        tags$script('$(document).on("keydown",
                                function (e) {
                                        if(e.which == 68) {
                                                Shiny.onInputChange("delete", new Date());
                                        } else if (e.which == 37) {
                                                Shiny.onInputChange("visit", new Date());
                                        } else if (e.which == 39) {
                                                Shiny.onInputChange("leave", new Date());
                                        } else if (e.which == 40) {
                                                Shiny.onInputChange("PatientNo_plus", new Date());
                                        } else if (e.which == 38) {
                                                Shiny.onInputChange("PatientNo_minus", new Date());
                                        }
                                });'
                        ),
                        actionButton("delete", "Down"),
                        actionButton("visit", "Up"), 
                        selectInput("pass_type", label = h3("Select box"), 
                                    choices = list("질문만" = "q_only", 
                                                   "키오스크" = "kiosk", 
                                                   "키오스크 (도움필요)" = "kiosk_c_aid", 
                                                   "QR" = "qr", 
                                                   "QR (도움필요)" = "qr_c_aid"),
                                    selected = 1),
                        downloadButton("downloadData", "Download df")), 
                mainPanel(htmlOutput("PatientNo"), 
                          htmlOutput("table"))),
        
        server <- function(session, input, output) {
                
                # 빈 데이터
                df <- reactiveValues(m = data.frame())
                vals <- reactiveValues(PatientNo = 1)
                observeEvent(input$PatientNo_plus, {vals$PatientNo <- vals$PatientNo - 1})
                observeEvent(input$PatientNo_minus, {vals$PatientNo <- vals$PatientNo + 1})
                # pt_no <- vals$PatientNo
                # 키 입력 시 데이터 변환
                observeEvent(input$delete, {df$m <- df$m %>% slice(1:nrow(df$m)-1)})
                observeEvent(input$visit, {df$m <- bind_rows(df$m, 
                                                               c(patient_No = vals$PatientNo,
                                                                 pass_type = as.character("visit"), 
                                                                 time = as.character(now())))})
                observeEvent(input$leave, {df$m <- bind_rows(df$m, 
                                                               c(patient_No = vals$PatientNo, 
                                                                 pass_type = as.character(input$pass_type),
                                                                 time = as.character(now())))})
                output$table <- renderTable(tail(df$m, 5))
                thedata <- reactive(df$m)
                
                # 데이터 저장 위함
                output$downloadData <- downloadHandler(
                        filename = function(){"PatientNo.csv"},
                        content = function(filename) {
                                write.csv(thedata(), filename, row.names = FALSE)
                        }
                )
                output$PatientNo <- renderText({paste("PatientNo: ", vals$PatientNo)})
        }
)