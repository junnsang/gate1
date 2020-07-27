library(shiny)
library(lubridate)
library(dplyr)

shinyApp(
        ui <- pageWithSidebar(
                headerPanel("gate1 visit count"),
                sidebarPanel(
                        tags$script('$(document).on("keydown",
                                function (e) {
                                        if(e.which == 37) {
                                                Shiny.onInputChange("downButton", new Date());
                                        } else if (e.which == 39) {
                                                Shiny.onInputChange("upButton", new Date());
                                        }
                                });'
                        ),
                        actionButton("downButton", "Down"),
                        actionButton("upButton", "Up"), 
                        downloadButton("downloadData", "Download df")), 
                mainPanel(htmlOutput("table"))),
        
        server <- function(session, input, output) {
                vals <- reactiveValues(m = data.frame())
                observeEvent(input$downButton, {vals$m <- vals$m %>% slice(1:nrow(vals$m)-1)})
                observeEvent(input$upButton, {vals$m <- bind_rows(vals$m, 
                                                                  c(count = as.character(nrow(vals$m)+1), time = as.character(now())))})
                output$table <- renderTable(tail(vals$m, 5))
                thedata <- reactive(vals$m)
                output$downloadData <- downloadHandler(
                        filename = function(){"visit_count.csv"},
                        content = function(filename) {
                                write.csv(thedata(), filename, row.names = FALSE)
                        }
                )
        }
)