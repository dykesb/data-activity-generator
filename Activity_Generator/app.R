library(shiny)
library(sortable)
library(rmarkdown)
library(readxl)

ui <- fluidPage(

    titlePanel("Activity Generator"),

    fileInput("file", label = "File input"),
    
    selectInput("yvar", label = "What is the response variable?",
                choices = "---"),
    
    selectInput("xvar", label = "What is the explanatory variable?",
                choices = "---"),
    
    textInput("population", label = "What is the population?"),
    
    bucket_list("Question Selection", orientation = "horizontal",
                add_rank_list(text = "Question Bank", labels = Question_Bank$Question[c(1:3)]),
                add_rank_list(text = "Selected Questions", input_id = "qselect")),
    
    downloadButton("activity", "Generate activity"),
    
    tableOutput("testprint"),
    
    hr()
)

server <- function(input, output, session) {
    
    data <- reactive(read_xlsx(input$file$datapath))
    
    observeEvent(input$file, {
        updateSelectInput(session, "yvar", choices = colnames(data()))
        updateSelectInput(session, "xvar", choices = colnames(data()))
    })
    
    # output$testprint <- renderTable({
    # 
    #     qlist <- list()
    # 
    #     for (i in 1:length(input$qselect)) {
    #         qlist[[i]] <- which(Question_Bank$Question == input$qselect[i])
    # }})
    # 
    output$activity <- downloadHandler(
        filename = "activity.html",
        content = function(file) {

            qlist <- list()
            
            for (i in 1:length(input$qselect)) {
                qlist[[i]] <- which(Question_Bank$Question == input$qselect[i])
            }
                
            #qlist = list(c(1, 0), 3, c(4,0), 5, 6, 7)
            
            params <- list(key = FALSE, time = Sys.time(), qlist = qlist, 
                           yvar = input$yvar, xvar = input$xvar, population = input$population)

            render("/cloud/project/QuestionFunction.Rmd", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        }
    )
}

shinyApp(ui = ui, server = server)


