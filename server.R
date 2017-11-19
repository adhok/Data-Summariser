library(ggplot2)

server <- function(input, output, session) {
  observeEvent(input$mydata, {
    len = length(input$mydata)
    output$tables <- renderUI({
      table_list <- lapply(1:len, function(i) {
        tableName <- names(input$mydata)[[i]]
        tableOutput(tableName)
      })
      do.call(tagList, table_list)
    })
    output$table_summary <- renderUI({
      
      h6("Table Summary")
    })
    output$num_buttons_1 <- renderUI({
      data_output <- read.csv(text=input$mydata[[name]])
      nums <- sapply(data_output, is.numeric)
      data_nums <- data_output[,nums]
      
      nums_names <- names(nums[nums==TRUE])
      selectInput('numerical_input_1',label="X-axis",choices=nums_names,selected = nums_names[1])
      
      
    })
    output$num_buttons_2 <- renderUI({
      data_output <- read.csv(text=input$mydata[[name]])
      nums <- sapply(data_output, is.numeric)
      data_nums <- data_output[,nums]
      nums_names <- names(nums[nums==TRUE])
      selectInput('numerical_input_2',label="Y-axis",choices=nums_names,selected = nums_names[2])
      
      
    })
    
    
    output$scatter <- renderPlot({
      
      data_output <- read.csv(text=input$mydata[[name]])
      nums <- sapply(data_output, is.numeric)
      data_nums <- data_output[,nums]
      # nums_names <- names(nums)
      title_text <- paste("Relationship between",input$numerical_input_1,'and',input$numerical_input_2)
      p <- ggplot(data=data_nums,aes(x=data_nums[,input$numerical_input_1],y=data_nums[,input$numerical_input_2]))+
        geom_point()+labs(title=title_text,x=input$numerical_input_1,y=input$numerical_input_2)
      print(p)
      
      
      
    })
    output$plots_first_row <- renderUI({
      
      
      
      
      fluidRow(
        column(width=4,plotOutput('scatter')),
        column(width=4),
        column(width=4)
        
      )
      
      
      
    })
    
    for (name in names(input$mydata)) {
      output[[name]] <- renderTable(head(read.csv(text=input$mydata[[name]]),4))
    }
  })
}