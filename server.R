library(ggplot2)
library(lazyeval)
#options(error = 999)
mean_function <- function(df,group.var,metric.var){
  
  df %>%
    group_by_(group.var) %>%
    summarise_(n=interp(~mean(v),v=as.name(metric.var)))
}

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
    output$factor_button <- renderUI({
      
      data_output <- read.csv(text=input$mydata[[name]])
      factor_variables <- sapply(data_output,is.factor)
      factor_variables <- factor_variables[factor_variables==TRUE]
      names_factors <- names(factor_variables)
      selectInput('factor_input',label="Input the Factor Variable",choices = names_factors,selected = names_factors[1])
      
    })
    
    output$numeric_input  <- renderUI({
      
      data_output <- read.csv(text=input$mydata[[name]])
      numeric_variables <- sapply(data_output,is.numeric)
      numeric_variables <- numeric_variables[numeric_variables==TRUE]
      names_numeric <- names(numeric_variables)
      selectInput('numeric_input',label="Input the Numeric Variable",choices = names_numeric,selected = names_numeric[1])
      
    })
    output$grouped_mean <- renderPlot({
      
      data_output <-read.csv(text=input$mydata[[name]])
      factor_variables <- sapply(data_output,is.factor)
      
      factor_variables <- factor_variables[factor_variables==TRUE]
      nums <- sapply(data_output, is.numeric)
      data_nums <- data_output[,nums]
      nums_names <- names(nums[nums==TRUE])
      names_factor <- names(factor_variables)
      data_group <- mean_function(data_output,names_factor[1],nums_names[1])
      data_group <- as.data.frame(data_group)
      p <- ggplot(data=data_group,aes(x=data_group[,names(data_group)[1]],y=data_group[,names(data_group)[2]]))+
        geom_bar(stat='identity')+labs(x=names(data_group)[1],y=names(data_group)[2])
      tryCatch(print(p))
        
      
      
    })
    
    
    output$scatter <- renderPlot({
      
      data_output <- read.csv(text=input$mydata[[name]])
      nums <- sapply(data_output, is.numeric)
      data_nums <- data_output[,nums]
      # nums_names <- names(nums)
      title_text <- paste("Relationship between",input$numerical_input_1,'and',input$numerical_input_2)
      p <- ggplot(data=data_nums,aes(x=data_nums[,input$numerical_input_1],y=data_nums[,input$numerical_input_2]))+
        geom_point()+labs(title=title_text,x=input$numerical_input_1,y=input$numerical_input_2)
      tryCatch(print(p))
      
      
      
    })
    output$plots_first_row <- renderUI({
      
      
      
      
      fluidRow(
         column(width=4,plotOutput('scatter')),
         column(width=4,plotOutput('grouped_mean')),
         column(width=4)
        
      )
      
      
      
    })
    
    for (name in names(input$mydata)) {
      output[[name]] <- renderTable(head(read.csv(text=input$mydata[[name]]),4))
    }
  })
}