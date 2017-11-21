library(ggplot2)
library(lazyeval)
library(tidyverse)
#options(error = 999)
appCSS <- "
#loading {
  position: fixed;
left: 50%;
top: 50%;
z-index: 1;
width: 150px;
height: 150px;
margin: -75px 0 0 -75px;
border: 16px solid #f3f3f3;
border-radius: 50%;
border-top: 16px solid #3498db;
width: 120px;
height: 120px;
-webkit-animation: spin 2s linear infinite;
animation: spin 2s linear infinite;
}
@-webkit-keyframes spin {
0% { -webkit-transform: rotate(0deg); }
100% { -webkit-transform: rotate(360deg); }
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}
"



plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 12,colour = "black",hjust=0.5),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=8),
    axis.title = element_text(size=5),
    axis.text = element_text(size=5),
    axis.title.x = element_text(hjust=1,size=10),
    axis.title.y = element_text(hjust=1,size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold"),
    axis.text.x = element_text(vjust=-1,angle=90,size=10))
}
server <- function(input, output, session) {
  
  
  observeEvent(input$mydata, {
    show("app-content")
    
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
    output$output_type <- renderUI({
      
      
      radioButtons('aggregate_method','Aggregate Function',choices = c('Mean'='mean','Median'='median','Distribution'='dist'),selected = 'mean',inline = T)
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
    
    group_function <- function(df,group.var,metric.var){
      if(input$aggregate_method=='mean'){
        
        df %>%
          group_by_(group.var) %>%
          summarise_(n=interp(~mean(v),v=as.name(metric.var)))
      }else if(input$aggregate_method=='median'){
        df %>% 
          group_by_(group.var) %>%
          summarise_(n=interp(~median(v),v=as.name(metric.var)))
        
      }else{
        df %>%
          select_(group.var,metric.var)
        
      }
      
      
    }
    output$grouped_plots <- renderPlot({
      
      data_output <-read.csv(text=input$mydata[[name]])
      data_group <- group_function(data_output,input$factor_input,input$numeric_input)
      
      
      
      data_group <- as.data.frame(data_group)
      #print(names(data_group))
      if(input$aggregate_method=='dist'){
        
        p <- ggplot(data=data_group,aes(x=data_group[,names(data_group)[1]],y=data_group[,names(data_group)[2]]))+
          geom_boxplot()+labs(x=names(data_group)[1],y=names(data_group)[2])
        
      }else{
        p <- ggplot(data=data_group,aes(x=data_group[,names(data_group)[1]],y=data_group[,names(data_group)[2]]))+
          geom_bar(stat='identity')+labs(x=names(data_group)[1],y=names(data_group)[2])
        
      }
      show(id="loading", anim = TRUE, animType = "fade")
      Sys.sleep(1.5)
      
      hide(id = "loading", anim = TRUE, animType = "fade") 
     
      print(p+plotTheme())
        
      
      
    })
    
    
    output$scatter <- renderPlot({
      
      data_output <- read.csv(text=input$mydata[[name]])
      nums <- sapply(data_output, is.numeric)
      data_nums <- data_output[,nums]
      # nums_names <- names(nums)
      title_text <- paste("Relationship between",input$numerical_input_1,'and',input$numerical_input_2)
      p <- ggplot(data=data_nums,aes(x=data_nums[,input$numerical_input_1],y=data_nums[,input$numerical_input_2]))+
        geom_point()+labs(title=title_text,x=input$numerical_input_1,y=input$numerical_input_2)
      show(id="loading", anim = TRUE, animType = "fade",'Graphs are loading...')
      Sys.sleep(1.5)
      
      hide(id = "loading", anim = TRUE, animType = "fade",'Graphs are loading...') 
      tryCatch(print(p+plotTheme()))
      
      
      
    })
    
    output$ring_chart <- renderPlot({
      
      data_output <- read.csv(text=input$mydata[[name]])
      factor_variables <- sapply(data_output,is.factor)
      factor_variables  <- names(factor_variables[factor_variables==TRUE])
      # data_output %>% 
      #   group_by_(factor_variables[1]) %>%
      #   summarise(n=n()) %>%
      #   mutate(n=n/sum(n)) %>% 
      #   rename_('Group'=factor_variables[1]) %>%
      #   ggplot(aes(x=Group,y=n))
      
      
    })
    output$plots_first_row <- renderUI({
      
      
      
      
      fluidRow(
         column(width=4,plotOutput('scatter')),
         column(width=4,plotOutput('grouped_plots')),
         column(width=4,plotOutput('ring_chart'))
        
      )
      
      
      
    })
    
    for (name in names(input$mydata)) {
      output[[name]] <- renderTable(head(read.csv(text=input$mydata[[name]]),4))
    }
  })
}