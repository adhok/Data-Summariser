library(ggplot2)
library(lazyeval)
library(tidyverse)
library(plotly)
library(V8)
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
    axis.title = element_text(size=15),
    axis.text = element_text(size=15),
    axis.title.x = element_text(hjust=1,size=15),
    axis.title.y = element_text(hjust=1,size=15),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold"),
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(vjust=-1,angle=90,size=15))
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
      #data_output <- read.csv(text=input$mydata[[name]])
      #tableOutput(head(data_output))
    })
    output$table_summary <- renderUI({
      
      h3("Table Summary")
    })
    observeEvent(input$refresh, {
      
      show(id="loading", anim = TRUE, animType = "fade")
      Sys.sleep(1.5)
      
      hide(id = "loading", anim = TRUE, animType = "fade") 
      shinyjs::js$refresh()
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
      
      validate(
        need(input$factor_input_for_ring!='', "Ooops...This data set does not have a factor variable."))
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
      
      data_nums <- data_output
      # nums_names <- names(nums)
      
      
      
      
      title_text <- paste("Relationship between",input$numerical_input_1,'and',input$numerical_input_2)
      if(input$grouped_ungrouped=='ungrouped'){
        p <- ggplot(data=data_nums,aes(x=data_nums[,input$numerical_input_1],y=data_nums[,input$numerical_input_2]))+
          geom_point()+labs(title=title_text,x=input$numerical_input_1,y=input$numerical_input_2)
        
      }else{
        
        validate(
          need(input$factor_input_for_ring!='', "Ooops...This data set does not have a factor variable."))
        #data_nums <- data_nums %>% group_by_(input$factor_input_for_ring) %>% summarise(n=)
        p <- ggplot(data=data_nums,aes(x=data_nums[,input$numerical_input_1],y=data_nums[,input$numerical_input_2],
                                       color=as.factor(data_nums[,input$factor_input])))+
          geom_point()+labs(title=title_text,x=input$numerical_input_1,y=input$numerical_input_2)+
          guides(fill=guide_legend(title=input$factor_input))+theme(legend.position = 'none')
        
      }
      
      show(id="loading", anim = TRUE, animType = "fade",'Graphs are loading...')
      Sys.sleep(1.5)
      
      hide(id = "loading", anim = TRUE, animType = "fade",'Graphs are loading...') 
      tryCatch(print(p+plotTheme()))
      
      
      
    })
    output$grouped_ungrouped <- renderUI({
      
      
      radioButtons(inputId = 'grouped_ungrouped',label="Grouped/Ungrouped",choices=c('Grouped'='grouped',
                                                                                     'Ungrouped'='ungrouped'),selected = 'ungrouped')
      
      
      
    })
    output$factor_button_for_ring_chart <- renderUI({
      
      data_output <- read.csv(text=input$mydata[[name]])
      factor_variables <- sapply(data_output,is.factor)
      factor_variables  <- names(factor_variables[factor_variables==TRUE])
      selectInput('factor_input_for_ring',label="Input Variable",selected = factor_variables[1],choices=factor_variables)
      
    })
    
    output$ring_chart <- renderPlotly({
      
      data_output <- read.csv(text=input$mydata[[name]])
      validate(
        need(input$factor_input_for_ring!='', "Ooops...This data set does not have a factor variable."))
      
      
        
        dat <- data_output %>% group_by_(input$factor_input_for_ring) %>% summarise(n=n()) %>% mutate(n=n/sum(n))
        p <- dat %>%
          group_by_(input$factor_input_for_ring) %>%
          summarize(count = n()) %>%
          rename_('group'=input$factor_input_for_ring) %>%
          plot_ly(labels = ~group, values = ~count) %>%
          add_pie(hole = 0.6) %>%
          layout(title = paste('Proportions of',input$factor_input_for_ring),  showlegend = FALSE,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
          config(displayModeBar=FALSE,sendData=FALSE,displaylogo=FALSE)
        
        p
        
        
      
      
      
      

      
        
        
      
      
    })
    output$bar_and_box_plot_header <- renderUI({
      
      if(input$aggregate_method=="dist"){
        
        HTML('<h3> Box Plots </h3>')
      }
      else{
        HTML('<h3> Bar Plots </h3>')
      }
      
    })
    output$plots_first_row <- renderUI({
      
      
      
      
      fluidRow(
        
        HTML("<br>"),
        HTML("<br>"),
        HTML("<h3>Scatter Plots</h3>"),
         plotOutput('scatter'),
         
         HTML("<br>"),
         HTML("<br>"),
        uiOutput('bar_and_box_plot_header'),
        
         
         plotOutput('grouped_plots'),
         HTML("<br>"),
         HTML("<br>"),
        HTML("<h3>Ring Plots</h3>"),
        
         plotlyOutput('ring_chart'),
         HTML("<br>"),
         HTML("<br>"),
         HTML('<button onclick="topFunction()" id="myBtn" title="Go to top">Go Back To The Top</button>')
        
      )
      
      
      
    })
    
    
    for (name in names(input$mydata)) {
      output[[name]] <- renderTable(head(read.csv(text=input$mydata[[name]]),4))
    }
  })
  
}















