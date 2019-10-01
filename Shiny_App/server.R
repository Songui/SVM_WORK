#Projet_SVM_AKS
#ACTIVER UTF8

#On doit permettre à l'utilisateur de faire entrer la variable target y
#######The minority class must be coded 1 and the majority 0 for the unbalanced family function ??? #####
####### We should imposed that the class have to be the last one of the data column and called "class" ?? #####

library(FNN)
library(shiny)
library(summarytools)
library(DescTools)
library(smotefamily)
library(unbalanced)
library(DataExplorer)
library(DT)
library(dplyr)
library(corrplot)
library(haven)
library(readxl)
library(shinydashboard)
library(DescTools)
library(plotly)
library(shinydashboardPlus)


oversampling = function (data, p, position) 
{
  summary = table(data[,position])
  
  r_event_value = names(summary)[which.min(table(data[,position]))]
  
  r_event_index = which(data[,position]==r_event_value)
  
  r_event_length = length(r_event_index)
  
  data_length = nrow(data)
  
  new_r_event_length = floor((data_length-r_event_length)/(1-p) - data_length)
  
  new_index = sample(r_event_index,size=new_r_event_length,replace = TRUE)
  
  new_data = rbind(data,data[new_index,])
  
  new_data
  
}

undersampling = function(data, p, position)
{
  summary = table(data[,position])
  
  m_event_value = names(summary)[which.max(table(data[,position]))]
  
  m_event_index = which(data[,position]==m_event_value)
  
  m_event_length = length(m_event_index)
  
  data_length = nrow(data)
  
  new_m_event_length = floor(data_length - (data_length-m_event_length)/p)
  
  new_index = sample(m_event_index,size=new_m_event_length)
  
  new_data = data[-new_index,]
  
  new_data
  
}

options(shiny.maxRequestSize=150*1024^2)


shinyServer(function(input, output) {
 
 
###################### DATA PART ##############################  
  
  output$file_options = renderUI({
    
    switch(input$type_file,
           "CSV" = list(radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                        checkboxInput(inputId = 'header', label =strong('Header'), value = TRUE),
                        radioButtons(inputId = 'dec', label = 'Decimal', choices = c(Point='.', Comma=','), selected = '.')
             ),
           "TXT" = list(radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                   checkboxInput(inputId = 'header', label = strong('Header'), value = TRUE),
                   radioButtons(inputId = 'dec', label = 'Decimal', choices = c(Point=".", Comma=','), selected = '.')
             ),
           "EXCEL" = checkboxInput(inputId = 'header', label = strong('Header'), value = TRUE))
    
  })
  
  
  raw_data = reactive ({
    validate(
      need(is.null(input$loading)== FALSE, "Please select a file")
    )
    
    file = input$loading
 
    data= switch(input$type_file,
           "CSV" = read.csv (file$datapath, header = input$header, sep = input$sep, dec = input$dec), 
           "TXT" = read.table (file$datapath, header = input$header, sep = input$sep, dec = input$dec),
           "EXCEL" = read_excel(file$datapath, col_names = input$header),
           "SAS" = read_sas(file$datapath)
           )
    as.data.frame(data)
    
    }) 

  
  output$target_ui = renderUI({
  
    selectInput("targetvar", "Select your target variable",names(raw_data()))
    
  })
  
  
  data = reactive ({
    validate(
      need(is.null(input$loading) == FALSE, "Please select a file")
    )  
    data = raw_data() 
    data[,input$targetvar] = as.factor(data[,input$targetvar])
    names(data)[which(names(data) == input$targetvar)] = "class"
    data
  })
  
  
  target_position = reactive({
    validate(
      need(is.null(data()) == FALSE, "Please select a file")
    )
    which(names(data()) == "class")
    
    })
  
  output$other_var_ui = renderUI({
    validate(
      need(is.null(input$targetvar) == FALSE, "Please select a file")
    )
    
    selectInput("other_var", "Select variables to withdraw from modelisation",names(data())[- target_position()],multiple = TRUE)
    })
  
  
  output$data = DT::renderDataTable(data () , options = list(scrollX = TRUE))
  
   
  output$plot3 = renderPlot({
    
    data = as.matrix(dplyr::select_if(data(), is.numeric))
    
    corrplot(cor(data,use = "complete.obs"),tl.col="black")
  })
  
  
  output$plot2 = renderPlot(
  
    ggplot(data=data()) + geom_bar(mapping = aes(x=class))
  )
  
  
  output$plot1 = renderUI(

    print(dfSummary(data(), graph.magnif = 0.8), 
              method = 'render',
              headings = FALSE,
              bootstrap.css = FALSE)
  )
  
  
 output$sum1 = renderPrint( introduce(data()) )
  
 
 output$boxplot_var = renderUI(
   
     selectInput("boxplot","Choose one variable for BoxPlot",names(dplyr::select_if(data(), is.numeric   )))
)
 
 output$plot4 = renderPlot(
   
   ggplot(data = data()) + 
     geom_boxplot(mapping = aes(x = class, y = get(input$boxplot))) +
       labs(y = input$boxplot)
 )
  
  
    ###################### SAMPLING PART ##############################
  
  train = reactive({
    data_nrow = nrow(data())
    train_length = round(input$select_train*data_nrow)
    sample (data_nrow,train_length) #Index pour l'échantillon d'apprentissage, on peut le changer.
    })
  
  train_sample = reactive(data()[train(),]) #Echantillon apprentissage
  
  test_sample = reactive(data()[-train(),]) #Echantillon test
  
  r_event_freq = reactive({
    freq = Freq(train_sample()$class)
    min(freq$perc)
    }) #la fréquence de l'évènement rare
  
  
  output$sampling_out = renderUI ({
    if(is.null(data())) {return()}
    switch(input$select_sampling,
           "Oversampling" = sliderInput("p","Rare class resampling probability", min = 0, max= 1, value=0.5, step=0.01),
           "Undersampling" = sliderInput("p","Rare class resampling probability", min = 0, max= 1, value=0.5, step=0.01),
           "SMOTE" = verticalLayout(numericInput("dup_size","Duplicated size",min=1,value=2), numericInput("k_near","K nearest neighbors",min=1, value=5)),
           "ADASYN" = numericInput("k_near","K nearest neighbors",min=1, value = 5)
           
    )
    
  })
  
  
  resampling_train = reactive({
    
    train_sample = train_sample()
    y = train_sample$class
    proba = input$p
    r_event_freq = r_event_freq()
    p1 = try({if (proba==1) {0.999} else {max(proba,r_event_freq)}}, silent=T)
    p2 = try({max(proba,r_event_freq)}, silent=T)
    
    y_position = which(names(train_sample)=="class")
    dup_size = try(floor(input$dup_size),silent = T)
    k_near = try(floor(input$k_near),silent = T)
    
    switch(input$select_sampling,
           "Oversampling" = oversampling(train_sample,p1,y_position) ,
           "Undersampling" = undersampling(train_sample,p2,y_position) ,
           "SMOTE" = SMOTE(train_sample[,-y_position],y, dup_size=dup_size, K= k_near)$data ,
           "ADASYN" = ADAS(train_sample[,-y_position],y, K=k_near)$data,
           "Tomek-Link" = {data = ubTomek(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                             cbind(data$X,class=data$Y)},
           "Tomek-Link + Undersampling" = {data = ubTomek(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                                           newData = cbind(data$X,class=data$Y);
                                           undersampling(newData,p2 ,y_position)},
           "Condensed Nearest Neighbor" = {data = ubCNN(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                                                  cbind(data$X,class=data$Y) }
    )
  })
  
  output$out = renderDataTable(resampling_train ()) #TEST pour montrer que ça marche, à delete plus tard
    
  

  
  
   
    
  })
  
