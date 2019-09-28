#Projet_SVM_AKS
#ACTIVER UTF8

#On doit permettre à l'utilisateur de faire entrer la variable target y
#######The minority class must be coded 1 and the majority 0 for the unbalanced family function ??? #####
####### We should imposed that the class have to be the last one of the data column and called "Class" ?? #####

library(shiny)
library(DescTools)
library(smotefamily)
library(unbalanced)

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
 
  data = reactive ({
    file = input$loading
    if (is.null(file)){return ()}
    data = read.csv (file$datapath)
    data$Class = as.factor(data$Class)
    data
    }) #Chargement de la table


  
  ###################### SAMPLING PART ##############################
  
  train = reactive({
    data_nrow = nrow(data())
    train_length = round(input$select_train*data_nrow)
    sample (data_nrow,train_length) #Index pour l'échantillon d'apprentissage, on peut le changer.
    })
  
  train_sample = reactive(data()[train(),]) #Echantillon apprentissage
  
  test_sample = reactive(data()[-train(),]) #Echantillon test
  
  r_event_freq = reactive({
    freq = Freq(train_sample()$Class)
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
    if(is.null(data())) {return()}
    
    train_sample = train_sample()
    y = train_sample$Class
    proba = input$p
    r_event_freq = r_event_freq()
    p1 = try({if (proba==1) {0.999} else {max(proba,r_event_freq)}}, silent=T)
    p2 = try({max(proba,r_event_freq)}, silent=T)
    
    y_position = which(names(train_sample)=="Class")
    dup_size = try(floor(input$dup_size),silent = T)
    k_near = try(floor(input$k_near),silent = T)
    
    switch(input$select_sampling,
           "Oversampling" = oversampling(train_sample,p1,y_position) ,
           "Undersampling" = undersampling(train_sample,p2,y_position) ,
           "SMOTE" = SMOTE(train_sample[,-y_position],y, dup_size=dup_size, K= k_near)$data ,
           "ADASYN" = ADAS(train_sample[,-y_position],y, K=k_near)$data,
           "Tomek-Link" = {data = ubTomek(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                             cbind(data$X,Class=data$Y)},
           "Tomek-Link + Undersampling" = {data = ubTomek(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                                           newData = cbind(data$X,Class=data$Y);
                                           undersampling(newData,p2 ,y_position)},
           "Condensed Nearest Neighbor" = {data = ubCNN(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                                                  cbind(data$X,Class=data$Y) }
    )
  })
  
  output$out = renderDataTable(tail(resampling_train())) #TEST pour montrer que ça marche, à delete plus tard
    
  

  
  
   
    
  })
  
