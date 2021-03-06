#Projet_SVM_AKS
#ACTIVER UTF8

library(shinyWidgets)
library(MASS)
library(FNN)
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
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(randomForest)
library (class)
library(tree)
library(gbm)
library(caret)
library(pROC)
library(e1071)
library(tinytex)




Logged = FALSE

my_username <- c("esa_ch", "esa_jd", "esa_ksh","esa_asm", "esa_slk")
my_password <- "aksgroup"
all_names   <- c("Christophe HURLIN", "Jéremy DUDEK", "Songui Hamed KONE", "Myra AKADJAME", "Labo Kalida SANI")


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


shinyServer(function(input, output, session) {
  
  
  ####Download Notice button###### 
  output$dow = downloadHandler (
    ###filename donne la sortie du document.Si c'est en pdf, il faut mettre Notice.pdf
    filename = "Notice.docx",
    #Dans l'aide, ils ont dit que content est égale à une fonction qui prend pour argument file et qui
    #représente un chemin de fichier pour un chemin temporaire non existant et écrit le contenu dans
    #ce chemin de fichier
    content= function(file) {
      tempReport <- file.path(tempdir(), "Notice.Rmd")
      file.copy("Notice.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown :: render(tempReport,output_file = file,
                          envir=new.env(parent=globalenv()))
    }
  )
  

  ##################### Authentification ########################
  
  values <- reactiveValues(authenticated = FALSE)
  
  dataModal1 <- function() {
    modalDialog(
      helpText((strong(h3("Authentification"))),br()),
      wellPanel(textInput("username", "Username :"),
                passwordInput("password", "Password :")
                
      ),
      footer = fluidRow(column(5,"If you can't access, please contact us to have access !", em(strong("esa.aks.group@gmail.com"))), column(4, " "), column(3,actionButton("ok", "LogIn"))),
      fade = FALSE
    )
  }
  
  
  obs1 <- observe({
    showModal(dataModal1())
  })
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) 
    { Logged <<- TRUE
    values$authenticated <- TRUE
    obs1$suspend()
    removeModal()
    
    } else {
      values$authenticated <- FALSE
      
    }     
  })
  
  #######Gestion du right side bar 
  
  output$authent = renderUI(
    {
      if (values$authenticated)
      {
        Username <- input$username
        Id <- which(my_username == Username)
        nameE =  all_names[Id]
        div(nameE,style = "color:rgb(180,85,85);font-family: verdana;
            font-size: 150%; border: 1px solid powderblue;
            padding: 5px;")
      } }
  )
  
  
  
  ###################### DATA PART ##############################  
  
  ############ 1% of data ############
  
 default_data = reactive ({ load(url("https://github.com/Songui/SVM_WORK/blob/master/creditcard.RData?raw=true"))
  
  index1 = which(creditcard$Class==1)
  l1=length(index1)
  index0= which(creditcard$Class==0)
  l0=length(index0)
  
  choix0 = sample (index0,size=l0*0.001)
  
  choix1 = sample (index1,size=l1*0.1)
  
  creditcard[c(choix1,choix0),] 
  })
  
  ########################################
  
  
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
  
  eR9 = eventReactive(input$submit5, { file = input$loading
                                      data= switch(input$type_file,
                                                   "CSV" = read.csv (file$datapath, header = input$header, sep = input$sep, dec = input$dec), 
                                                   "TXT" = read.table (file$datapath, header = input$header, sep = input$sep, dec = input$dec),
                                                   "EXCEL" = read_excel(file$datapath, col_names = input$header),
                                                   "SAS" = read_sas(file$datapath)
                                      )
                                      as.data.frame(data)}
                      )
  
  
  raw_data = reactive ({
    
    if (!input$submit5)
    {
    default_data()}
    # validate(
    #   need(is.null(input$loading)== FALSE, "Please select a file")
    # )
    else
      
    {eR9()} 
    
  }) 
  
  
  output$target_ui = renderUI({
    
    selectInput("targetvar", "Select your target variable",names(raw_data()),selected ="Class")
  })
  
  data = reactive ({
    # validate(
    #   need(is.null(input$loading) == FALSE, "Please select a file")
    # )  
    data = raw_data() 
    data[,input$targetvar] = as.factor(data[,input$targetvar])
    names(data)[which(names(data) == input$targetvar)] = "class"
    data
  })
  
  
  target_position = reactive({
    # validate(
    #   need(is.null(data()) == FALSE, "Please select a file")
    # )
    which(names(data()) == "class")
  })
  
  output$other_var_ui = renderUI({
    # validate(
    #   need(is.null(input$targetvar) == FALSE, "Please select a file")
    # )
    
    selectInput("other_var", "Select variables to withdraw from modelisation",names(data())[- target_position()],multiple = TRUE)
  })
  
  
  eR10 = eventReactive(input$submit6, data())
  output$data = renderDataTable(eR10() , options = list(scrollX = TRUE))
  
  
  eR11 = eventReactive(input$submit6,{data = as.matrix(dplyr::select_if(data(), is.numeric))
                                      corrplot(cor(data,use = "complete.obs"),tl.col="black")}
                       )
  
  output$plot3 = renderPlot({
    eR11()
    
  })
  
  eR12 = eventReactive(input$submit6,{ggplot(data=data()) + geom_bar(mapping = aes(x=class))})
  
  output$plot2 = renderPlot(
    
    eR12()
  )
  
  
  eR13 = eventReactive(input$submit6,  print(dfSummary(data(), graph.magnif = 0.8), 
                                             method = 'render',
                                             headings = FALSE,
                                             bootstrap.css = FALSE) )
  output$plot1 = renderUI(
    
    eR13()
  
  )
  
  
  eR14 = eventReactive(input$submit6,introduce(data()))
  output$sum1 = renderPrint( eR14() )
  
  eR15 = eventReactive(input$submit6, selectInput("boxplot","Choose one variable for BoxPlot",names(dplyr::select_if(data(), is.numeric   ))))
  
  output$boxplot_var = renderUI(
    eR15()
  )
  
  eR16 = eventReactive(input$submit6,{ ggplot(data = data()) + 
      geom_boxplot(mapping = aes(x = class, y = get(input$boxplot))) +
      labs(y = input$boxplot)})
  
  output$plot4 = renderPlot(
    eR16()
  )
  
  
  ###################### SAMPLING PART ##############################
  
  train = reactive({
    data_nrow = nrow(data())
    train_length = round(input$select_train*data_nrow)
    sample (data_nrow,train_length) #Index pour l'échantillon d'apprentissage, on peut le changer.
  })
  
  
  train_sample = reactive({
    var = setdiff(names(data()), input$other_var)
    data()[train(),var]
  }) #Echantillon apprentissage
  
  test_sample = reactive({
    var = setdiff(names(data()), input$other_var)
    data()[-train(), var]
  }) #Echantillon test
  
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
    
    dat = switch(input$select_sampling,
                "Oversampling" = oversampling(train_sample,p1,y_position) ,
                "Undersampling" = undersampling(train_sample,p2,y_position) ,
                "SMOTE" = {s = SMOTE(train_sample[,-y_position],y, dup_size=dup_size, K= k_near)$data 
                s$class = as.factor(s$class)
                s},
                "ADASYN" = {a = ADAS(train_sample[,-y_position],y, K=k_near)$data
                a$class = as.factor(a$class)
                a},
                "Tomek-Link" = {data = ubTomek(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                cbind(data$X,class=data$Y)},
                "Tomek-Link + Undersampling" = {data = ubTomek(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                newData = cbind(data$X,class=data$Y);
                undersampling(newData,p2 ,y_position)},
                "Condensed Nearest Neighbor" = {data = ubCNN(X=train_sample[,-y_position], Y= train_sample[,y_position]);
                cbind(data$X,class=data$Y) }
    )
  })

  eR8 = eventReactive(input$submit4,resampling_train())
  
  output$out = renderDataTable(eR8(), options = list(scrollX = TRUE)) 
  

    #################### SVM PART ########################
  
  
  output$choix_param = renderUI({
    
    switch(input$selection_auto,
           
           "No" = list(
             popify(el = selectInput(inputId = "cost",label = list("Cost parameter",icon("question-circle")), choices = c(2^-5,2^-3,2^-1,2^1,2^3,2^5,2^7,2^9,2^11,2^13,2^15)),
                    title="Cost parameter",
                    content="This hyper-parameter designates a coefficient of penalization parameter. The smaller it is, the less the classification errors are penalized and the focus is on maximizing the margin. The larger it is, the more emphasis is placed on the absence of misclassification and the margin becomes lower.",               placement = "top"),
             
             popify(el = selectInput(inputId = 'kernel', label = list('Kernel function',icon("question-circle")), choices = c('linear','sigmoid','radial','polynomial'), selected='linear'),
                    title = "<b>Kernel function</b>",
                    content="Functions that allow the projection in a space of greater dimension to find the adequate hyperplane without having to know explicitly this new space of representation.", 
                    placement="top")
            
           ),
           
           "Yes" = list(
             p('Cross-validation will be used to select the best model and hyper-parameters'),
             
             h5(strong("Choose the parameter range for the kernel functions :")),
             
             numericInput("tune_kfold",label = "Number of fold for Cross-validation", min = 2, max = 10, value = 4, step=2),
             
             popify(el = sliderInput(inputId = "auto_cost",label = list("Cost parameter",icon("question-circle")), min = 2^-5, max = 2^15, value=c(2^-3,2^5), step=2^3),
                    title="Cost parameter",
                    content="This hyper-parameter designates a coefficient of penalization parameter. The smaller it is, the less the classification errors are penalized and the focus is on maximizing the margin. The larger it is, the more emphasis is placed on the absence of misclassification and the margin becomes lower.",
                    placement = "top"),
             
             popify(el = sliderInput(inputId='auto_gamma', 
                                     label = list("Degree of linearity of the hyperplane",icon("question-circle")),
                                     min=2^-15, max=2^3, value=c(2^-3,2^1),step=2^-3),
                    title = "<b>Gamma</b>", 
                    content = "Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                    placement = "top"),
             
             popify(el = sliderInput(inputId='auto_degree',label = list("Polynomial degree",icon("question-circle")),
                                     min =1, max = 30, value=c(2,5),step=1),
                    title="<b>Degree</b>", 
                    content ="Corresponds to the degree of polynomial used by the kernel function to find the optimal hyperplane",
                    placement ="top")
            
           )
    )
  })
  
  
  result = reactive({
    
      if (input$selection_auto =="Yes")
        
      {
          t_l = tune(svm, class ~., data = resampling_train(), 
                     ranges = list(cost = seq(as.numeric(input$auto_cost[1]),as.numeric(input$auto_cost[2]),2^3), 
                                   kernel = "linear"),
                     tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold),
                     tolerance = 0.01)
          
          t_r = tune(svm, class ~., data = resampling_train(), 
                     ranges = list(gamma = seq(input$auto_gamma[1],input$auto_gamma[2], by=2^-3), 
                                   cost = seq(as.numeric(input$auto_cost[1]),as.numeric(input$auto_cost[2]),2^3), 
                                   kernel = "radial"),
                     tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold),
                     tolerance = 0.01)
          
          t_p = tune(svm, class ~., data = resampling_train(), 
                     ranges = list(degree = seq(input$auto_degree[1],input$auto_degree[2],by=1),
                                   gamma = seq(input$auto_gamma[1],input$auto_gamma[2], by=2^-3), 
                                   cost = seq(input$auto_cost[1],input$auto_cost[2],by=2^3), 
                                   kernel = "polynomial"),
                     tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold),
                     tolerance = 0.01)
          
          t_s = tune(svm, class ~., data = resampling_train(), 
                     ranges = list(gamma = seq(input$auto_gamma[1],input$auto_gamma[2], by=2^-3), 
                                   cost = seq(input$auto_cost[1],input$auto_cost[2],by=2^3), 
                                   kernel = "sigmoid"),
                     tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold),
                     tolerance = 0.01)
          
            
              data.frame(hyperparameter=c("Cost       ","Gamma      ","Degree     ","Performance", " ", "The best model","The Number of"),
                         Polynomial=c(round(t_p$best.parameters[,3],2),round(t_p$best.parameters[,2],2), round(t_p$best.parameters[,1],2), round(t_p$best.performance,2)," ","of all model is", "supports vectors:"),
                         Linear = c(round(t_s$best.parameters[,1],2),0,0,round(t_l$best.performance,2)," ", " ", " "),
                         Radial=c(round(t_r$best.parameters[,2],2), round(t_r$best.parameters[,1],2),0,round(t_r$best.performance,2)," "," "," "),
                         Sigmoid=c(round(t_s$best.parameters[,2],2),round(t_s$best.parameters[,1],2),0,round(t_s$best.performance,2)," "," "," "),
                         stringsAsFactors = FALSE
                         )
      }    
      
    })
  
  
    
    output$value2 = renderUI({
       
       if (input$selection_auto == "No")
         {
         
         switch (input$kernel,
                 
                 "sigmoid" = popify(el = selectInput("gamma_s",
                                                   label = list("Degree of linearity of the hyperplane",icon("question-circle")),
                                                   choices = c(2^-15,2^-13,2^-11,2^-9,2^-7,2^-5,2^-3,2^-1,2^1,2^3)),
                                    title = "<b>Gamma</b>", 
                                    content = "Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                                    placement = "top"),
                 
                 "polynomial" = list(
                                    popify(el = selectInput("gamma_p",label = list("Degree of linearity of the hyperplane",icon("question-circle")), 
                                                            choices = c(2^-15,2^-13,2^-11,2^-9,2^-7,2^-5,2^-3,2^-1,2^1,2^3)),
                                            title = "<b>Gamma</b>",
                                            content ="Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                                            placement = "top"),
                                     popify(el = sliderInput("degre_p",label = list("Polynomial degree",icon("question-circle")),
                                                              min =1, max = 30, value = 1),
                                            title="<b>Degree</b>", 
                                            content ="Corresponds to the degree of polynomial used by the kernel function to find the optimal hyperplane",
                                            placement ="top")
                                    ),
                 "radial" = popify(el = selectInput("gamma_rd",label = list("Degree of linearity of the hyperplane",icon("question-circle")), 
                                                    choices = c(2^-15,2^-13,2^-11,2^-9,2^-7,2^-5,2^-3,2^-1,2^1,2^3)),
                                         title = "<b>Gamma</b>",
                                         content = "Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                                         placement = "top")
         )
       
       }
    })
  
     
    gamma_degree = reactive(if (input$selection_auto == "No")
                            { 
                               try ({
                                 
                                 if (input$kernel == "sigmoid") { input$gamma_s } 
                                 
                                 else if (input$kernel == "polynomial") { x = c(input$gamma_p,input$degre_p) ; x }
                                 
                                 else if (input$kernel == "radial") { input$gamma_rd }
                                 
                                 }, silent = TRUE)
                            }
                          )
     
     
     model_svm = reactive({
       
       if (input$selection_auto == "Yes")
       {
         min_error_position = which.min(result()[4,2:5])+1
         
         best_parameter = result()[1:3,min_error_position]
         
         result_col_names = names(result())
         
         kernel = tolower(result_col_names[min_error_position])
         
         svm(class ~., data = resampling_train(),
             kernel = kernel, cost = best_parameter[1],
             gamma = best_parameter[2], degree = best_parameter[3],
             type = "C-classification", scale = input$scale)
       }
       else
       {
         gamma = gamma_degree()[1]
         degree = gamma_degree()[2] 
         
         if (input$kernel == "linear") 
         {
           svm(class ~., data = resampling_train(), 
               kernel = input$kernel, cost = input$cost, 
               type = "C-classification", scale = input$scale,
               tolerance = 0.01)
         }
         
         else if (input$kernel == "polynomial")
         {
           svm(class ~., data = resampling_train(),
               kernel = input$kernel, gamma = gamma , 
               degree = degree, cost = input$cost,
               type = "C-classification", scale = input$scale,
               tolerance = 0.01)
         }
         
         else  
         {
           svm(class ~., data = resampling_train(),
               kernel = input$kernel, gamma = gamma, cost = input$cost, 
               type = "C-classification", scale = input$scale,
               tolerance = 0.01)
         } 
       }
       
     })
     
     
     eR1 = eventReactive(input$submit1,   

       try(
        {
            if (input$selection_auto == "No")
           {
             gamma = gamma_degree()[1]
             degree = gamma_degree()[2] 
             
             if (input$kernel == "linear") 
             {
               list (paste ("Number of Support Vectors:", model_svm()$tot.nSV), 
                     paste ("cost:", model_svm()$cost))
             }
             
             else if (input$kernel == "polynomial")
             {
               list (paste ("Number of Support Vectors:", model_svm()$tot.nSV), 
                     paste ("cost:", model_svm()$cost),
                     paste ("Gamma:", model_svm()$gamma),
                     paste ("Degree:", model_svm()$degree))
             }
             
             else  
             {
               list (paste ("Number of Support Vectors:", model_svm()$tot.nSV), 
                     paste ("cost:", model_svm()$cost),
                     paste ("Gamma:", model_svm()$gamma))
             }
            }
          else
          {
                 best_mod =  names(result())[which.min(result()[4,2:5])+1]
                 result = result()
                 result[6,3] = best_mod 
                 result[7,3] = model_svm()$tot.nSV
                 result
              }
          
         } , silent = TRUE)
       
     )
     
     output$value = renderPrint(eR1())
     
     
     prediction_svm = reactive(try( predict(model_svm(), test_sample()), silent = TRUE))
     
     eR2 = eventReactive(input$submit1,
                         try(confusionMatrix (prediction_svm(), test_sample()$class, positive = "1"),
                             silent = TRUE)
                         ) 
                         
                         
     output$tab_confus = renderPrint(eR2())
  
        
     eR3 = eventReactive(input$submit1,
                        try({svm_roc = roc(as.numeric(test_sample()$class), as.numeric(prediction_svm()))
                         
                            svm_roc_plot = ggroc(svm_roc, legacy.axes = TRUE, alpha = 1, colour = "tomato", size = 1) + 
                              xlab("False Positive Rate") + ylab("True Positive Rate") + 
                              ggtitle("SVM ROC CURVE") +
                              geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
                              annotate("text", x = 0.9, y = 0.2, label = paste("AUC =",sprintf("%.3f",svm_roc$auc)),colour="tomato",size=3) +
                              annotate("text", x = 0.9, y = 0.15, label = paste("GINI =",sprintf("%.3f",2*svm_roc$auc-1)),colour="tomato",size=3)
                            svm_roc_plot}, silent = TRUE)
                         )
                         
      output$roc = renderPlot (eR3())
    
  #########################Benchmarking##########################
    
    output$method_param = renderUI ({
      
      if(is.null(data())) {return()}
      
      switch(input$methods,
             "Logistic regression" = sliderInput("threshold","classification Threshold",min =0, max = 1,value =0.5),
             "KNN" = sliderInput("knn","K-nearest",min = 1, max = 20,value = 4,step=1),
             "LDA" = sliderInput("threshold","classification Threshold",min =0, max = 1,value =0.5)
             ,"Classifications trees" = sliderInput("threshold","classification Threshold",min =0, max = 1,value =0.5),
             "Boosting" = list(
               sliderInput("threshold","classification Threshold",min =0, max = 1,value =0.5),
               sliderInput("tree","Number of trees to build",min =100, max = 500,value =100, step=50),
               sliderInput("shrink","Shrinkage to apply",min =0, max = 0.1,value =0.01, step=0.001)
             )
      )
      
    })
    
    n_target_position = reactive({
      # validate(
      #   need(is.null(resampling_train()) == FALSE, "Please select a file")
      # )
      which(names(resampling_train()) == "class")
      
    })
    
    
    
    
    ##LOGIT 
    
    logit.prob = reactive ({
      logit = glm(class~.,data = resampling_train(),family = binomial)
      logit.prob = predict(logit, newdata = test_sample(), type = "response")
    })
    
    logit.conf = reactive({
      p = ifelse(logit.prob()<input$threshold, "0", "1")
      confusionMatrix(as.factor(p), test_sample()$class, positive = "1")
    })
    
    
    ###KNN
    
    
    knn.conf = reactive({
      knn = knn (resampling_train()[,-(n_target_position())],
                 test_sample()[,-(n_target_position())],
                 resampling_train()[,n_target_position()],
                 k=input$knn)
      confusionMatrix(knn, test_sample()[,n_target_position()], positive = "1")
    })
    
    
    
    ####LDA 
    
    
    lda.prob = reactive ({
      lda = lda(class~.,data = resampling_train())
      lda.pred = predict(lda, newdata = test_sample())
      lda.pred$posterior[,"1"]
    })
    
    lda.conf = reactive ({
      p = ifelse(lda.prob()<input$threshold, "0", "1")
      confusionMatrix(as.factor(p), test_sample()$class, positive = "1")
    })
    
    
    ####Classification tree
    
    tree.prob = reactive({
      tree = tree(class~.,resampling_train())
      tree.class = predict (tree, newdata = test_sample())
      tree.class[,"1"]
    })
    
    tree.conf = reactive({
      p = ifelse(tree.prob()<input$threshold, "0", "1")
      confusionMatrix(as.factor(p), test_sample()$class, positive = "1")
    })
    
    
    ####Boosting
    
    boosting.prob =  reactive({
      boosting = gbm(as.character(class)~., data=resampling_train(), n.trees = input$tree, distribution = "bernoulli",shrinkage=input$shrink)
      predict(boosting, newdata = test_sample(),type ='response', n.trees = input$tree, shrinkage=input$shrink)
      
    })
    
    boosting.conf = reactive({
      p = ifelse(boosting.prob()<input$threshold, "0", "1")   
      confusionMatrix(as.factor(p), test_sample()$class, positive = "1")
    })
    
    
    
    ####Random Forest
    
    random.conf = reactive({
      random = randomForest(class~., resampling_train())
      random.class = predict (randomForest(class~., resampling_train()), test_sample(), type = "class")
      confusionMatrix(random.class, test_sample()$class, positive = "1")
    })
    

    bench_prediction = reactive({
      
      try(
        {
          switch(input$methods,
                 "Logistic regression" = ifelse(logit.prob()<input$threshold, "0", "1") ,
                 "KNN" = knn (resampling_train()[,-(n_target_position())],
                              test_sample()[,-(n_target_position())],
                              resampling_train()[,n_target_position()],
                              k=input$knn),
                 "LDA" = ifelse(lda.prob()<input$threshold, "0", "1"),
                 "Classifications trees" = ifelse(tree.prob()<input$threshold, "0", "1"),
                 "Boosting" = ifelse(boosting.prob()<input$threshold, "0", "1"),
                 "Random Forest" = predict (randomForest(class~., resampling_train()), test_sample(), type = "class")
          )
        }, silent = TRUE)
      
    })
    

    eR4 = eventReactive(input$submit2,
      try(
        {
          switch(input$methods,
                 "Logistic regression" = logit.conf(),
                 "KNN" = knn.conf(),
                 "LDA" = lda.conf(),
                 "Classifications trees" = tree.conf(),
                 "Boosting" = boosting.conf(),
                 "Random Forest" = random.conf()
          )
        }, silent = TRUE)
    )
    output$confusion = renderPrint(eR4())
    
    eR5 = eventReactive(input$submit2,
                        try(confusionMatrix (prediction_svm(), test_sample()$class, positive = "1"),
                            silent = TRUE))
    output$svm_perform = renderPrint(eR5()) 
    
    eR6 = eventReactive(input$submit2,
                        try(     
                          {svm_roc = roc(as.numeric(test_sample()$class), as.numeric(prediction_svm()))
                          bench_roc = roc(as.numeric(test_sample()$class), as.numeric(bench_prediction()))
                          
                          g <- ggroc(list(SVM=svm_roc, Selected_Method=bench_roc), size = 1, legacy.axes = TRUE)
                          g + xlab("False Positive Rate") + ylab("True Positive Rate") + 
                            geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") +
                            annotate("text", x = 0.8, y = 0.2, label = paste("AUC =",sprintf("%.3f",svm_roc$auc)), size=3, colour= "tomato2") +
                            annotate("text", x = 0.8, y = 0.15, label = paste("GINI =",sprintf("%.3f",2*svm_roc$auc-1)), size=3, colour= "tomato2") +
                            annotate("text", x = 0.8, y = 0.4, label = paste("AUC =",sprintf("%.3f", bench_roc$auc)), size=3, colour = "turquoise") + 
                            annotate("text", x = 0.8, y = 0.35, label = paste("GINI =",sprintf("%.3f",2*bench_roc$auc-1)), size=3, colour= "turquoise")}, silent = TRUE)
                )
    
    output$roc_curve = renderPlot(eR6())
     

  ###########################Prevision#############################
  

    k = reactive({
      
      l=list()
      
      
      boxlist = setdiff(names(resampling_train()),"class")
      
      n=length(boxlist)
      
      for (i in 1:n) {
        
        l[[i]]=box(numericInput(boxlist[i],"",value=0), width = 2,   
                   title = h5(boxlist[i], style = "display:inline; font-weight:bold"),
                   status = "primary")
      }
        l
    })
    
    output$box_pred = renderUI(
     
        try(k(), silent = TRUE)
    ) 

    output$pred = renderUI(
     
        box(verbatimTextOutput("targetpred"), 
            actionBttn("submit3","Submit",
                       color = "primary",
                       size = "xs",
                       style = "gradient",
                       icon = icon("refresh"),
                       block = FALSE,
                       no_outline=FALSE), width = 2,   
            title = h5("Prediction", style = "display:inline; font-weight:bold"),
            solidHeader = TRUE , status = "primary")
        )
    

      eR7 = eventReactive(input$submit3,
                          try(
                             
                            {Names = setdiff(names(resampling_train()),"class")
                            
                            n = length(Names)
                            
                            data = data.frame()
                            
                            for (i in 1:n)
                            {
                              data [1,i] = input[[Names[i]]]  
                            }
                            
                            names(data) = Names
                            
                            pred = predict(model_svm(), data)
                            
                            paste("Value :", as.numeric(pred))},
                            
                            silent = TRUE)
                          )
                         
      output$targetpred = renderPrint(eR7())
      

})

