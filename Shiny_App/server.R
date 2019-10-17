#Projet_SVM_AKS
#ACTIVER UTF8

#On doit permettre à l'utilisateur de faire entrer la variable target y
#######The minority class must be coded 1 and the majority 0 for the unbalanced family function ??? #####
####### We should imposed that the class have to be the last one of the data column and called "class" ?? #####
library (MASS )
library(class)
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
library(randomForest)
library (class)
library(tree)
library(gbm)
library(caret)
library(pROC)
library(ROCR)
library(shinyBS)

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
  
  ##################### Authentification ########################
  
  values <- reactiveValues(authenticated = FALSE)
  
  dataModal1 <- function() {
    modalDialog(
      helpText((strong(h3("Authentification"))),br()),
      wellPanel(textInput("username", "Username :"),
                passwordInput("password", "Password :")
                
      ),
      footer = actionButton("ok", "LogIn"),
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
        # c = paste("You are authentified as", nameE)
        # background-color:powderblue;
        div(nameE,style = "color:rgb(180,85,85);font-family: verdana;
            font-size: 150%; border: 1px solid powderblue;
            padding: 5px;")
      }
      
      
      
      }
  )
  
  #output$authent1 = renderText(
  
  # "  Fully enjoy our app !" )
  
  
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
  
  
  output$data = renderDataTable(data() , options = list(scrollX = TRUE))
  
  
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
    
    dat= switch(input$select_sampling,
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
  
  output$out = renderDataTable(resampling_train(), options = list(scrollX = TRUE)) 
  
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
    validate(
      need(is.null(resampling_train()) == FALSE, "Please select a file")
    )
    which(names(resampling_train()) == "class")
    
  })
  
  
  ##LOGIT courbe roc
  
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
    confusionMatrix(test_sample()[,n_target_position()], knn, positive = "1")
  })
  
  
  
  ####LDA courbe ROC à faire
  
  
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
  
  
  ####Boosting courbe ROC
  
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
    random.class = predict (random, test_sample(), type = "class")
    confusionMatrix(random.class, test_sample()$class, positive = "1")
  })
  
  
  
  ####
  
  output$confusion = renderPrint({
    
    switch(input$methods,
           "Logistic regression" = logit.conf(),
           "KNN" = knn.conf(),
           "LDA" = lda.conf(),
           "QDA" = qda.conf(),
           "Classifications trees" = tree.conf(),
           "Boosting" = boosting.conf(),
           "Random Forest" = random.conf()
    )
  })
  
  
  output$svm_perform = renderPrint({
    
    
  }) #A revoir................
  
  
  
  ####################SVM PART ################################
  
  
  output$choix_param = renderUI({
    
    switch(input$selection_auto,
           
           "No" = list(
             popify(el = sliderInput(inputId = "cost",label = list("Cost parameter",icon("question-circle")), min = 1, max = 1000, value = 10),
                    title="Cost parameter",
                    content="This hyper-parameter designates a coefficient of penalization parameter. The smaller it is, the less the classification errors are penalized and the focus is on maximizing the margin. The larger it is, the more emphasis is placed on the absence of misclassification and the margin becomes lower.",               placement = "top"),
             
             popify(el = selectInput(inputId = 'kernel', label = list('Kernel function',icon("question-circle")), choices = c('linear','sigmoid','radial basis','polynomial'), selected='linear'),
                    title = "<b>Kernel function</b>",
                    content="Functions that allow the projection in a space of greater dimension to find the adequate hyperplane without having to know explicitly this new space of representation.", 
                    placement="top")
             # ,
             # br(), br(),
             # 
             # actionButton("sumbit1", "Submit")
             
           ),
           
           "Yes" = list(
             p('Cross-validation will be used to select the best model and hyper-parameters'),
             
             h5(strong("Choose the parameter range for the kernel functions :")),
             
             numericInput("tune_kfold",label = "Number of fold for Cross-validation", min = 0, max = 10, value = 5, step=1),
             
             popify(el = sliderInput(inputId = "auto_cost",label = list("Cost parameter",icon("question-circle")), min = 1, max = 1000, value=c(1,5)),
                    title="Cost parameter",
                    content="This hyper-parameter designates a coefficient of penalization parameter. The smaller it is, the less the classification errors are penalized and the focus is on maximizing the margin. The larger it is, the more emphasis is placed on the absence of misclassification and the margin becomes lower.",
                    placement = "top"),
             
             popify(el = sliderInput(inputId='auto_gamma', 
                                     label = list("Degree of linearity of the hyperplane",icon("question-circle")),
                                     min=0, max=10, value=c(2,3),step=0.01),
                    title = "<b>Gamma</b>", 
                    content = "Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                    placement = "top"),
             
             popify(el = sliderInput(inputId='auto_degree',label = list("Polynomial degree",icon("question-circle")),
                                     min =1, max = 30, value=c(2,5),step=1),
                    title="<b>Degree</b>", 
                    content ="Corresponds to the degree of polynomial used by the kernel function to find the optimal hyperplane",
                    placement ="top")
             # ,
             # br(), br(),
             # 
             # actionButton("sumbit2", "Submit"),
             # br(), br()
             
           )
    )
  })
  
    
    output$best_model = renderPrint({
      
      if (input$selection_auto =="Yes")
        
      {
        t_l = tune(svm, class ~., data = resampling_train(), 
                     ranges = list(cost = seq(as.numeric(input$auto_cost[1]),as.numeric(input$auto_cost[2]),5), 
                                   kernel = "linear"),
               tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold))
        
        t_r = tune(svm, class ~ . , data = resampling_train(), 
                   ranges = list(gamma = seq(input$auto_gamma[1],input$auto_gamma[2], by=0.1), 
                                 cost = seq(as.numeric(input$auto_cost[1]),as.numeric(input$auto_cost[2]),5), 
                                 kernel = "radial"),
                   tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold))
        
        t_p = tune(svm, class ~ . , data = resampling_train(), 
                              ranges = list(degree = seq(input$auto_degree[1],input$auto_degree[2],by=1),
                                            gamma = seq(input$auto_gamma[1],input$auto_gamma[2], by=0.1), 
                                            cost = seq(input$auto_cost[1],input$auto_cost[2],by=5), 
                                            kernel = "polynomial"),
                              tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold))
        
        t_s = tune(svm, class ~ . , data = resampling_train(), 
                       ranges = list(gamma = seq(input$auto_gamma[1],input$auto_gamma[2], by=0.1), 
                                      cost = seq(input$auto_cost[1],input$auto_cost[2],by=5), 
                                      kernel = "sigmoid"),
                        tunecontrol = tune.control(sampling = "cross", cross = input$tune_kfold))
        
        
        
                perf = c(Radial_Basis = t_r$best.performance, Linear = t_l$best.performance,Polynomial = t_p$best.performance, Sigmoid = t_s$best.performance)
                
                
                best_mod = paste("The best model of all best models is,", names(perf)[which.min(perf)])
                
                result=data.frame(hyperparameter=c("Cost","Gamma","Degree","Performance"),
                                  Polynomial=c(round(t_p$best.parameters[,3],2),round(t_p$best.parameters[,2],2), round(t_p$best.parameters[,1],2), round(t_p$best.performance,2)),
                                  Radial_Basis=c(round(t_r$best.parameters[,2],2), round(t_r$best.parameters[,1],2),0,round(t_r$best.performance,2)),
                                  Sigmoid=c(round(t_s$best.parameters[,2],2),round(t_s$best.parameters[,1],2),0,round(t_s$best.performance,2)),
                                  Linear = c(round(t_s$best.parameters[,1],2),0,0,round(t_l$best.performance,2)))
                
                list("The best hyper-parameters and performance for each method", result, best_mod)
                
                
        }})
    
  
     output$value2 = renderUI({
       
       if (input$selection_auto == "No"){
         
         switch (input$kernel,
                 
                 "sigmoid" = popify(el = sliderInput("gamma_s",
                                                   label = list("Degree of linearity of the hyperplane",icon("question-circle")),
                                                   min = 0.001, max = 10, value = 0.1),
                                    title = "<b>Gamma</b>", 
                                    content = "Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                                    placement = "top"),
                 
                 "polynomial" = list(
                                    popify(el = sliderInput("gamma_p",label = list("Degree of linearity of the hyperplane",icon("question-circle")), 
                                                          min = 0.001, max = 10, value = 0.1),
                                            title = "<b>Gamma</b>",
                                            content ="Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                                            placement = "top"),
                                     popify(el = sliderInput("degre_p",label = list("Polynomial degree",icon("question-circle")),
                                                              min =1, max = 30, value = 1),
                                            title="<b>Degree</b>", 
                                            content ="Corresponds to the degree of polynomial used by the kernel function to find the optimal hyperplane",
                                            placement ="top")
                                    ),
                 "radial basis" = popify(el = sliderInput("gamma_rd",label = list("Degree of linearity of the hyperplane",icon("question-circle")), 
                                                          min = 0.001, max = 10, value = 0.1),
                                         title = "<b>Gamma</b>",
                                         content = "Hyper-parameter for nonlinear hyperplanes. The higher the value, the more the method adjusts to the data.",
                                         placement = "top")
         )
       
       }
    })
  
  #   param_gamma = reactive(try(if(input$kernel == "sigmoid"){input$gamma_s} 
  #                              else{if(input$kernel == "polynomial"){input$gamma_p} else if(input$kernel == "radial basis"){input$gamma_rd}}))
  #   
  #   degree = reactive(input$degre_p)
  #   
  #   
  #   # modele svm si choix manuel
  #   
  #   output$value = renderPrint({
  #     if(input$kernel == "linear"){method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, cost=input$cost, type="C-classification", scale=input$scale))}
  #     else if (input$kernel == "polynomial"){method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, gamma = param_gamma(), degree = degree(), cost=input$cost, type="C-classification", scale=input$scale))}
  #     else {method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, gamma = param_gamma(), cost=input$cost, type="C-classification", scale=input$scale))}
  #     
  #     method()
  #     
  #   })
  #   
  #   output$tab_confus = renderPrint({
  #     if(input$kernel == "linear"){method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, cost=input$cost, type="C-classification", scale=input$scale))}
  #     else if (input$kernel == "polynomial"){method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, gamma = param_gamma(), degree = degree(), cost=input$cost, type="C-classification", scale=input$scale))}
  #     else {method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, gamma = param_gamma(), cost=input$cost, type="C-classification", scale=input$scale))}
  #     
  #     #svm_method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, cost=input$cost, scale=input$scale, type="C-classification"))  
  #     pred=reactive(predict(method(), test_sample()))
  #     Conf(pred() ,test_sample()$class)
  #     
  #     
  #   })
  #   
  #   output$roc = renderPlot ({
  #     
  #     if(input$kernel == "linear"){method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, cost=input$cost, type="C-classification", scale=input$scale))}
  #     else if (input$kernel == "polynomial"){method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, gamma = param_gamma(), degree = degree(), cost=input$cost, type="C-classification", scale=input$scale))}
  #     else {method = reactive(svm(class ~ . , data=resampling_train(), kernel=input$kernel, gamma = param_gamma(), cost=input$cost, type="C-classification", scale=input$scale))}
  #     
  #     
  #     pred=reactive(predict(method(), test_sample()))
  #     predic <- prediction(predictions = as.numeric(pred()), labels = as.numeric(test_sample()$class))
  #     perf <- performance(predic, measure = "tpr", x.measure = "fpr")
  #     plot(perf, col=rainbow(10))
  #     
  #     # rocobj <- roc(response = as.numeric(test_sample()$class), predictor = as.numeric(pred))
  #     # plot.roc(rocobj,main="pROC")
  #     
  #   })
  #   
  #   
  #   # modele svm si choix auto
  #   
  #   
  #   resampling_train2 = reactive({
  #     n_validat = reactive(floor(input$p_validation * nrow(resampling_train())))
  #     train2 = reactive(sample(1:nrow(resampling_train()),n_validat()))
  #     resampling_train()[train2(),]
  #   })
  #   
  #   resampling_validat = reactive({
  #     n_validat = reactive(floor(input$p_validation * nrow(resampling_train())))
  #     train2 = reactive(sample(1:nrow(resampling_train()),n_validat()))
  #     resampling_train()[-train2(),]
  #   })
  
  
  
  
  
  ###########################Prevision#############################
  # 
  # k=reactive({
  #   
  #   l=list()
  #   
  #   
  #   
  #   boxlist = setdiff(names(resampling_train()),"class")
  #   
  #   n=length(boxlist)
  #   
  #   for (i in 1:n) {
  #     
  #     x = paste("num",i, sep="") 
  #     l[[i]]=box(numericInput(x,"",value=NA), width = 3, background = "navy",  
  #                title = h5(boxlist[i], style = "display:inline; font-weight:bold"),
  #                solidHeader = TRUE ,status = "primary",collapsible = TRUE)
  #   }
  #   l
  # })
  # 
  # output$pred = renderUI(k())
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})

