#Projet_SVM_AKS

library(e1071)
library (MASS)
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
library(shinydashboardPlus)
library(shinydashboard)
library(randomForest)
library(tree)
library(gbm)
library(caret)
library(pROC)
library(shinyBS)
library(shinyWidgets)



shinyUI(
  
  dashboardPagePlus(
    header = dashboardHeaderPlus(
      title = "FRAUD DETECTION",
      dropdownMenu(type = "notifications", notificationItem(
        text = "Download notice downside",
        icon("bell"),
        status = "info"
      )),
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears"
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home",icon =icon("home")),
        menuItem("Data", tabName = "data",icon =icon("database")),
        menuItem("Model", icon =icon("th"),
                 menuSubItem("Sampling", tabName = "sample"),
                 menuSubItem("SVM method", tabName = "svm"),
                 menuSubItem("Benchmarking analysis", tabName = "benchmark")),
        menuItem("Prediction", tabName = "prediction", icon = icon("line-chart"))
      )),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "home",
                fluidPage(
                  box(h3("Welcome to the credit card fraud detection app !", align="center"), width=12, background = "navy"),
                  
                  box(h5(p("This app allows you to detect a case of credit card fraud, quickly and easily by mainly using", em(strong("Supports Vectors Machines.")), "By default, we use data from Machine Learning Group (MLG) of Brussels University.")), width=12),
                  
                  box(h3(strong("Context")),
                      p("It is important that credit card companies are able to recognize fraudulent credit card transactions so that customers are not charged for items that they did not purchase."),
                      h3(strong("Content")),
                      p("The datasets contains transactions made by credit cards in September 2013 by european cardholders. This dataset presents transactions that occurred in two days, where we have 492 frauds out of 284,807 transactions. The dataset is highly unbalanced, the positive class (frauds) account for 0.172% of all transactions."),
                      p("It contains only numerical input variables which are the result of a PCA transformation. Unfortunately, due to confidentiality issues, we cannot provide the original features and more background information about the data. Features V1, V2, ... V28 are the principal components obtained with PCA, the only features which have not been transformed with PCA are 'Time' and 'Amount'. Feature 'Time' contains the seconds elapsed between each transaction and the first transaction in the dataset. The feature 'Amount' is the transaction Amount, this feature can be used for example-dependant cost-senstive learning. Feature 'Class' is the response variable and it takes value 1 in case of fraud and 0 otherwise."),
                      h3(strong("Inspiration")),
                      p("Identify fraudulent credit card transactions."),
                      p("Given the class imbalance ratio, we recommend measuring the accuracy using the Area Under the Precision-Recall Curve (AUPRC). Confusion matrix accuracy is not meaningful for unbalanced classification."),
                      h3(strong("Acknowledgements")),
                      p("The dataset has been collected and analysed during a research collaboration of Worldline and the Machine Learning Group (http://mlg.ulb.ac.be) of ULB (Université Libre de Bruxelles) on big data mining and fraud detection. More details on current and past projects on related topics are available on https://www.researchgate.net/project/Fraud-detection-5 and the page of the DefeatFraud project"),
                      p("Please cite the following works:"),
                      p("Andrea Dal Pozzolo, Olivier Caelen, Reid A. Johnson and Gianluca Bontempi. Calibrating Probability with Undersampling for Unbalanced Classification. In Symposium on Computational Intelligence and Data Mining (CIDM), IEEE, 2015"),
                      p("Dal Pozzolo, Andrea; Caelen, Olivier; Le Borgne, Yann-Ael; Waterschoot, Serge; Bontempi, Gianluca. Learned lessons in credit card fraud detection from a practitioner perspective, Expert systems with applications,41,10,4915-4928,2014, Pergamon"),
                      p("Dal Pozzolo, Andrea; Boracchi, Giacomo; Caelen, Olivier; Alippi, Cesare; Bontempi, Gianluca. Credit card fraud detection: a realistic modeling and a novel learning strategy, IEEE transactions on neural networks and learning systems,29,8,3784-3797,2018,IEEE"),
                      p("Dal Pozzolo, Andrea Adaptive Machine learning for credit card fraud detection ULB MLG PhD thesis (supervised by G. Bontempi"),
                      p("Carcillo, Fabrizio; Dal Pozzolo, Andrea; Le Borgne, Yann-Aël; Caelen, Olivier; Mazzer, Yannis; Bontempi, Gianluca. Scarff: a scalable framework for streaming credit card fraud detection with Spark, Information fusion,41, 182-194,2018,Elsevier"),
                      p("Carcillo, Fabrizio; Le Borgne, Yann-Aël; Caelen, Olivier; Bontempi, Gianluca. Streaming active learning strategies for real-life credit card fraud detection: assessment and visualization, International Journal of Data Science and Analytics, 5,4,285-300,2018,Springer International Publishing"),
                      p("Bertrand Lebichot, Yann-Aël Le Borgne, Liyun He, Frederic Oblé, Gianluca Bontempi Deep-Learning Domain Adaptation Techniques for Credit Cards Fraud Detection, INNSBDDL 2019: Recent Advances in Big Data and Deep Learning, pp 78-88, 2019"),
                      p("Fabrizio Carcillo, Yann-Aël Le Borgne, Olivier Caelen, Frederic Oblé, Gianluca Bontempi Combining Unsupervised and Supervised Learning in Credit Card Fraud Detection Information Sciences, 2019"),
                      
                      title="Data description from MLG kaggle page",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12))
                
        ),
        tabItem(tabName = "data",
                fluidPage(
                   box( 
                    box(selectInput("type_file", "Type of file", c("CSV","TXT","EXCEL","SAS")), 
                        fileInput("loading",""),
                        uiOutput("file_options"),
                        background = "navy", title="Load data",width = 4, solidHeader = TRUE, status = "primary",collapsible = TRUE ),
                    
                    box(uiOutput("target_ui"),
                        background = "navy",title="Target Variable",width= 4, solidHeader = TRUE, status = "primary",collapsible = TRUE, footer = helpText("Make sure that you a choose categorical variable in order to avoid errors !")),
                    
                    box(uiOutput("other_var_ui"),
                        background = "navy",width= 4,title="Others Variables", solidHeader = TRUE, status = "primary",collapsible = TRUE, footer = helpText("You can't remove all variables ! These variables will be removed only from the model and prediction part.")),
                    
                    tabBox (width = 12,title="Statistics and graphics", id = "stat",
                            tabPanel("Data",
                                     dataTableOutput("data")
                            ),
                            tabPanel("Summary",
                                     verbatimTextOutput("sum1"),
                                     htmlOutput("plot1")
                            ),
                            tabPanel("Graphics",
                                     fluidRow(column(width=6,
                                                     box(plotOutput("plot2"),
                                                         background = "navy", width = 12,title="Target Variable Barplot", solidHeader = TRUE,status = "primary",collapsible = TRUE)),
                                              column(width=6,
                                                     box(plotOutput("plot3"),
                                                         background = "navy",width = 12, title="Correlation Matrix",solidHeader = TRUE ,status = "primary",collapsible = TRUE))
                                     ),
                                     fluidRow(column(width=2),
                                              column(width=10,
                                                     box(column(width=3 ,uiOutput("boxplot_var")),
                                                         column(width=9 , plotOutput("plot4")),
                                                         background = "navy", width = 10,title="BoxPlot",solidHeader = TRUE,status = "primary",collapsible = TRUE)),
                                              column(width=2)
                                              
                                     )
                            )
                    )
                    , width = 12))      
        ),
        tabItem(tabName = "svm" , 
                
                fluidPage(tabBox (width=12,
                                  title="SVM", id = "svm",
                                  tabPanel("Description",
                                           sidebarLayout(
                                             div(class="coin",box(
                                               
                                               fluidRow(
                                                 column(width = 4,div(class="coin",img(src='logo3.jpg', height = 260 ,width = 400),style="text-align: left;")),
                                                 
                                                 column(width=8,box(p("It is interesting to understand machine learning in order to better understand its importance and better exploit it."), 
                                                                    p("Machine Learning is based on mathematical reasoning, which is computer-generated in algorithms capable of digesting large amounts of informations to acquire new knowledge or to understand behavior."),
                                                                    p("Its principle is to be able to learn independently from these data and to evolve recursively continuously."),
                                                                    p("It is much more efficient, applied to large sets of varied data, than traditional methods in terms of accuracy and speed."), 
                                                                    p("There are a plethora of areas in which machine learning intervenes, namely in bank,in finance etc. Thus, in bank, based on information associated with a transaction such as amount and location, and historical and social data, Machine Learning can help to detect potential credit card fraud in record time.")
                                                                    ,width=12))
                                                 
                                               ),
                                               title="Machine learning : definition and principle",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12)),
                                             
                                             box(h3(strong("Understanding SVM")),
                                                 box(
                                                   p("SVMs are a family of machine learning algorithms that solve both classification and regression problems."),
                                                   p("Supports vectors machines are based on two key ideas: the notion of maximum margin and the kernel trick.", strong("The margin"), "is two times the distance between the hyperplane separator and the closests observations (called supports vectors). Then Support vectors correspond to observations on the margin. In the SVM, the hyperplane separator is chosen as the one that maximizes the margin."),
                                                   p("The second idea of this algorithm,", strong("kernel trick"),",allows to deal with cases where the data are not linearly separable. It is a question of transforming the representation space of the input data into a space of larger dimension (or better dimension), in which it is probable that there is a linear separation. This is achieved through a kernel function (whose explicit knowledge is not necessary) that must meet some conditions."),
                                                   width=12),
                                                 
                                                 h3(strong("SVM formalization")),
                                                 box(
                                                   p("We search for the classifier",em("g"),"of the form:"),
                                                   div(class="coin",img(src='logo11.png', height = 100 ,width = 400)),
                                                   p("The formulation of optimization programs for support vector machines differs according to whether the sample is linearly separable, almost linearly separable (soft margin: introduction of spring variables) or not linearly separable (kernel trick):"),
                                                   fluidRow(
                                                     column(4, div(class="coin",img(src='logo9.png', height = 200 ,width = 300)), strong("linearly separable sample")),
                                                     column(4, div(class="coin",img(src='logo8.png', height = 200 ,width = 300)), strong("almost linearly separable sample")),
                                                     column(4, div(class="coin",img(src='logo7.png', height = 200 ,width = 300)), strong("non-linearly separable sample"))
                                                   ),
                                                   br(),
                                                   
                                                   p("In practice, most classification problems are nonlinear separations, hence the use of kernel trick to make them linearly separable."),
                                                   p("In this context, the optimization program is thus given by:"),
                                                   br(),
                                                   div(class="coin",img(src='logo6.png', height = 200 ,width = 400),style="text-align: center;"),
                                                   br(),
                                                   p("At this level, several parameters intervene:"),
                                                   p(strong("C") ,": controls the arbitration between the margin dimension and the error rate and whose selection can be made by cross validation"),
                                                   p(strong("ξ"), ": spring variables that control the number of errors to allow"),
                                                   p("Also appears the function of transforming the data space into a larger space,", strong("ϕ(.)")),
                                                   
                                                   width = 12),
                                                 
                                                 box(h3(strong("Advantages of supports vectors machines")),
                                                     br(),
                                                     fluidRow(
                                                       tags$head(tags$style(HTML(".small-box {height: 130px}"))),
                                                       valueBox("Robust...", "on small sample", icon = icon("thumbs-up"),color = "light-blue"),
                                                       valueBox("Good...", "predictive ability when hyperparameters are well chosen", icon = icon("thumbs-up"), color = "light-blue"),
                                                       valueBox("Flexibility...", "of the method that adapts according to the nature of the data", icon = icon("thumbs-up"), color = "light-blue")
                                                     ),  width = 12),
                                                 box(h3(strong("Drawbacks of supports vectors machines")),
                                                     br(),
                                                     fluidRow(
                                                       
                                                       valueBox("Difficulty...", "in identifying the correct values of the hyperparameters and good kernel", icon = icon("thumbs-down"), color = "red"),
                                                       valueBox("High cost...", " in computation time on large databases", icon = icon("thumbs-down"),color = "red"),
                                                       valueBox("Interpretability...", "of the final model is not easy - variable weights and individual impact", icon = icon("thumbs-down"), color = "red")
                                                     ),  width = 12),
                                                 
                                                 
                                                 title="Supports vectors machines",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12)
                                           )),
                                  tabPanel("Implementation and Performance",
                                           sidebarLayout(
                                             box(
                                               h4(strong("Hyper-parameters choices")),
                                               box(
                                                 checkboxInput(inputId = 'scale', label = 'Center and reduce the variables',value=TRUE),
                                                 radioButtons(inputId = 'selection_auto', label = 'Do you want to use automatic parameter selection with cross-validation?',choices=c('Yes', 'No'),inline=TRUE),
                                                 bsPopover(id="selection_auto",title="Cross validation", content="If yes, optimization of the choice of hyper-parameters in order to avoid over-learning, cross validation will be used to find the best model and hyper-parameters.",placement="left"),
                                                 uiOutput("choix_param"),
                                                 uiOutput("value2"),
                                                 actionBttn("submit1","Submit",
                                                            color = "primary",
                                                            size = "xs",
                                                            style = "gradient",
                                                            icon = icon("refresh"),
                                                            block = FALSE,
                                                            no_outline=FALSE), width = 12),
                                               br(),br(),br(),
                                               h4(strong("Summary")),
                                               box(verbatimTextOutput("value"), width = 12),

                                               title="Implementation",status = "primary", solidHeader = TRUE, width = 6),
                                             box(
                                                 verbatimTextOutput("tab_confus"),
                                                 br(),
                                                 plotOutput("roc"),
                                                 title="Performance",status = "primary", solidHeader = TRUE, width = 6)
                                           )
                                  )  
                ))),
        tabItem(tabName = "benchmark",
                fluidPage(
                   box (sidebarLayout( 
                     sidebarPanel(selectInput("methods","Choose Methods",c("Logistic regression","KNN","LDA","Classifications trees","Boosting", "Random Forest")),
                                  uiOutput("method_param"),
                                  actionBttn("submit2","Submit",
                                             color = "primary",
                                             size = "xs",
                                             style = "gradient",
                                             icon = icon("refresh"),
                                             block = FALSE,
                                             no_outline=FALSE), width = 4),
                     mainPanel(" ",width = 8)
                     
                   ),
                   fluidRow(
                     column(width=4, box(verbatimTextOutput("confusion"),
                                         background = "navy",width = 12, title="Selected Method Performance",solidHeader = TRUE , status = "primary",collapsible = TRUE)),
                     column(width=4, box(verbatimTextOutput("svm_perform"), background = "navy", width = 12, title="SVM Performance", solidHeader = TRUE , status = "primary", collapsible = TRUE)),
                     column(width=4, box(plotOutput("roc_curve"), background = "navy", width = 12, title="Roc Curve", solidHeader = TRUE ,status = "primary",collapsible = TRUE))
                   ), width=12, title="SVM Comparison with others Methods"
                ))),
        
        
        tabItem(tabName = "prediction",
                fluidPage(
                  box(
                   fluidRow(column(width=12,
                                   uiOutput("box_pred"),
                                   uiOutput("pred")))
                   , width = 12))),
        
        
        tabItem(tabName = "sample",
                fluidPage(
                  box(
                    fluidRow(
                      sidebarPanel(
                        numericInput("select_train","Training proportion choice",min=0, max=1, value=0.7, step=0.01),
                        selectInput("select_sampling","Choose the method",c("Oversampling", "Undersampling", "SMOTE", "ADASYN", "Tomek-Link", "Tomek-Link + Undersampling","Condensed Nearest Neighbor")),
                    uiOutput("sampling_out"),
                    actionBttn("submit4","Submit",
                               color = "primary",
                               size = "xs",
                               style = "gradient",
                               icon = icon("refresh"),
                               block = FALSE,
                               no_outline=FALSE)
                      ),
                      sidebarPanel(
                        helpText(strong("This sampling part allows you to choose the way by which data will be splitted.
                         In general, 70% (or 80%) of the whole sample is used for the training sample and 30% (or 20%) for the test sample. In our case, the choice is given to user to opt for the subdivision he wants even if a 70/30 splitting is advised.")), helpText(strong("It is also possible to use the resampling method for the class whose realization is rare (credit card fraud).")) , width = 5)
                    ),
                    box(h3(strong("Resampling data")),
                        br(),
                        dataTableOutput("out"), width = 12)
          , width = 12)))
        
      )
    ),
    rightsidebar = rightSidebar(titlePanel(h4("Connected")), helpText(br(),h4(strong("You are authentified as"))),htmlOutput("authent")),
    footer = dashboardFooter(
      left_text = h6("© GROUP AKS OF MASTER ESA - Any reproduction, even partial, of the page is strictly forbidden"),
      right_text = h6("ORLEANS 2019")
    ),
    
    enable_preloader = TRUE,
    #collapse_sidebar = TRUE,
    sidebar_background ="light",
    loading_duration = 0.5
    #md =TRUE
    # dashboardControlbar()
    
  )
)





