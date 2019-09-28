#Projet_SVM_AKS
library(shiny)
library(shinydashboard)
library(DescTools)

shinyUI(
  
  dashboardPage(skin="red", 
               
    dashboardHeader(
      title = "FRAUD DETECTION",
      dropdownMenu(type = "notifications", notificationItem(
        text = "Download notice downside",
        icon("bell"),
        status = "info"
      ))
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home",icon =icon("home")),
        menuItem("Data", tabName = "data",icon =icon("database")),
        menuItem("Model", icon =icon("th"),
                 menuSubItem("Sampling", tabName = "sample"),
                 menuSubItem("SVM method", tabName = "svm"),
                 menuSubItem("Benchmarking analysis", tabName = "benchmark")),
        menuItem("Prediction", tabName = "prediction", icon = icon("dashboard"))
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "home",
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
                    
                    title="Data description from MLG kaggle page",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12)
          
        ),
        tabItem(tabName = "data",
                
                box(fileInput("loading",""),background = "navy", title="Load data",width = 3, solidHeader = TRUE, status = "primary" ),
                
                
                tabBox (width = 9,
                    title="Statistics and graphics", id = "stat",
                    tabPanel("Summary",
                             plotOutput("plot1")),
                    tabPanel("Visualisation","MISE EN PLACE DE GRAPHIQUES DYNAMIQUES OU STATIQUES AVEC PLOTLY ET GGPLOT")
                  )
                
                
        ),
        tabItem(tabName = "svm" , 
                
                tabBox (width=12,
                        title="SVM", id = "svm",
                        tabPanel("Description","ICI NOUS ALLONS DECRIRE LA METHODE SVM"),
                        tabPanel("Implementation","IMPLEMENTATION DU SVM")
                )
                
        ),
        tabItem(tabName = "benchmark",
                
                tabBox (
                  title="Comparison SVM with others Methods", id = "comparison",
                  selectInput("methods","Choose Methods",c("Logistic regression","KNN","K-means","Boosting", "Random Forest", "Bagging") )
                )
                
        ),
        tabItem(tabName = "prediction",
                selectInput("select_predict","Please Choose one way !",c("Load external data", "Enter data"))
                
                
        ),
        tabItem(tabName = "sample",
                numericInput("select_train","Training proportion",min=0, max=1, value=0.7, step=0.01),
                selectInput("select_sampling","Choose the method",c("Oversampling", "Undersampling", "SMOTE", "ADASYN", "Tomek-Link", "Tomek-Link + Undersampling","Condensed Nearest Neighbor" )),
                uiOutput("sampling_out"),
                dataTableOutput("out")
       
        )
      )
      
    )
    
  )
  
)