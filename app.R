##### READ DATASET ##### 
library(readr)
Dataset<-read.csv("diabetes.csv")



##### DATA PREPROCESSING #####

# Remove Unwanted Features
diabetes_subset <- subset(Dataset, select = -c(Fruits, Veggies, Sex, AnyHealthcare, NoDocbcCost))

# Select same number of 0, 1, 2 rows in Diabetes_012, and making it smaller
num_rows <- min(table(diabetes_subset$Diabetes_012))
subset_0 <- subset(diabetes_subset, Diabetes_012 == 0)[1:num_rows, ]
subset_1 <- subset(diabetes_subset, Diabetes_012 == 1)[1:num_rows, ]
subset_2 <- subset(diabetes_subset, Diabetes_012 == 2)[1:num_rows, ]
result <- rbind(subset_0, subset_1, subset_2)

# Remove more features for linear model
training_set <- subset(result, select = -c(Income, Age, Education, CholCheck, PhysActivity))

##### DATA VISUALIZATION #####

# Measure Correlation between diabetes and feature
diabetes_vs_BMI <- (cor.test(result$Diabetes_012, result$BMI, method = "pearson"))$estimate
diabetes_vs_Smoker <- (cor.test(result$Diabetes_012, result$Smoker, method = "pearson"))$estimate
diabetes_vs_HighBP <- (cor.test(result$Diabetes_012, result$HighBP, method = "pearson"))$estimate
diabetes_vs_HighChol <- (cor.test(result$Diabetes_012, result$HighChol, method = "pearson"))$estimate
diabetes_vs_Stroke <- (cor.test(result$Diabetes_012, result$Stroke, method = "pearson"))$estimate
diabetes_vs_HeartDiseaseorAttack  <- (cor.test(result$Diabetes_012, result$HeartDiseaseorAttack , method = "pearson"))$estimate
diabetes_vs_HvyAlcoholConsump  <- (cor.test(result$Diabetes_012, result$HvyAlcoholConsump , method = "pearson"))$estimate
diabetes_vs_GenHlth  <- (cor.test(result$Diabetes_012, result$GenHlth , method = "pearson"))$estimate
diabetes_vs_MentHlth  <- (cor.test(result$Diabetes_012, result$MentHlth , method = "pearson"))$estimate
diabetes_vs_PhysHlth  <- (cor.test(result$Diabetes_012, result$PhysHlth , method = "pearson"))$estimate
diabetes_vs_DiffWalk   <- (cor.test(result$Diabetes_012, result$DiffWalk  , method = "pearson"))$estimate
diabetes_vs_Age   <- (cor.test(result$Diabetes_012, result$Age  , method = "pearson"))$estimate
diabetes_vs_Education <- (cor.test(result$Diabetes_012, result$Education  , method = "pearson"))$estimate
diabetes_vs_Income   <- (cor.test(result$Diabetes_012, result$Income  , method = "pearson"))$estimate



##### Shiny App #####
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Likelihood of Diabetes"),
  dashboardSidebar(
    width = 135,
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dataset", tabName = "dataset", icon = icon("th")),
      menuItem("Check ur risk", tabName = "check", icon = icon("check"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "SELECT FEATURE", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE,
                  selectInput("var",
                              label = "Select a condition",
                              choices = list("BMI",
                                             "Smoking",
                                             "High Blood Pressure",
                                             "High Cholesterol",
                                             "Stroke",
                                             "Heart Disease or Attack",
                                             "Alcohol Consumption",
                                             "General Hlth",
                                             "Mental Hlth",
                                             "Physical Hlth",
                                             "Difficulty Walking",
                                             "Age",
                                             "High Education",
                                             "High Income",
                                             "All of the above"),
                              selected = "BMI")),
                box(textOutput("desc"))
              ),
              fluidRow(
                column(
                  width=12,
                  box(
                    width=12,
                    title = "GRAPH", status = "warning", solidHeader = TRUE,
                    plotOutput("plot",width = "100%"))
                  )
              )
      ),
      tabItem(tabName = "dataset",
              DT::dataTableOutput("mytable")),
      tabItem(tabName = "about",
              h1("About this project"),
              p("Diabetes is a serious chronic disease in which it impairs glucose regulation and can decrease lifespan and wellbeing. How can we as individuals know how much risk we are in of getting diabetes?"),
              h3("Our goal"),
              p("In this project, I use a dataset that records the lifestyle conditions of people with and without diabetes, and look for a correlation between a conditions that puts one in a higher risk of getting diabetes."),
              h3("Data"),
              p("A clean dataset of 253,680 survey responses to the CDC's BRFSS2015 is used. 0 is for no diabetes or only during pregnancy, 1 is for prediabetes, and 2 is for diabetes. There is class imbalance in this dataset. This dataset has 21 feature variables"),
              h3("Implementation Decisions"),
              p("A common tool for data visualization is the bar chart, which displays data in either a horizontal or vertical bar style. In this instance, the author decided to use bar charts to show correlations between the dataset's various variables. The x-axis represented 'Diabetes 012' while the other characteristics (such as 'BMI', 'HighBP,' etc.) were represented on the y-axis. The bar charts were created to demonstrate the relationship between two variables at once.")),
      tabItem(tabName = "check",
              box(
                numericInput("BMI", "BMI", value = 0, width = "100%"),
                checkboxInput("Smoker", "Are you a Smoker", FALSE),
                checkboxInput("HighBP","Do you have High Blood Pressure?", FALSE),
                checkboxInput("HighChol","Do you have High Cholesterol?", FALSE),
                checkboxInput("Stroke", "Did you had a stroke?",FALSE),
                checkboxInput("HeartDiseaseorAttack", "Do you have Heart disease/attack(s)?",FALSE),
                checkboxInput("HvyAlcoholConsump", "Do you have more than 14 alcoholic drinks per week?",FALSE),
                checkboxInput( "DiffWalk", "Do you have difficulty walking (up/downs stairs)?",FALSE),
                sliderInput( "GenHlth", "How would you rate your General Health (1=Good, 5=Bad)", 1, 5, 1),
                sliderInput("MentHtlh:","How many days were your mental health affected in the last 30 days? (Stressed, anxious, depressed, etc.)",  1, 30, 1),
                sliderInput( "PhysHlth", "How many days were your physical health affected in the last 30 days? (Injuries, illnesses, etc.)",1, 30, 1),
                actionButton("save", "Save Responses")
              ),
              box(textOutput("diabetes_risk"))
            )
    )
  )
)


server <- function(input, output) {
  model1 <- lm(Diabetes_012 ~ HighBP + HighChol + BMI + Smoker + Stroke + HeartDiseaseorAttack + HvyAlcoholConsump + GenHlth + MentHlth + PhysHlth + DiffWalk, data = training_set)
  
  BMI_sum <- aggregate(BMI ~ Diabetes_012, data = result, sum)
  Smoker_sum <- aggregate(Smoker ~ Diabetes_012, data = result, sum)
  HighBP_sum <- aggregate(HighBP ~ Diabetes_012, data = result, sum)
  HighChol_sum <- aggregate(HighChol ~ Diabetes_012, data = result, sum)
  Stroke_sum <- aggregate(Stroke ~ Diabetes_012, data = result, sum)
  HeartDiseaseorAttack_sum <- aggregate(HeartDiseaseorAttack ~ Diabetes_012, data = result, sum)
  HvyAlcoholConsump_sum <- aggregate(HvyAlcoholConsump ~ Diabetes_012, data = result, sum)
  GenHlth_sum <- aggregate(GenHlth ~ Diabetes_012, data = result, sum)
  MentHlth_sum <- aggregate(MentHlth ~ Diabetes_012, data = result, sum)
  PhysHlth_sum <- aggregate(PhysHlth ~ Diabetes_012, data = result, sum)
  DiffWalk_sum <- aggregate(DiffWalk ~ Diabetes_012, data = result, sum)
  Age_sum <- aggregate(Age ~ Diabetes_012, data = result, sum)
  Income_sum <- aggregate(Income ~ Diabetes_012, data = result, sum)
  Education_sum <- aggregate(Education ~ Diabetes_012, data = result, sum)
  
  output$mytable = DT::renderDataTable({
    datatable(result, options = list(scrollX = TRUE))
  })
  
  output$desc <- renderText({
    if(input$var == "BMI"){
      paste("Correlation strength: ", diabetes_vs_BMI*100, "%. The sum of Body Mass Index (BMI) in each category is shown below.")
    }
    else if(input$var == "Smoking"){
      paste("Correlation strength: ", diabetes_vs_smoker*100, "%. Smoker is a binary variable that indicates if the individual smokes.")
    }  
    else if(input$var == "High Blood Pressure"){
      paste("Correlation strength: ", diabetes_vs_HighBP*100, "%. HighBP is a binary variable that indicates if the individual has High Blood Pressure.")
    }  
    else if(input$var == "High Cholesterol"){
      paste("Correlation strength: ", diabetes_vs_HighChol*100, "%. HighChol is a binary variable that indicates if the individual has High Cholesterol.")
    }  
    else if(input$var == "Stroke"){
      paste("Correlation strength: ", diabetes_vs_HighChol*100, "%. Stroke is a binary variable that indicates if the individual has ever gotten a stroke.")
    }  
    else if(input$var == "Heart Disease or Attack"){
      paste("Correlation strength: ", diabetes_vs_HeartDiseaseorAttack*100, "%. HeartDiseaseorAttack is a binary variable that indicates if the individual has a heart disease or gotten a heart attack.")
    }  
    else if(input$var == "Alcohol Consumption"){
      paste("Correlation strength: ", diabetes_vs_HvyAlcoholConsump*100, "%. HvyAlcoholConsump is a binary variable that indicates (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week) 0 = no 1 = yes.")
    }  
    else if(input$var == "General Hlth"){
      paste("Correlation strength: ", diabetes_vs_GenHlth*100, "%. GenHlth indicates in general your health is: scale 1-5 1 = excellent 2 = very good 3 = good 4 = fair 5 = poor.")
    }  
    else if(input$var == "Mental Hlth"){
      paste("Correlation strength: ", diabetes_vs_MentHlth*100, "%. MentHlth indicates your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days.")
    }  
    else if(input$var == "Physical Hlth"){
      paste("Correlation strength: ", diabetes_vs_PhysHlth*100, "%. PhysHlth indicates your physical health, which includes physical illness and injury, for how many days during the past 30 days.")
    }  
    else if(input$var == "Difficulty Walking"){
      paste("Correlation strength: ", diabetes_vs_DiffWalk*100, "%. DiffWalk indicates if  you have serious difficulty walking or climbing stairs? 0 = no 1 = yes.")
    }  
    else if(input$var == "Age"){
      paste("Correlation strength: ", diabetes_vs_Age*100, "%. Age is indicated as such: 13-level age category (_AGEG5YR see codebook) 1 = 18-24 9 = 60-64 13 = 80 or older")
    }  
    else if(input$var == "High Education"){
      paste("Correlation strength: ", diabetes_vs_Education*100, "%. Education indicates your Education level (EDUCA see codebook) scale 1-6 1 = Never attended school or only kindergarten 2 = Grades 1 through 8")
    }  
    else if(input$var == "High Income"){
      paste("Correlation strength: ", diabetes_vs_GenHlth*100, "%. Income scale (INCOME2 see codebook) scale 1-8 1 = less than $10,000 5 = less than $35,000 8 = $75,000 or more")
    }  
    else if(input$var == "All of the above"){
     "The features from left to right are: High BMI, Smoking, High Blood Pressure, High Cholesterol, Stroke, Heart Failures, Alcohol, Poor General Health, Poor Mental Health, Poor Physical Health, Difficulties Walking, Age, High Education, High Income."
    }
  })
  
  output$plot <- renderPlot({
    if(input$var == "BMI"){
      barplot(BMI_sum$BMI, names.arg = BMI_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }
    if(input$var == "Smoking"){
      barplot(Smoker_sum$Smoker, names.arg = Smoker_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "High Blood Pressure"){
      barplot(HighBP_sum$HighBP, names.arg = HighBP_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "High Cholesterol"){
      barplot(HighChol_sum$HighChol, names.arg = HighChol_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Stroke"){
      barplot(Stroke_sum$Stroke, names.arg = Stroke_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Heart Disease or Attack"){
      barplot(HeartDiseaseorAttack_sum$HeartDiseaseorAttack, names.arg = HeartDiseaseorAttack_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Alcohol Consumption"){
      barplot(HvyAlcoholConsump_sum$HvyAlcoholConsump, names.arg = HvyAlcoholConsump_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "General Hlth"){
      barplot(GenHlth_sum$GenHlth, names.arg = GenHlth_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Mental Hlth"){
      barplot(MentHlth_sum$MentHlth, names.arg = MentHlth_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Physical Hlth"){
      barplot(PhysHlth_sum$PhysHlth, names.arg = PhysHlth_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Difficulty Walking"){
      barplot(DiffWalk_sum$DiffWalk, names.arg = DiffWalk_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "Age"){
      barplot(Age_sum$Age, names.arg = Age_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "High Education"){
      barplot(Education_sum$Education, names.arg = Education_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "High Income"){
      barplot(Income_sum$Income, names.arg = Income_sum$Diabetes_012, main = paste(input$var, "vs Diabetes"), xlab = "(0=Healthy, 1=Prediabetes, 2=Diabetes)", ylab = paste("Sum of", input$var))
    }  
    if(input$var == "All of the above"){
      barplot(c(diabetes_vs_BMI, 
                diabetes_vs_Smoker, 
                diabetes_vs_HighBP,
                diabetes_vs_HighChol,
                diabetes_vs_Stroke,
                diabetes_vs_HeartDiseaseorAttack,
                diabetes_vs_HvyAlcoholConsump,
                diabetes_vs_GenHlth,
                diabetes_vs_MentHlth,
                diabetes_vs_PhysHlth,
                diabetes_vs_DiffWalk,
                diabetes_vs_Age,
                diabetes_vs_Education,
                diabetes_vs_Income), 
              names.arg =c("BMI", 
                           "Smoking", 
                           "HighBP", 
                           "HighChol", 
                           "Stroke", 
                           "HeartFailures", 
                           "Alch", 
                           "GenHlth", 
                           "MentalHlth", 
                           "PhysHlth", 
                           "DiffWalk", 
                           "Age", 
                           "Education", 
                           "Income"), 
              main = "Stregth of Correlation between Condition and Diabetes")
    }  
  })
  
  predict_info <- reactive({
    data.frame(
        HighBP =  as.numeric(input$HighBP),
        HighChol =  as.numeric(input$HighChol),
        BMI = as.numeric(input$BMI),
        Smoker =  as.numeric(input$Smoker),
        Stroke =  as.numeric(input$Stroke),
        HeartDiseaseorAttack = as.numeric(input$HeartDiseaseorAttack),
        HvyAlcoholConsump =  as.numeric(input$HvyAlcoholConsump),
        GenHlth = ifelse(is.na(input$GenHlth), 1, as.numeric(input$GenHlth)), 
        MentHlth = ifelse(is.na(input$MentHtlh), 1, as.numeric(input$MentHtlh)),
        PhysHlth = ifelse(is.na(input$PhysHlth), 1, as.numeric(input$PhysHlth)),
        DiffWalk =  as.numeric(input$DiffWalk)
      )
  })
  
  # Add an observer to update the output when the save button is clicked
  observeEvent(input$save, {
    predict_value <- reactive({
        predict(model1, newdata = predict_info())
    })
    
    output$diabetes_risk <- renderText({
      # Check if the predicted value is NA
      if (is.na(predict_value())) {
        "An error occurred while making the prediction."
      } else {
        paste("The predicted likelihood of having diabetes is:", predict_value())

      }
    })
  })
}

shinyApp(ui, server)



