####NSDC R_Shiny Dashboard########
nsdc_data<-read.csv("E:/PGPDM/Capstone Project/R Shiny Dashboard/cleaned_data.csv",stringsAsFactors = F, header = T)
salary<-read.csv("E:/PGPDM/Capstone Project/R Shiny Dashboard/cleaned_data_sal.csv",stringsAsFactors = F, header = T)

nsdc_data$Sector.ID=as.factor(nsdc_data$Sector.ID)
nsdc_data$JobRoleID=as.factor(nsdc_data$JobRoleID)
nsdc_data$JobRoleLevel=as.factor(nsdc_data$JobRoleLevel)
nsdc_data$HostelFacilityMale=as.factor(nsdc_data$HostelFacilityMale)
nsdc_data$Centre.Ownership=as.factor(nsdc_data$Centre.Ownership)
nsdc_data$CentreType=as.factor(nsdc_data$CentreType)
nsdc_data$TG1=as.factor(nsdc_data$TG1)
nsdc_data$Assessed=as.factor(nsdc_data$Assessed)
nsdc_data$employertype=as.factor(nsdc_data$employertype)
nsdc_data$Result.are.approved.by.ssc=as.factor(nsdc_data$Result.are.approved.by.ssc)
nsdc_data$Certified=as.factor(nsdc_data$Certified)
nsdc_data$placed=as.factor(nsdc_data$placed)
nsdc_data$Gender=as.factor(nsdc_data$Gender)
nsdc_data$TrainingType=as.factor(nsdc_data$TrainingType)
nsdc_data$subsectorid=as.factor(nsdc_data$subsectorid)
nsdc_data$PartnerID=as.factor(nsdc_data$PartnerID)
nsdc_data$CentreID=as.factor(nsdc_data$CentreID)
nsdc_data$TrainerID=as.factor(nsdc_data$TrainerID)
nsdc_data$BatchID=as.factor(nsdc_data$BatchID)
nsdc_data$JobRoleType=as.factor(nsdc_data$JobRoleType)
nsdc_data$PartnerType=as.factor(nsdc_data$PartnerType)
nsdc_data$VM1=as.factor(nsdc_data$VM1)
nsdc_data$VM2=as.factor(nsdc_data$VM2)
nsdc_data$EducationAttained=as.factor(nsdc_data$EducationAttained)
nsdc_data$TC.District=as.factor(nsdc_data$TC.District)
nsdc_data$TC.State=as.factor(nsdc_data$TC.State)
nsdc_data$SSCName=as.factor(nsdc_data$SSCName)
nsdc_data$Region=as.factor(nsdc_data$Region)
nsdc_data$StateUT=as.factor(nsdc_data$StateUT)
nsdc_data$District=as.factor(nsdc_data$District)
nsdc_data$City_type=as.factor(nsdc_data$City_type)

## app.R ##
library(ggplot2)
#library(dplyr)
library(shiny)
library(shinydashboard)
#library(googleVis)
library(ggvis)
#library(RColorBrewer)
library(DT)
library(data.table)
library(shinythemes)
#library(tidyverse)
library(caret)

options(shiny.maxRequestSize=800*1024^2)


header<- dashboardHeader(title = "NSDC Dashboard")

sidebar<- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Descriptive Viz_Placement", tabName = "tabforviz_plcmnt", icon = icon("bar-chart-o")),
    #menuItem("Placement Data Explorer", tabName = "tabfordatatable", icon = icon("fa fa -table")),
    menuItem("Descriptive Viz_Salary", tabName = "tabforviz_sal", icon = icon("bar-chart-o")),
    #menuItem("Map", tabName = "tabformap", icon = icon("fa fa-map-maker")),
    menuItem("Model_Deployment", tabName = "model_deployment", icon = icon("fa fa table"))
  )
)        

body<- dashboardBody(
  
  tabItems(
    
    ##########################################
    # Placement Viz
    ###########################################
    
    tabItem("tabforviz_plcmnt",
            
            h2(" Exploring different independent variables with respect to Placement"),
            
            fluidRow(
              infoBox("Number of Placements",length(which(nsdc_data$placed==2)), icon = icon("fas fa-bullhorn"),fill = FALSE, color = "purple"),
              infoBoxOutput("approvalBox")
              #      valueBox("value3")
            ),
            
            
            fluidRow(
              selectInput("Xaxis", "Select Independent Variables", colnames(nsdc_data),selected = "Assessed"),
              #),
              
              #fluidRow( 
              titlePanel(title = h4("NSDC", align="centre")),
              plotOutput("Plot")
              
            )
            
            
    ), # End of Tabmenu 1
    
    ######################################
    # Salary Viz Tab
    #####################################    
    
    tabItem("tabforviz_sal",
            h2(" Salary Explorer"),
            
            fluidRow(
              box(
                title = "About the Visualisation (Toggle the + button", solidHeader = TRUE,
                status="warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                h5("This salary dashboard has been created only using the non-missing values."),
                ("Rows having missing Monthly Income has been deleted."),
                br(),
                h5("The Salaries in the value boxes are all averages and not exact.")
              )
              
              
              
            ),
            
            fluidRow(
              valueBoxOutput("value1")
              ,valueBoxOutput("value2")
              ,valueBoxOutput("value3")
            ),
            
            fluidRow(
              
              box(
                title = "Monthly Income per Grade"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("revenuebyPrd", height = "300px")
              )
              
              ,box(
                title = "Monthly Income per Gender"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("revenuebyRegion", height = "300px")
              ) 
              
            ),
            
            fluidRow( # Top 10 States
              
              column(width = 8,
                     box(
                       title = "Top 10 TC.States by Monthly Income",
                       status = "primary",
                       width =15 ,
                       height = 400,
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       showOutput("top10StatesBar", "nvd3")
                     )
              )
              
            ),# End of Top 10 states  
            
            fluidRow(
              column(width = 8,
                     box(
                       title = " Top 10 Job Role with highest Income",
                       status = "primary",
                       width = 15,
                       height = 400,
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       showOutput("top10JobRoleBar", "nvd3")
                       
                     ))
              
              
            )
            
            
            
    ),
    
    
    ###################################################################
    # Model Deployment
    ###################################################################
    tabItem("model_deployment",
    navbarPage("This is a sample model deployment",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(uiOutput("tb1"))
                        ) ),
               tabPanel("Model_dev",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show"))))
    ))    
    
    #End of Tab Items
  )
  
)

################################################################
####   SERVER
##################################################################


server <- function(input, output) { 
  
  ######################################
  ## Tab 1:  Placement
  ########################################
  
  output$distPlot<- renderPlot({
    
    colm<-as.numeric(input$var)
    x<-na.omit(nsdc_data[,colm])
    bins<-seq(min(x), max(x), length.out = input$bins +1)
    
    hist(x, breaks = bins, col = input$color)
    
  })
  
  
  output$Plot<- renderPlot({
    
    #x<- na.omit(nsdc_data$TotalPercentage)
    colm<- input$Xaxis
    
    
    ggplot(nsdc_data, aes(x=nsdc_data[,colm], fill=placed))+
      geom_bar( size = 3, position = "dodge")+
      labs(x = colm)
    
  })
  
  ## creating progress bar item
  output$approvalBox<- renderInfoBox({
    
    infoBox(
      "Placement Percentage", "42%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = FALSE
    )
  })
  
  #############################################################  
  # Tab 2: Salary Viz
  #########################################################  
  
  total.MonthlyIncome <- mean(salary$MonthlyIncome)
  Income.JobRoleID <- salary %>% group_by(JobRoleID) %>% summarise(value = mean(MonthlyIncome)) %>% filter(value==max(value))
  prof.prod <- salary %>% group_by(Sector.ID) %>% summarise(value = mean(MonthlyIncome)) %>% filter(value==max(value))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(Income.JobRoleID$value, format="d", big.mark=',')
      ,paste('Top JobRole :',Income.JobRoleID$JobRoleID)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.MonthlyIncome, format="d", big.mark=',')
      ,'Average Monthly Salary'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Sector:',prof.prod$Sector.ID)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  
  output$revenuebyPrd <- renderPlot({
    ggplot(data = salary, 
           aes(x=Grade, y=MonthlyIncome, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("MonthlyIncome (in Rupees)") + 
      xlab("Grade") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Monthly Income by Region") + labs(fill = "Region")
  })
  
  
  output$revenuebyRegion <- renderPlot({
    ggplot(data = salary, 
           aes(x=Gender, y=MonthlyIncome, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("MonthlyIncome (in Rupees)") + 
      xlab("Gender") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Monthly Income Region") + labs(fill = "Region")
  })
  
  # Render top 10 states bar chart
  
  output$top10StatesBar<- renderChart({
    
    current<- salary
    current<- arrange(current, desc(MonthlyIncome))
    current <- subset(current[1:10,],select=c(TC.State, MonthlyIncome))
    p<- nPlot(MonthlyIncome~TC.State, data = current, type = "discreteBarChart", dom = "top10StatesBar")
    p$params$width <- 1000
    p$params$height <- 350
    p$xAxis(staggerLabels = TRUE)
    p$yAxis(axisLabel = " Monthly Income(Rs.)", width = 50)
    return(p)
    
    
  })
  
  output$top10JobRoleBar<- renderChart({
    
    role<- salary
    role<-arrange(role, desc(MonthlyIncome))
    role<- subset(role[1:10,],select=c(JobRoleID, MonthlyIncome))
    q<- nPlot(MonthlyIncome~JobRoleID, data = role, type = "discreteBarChart", dom = "top10JobRoleBar")
    q$params$width <-1000
    q$params$height <- 300
    q$xAxis(staggerLabels = TRUE)
    q$yAxis(axisLabel = "Monthly Income (Rs.)", width = 50)
    return(q)
    
  })
  
  
  
  ##############################  
  # Tab 3: model deplyment
  ############################  
  
  
  data <- reactive({
    
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })  
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  output$model_select<-renderUI({
    selectInput("modelselect","Choice of Algorithm",choices = c("Logistic_reg"="logreg","SVM"="svm"))
  })
  
  output$var1_select<-renderUI({
    selectInput("ind_var_select","Select Dependent Variable(Y)", choices =as.list(names(data())),multiple = FALSE)
  })
  output$rest_var_select<-renderUI({
    checkboxGroupInput("other_var_select","Select Independent Variables(X)",choices =as.list(names(data())))
  })
  output$other_val_show<-renderPrint({
    input$other_var_select
    input$ind_var_select
    f<-data()
    
        form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
    print(form)
    
    logreg <-glm(as.formula(form),family=binomial(),data=f)
    print(summary(logreg))
    
    
  })
  
  #####  
}


ui = dashboardPage(header,sidebar,body, skin = "green")
shinyApp(ui, server)
