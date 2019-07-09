
library(shiny)
library(shinyWidgets)
library(data.table)
library(openxlsx)

df.p <- read.xlsx("nomo_points_5_8_update.xlsx", sheet = "Predictors") 
## This sheet contains 3 columns - Variable, Value, Points
# Variable - List of variables in predictive model, occurs for every level of "Value"
# Value - Associated levels for the "Variable" that the user can select
# Points - The nomogram points associated with the "Value" selected

df.o <- read.xlsx("nomo_points_5_8_update.xlsx", sheet = "Outcomes")
## This sheet contains 4 columns - Total Points, Predicted 12-Month Survival Probability:, Predicted 18-Month Survival Probability:,Predicted 24-Month Survival Probability:
# Total Points - Total point values associated with the sum of the "Value" points from the first sheet
# The remaining 3 variables provide the survival probabilities for the associated "Total Points" value

age.values <- subset(df.p, (Variable =="Age at Diagnosis"))

ui <- fixedPage(
  fixedRow(
    img(src='220th_sm_b7880MkexsCq.png', align = "left"),
    img(src='DPQHS-Logo-FINAL_1500-px-700x300.png', align = "right", width = "500px")
  ),
  
  setBackgroundColor(
    color = c("#FFFFFF", "#003A62"),
    gradient = "linear",
    direction = "bottom"
  ),
  wellPanel(
    # Application title
    fixedRow(
      h3(HTML(paste("Individualized Prediction of Glioblastoma Survival", "for IDH-Wildtype Patients", sep="<br/>")), align = "center")
    ),
    
    fixedRow(
      (
        h5("This nomogram calculates survival probabilities for an individual based on 
           their age at diagnosis, sex, extent of resection, treatment,
           Karnofsky performance score, and MGMT status. Select values that correspond to the patient 
           to view corresponding 12-month, 18-month and 24-month predicted survival probabilities.", align = "center")
        )),
    
    br(),
    
    sidebarLayout(
      sidebarPanel(
        shinyWidgets::sliderTextInput("Age.dx",
                                      "Age at Diagnosis",
                                      choices = c(age.values$Value),
                                      grid = T)
        ,
        selectInput('Sex', 
                    'Sex',
                    c("Female","Male"))
        ,
        selectInput('Surg.Resec', 
                    'Surgical Resection',
                    c("Gross Total Resection","Subtotal Resection"))
        ,
        selectInput('Treatment', 
                    'Concurrent Radiation/TMZ',
                    c("Yes","No"))
        ,
        selectInput('KPS', 
                    'Karnofsky Performance Status',
                    c("≥70","<70"))
        ,
        selectInput('MGMT', 
                    div(HTML('<em>MGMT</em> Methylation')),
                    c("Yes","No"))
      ),
      
      mainPanel(
        br(),
        h4("Expected Survival Probabilities", align ="center"),
        fluidRow(
          column(12,align="center",
                 h5(tableOutput("Predicted_Surv_Prob")))),
        fixedRow(
          img(src='Figure 2_new.png', height = 500, width = 700, style="display: block; margin-left: auto; margin-right: auto;")
        ),
        uiOutput(outputId = "text")
      )
    ),
    
    HTML('<style type="text/css">
         .span8 .well { background-color: #00FFFF; }
         </style>')
    
    )
  
        )

server <- function(input, output, session) {
  
  
  test <- reactive({subset(df.p, (Variable =="Age at Diagnosis" & Value == as.character(input$Age.dx))| 
                             (Variable =="Sex" & Value == as.character(input$Sex))|
                             (Variable =="Surgical Resection" & Value == as.character(input$Surg.Resec))|
                             (Variable =="Concurrent Radiation/TMZ" & Value == as.character(input$Treatment))|
                             (Variable =="Karnofsky Performance Status" & Value == as.character(input$KPS))|
                             (Variable =="MGMT Methylation" & Value == as.character(input$MGMT))
  )
  })
  
  final.df <- reactive({
    
    total.points.display <- sum(test()$Points)
    total.points <- ifelse(total.points.display >= 250, 250, ifelse(total.points.display <= 7, 7, total.points.display))
    
    wide.prob <-setDT(as.data.frame(t(round(subset(df.o, Total.Points == total.points),2)[,-1])), keep.rownames = TRUE)
    wide.prob$rn <- gsub("[.]", " ", wide.prob$rn)
    names(wide.prob)<-c("rn","1")
    
    tp <- data.frame("Total Points",total.points.display)
    names(tp)<-c("rn","1")
    
    final.display <- rbind(tp,wide.prob, fill = T)
    final.display$`1` <- as.character(final.display$`1`)
    
    
    return(final.display)
    
  })
  
  
  output$Predicted_Surv_Prob <- renderTable(final.df(), colnames = FALSE)
  
  output$text <- renderText({
    HTML(paste0('<br/>',"<b>","Title: ","</b>", "An independently validated nomogram for IDH-wildtype glioblastoma patient survival",'<br/>','<br/>',
                "<b>","Authors: ","</b>","Haley Gittleman, Gino Cioffi, Pranathi Chunduru, Annette M. Molinaro, Mitchel S. Berger, Andrew E. Sloan, Jill S. Barnholtz-Sloan",'<br/>','<br/>',
                "<b>","Affiliations: ","</b>","Department of Population and Quantitative Health Sciences, Case Western Reserve University School of Medicine, Cleveland, Ohio (HG, GC, JSB);
                Case Comprehensive Cancer Center, Case Western Reserve University School of Medicine, Cleveland, Ohio (HG, AES, JSB);
                Department of Neurological Surgery, University of California San Francisco, San Francisco, California (PC, AMM, MB);
                Department of Neurological Surgery, University Hospitals of Cleveland and Case Western University School of Medicine, Cleveland, Ohio (AES);
                Seidman Cancer Center, University Hospitals of Cleveland, Cleveland, Ohio (AES)")
    )
  })
  
  }


# Run the application 
shinyApp(ui = ui, server = server)
