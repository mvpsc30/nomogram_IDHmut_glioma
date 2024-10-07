
library(shiny)
library(shinyWidgets)
library(data.table)

age.values <- 20:80

ui <- fixedPage( 
  title = "Nomogram_SZFlab",
  fixedRow(
    # imageOutput('huashan1'),
    # imageOutput('huashan2')
    
    img(src='huashan1.png', align = "left",height="150px"),
    img(src='left.png', align = "left",height="150px"),
    img(src='huashan3.png', align = "right",height="150px"),
    img(src='huashan2.png', align = "right",height="150px")
  ),
  
  setBackgroundColor(
    color = c("#365DBA","#FFF"),
    gradient = "linear",
    direction = "bottom"
  ),
  wellPanel(
    # Application title
    fixedRow(
      h3(HTML(paste("Individualized Survival Prediction of", 
                    "IDH-Mutant Glioma Patients", sep="<br/>")), align = "center")
    ),
    
    fixedRow(
      (
        h4("This nomogram calculates survival probabilities for an individual based on 
           their age, TERTp status, extent of resection, grade and radiotherapy,
           Select values that correspond to the patient 
           to view corresponding 3-year, 5-year and 10-year predicted survival probabilities.", align = "center")
        )),
    
    br(),
    
    sidebarLayout(
      sidebarPanel(
        shinyWidgets::sliderTextInput("Age.dx",
                                      "Age",
                                      choices = as.numeric(age.values),
                                      selected = 40,
                                      grid = T)
        ,
        # selectInput('X1p19q', 
        #             '1p19q status',
        #             c("NCD","COD"))
        # ,
        selectInput('TERT', 
                    'TERTp status',
                    selected = "WT",
                    c("MUT","WT"))
        ,
        selectInput('Surg.Resec', 
                    'Surgical Resection',
                    c("Total Resection","Non-Total Resection"))
        ,
        selectInput('Grade', 
                    'Grade2021',
                    c("2","3","4"))
        ,
        selectInput('Treatment', 
                    'Radiotherapy',
                    selected = "No",
                    c("Yes","No"))
        ,
        # selectInput('Histology', 
        #             'Histology',
        #             selected = "Astrocytoma",
        #             c("Oligodendroglioma","Astrocytoma"))
        # ,
        h4("Variable points", align ="center"),
        fluidRow(column(12,align="center",
               h5(tableOutput("res_out"))))
      ),
      
      mainPanel(
        br(),
        h4("Expected Survival Probabilities", align ="center"),
        fluidRow(
          column(12,align="center",
                 h5(tableOutput("Predicted_Surv_Prob")))),
        fixedRow(
          imageOutput('nomo')
        ),
        fixedRow(
          br(),
          br()
        ),
        fixedRow(
          uiOutput(outputId = "text")
        )
      )
    ),
    
    HTML('<style type="text/css">
         .span8 .well { background-color: #00FFFF; }
         </style>')
    
    )
  
        )

server <- function(input, output, session) {
  
  output$nomo <- renderImage({
    list(src = 'nomo.png',
         width = '100%',
         height="410px")
  }	, deleteFile = FALSE)
  
  
  test <- reactive({
    data.frame(age.p=round(0 * input$Age.dx ^2 + 1.315734374 * input$Age.dx + -26.314687475,digits = 2),
               #`1p19q.p`=round(ifelse(input$X1p19q=="COD",7.424052,0),digits = 2),
               TERTp.p=round(ifelse(input$TERT=="WT",47.15306,0),digits = 2),
               surgery.p=round(ifelse(input$Surg.Resec=="Total Resection",0,24.21979),digits = 2),
               grade.p=round(ifelse(input$Grade=="2",0,ifelse(input$Grade=="3",49.98785,100)),digits = 2),
               Treatment.p=round(ifelse(input$Treatment=="Yes",0,22.89726),digits = 2))
               #Histology.p=round(ifelse(input$Histology=="Oligodendroglioma",0,34.67866),digits = 2))
  })
  
  test2<- reactive({data.frame(Variable=c("Age","TERTp status","Surgery Resection","Grade",
                                          "Radiotherapy"),
                               points=as.numeric(t(test())))})
  
  output$res_out<-renderTable(test2())
  
  final.df <- reactive({

    points.display <- sum(as.numeric(t(test())))
    
    points <- ifelse(points.display >= 280, 280, ifelse(points.display <= 0, 0, points.display))
    
    s3<-round((8e-08 * points ^3 + -5.439e-05 * points ^2 + 0.004688066 * points + 0.837117017)*100,digits = 0)
    s5<-round((8e-08 * points ^3 + -4.5382e-05 * points ^2 + 0.000924429 * points + 0.940838353)*100,digits = 0)
    s10<-round((1.91e-07 * points ^3 + -6.0145e-05 * points ^2 + -0.00100974 * points + 0.895302563)*100,digits = 0)
    
    s5<-ifelse(s5>s3,s3,s5)
    s10<-ifelse(s10>s5,s5,s10)
    s3<-ifelse(s3>=0,s3,0)
    s5<-ifelse(s5>=0,s5,0)
    s10<-ifelse(s10>=0,s10,0)
    
    s3<-paste0(s3,"%")
    s5<-paste0(s5,"%")
    s10<-paste0(s10,"%")
    
    final.display<-data.frame(name=c("Total Points",
                                     "Predicted 3-year Survival Probability",
                                     "Predicted 5-year Survival Probability",
                                     "Predicted 10-year Survival Probability"),
                              pr=c(points,s3,s5,s10))

    return(final.display)

  })
  
  
  output$Predicted_Surv_Prob <- renderTable(final.df(), colnames = FALSE)
  
  output$text <- renderText({
    HTML(paste0('<br/>',"<b>","Title: ","</b>", "A personalized nomogram for IDH-mutant glioma patient survival",'<br/>','<br/>',
                "<b>","Authors: ","</b>","Houshi Xu, houshi1996@gmail.com",'<br/>','<br/>',
                "<b>","Affiliations: ","</b>",
                "Department of Neurosurgery, Huashan Hospital, Shanghai Medical College, Fudan University, Shanghai, China;
                Department of Neurosurgery, Peking Union Medical College Hospital, Chinese Academy of Medical Sciences and Peking Union Medical College (CAMS & PUMC), Beijing, China
                ")
    )
  })
  
  }


# Run the application 
shinyApp(ui = ui, server = server)
