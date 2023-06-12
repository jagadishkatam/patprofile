#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidyCDISC)
library(DT)
library(shinyjs)
library(timevis)
library(rlang)

data('adae', package = 'tidyCDISC')

adae <- adae %>% rename_all(tolower) %>% 
  mutate(astdt=as.Date(astdt, format='%d%B%Y'),trtsdt=as.Date(trtsdt, format='%d%B%Y'),trtedt=as.Date(trtedt, format='%d%B%Y'), 
         aendt2=ifelse((is.na(aendt) & astdt>=trtsdt & !is.na(trtedt)), trtedt, as.Date(aendt, format='%d%B%Y')),
         aendt2=as.Date(aendt2, origin = "1970-01-01")) %>% 
  select(usubjid, trtsdt, trtedt, astdt, aendt, aendt2, aedecod) %>% rename(content=aedecod, start=astdt, end=aendt)

usubjid <- unique(adae$usubjid)

data('adsl', package = 'tidyCDISC')

adsl2 <- adsl %>% rename_all(tolower) %>% mutate(id=row_number())

adae <- adae %>% filter(!is.na(start)) %>% mutate(id=row_number()) %>% filter(id<=1000)

data('advs', package = 'tidyCDISC')
advs <- advs %>% rename_all(tolower) %>% filter(anl01fl=='Y')

parvs <- unique(advs$param)

data('adlbc', package = 'tidyCDISC')
adlbc <- adlbc %>% rename_all(tolower) 

parlb <- unique(adlbc$param)
#timevis(adae)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Patient Profile"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("subj",
                        "Please Select a USUBJID",
                        choices = usubjid,
                        selected = usubjid[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = 'dataset',
            tabPanel("ADSL", varSelectInput("variables", "Variable:", NULL, multiple = TRUE), DTOutput("dataset1")),
            tabPanel("AE Timeline", timevisOutput("plot")),
            tabPanel("ADAE", varSelectInput("variables2", "Variable:", NULL, multiple = TRUE),DTOutput("dataset2")),
            tabPanel("ADVS", tags$div(selectInput('para', 'Parameter', choices = parvs, selected = parvs[[1]]), style="display:inline-block"), 
                             tags$div(selectInput('yaxis', 'Analysis Y Variable', choices = list('aval','chg', 'pchg')), style="display:inline-block") ,
                             tags$div(selectInput('xaxis', 'Analysis X Variable', choices = list('ady','visit', 'avisit')), style="display:inline-block") ,
                             plotOutput("plot2")),
            tabPanel("ADLB", tags$div(selectInput('lpara', 'Parameter', choices = parlb, selected = parlb[[1]]), style="display:inline-block"), 
                     tags$div(selectInput('lyaxis', 'Analysis Y Variable', choices = list('aval','chg')), style="display:inline-block") ,
                     tags$div(selectInput('lxaxis', 'Analysis X Variable', choices = list('ady','visit', 'avisit')), style="display:inline-block") ,
                     tags$div(checkboxInput("chkb", "No Group", FALSE), style="display:inline-block"),
                     plotOutput("plot3")),
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  adsl_df <-  reactive({ 
    req(input$subj)
    adsl2 %>% filter(usubjid==input$subj) 
    })
  
  adae2 <-  reactive({ 
    req(input$subj)
    adae %>% filter(usubjid==input$subj) 
  })
  
  observe({
    req(adsl_df())
    updateVarSelectInput(session, "variables", "Variable:", adsl_df())
  })
  
  observe({
    req(adae2())
    updateVarSelectInput(session, "variables2", "Variable:", adae2())
  })
  
    output$dataset1 <- renderDataTable({
      if (length(input$variables)==0) {
     adsl_df() #%>% filter(usubjid==input$subj) 
      } else {
     adsl_df() %>% filter(usubjid==input$subj) %>% dplyr::select(!!!input$variables)
      }
    })
    
    output$dataset2 <- renderDataTable({
      if (length(input$variables2)==0) {
        adae2() #%>% filter(usubjid==input$subj) 
      } else {
        adae2() %>% dplyr::select(!!!input$variables2)
      }
    })
    
    output$plot <- renderTimevis({
      config <- list(
        editable = TRUE,
        align = "center",
        orientation = "top",
        snap = NULL,
        margin = list(item = 30, axis = 50)
      )
      adae2 <- adae %>% filter(usubjid==input$subj)
      timevis(adae2, zoomFactor = 1, options = config)
    })
    
    output$plot2 <- renderPlot({
      req(input$para, input$yaxis)
      
      if (input$xaxis=='ady') {
        xaxis <- parse_expr('.data[[input$xaxis]]')
        xaxisl <- 'Study Day'
      } else if (input$xaxis=='visit') {
        xaxis <- parse_expr("reorder(.data[[input$xaxis]], visitnum)")
        xaxisl <- 'Visit'
      } else if (input$xaxis=='avisit') {
        xaxis <- parse_expr("reorder(.data[[input$xaxis]], avisitn)")
        xaxisl <- 'Analysis Visit'
      }
      
      if (input$yaxis=='aval') {
        yaxisl <- 'Analysis Result'
      } else if (input$yaxis=='chg') {
        yaxisl <- 'Change from Baseline'
      } else if (input$yaxis=='pchg') {
        yaxisl <- '%Change from Baseline'
      }
      
      ggplot(advs %>% filter(usubjid==input$subj & param==input$para), aes(x=eval_tidy(xaxis),y=.data[[input$yaxis]], group=atpt)) + 
        geom_line(aes(color=atpt)) +
        labs(x=xaxisl,y=yaxisl) +
        ggtitle(paste(input$para, 'Line Plot')) +
        theme_classic() +
        guides(color=guide_legend(title="ATPT"))
    })
    
    
    output$plot3 <- renderPlot({
      req(input$lpara, input$lyaxis)
      
      if (input$lxaxis=='ady') {
        lxaxis <- parse_expr('.data[[input$lxaxis]]')
        xaxisl <- 'Study Day'
      } else if (input$lxaxis=='visit') {
        lxaxis <- parse_expr("reorder(.data[[input$lxaxis]], visitnum)")
        xaxisl <- 'Visit'
      } else if (input$lxaxis=='avisit') {
        lxaxis <- parse_expr("reorder(.data[[input$lxaxis]], avisitn)")
        xaxisl <- 'Analysis Visit'
      }
      
      if (input$lyaxis=='aval') {
        yaxisl <- 'Analysis Result'
      } else if (input$lyaxis=='chg') {
        yaxisl <- 'Change from Baseline'
      } 
      
      chk <- reactive({
        if(input$chkb==FALSE) {
          NULL
        } else {
          1
        }
      })

      ggplot(adlbc %>% filter(usubjid==input$subj & param==input$lpara), aes(x=eval_tidy(lxaxis),y=.data[[input$lyaxis]], group=chk())) + 
        geom_line(color='red') +
        labs(x=xaxisl,y=yaxisl) +
        ggtitle(paste(input$lpara, 'Line Plot')) +
        theme_classic() +
        theme(legend.position = 'none')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
