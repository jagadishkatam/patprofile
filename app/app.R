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
library(plotly)
library(RColorBrewer)

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
          # Checkbox group to select multiple datasets
          checkboxGroupInput("selected_dfs", "Choose Datasets:", 
                             choices = c("ADVS" = "advs", "ADSL2" = "adsl2", "ADAE" = "adae"),
                             selected = "advs"),  # Default selection
            selectInput("subj",
                        "Please Select a USUBJID",
                        choices = usubjid,
                        selected = usubjid[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = 'dataset',
            tabPanel("ADSL", 
                     varSelectInput("variables", "Variable:", NULL, multiple = TRUE), 
                     DTOutput("dataset1")),
            # tabPanel("AE Timeline", timevisOutput("plot")),
            # tabPanel("ADAE", varSelectInput("variables2", "Variable:", NULL, multiple = TRUE),DTOutput("dataset2")),
            tabPanel("ADAE",  # Main tab
                     fluidRow(
                       column(12, h4("AE Timeline")),  # Section header (optional)
                       column(12, timevisOutput("plot"))  # First row: AE Timeline
                     ),
                     fluidRow(
                       column(12, h4("ADAE Data")),  # Section header (optional)
                       column(12, 
                              varSelectInput("variables2", "Variable:", NULL, multiple = TRUE),
                              DTOutput("dataset2")  # Second row: ADAE Data Table
                       )
                     )
            ),
            
            tabPanel("ADVS",  # Main tab
                     fluidRow(column(12, h4("ADVS Line Plot")),
                              column(12,
                              tags$div(selectInput('para', 'Parameter', choices = parvs, selected = parvs[[1]]), style="display:inline-block"), 
                              tags$div(selectInput('yaxis', 'Analysis Y Variable', choices = list('aval','chg', 'pchg')), style="display:inline-block") ,
                              tags$div(selectInput('xaxis', 'Analysis X Variable', choices = list('ady','visit', 'avisit')), style="display:inline-block"),
                              tags$div(checkboxInput("chkb", "Baseline", FALSE), style="display:inline-block"),
                              plotlyOutput("plot2")
                     )),
                     
                     fluidRow(
                       column(12, h4("ADVS Data")),
                       column(12, DTOutput("advs2")  
                       )
                     )
            ),
            
            tabPanel("ADLB",  # Main tab
                     fluidRow(column(12, h4("ADLB Line Plot")),
                              column(12,
                                     tags$div(selectInput('lpara', 'Parameter', choices = parlb, selected = parlb[[1]]), style="display:inline-block"), 
                                     tags$div(selectInput('lyaxis', 'Analysis Y Variable', choices = list('aval','chg')), style="display:inline-block") ,
                                     tags$div(selectInput('lxaxis', 'Analysis X Variable', choices = list('ady','visit', 'avisit')), style="display:inline-block"),
                                     tags$div(checkboxInput("chkb", "Baseline", FALSE), style="display:inline-block"),
                                     plotlyOutput("plot3")
                              )),
                     
                     fluidRow(
                       column(12, h4("ADLB Data")),
                       column(12, DTOutput("adlb2")  
                       )
                     )
            ),
            
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  adsl_df <-  reactive({ 
    req(input$subj)
    adsl2 %>% #rename_all(toupper) |> 
      filter(usubjid==input$subj)
       
    })
  
  adae2 <-  reactive({ 
    req(input$subj)
    adae %>% filter(usubjid==input$subj) 
  })
  
  advs1 <-  reactive({ 
    req(input$subj)
    advs %>% filter(usubjid==input$subj & param == input$para) 
  })
  
  adlb1 <-  reactive({ 
    req(input$subj)
    adlbc %>% filter(usubjid==input$subj & param == input$lpara) 
  })
  
  observe({
    req(adsl_df())
    updateVarSelectInput(session, "variables", "Variable:", 
                         adsl_df()
                         )
  })
  
  observe({
    req(adae2())
    updateVarSelectInput(session, "variables2", "Variable:", adae2() )
  })
  
     output$dataset1 <- renderDT({
      if (length(input$variables)==0) {
        datatable(adsl_df() |> rename_all(toupper),  # Use the full dataset without subsetting columns
                  options = list(
                    scrollX = TRUE,  # Enable horizontal scrolling
                    pageLength = 10  # Set number of rows per page
                  )
        )
      } else {
        
        datatable(adsl_df() %>% 
                    filter(usubjid==input$subj) %>% dplyr::select(!!!input$variables) |> 
                    rename_all(toupper),  # Use the full dataset without subsetting columns
                  options = list(
                    scrollX = TRUE,  # Enable horizontal scrolling
                    pageLength = 10  # Set number of rows per page
                  )
        )
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
    
    output$plot2 <- renderPlotly({
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
      
      # Initialize shapes list (empty by default)
      shapes_list <- list()
      
      # If checkbox is selected, add a baseline reference line
      if (input$chkb & input$yaxis == 'aval') {
        baseline <- advs1() %>% filter(ablfl == 'Y')
        
        if (nrow(baseline) > 0) {
          
          shapes_list <- list(
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              y0 = baseline$base,
              y1 = baseline$base,
              xref = "paper",
              yref = "y",
              line = list(color = "red", width = 2, dash = "dash")
            )
          )
        }
      }
      
      N <- max(3, length(unique(advs1()$atpt)))
      colors <- brewer.pal(N, "Set3")
      
      filtered_data <- advs1() %>% 
        filter(param == input$para)
      
      plot_ly(
        data = filtered_data,
        x = ~eval_tidy(xaxis),
        y = ~.data[[input$yaxis]],
        color = ~atpt,
        colors = colors,
        type = 'scatter',
        mode = 'lines+markers',
        text = ~atpt,
        hovertemplate = paste(
          "<b>", xaxisl, ":</b> %{x}<br>",
          "<b>", yaxisl, ":</b> %{y}<br>",
          "<b>ATPT:</b> %{text}<extra></extra>" 
        )
      ) %>%
        layout(
          title = paste(input$para, 'Line Plot'),
          xaxis = list(title = xaxisl),
          yaxis = list(title = yaxisl),
          shapes = shapes_list ,
          legend = list(
            title = list(text = "ATPT"),
            orientation = "h",   # Set legend to horizontal
            x = 0.5,             # Center the legend horizontally
            y = -0.2,            # Move legend below the plot
            xanchor = "center",  # Align by center
            yanchor = "top"      # Anchor to top of legend box
          )
          
        )
      })
    
    output$advs2 <- renderDT({
      req(advs1())  # Ensure dataset exists
      
      datatable(advs1(),  # Use the full dataset without subsetting columns
                options = list(
                  scrollX = TRUE,  # Enable horizontal scrolling
                  pageLength = 10  # Set number of rows per page
                )
      )
    })
    
    
    output$plot3 <- renderPlotly({
      req(input$lpara, input$lyaxis)
      
      # Determine X-axis expression
      if (input$lxaxis == 'ady') {
        lxaxis <- parse_expr('.data[[input$lxaxis]]')
        xaxisl <- 'Study Day'
      } else if (input$lxaxis == 'visit') {
        lxaxis <- parse_expr("reorder(.data[[input$lxaxis]], visitnum)")
        xaxisl <- 'Visit'
      } else if (input$lxaxis == 'avisit') {
        lxaxis <- parse_expr("reorder(.data[[input$lxaxis]], avisitn)")
        xaxisl <- 'Analysis Visit'
      }
      
      # Determine Y-axis label
      if (input$lyaxis == 'aval') {
        yaxisl <- 'Analysis Result'
      } else if (input$lyaxis == 'chg') {
        yaxisl <- 'Change from Baseline'
      }
      
      
      # Initialize shapes list (empty by default)
      shapes_list <- list()
      
      # If checkbox is selected, add a baseline reference line
      if (input$chkb & input$lyaxis == 'aval') {
        baseline <- adlb1() %>% filter(ablfl == 'Y')
        
        if (nrow(baseline) > 0) {
          
          shapes_list <- list(
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              y0 = baseline$base,
              y1 = baseline$base,
              xref = "paper",
              yref = "y",
              line = list(color = "red", width = 2, dash = "dash")
            )
          )
        }
      }
      
      # Generate plot
      plot_ly(
        data = adlb1(),
        x = ~eval_tidy(lxaxis),
        y = ~.data[[input$lyaxis]],
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(color = 'blue'),
        hovertemplate = paste(
          "<b>", xaxisl, ":</b> %{x}<br>",
          "<b>", yaxisl, ":</b> %{y}<br>"
        )
      ) %>%
        layout(
          title = paste(input$lpara, 'Line Plot'),
          xaxis = list(title = xaxisl),
          yaxis = list(title = yaxisl),
          shapes = shapes_list  # Add reference line if checkbox is checked
        )
    
    })
    

      
    
    output$adlb2 <- renderDT({
      req(adlb1())  # Ensure dataset exists
      
      datatable(adlb1(),  # Use the full dataset without subsetting columns
                options = list(
                  scrollX = TRUE,  # Enable horizontal scrolling
                  pageLength = 10  # Set number of rows per page
                )
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
