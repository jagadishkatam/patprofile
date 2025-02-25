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
library(shinyFiles)
library(bslib) 

# pre-load data -----------------------------------------------------------


preloaded_data <- list(
  'adsl' = tidyCDISC::adsl,
  'adae' = tidyCDISC::adae,
  'adlbc' = tidyCDISC::adlbc,
  'advs' = tidyCDISC::advs
)


# adsl --------------------------------------------------------------------


adsl <- preloaded_data$adsl

adsl2 <- adsl %>% rename_all(tolower) %>% mutate(id=row_number())|> mutate(group=1)


# adae --------------------------------------------------------------------


adae <- preloaded_data$adae

adae_data <- function(data){
adae <- data %>% rename_all(tolower) %>% 
  mutate(astdt=as.Date(astdt, format='%d%B%Y'),
         trtsdt=as.Date(trtsdt, format='%d%B%Y'),
         trtedt=as.Date(trtedt, format='%d%B%Y'), 
         aendt2=ifelse((is.na(aendt) & astdt>=trtsdt & !is.na(trtedt)), trtedt, as.Date(aendt, format='%d%B%Y')),
         aendt2=as.Date(aendt2, origin = "1970-01-01")) %>% 
  select(usubjid, trtsdt, trtedt, astdt, aendt, aendt2, aedecod) %>% 
  rename(content=aedecod, start=astdt, end=aendt) |> 
  filter(!is.na(start)) %>% mutate(id=row_number()) #%>% filter(id<=1000)
}

adae <- adae_data(adae)

usubjid <- unique(adae$usubjid)


# advs --------------------------------------------------------------------

advs <- preloaded_data$advs
advs <- advs %>% rename_all(tolower) %>% filter(anl01fl=='Y')

parvs <- unique(advs$param)

# adlbc -------------------------------------------------------------------

adlbc <- preloaded_data$adlbc
adlbc <- adlbc %>% rename_all(tolower) 

parlb <- unique(adlbc$param)
#timevis(adae)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly" ),
    # Application title
    titlePanel("Patient Profile"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("sas_files", "Upload SAS Files", multiple = TRUE, accept = c(".sas7bdat")),
          uiOutput("datasetSelector") ,  # Dynamically generated checkboxes for datasets
          uiOutput("subjSelector")  # Dynamic subject selection
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = 'dataset',
            tabPanel("Data View", 
                     textInput("filter_val", tags$span("Enter Filter Expression (e.g., Age > 30 & Gender == 'M')", style = "font-weight: bold; color: red;") ), 
                     varSelectInput("variables1", tags$span("Select Variables:", style = "font-weight: bold; color: red;"), NULL, multiple = TRUE), 
                     DTOutput("dataTable"),
                     downloadButton("downloadData", "Download")),
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
                              tags$div(selectInput('para', 'Parameter', choices = parvs, selected = parvs[[4]]), style="display:inline-block"), 
                              tags$div(selectInput('yaxis', 'Analysis Y Variable', choices = list('aval','chg', 'pchg')), style="display:inline-block") ,
                              tags$div(selectInput('xaxis', 'Analysis X Variable', choices = list('ady','visit', 'avisit')), style="display:inline-block"),
                              tags$div(checkboxInput("vchkb", tags$span("Baseline", style = "font-weight: bold; color: red;"), FALSE), style="display:inline-block"),
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
                                     tags$div(checkboxInput("lchkb", tags$span("Baseline", style = "font-weight: bold; color: red;"), FALSE), style="display:inline-block"),
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
  
 
  # 🔹 Reactive Storage for Datasets (Initially Preloaded)
  data_storage <- reactiveVal(preloaded_data)
  
  # 🔹 Handle File Uploads and Merge with Existing Datasets
  observeEvent(input$sas_files, {
    req(input$sas_files)
    
    files <- input$sas_files$datapath
    names(files) <- tools::file_path_sans_ext(input$sas_files$name)  # Extract file names without .sas7bdat
    
    uploaded_data <- lapply(files, haven::read_sas)  # Read all uploaded files
    names(uploaded_data) <- names(files)  # Assign dataset names
    
    updated_data <- data_storage()  # Get existing datasets
    updated_data[names(uploaded_data)] <- uploaded_data  # Replace matching datasets
    
    data_storage(updated_data)  # Update reactive storage
    
    # 🔹 Force UI Refresh (Important!)
    updateCheckboxGroupInput(session, "selectedDataset",
                             choices = names(updated_data),
                             selected = names(updated_data)[1])
  })
  
  # 🔹 Dynamic Checkboxes for Selecting Datasets
  output$datasetSelector <- renderUI({
    req(data_storage())
    checkboxGroupInput("selectedDataset", "Select Datasets:", 
                       choices = names(data_storage()), 
                       selected = names(data_storage())[1])  # Auto-select all available datasets
  })
  
  # 🔹 Reactive Expression for Selected Dataset
  selected_data <- reactive({
    req(input$selectedDataset)
    selected_name <- input$selectedDataset[1]  # Take the first selected dataset
    
    req(selected_name %in% names(data_storage()))  # Ensure dataset exists
    data_storage()[[selected_name]]  # Get first selected dataset
  })
  
  # 🔹 Update USUBJID Dropdown When Dataset Changes
  observeEvent(selected_data(), {
    req(selected_data())
    
    # Extract unique USUBJID values
    usubjid_list <- unique(selected_data()$USUBJID)
    
    updateSelectInput(session, "subj",
                      choices = usubjid_list,
                      selected = usubjid_list[1])
  })
  
  
  
  # 🔹 UI for Subject Selection
  output$subjSelector <- renderUI({
    req(selected_data())
    selectInput("subj", "Please Select a USUBJID",
                choices = unique(selected_data()$USUBJID))
  })
  
  observeEvent(selected_data(), {
    req(selected_data())
    updateVarSelectInput(session, "variables1",
                         data  = selected_data()[1]
    )
  })
  
  # 🔹 Display First Selected Dataset in Data View
  output$dataTable <- renderDT({
    req(selected_data())

    filtered_data <- selected_data()
    
    if (!is.null(input$filter_val) && input$filter_val != "") {
      tryCatch({
        filtered_data <- filtered_data %>% filter(eval(parse(text = input$filter_val)))
      }, error = function(e) {
        showNotification("Invalid filter expression", type = "error")
      })
    }
    
    
    # The requested dataset
    data_download <- reactive({
      get(input$selectedDataset)
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0(input$selectedDataset, ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write.csv(data_download(), file)
      }
    )
    
    
    if (length(input$variables1)==0) {
      datatable(filtered_data |> rename_all(toupper),  # Use the full dataset without subsetting columns
                options = list(
                  scrollX = TRUE,  # Enable horizontal scrolling
                  pageLength = 10  # Set number of rows per page
                )
      )
    } else {
      
      datatable(filtered_data %>% 
                  dplyr::select(!!!input$variables1) |> 
                  rename_all(toupper),  # Use the full dataset without subsetting columns
                options = list(
                  scrollX = TRUE,  # Enable horizontal scrolling
                  pageLength = 10  # Set number of rows per page
                )
      )
    }
    
  
  })
  
  
  

  adsl_df <-  reactive({ 
    req(selected_data())
    adsl2 <- data_storage()[['adsl']] |> rename_all(tolower)
    req(adsl2)  # Ensure dataset exists
    req(input$subj)
    adsl2 %>% #rename_all(toupper) |> 
      filter(usubjid==input$subj) |> mutate(group=1)
       
    })

# adae server -------------------------------------------------------------

  
  adae2 <-  reactive({ 
    req(selected_data())

    adae <- data_storage()[['adae']] %>% rename_all(tolower) %>% 
      mutate(astdt=as.Date(astdt, format='%d%B%Y'),trtsdt=as.Date(trtsdt, format='%d%B%Y'),trtedt=as.Date(trtedt, format='%d%B%Y')
             # aendt2=ifelse((is.na(aendt) & astdt>=trtsdt & !is.na(trtedt)), trtedt, as.Date(aendt, format='%d%B%Y')),
             # aendt2=as.Date(aendt2, origin = "1970-01-01")
             ) %>% 
      select(usubjid, trtsdt, trtedt, astdt, aendt, #aendt2, 
             aedecod) %>% rename(content=aedecod, start=astdt, end=aendt) |> 
      mutate(group=2)
    
    
    adsl3 <- adsl_df() |>  rename(content=trt01a, start=trtsdt, end=trtedt) |> mutate(style = c("color: red;background:yellow;"))
    
    adae <- bind_rows(adae,adsl3)
    
    # usubjid <- unique(adae$usubjid)
    
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
        editable = FALSE,
        align = "center",
        orientation = "top",
        snap = NULL,
        margin = list(item = 30, axis = 50)
      )
      adae3 <- adae2() %>% filter(usubjid==input$subj)
      timevis(adae3, zoomFactor = 1, options = config, groups = data.frame(id=c(1:2), content=c('Treatment', 'Adverse Events')))
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
      
      # Get unique ATPT values in a consistent order
      atpt_levels <- advs1() %>% filter(param == input$para) %>% pull(atpt) %>% unique() %>% sort()
      
      N <- max(3, length(atpt_levels))  
      colors <- setNames(brewer.pal(N, "Set2")[1:length(atpt_levels)], atpt_levels)  # Create a named vector of colors
 
      
      # Initialize shapes list (empty by default)
      shapes_list <- list()
      
      # If checkbox is selected, add a baseline reference line
      if (input$vchkb & input$yaxis == 'aval') {
        baseline <- advs1() %>% filter(ablfl == 'Y') |> arrange(desc(atpt))
        
        if (nrow(baseline) > 0) {
          
          # Create a list of reference lines for each group
          shapes_list <- lapply(seq_along(unique(baseline$atpt)), function(i) {
            group <- unique(baseline$atpt)[i]
            baseline_value <- min(baseline |> arrange(atpt) %>% filter(atpt == group) %>% pull(input$yaxis), na.rm = TRUE)
            
            list(
              type = "line",
              y0 = baseline_value,
              y1 = baseline_value,
              x0 = 0,   # Spans the full width of the plot
              x1 = 1,   # Spans the full width of the plot
              xref = "paper",
              yref = "y",
              line = list(color =  colors[group], width = 2, dash = "dash")
            )
          })
        }
      }
      
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
    
    # Reactive to capture the index of the clicked point.
    selectedPointadvs <- reactiveVal(NULL)
    
    # Observe plotly_click events to update the selected point.
    observeEvent(event_data("plotly_click"), {
      clickData <- event_data("plotly_click")
      if (!is.null(clickData)) {
        # Plotly returns 0-indexed pointNumber. Convert to 1-indexed.
        idx <- as.numeric(clickData$pointNumber) + 1  
        # print(class(clickData$pointNumber))
        selectedPointadvs(idx)
      }
    })
    
    output$advs2 <- renderDT({
      req(!is.null(selectedPointadvs()), advs1())  # Ensure dataset exists
      
      datatable(advs1()[selectedPointadvs(),, drop=FALSE],  # Use the full dataset without subsetting columns
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
      if (input$lchkb & input$lyaxis == 'aval') {
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
    

    
    # Reactive to capture the index of the clicked point.
    selectedPoint <- reactiveVal(NULL)
    
    # Observe plotly_click events to update the selected point.
    observeEvent(event_data("plotly_click"), {
      clickData <- event_data("plotly_click")
      if (!is.null(clickData)) {
        # Plotly returns 0-indexed pointNumber. Convert to 1-indexed.
        idx <- as.numeric(clickData$pointNumber) + 1  
        # print(class(clickData$pointNumber))
        selectedPoint(idx)
      }
    })
    

    
      
    
    output$adlb2 <- renderDT({
      req(!is.null(selectedPoint()), adlb1())
    
      datatable(adlb1()[selectedPoint(), ,drop=FALSE],  # Use the full dataset without subsetting columns
                options = list(
                  scrollX = TRUE,  # Enable horizontal scrolling
                  pageLength = 10  # Set number of rows per page
                )
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
