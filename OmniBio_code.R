#List of required packages
required_packages <- c("shiny", "readxl", "xlsx", "dplyr", "tidyr", "ggplot2", "gcplyr", "bslib", "shinyjs", "viridis")

#Function to install packages if they are not available
install_if_missing <- function(packages) {
  to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(to_install) > 0) {
    install.packages(to_install, dependencies = TRUE)
  }
}

#Execute the function to install required packages
install_if_missing(required_packages)

#Load needed libraries
library(shiny)
library(readxl)
library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gcplyr)
library(bslib)
library(shinyjs)
library(viridis)

#User Interfase
ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"),
  
  #Conditional Buttons
  useShinyjs(),
  
  titlePanel("OmniBio: An easy-to-use kinetic parameters calculator"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "input_program",
        label = "Select your microplate reader software: ",
        choices = list("Sinergy HTX/Cytation - Gen5" = "method_gen", "Tecan Infinite - iControl" = "method_icont"),
        selected = "method_gen"
      ),
      fileInput("excel_file", "Upload your Optical Density measurements: (Excel file)", accept = c(".xlsx", ".xls")),
      uiOutput("sheet_selector"),
      textInput("new_sheet_name", "Enter new sheet name", value = ""),
      numericInput("time_measurement", "Please enter the time between measurements of your experiment: (fraction of hours)", value = ""),
      conditionalPanel(
        condition = "input.input_program == 'method_icont'",
        numericInput("label", "*Enter the number of the Label where the Optical Density data is located:", value = "")
      ),
      fileInput("csv_file", "Upload your experimental design: (Excel file)", accept = c(".xlsx", "xls")),
      textInput("metadata_block_names", "Specify the specie of your microorganism"),
      actionButton("process_data", "Process"),
      downloadButton("download_data", "Download your kinetic parameters"),
      downloadButton("download_plots", "Download your growth curve plots", class = "btn-sm"),
      downloadButton("download_lag_plots", "Download your lag phase plots", class = "btn-sm"),
      downloadButton("download_max_percap_plot", "Download your µmax plots", class = "btn-sm"),
      downloadButton("download_performance_plot", "Download your Performance plots", class = "btn-sm"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("HOW TO USE?", p("This app helps you to avoid using the big chunk of code
                                   when using gcplyr (Blazanin, M. gcplyr:
                                   an R package for microbial growth curve data analysis.
                                   BMC Bioinformatics 25, 232 (2024).
                                   https://doi.org/10.1186/s12859-024-05817-3)
                                   to calculate kinetic parameters. In the side panel, you need to load
                                   an Excel file only with the output provided by Gen5 or iControl software,
                                   as well as your experimental design (metadata) in Excel format, too.
                                   When you finally load all your data, push the PROCESS button and
                                   all the calculations will be performed.
                                   *Important: note that when choosing iControl software, the Label panel is available
                                   and mandatory for performing the calculations*
                                   Thanks for using our app!")),
        tabPanel("Growth curve checker", p("In this tab you will see a plot
                                           that contains all of your growth curves per well.
                                           Use it to check all the replicates and the success of
                                           your experiment."), plotOutput("growth_curve")),
        tabPanel("Results", p("Here are your results of the calculations made for kinetic parameters."),
                 tabsetPanel(
                   tabPanel("Lag time data", p("In this tab is shown the first rows
                                               of your data when calculating lag phase time.
                                               Below, it is represented the plots of the growth curves,
                                               where the dashed line is the cut where lag phase ended."),
                            tableOutput("lag"), plotOutput("lag_time_plot")),
                   tabPanel("µmax calculations", p("max_percap corresponds to the
                                                   maximum growth rate (µmax) ocurring in a culture media.
                                                   In this app, the µmax is calculated via the derivate
                                                   of the growth in its maximum point. The table shows the first rows of the data,
                                                   where max_percap corresponds to max_percap.
                                                   The plots shown here are
                                                   a representation of that derivate, where the red dot is the maximum
                                                   point."),
                            tableOutput("max_percap"), plotOutput("max_percap_plot")),
                   tabPanel("Performance", p("Performance is another way to say maximum optical density.
                                             Here its shown the table of that calculations, and also plots, where
                                             the red dot shows the point of saturation of cells in the culture media."),
                            tableOutput("ODmax"), plotOutput("ODmax_plot")),
                   tabPanel("Area Under the Curve", p("Area Under the Curve (AUC) is just all the area
                                                      below the growth curve."),
                            tableOutput("AUC")),
                   tabPanel("Table Summary", p("Here its shown the first rows
                                               of all the calculated data, which you can download
                                               as an Excel file, using the button in the side panel."),
                            tableOutput("all"))
                 )),
        tabPanel("Stored Results", p("Here you can view the stored results for each sheet."),
                 selectInput("stored_sheet", "Select Stored Sheet", choices = NULL),
                 tabsetPanel(
                   tabPanel("Table summary", tableOutput("stored_results_data")),
                   tabPanel("Growth Curves", plotOutput("stored_results_growth_curve")),
                   tabPanel("Lag Time Plot", plotOutput("stored_results_lag_plot")),
                   tabPanel("µmax Plot", plotOutput("stored_results_max_percap_plot")),
                   tabPanel("Performance Plot", plotOutput("stored_results_ODmax_plot"))
                 )
        ),
        tabPanel("Heatmaps by Species",
                 plotOutput("lag_heatmap_species"),
                 plotOutput("AUC_heatmap_species"),
                 plotOutput("odmax_heatmap_species"),
                 plotOutput("max_percap_heatmap_species")
        )
      )
    )
  )
)

#Server logic
server <- function(input, output, session) {
  
  processed_data <- reactiveVal(FALSE)
  stored_results <- reactiveValues()
  
  #Disable download buttons
  observe({
    if (!is.null(input$excel_file) && !is.null(input$csv_file) && processed_data()) {
      shinyjs::enable("download_data")
      shinyjs::enable("download_plots")
      shinyjs::enable("download_lag_plots")
      shinyjs::enable("download_max_percap_plot")
      shinyjs::enable("download_performance_plot")
    } else {
      shinyjs::disable("download_data")
      shinyjs::disable("download_plots")
      shinyjs::disable("download_lag_plots")
      shinyjs::disable("download_max_percap_plot")
      shinyjs::disable("download_performance_plot")
    }
  })
  
  #Reactive for Excel file and obtain sheet names
  sheet_names <- reactive({
    req(input$excel_file)
    excel_sheets(input$excel_file$datapath)
  })
  
  #UI to select the Excel sheet
  output$sheet_selector <- renderUI({
    req(sheet_names())
    selectInput("selected_sheet", "Select Sheet", choices = sheet_names())
  })
  
  
  #Reactive for file upload
  raw_data <- reactive({
    req(input$excel_file, input$selected_sheet)
    if (input$input_program == "method_gen") {
      data <- read_excel(input$excel_file$datapath, col_names = F, sheet = input$selected_sheet)
      time_pos <- which(data == "Time", arr.ind = T)
      col_names <- as.list(data[time_pos[2,1],2:ncol(data)])
      data <- data[(time_pos[2,1] + 1):nrow(data),
                   (time_pos[2,2]:ncol(data))]
      colnames(data) <- col_names
      data$Time <- seq(0, nrow(data)-1, by = input$time_measurement)
      data <- data[,-2]
      data <- na.omit(data)
      return(data)
    } else if (input$input_program == "method_icont") {
      tecan_out <- read_excel(input$excel_file$datapath, col_names = FALSE, sheet = input$selected_sheet)
      #Find "Label" position
      label_pos <- grep("^Label", tecan_out$...1)
      
      #Handle less than 2 or more Label cases
      start_pos <- label_pos[1]
      end_pos <- if (length(label_pos) > 1) {
        label_pos[2] - 2  
      } else {
        nrow(tecan_out)  
      }
      label_x <- tecan_out[start_pos:end_pos,]
      
      label_x <- label_x[which(label_x == "A1"):nrow(label_x),]
      row.names(label_x) <- label_x$...1
      label_x <- data.frame(t(label_x))
      label_x <- label_x[-1,]
      label_x <- na.omit(label_x)
      
      #Create a Time column
      label_x <- cbind(Time = seq(0, nrow(label_x) - 1, by = 1), label_x)
      
      row.names(label_x) <- seq(1, nrow(label_x))

      return(label_x)
    }
    
  })
  
  #Reactive for metadata file upload
  experimental_design <- reactive({
    req(input$csv_file)
    metadata <- import_blockdesigns(input$csv_file$datapath, block_names = input$metadata_block_names)

    return(metadata)
  })
  
  #Transform data to tidy format
  tidy_data <- eventReactive(input$process_data, {
    req(raw_data())
    data_tidy <- trans_wide_to_tidy(raw_data(), id_cols = c("Time"))

    return(data_tidy)
  })
  
  #Merge data
  merge_raw <- eventReactive(input$process_data, {
    req(tidy_data(), experimental_design()) 
    merge_data <- merge_dfs(tidy_data(), experimental_design())
    merge_data <- merge_data %>%
      na.omit() %>%
      mutate(Well = toupper(Well),
             Well = factor(Well, levels = paste0(rep(LETTERS[1:8], each = 12), 1:12))) %>%
      mutate(Time = as.numeric(Time))

    return(merge_data)
  })
  
  #Plotting growth curves
  growth_curve <- eventReactive(input$process_data, {
    req(merge_raw())
    ggplot(data = merge_raw(), aes(x = Time, y = Measurements, group = Well)) +
      geom_line() +
      facet_wrap(~Well, nrow = 8, ncol = 12) +
      labs(title = "Growth curves for each well")
  })
  
  #Kinectic derivatives calculations 
  deriv_cin <- eventReactive(input$process_data, {
    req(merge_raw(), input$metadata_block_names) 
    deriv <- merge_raw() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      mutate(deriv = calc_deriv(x = Time, y = Measurements))

    return(deriv)
  })
  percap <- eventReactive(input$process_data, {
    req(deriv_cin(), input$metadata_block_names)
    percap_cin <- deriv_cin() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      mutate(deriv_percap = calc_deriv(x = Time, y = Measurements,
                                       percapita = TRUE, blank = 0))

    return(percap_cin)
  })
  doub_ti <- eventReactive(input$process_data, {
    req(percap(), input$metadata_block_names)
    doub_times <- percap() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      mutate(doub_time = doubling_time(y = deriv_percap))

    return(doub_times)
  })
  
  #Kinetic Parameters
  ##Lag time
  lagtime <- eventReactive(input$process_data, {
    req(doub_ti(), input$metadata_block_names)
    lag_data <- doub_ti() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      summarize(lag_time = lag_time(y = Measurements, x = Time,
                                    deriv = deriv_percap))

    return(lag_data)
  })
  
  ##Plot lag
  lag_time_plot <- eventReactive(input$process_data, {
    req(lagtime(), doub_ti())
    lag_data <- lagtime()
    ggplot(doub_ti(), aes(x = Time, y = Measurements)) +
      geom_line() +
      facet_wrap(~Well, nrow = 8, ncol = 12) +
      geom_vline(data = lag_data, aes(xintercept = lag_time), lty = 2) +
      labs(title = "Lag time of each strain", caption = "Vertical lines represent the time when the lag time ends")
  })
  
  ##max_percap
  max_percap <- eventReactive(input$process_data, {
    req(doub_ti(), input$metadata_block_names)
    max_percap_data <- doub_ti() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      summarize(max_percap = max_gc(deriv_percap, na.rm = TRUE),
                max_percap_time = extr_val(Time, which_max_gc(deriv_percap)),
                doub_time = doubling_time(y = max_percap)) # Esta línea parece incorrecta, doub_time debe calcularse con max_percap

    return(max_percap_data)
  })
  
  ##max_percap plot
  max_percap_plot <- eventReactive(input$process_data, {
    req(max_percap(), doub_ti())
    max_percap_data <- max_percap()
    ggplot(data = doub_ti(),
           aes(x = Time, y = deriv_percap)) +
      geom_line() +
      facet_wrap(~Well) +
      geom_point(data = max_percap_data,
                 aes(x = max_percap_time, y = max_percap),
                 size = 2, color = "red") +
      coord_cartesian(ylim = c(-1, NA)) +
      labs(title = "µmax of each strain")
  })
  
  ##OD max
  ODmax <- eventReactive(input$process_data, {
    req(doub_ti(), input$metadata_block_names)
    odmax_data <- doub_ti() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      summarize(max_dens = max_gc(Measurements, na.rm = TRUE),
                max_time = extr_val(Time, which_max_gc(Measurements)))

    return(odmax_data)
  })
  
  ODmax_plot <- eventReactive(input$process_data, {
    req(doub_ti(), ODmax())
    ODmax_data <- ODmax()
    ggplot(data = doub_ti(),
           aes(x = Time, y = Measurements)) +
      geom_line() +
      facet_wrap(~Well) +
      geom_point(data = ODmax_data,
                 aes(x = max_time, y = max_dens),
                 size = 2, color = "red") +
      labs(title = "OD max reached by strain", y = "Optical Density at 600 nm")
  })
  
  ##AUC
  AUC <- eventReactive(input$process_data, {
    req(doub_ti(), ODmax())
    doub_ti() %>% group_by(Well, !!sym(input$metadata_block_names)) %>% 
      summarize(auc = auc(x = Time, y = Measurements))
  })
  
  ##Merge of every kinetic parameters
  all_data <- eventReactive(input$process_data, {
    req(lagtime(), max_percap(), ODmax(), AUC(), input$metadata_block_names)
    lag_data <- lagtime()
    max_percap_data <- max_percap()
    ODmax_data <- ODmax()
    AUC_data <- AUC()
    data.frame(lag_data, AUC = AUC_data$auc, ODmax = ODmax_data$max_dens, µmax = max_percap_data$max_percap)

  })
  
  observeEvent(input$process_data, {
    if (!is.null(merge_raw()) && nrow(merge_raw()) > 0) {
      processed_data(TRUE)
      
      new_sheet_label <- input$new_sheet_name
      if (trimws(new_sheet_label) == "") {
        new_sheet_label <- input$selected_sheet
      }
      
      #Create a list to save the results of the actual sheet
      current_results <- list(
        data = all_data(), 
        growth_curve = growth_curve(),
        lag_plot = lag_time_plot(),
        max_percap_plot = max_percap_plot(),
        ODmax_plot = ODmax_plot()
      )
      
      #Save the list
      stored_results[[new_sheet_label]] <- current_results
      updateSelectInput(session, "stored_sheet", choices = names(stored_results))
      updateTextInput(session, "new_sheet_name", value = "")
    } else {
      processed_data(FALSE)
    }
  })
  
  observeEvent(input$process_data, {
    withProgress(message = "Processing data...", value = 0, {
      
      incProgress(0.1, detail = "Reading raw data")
      raw_data()
      
      incProgress(0.2, detail = "Tidying data")
      tidy_data()
      
      incProgress(0.4, detail = "Merging with metadata")
      merge_raw()
      
      incProgress(0.6, detail = "Calculating derivatives")
      deriv_cin()
      percap()
      doub_ti()
      
      incProgress(0.8, detail = "Calculating kinetic parameters")
      growth_curve()
      max_percap()
      max_percap_plot()
      lagtime()
      lag_time_plot()
      ODmax()
      ODmax_plot()
      AUC()
      
      incProgress(1, detail = "Done!")
    })
  })
  
  #Enable buttons post-calculations
  observe({
    if (processed_data()) {
      shinyjs::enable("download_data")
      shinyjs::enable("download_plots")
      shinyjs::enable("download_lag_plots")
      shinyjs::enable("download_max_percap_plot")
      shinyjs::enable("download_performance_plot")
    } else {
      shinyjs::disable("download_data")
      shinyjs::disable("download_plots")
      shinyjs::disable("download_lag_plots")
      shinyjs::disable("download_max_percap_plot")
      shinyjs::disable("download_performance_plot")
    }
  })
  
  #Reactive to generate the kinectic parameters tables
  parameter_tables <- reactive({
    req(stored_results, input$metadata_block_names)
    
    lag_table <- NULL
    AUC_table <- NULL
    odmax_table <- NULL
    max_percap_table <- NULL
    
    species_col_name <- input$metadata_block_names
    
    for (sheet_name in names(stored_results)) {
      data <- stored_results[[sheet_name]]$data
      if (!is.null(data) && nrow(data) > 0) {
        
        
        if (!(species_col_name %in% names(data))) {
          warning(paste("Column", species_col_name, "not found in stored data for sheet", sheet_name, ". Skipping this sheet for heatmap averaging."))
          next 
        }
        
        lag_temp <- data %>% select(Well, !!sym(species_col_name), lag_time) %>%
          rename(value = lag_time)
        colnames(lag_temp)[3] <- sheet_name
        
        AUC_temp <- data %>% select(Well, !!sym(species_col_name), AUC) %>%
          rename(value = AUC)
        colnames(AUC_temp)[3] <- sheet_name
        
        odmax_temp <- data %>% select(Well, !!sym(species_col_name), ODmax) %>%
          rename(value = ODmax)
        colnames(odmax_temp)[3] <- sheet_name
        
        max_percap_temp <- data %>% select(Well, !!sym(species_col_name), µmax) %>%
          rename(value = µmax)
        colnames(max_percap_temp)[3] <- sheet_name
        
        if (is.null(lag_table)) {
          lag_table <- lag_temp
          AUC_table <- AUC_temp
          odmax_table <- odmax_temp
          max_percap_table <- max_percap_temp
        } else {
          
          lag_table <- merge(lag_table, lag_temp, by = c("Well", species_col_name), all = TRUE)
          AUC_table <- merge(AUC_table, AUC_temp, by = c("Well", species_col_name), all = TRUE)
          odmax_table <- merge(odmax_table, odmax_temp, by = c("Well", species_col_name), all = TRUE)
          max_percap_table <- merge(max_percap_table, max_percap_temp, by = c("Well", species_col_name), all = TRUE)
        }
      }
    }

    return(list(lag = lag_table, AUC = AUC_table, odmax = odmax_table, max_percap = max_percap_table))
  })
  
  #Function to calculate the average of each parameter
  average_by_species <- function(data, species_col) {
    req(data, species_col %in% names(data), nrow(data) > 0)
    
    averaged_data <- data %>%
      group_by(!!sym(species_col)) %>% 
      summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
      ungroup() 

    
    if (nrow(averaged_data) > 0 && species_col %in% names(averaged_data)) {
     
      averaged_data_df <- as.data.frame(averaged_data)
      
      
      rownames(averaged_data_df) <- averaged_data_df[[species_col]]
      
      
      averaged_data_df <- averaged_data_df %>% select(-!!sym(species_col)) 
      

      averaged_data <- averaged_data_df
      
    } else {
      print(paste("Warning: Column", species_col, "not found in averaged_data or averaged_data is empty."))
    }
    
 
    result_matrix <- as.matrix(averaged_data)
   
    return(result_matrix)
  }
  
  
  averaged_parameters_by_species <- reactive({
    req(parameter_tables(), input$metadata_block_names, experimental_design())
    
    species_col <- input$metadata_block_names
    return(list(
      lag = average_by_species(parameter_tables()$lag, species_col),
      AUC = average_by_species(parameter_tables()$AUC, species_col),
      odmax = average_by_species(parameter_tables()$odmax, species_col),
      max_percap = average_by_species(parameter_tables()$max_percap, species_col)
    ))
  })
  
  
  #Function to render heatmap
  render_parameter_heatmap <- function(data, parameter_name) {
    renderPlot({
      req(data)

      heatmap(data(),
              main = paste("Heatmap of Average", parameter_name, "by Species"),
              xlab = "Processed Sheets",
              ylab = input$metadata_block_names,
              col = viridis::viridis(256),
              margins = c(10, 10)
      )
    })
  }
  
  #Render heatmaps
  output$lag_heatmap_species <- render_parameter_heatmap(
    reactive({
    data <- averaged_parameters_by_species()$lag
    if (!is.null(data) && ncol(data) > 0) {
      return(scale(data))
    } else {
      return(NULL)
    }
    }), "Lag Time (Z-score)")
  
  output$AUC_heatmap_species <- render_parameter_heatmap(
    reactive({
      data <- averaged_parameters_by_species()$AUC
      if (!is.null(data) && ncol(data) > 0) {
        return(scale(data))
      } else {
        return(NULL)
      }
    }), "AUC (Z-score)")
  
  output$odmax_heatmap_species <- render_parameter_heatmap(
    reactive({
      data <- averaged_parameters_by_species()$odmax
      if (!is.null(data) && ncol(data) > 0) {
        return(scale(data))
      } else {
        return(NULL)
      }
    }), "ODmax (Z-score)")
  
  output$max_percap_heatmap_species <- render_parameter_heatmap(
    reactive({
      data <- averaged_parameters_by_species()$max_percap
      if (!is.null(data) && ncol(data) > 0) {
        return(scale(data))
      } else {
        return(NULL)
      }
    }), "µmax (Z-score)")
  
  #Show original data
  output$growth_curve <- renderPlot({
    req(growth_curve())
    growth_curve()
  })
  
  output$lag_time_plot <- renderPlot({
    req(lag_time_plot())
    lag_time_plot()
  })
  
  output$max_percap_plot <- renderPlot({
    req(max_percap_plot())
    max_percap_plot()
  })
  
  output$deriv <- renderTable({
    req(doub_ti())
    head(doub_ti())
  })
  
  output$lag <- renderTable({
    req(lagtime())
    head(lagtime())
  })
  
  output$max_percap <- renderTable({
    req(max_percap())
    head(max_percap())
  })
  
  output$ODmax <- renderTable({
    req(ODmax())
    head(ODmax())
  })
  
  output$AUC <- renderTable({
    req(AUC())
    head(AUC())
  })
  
  output$all <- renderTable({
    req(all_data())
    head(all_data())
  })
  
  output$ODmax_plot <- renderPlot({
    req(ODmax_plot())
    ODmax_plot()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Kinetic_parameters_", input$metadata_block_names, "_", input$new_sheet_name, ".xlsx", sep = "")
    },
    content = function(file) {
      req(all_data())
      write.xlsx(all_data(), file, row.names = FALSE)
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste("Growth_curve_plots_", input$metadata_block_names, "_", input$new_sheet_name, ".pdf", sep = "")
    },
    content = function(file) {
      req(growth_curve())
      ggsave(file, plot = growth_curve(), device = "pdf", width = 15, height = 10)
    }
  )
  
  output$download_lag_plots <- downloadHandler(
    filename = function() {
      paste("lag_plots_", input$metadata_block_names, "_", input$new_sheet_name, ".pdf", sep = "")
    },
    content = function(file) {
      req(lag_time_plot())
      ggsave(file, plot = lag_time_plot(), device = "pdf", width = 15, height = 10)
    }
  )
  
  output$download_max_percap_plot <- downloadHandler(
    filename = function() {
      paste("max_percap_plots_", input$metadata_block_names, "_", input$new_sheet_name, ".pdf", sep = "")
    },
    content = function(file) {
      req(max_percap_plot())
      ggsave(file, plot = max_percap_plot(), device = "pdf", width = 15, height = 10)
    }
  )
  
  output$download_performance_plot <- downloadHandler(
    filename = function() {
      paste("Performance_plots_", input$metadata_block_names, "_", input$new_sheet_name, ".pdf", sep = "")
    },
    content = function(file) {
      req(ODmax_plot())
      ggsave(file, plot = ODmax_plot(), device = "pdf", width = 15, height = 10)
    }
  )
  
  output$stored_results_table <- renderTable({
    req(input$stored_sheet)
    head(stored_results[[input$stored_sheet]])
  }
  )
  
  output$stored_results_data <- renderTable({
    req(input$stored_sheet, stored_results[[input$stored_sheet]]$data)
    head(stored_results[[input$stored_sheet]]$data)
  })
  
  output$stored_results_growth_curve <- renderPlot({
    req(input$stored_sheet, stored_results[[input$stored_sheet]]$growth_curve)
    stored_results[[input$stored_sheet]]$growth_curve
  })
  
  output$stored_results_lag_plot <- renderPlot({
    req(input$stored_sheet, stored_results[[input$stored_sheet]]$lag_plot)
    stored_results[[input$stored_sheet]]$lag_plot
  })
  
  output$stored_results_max_percap_plot <- renderPlot({
    req(input$stored_sheet, stored_results[[input$stored_sheet]]$max_percap_plot)
    stored_results[[input$stored_sheet]]$max_percap_plot
  })
  
  output$stored_results_ODmax_plot <- renderPlot({
    req(input$stored_sheet, stored_results[[input$stored_sheet]]$ODmax_plot)
    stored_results[[input$stored_sheet]]$ODmax_plot
  })
}

#Run the app
shinyApp(ui = ui, server = server)
