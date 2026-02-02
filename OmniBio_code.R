#List of required packages
required_packages <- c("shiny", "readxl", "xlsx", "dplyr", "tidyr", "ggplot2", "gcplyr", "bslib", "shinyjs", "viridis", "lubridate")

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
library(lubridate)

#User Interfase
ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"),
  
  #Conditional Buttons
  useShinyjs(),
  
  titlePanel("OmniBio: An easy-to-use kinetic parameters calculator"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "file_type",
        label = "Select the file type/extension where your date is saved: ",
        choices = list(".CSV" = "csv_type", ".TSV" = "tsv_type", "EXCEL" = "excel_type")
      ),
      conditionalPanel(
        condition = "input.file_type == 'csv_type'",
        fileInput("csv_file", "Upload your Optical Density measurements: (.csv file)", accept = ".csv")
      ),
      conditionalPanel(
        condition = "input.file_type == 'tsv_type'",
        fileInput("tsv_file", "Upload your Optical Density measurements: (.tsv file)", accept = ".tsv")
      ),
      conditionalPanel(
        condition = "input.file_type == 'excel_type'",
        fileInput("excel_file", "Upload your Optical Density measurements: (Excel file)", accept = c(".xlsx", ".xls")),
        uiOutput("sheet_selector"),
        textInput("new_sheet_name", "Enter new sheet name", value = ""),
        textInput("range_excel", "Specify the start/end of rows and columns of your data (Example B24:CU73)", value = "B24:CU73")
      ),
      fileInput("metadata_file", "Upload your experimental design: (Excel file)", accept = c(".xlsx", "xls")),
      numericInput("blank", "Add the absorbance of your blank solution", value = 0),
      radioButtons(
        inputId = "noisy",
        label = "Is your data noisy or have density close to zero? Try out this smoothing option.",
        choices = list("Yes" = "noisy_yes", "No" = "noisy_no"),
        selected = "noisy_no"
      ),
      conditionalPanel(
        condition = "input.noisy == 'noisy_yes'",
        sliderInput(inputId = "noisy_data",
                    label = "Choose between these different windows options: (3 or 5 is often a good default)",
                    min = 3,
                    max = 9,
                    value = 3, 
                    step = 2)
      ),
      checkboxInput("use_time_limit", "Set a time limit for calculation (AUC/ODmax)?", value = FALSE),
      conditionalPanel(
        condition = "input.use_time_limit == true",
        numericInput("time_limit_val", "Calculate up to (hours): (This time must be the maximum value of the last point of measurement)", value = 24, min = 0)
      ),
      textInput("metadata_block_names", "Specify a Plate Label (ex. microorganism specie or culture condition)"),
      actionButton("process_data", "Process"),
      downloadButton("download_data", "Download your kinetic parameters"),
      downloadButton("download_plots", "Download your growth curve plots", class = "btn-sm"),
      downloadButton("download_lag_plots", "Download your lag phase plots", class = "btn-sm"),
      downloadButton("download_max_percap_plot", "Download your µmax plots", class = "btn-sm"),
      downloadButton("download_performance_plot", "Download your Performance plots", class = "btn-sm"),
      downloadButton("download_all_heatmaps", "Download All Heatmaps", class = "btn-sm"),
      
    
    hr(),
    h4("Activity Log"),
    verbatimTextOutput("app_log"),
    tags$style(type="text/css", "#app_log { height: 200px; overflow-y: scroll; font-size: 11px; color: #333; background-color: #f5f5f5; border: 1px solid #ccc; }")
  ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("HOW TO USE?", p("This app helps you to avoid using the big chunk of code
                                   when using gcplyr R package", 
                                  a("Blazanin, M. gcplyr:
                                   an R package for microbial growth curve data analysis.
                                   BMC Bioinformatics 25, 232 (2024)", href = "https://doi.org/10.1186/s12859-024-05817-3",
                                    target = "_blank"),
                                  "to calculate kinetic parameters. See this tutorial to know how to use the app",
                                  a("(Video Tutorial)", href = "https://youtu.be/gnTkjyRKTRU",
                                    target = "_blank"),
                                  br(),
                                  "In the side panel, you need to load
                                   a file in .CSV, .TSV, or Excel format with the output provided by your favourite plate reader,
                                   as well as your experimental design (metadata) as an Excel file.
                                   When you finally load all your data, push the PROCESS button and
                                   all the calculations will be performed.
                                   Below this text, you can find figures of how your data should look like, as well as your experimental design.
                                   See our example data, click the link below to download them.",
                                  br(),
                                  a("Example data", href = "https://drive.google.com/drive/folders/1ttOatQ1jJVrthyMaTp1FP7v-6P0tubG6?usp=sharing", 
                                    target = "_blank"),
                                  br(),
                                  "Thanks for using our app!",
                                  br(),
                                  "Need to run the app locally? Dowload our source code!",
                                  br(),
                                  a("Link to the source code", href = "https://github.com/sidehnhardt/OmniBio/blob/main/OmniBio_code.R",
                                    target = "_blank")),
                 br(),
                 tags$img(src = "Measurements.png", height = "180px", width = "516px"),
                 br(),
                 strong("Figure 1. This is how your measurements should look like, either is separated by commas, tabs, or is in Excel format.
                        Please, ignore the Temperature column, since it is not mandatory for the app to work."),
                 br(),
                 tags$img(src = "Metadata.png", height = "127px", width = "650px"),
                 br(),
                 strong("Figure 2. This is how your experimental design in Excel file should look like. Colors are irrelevant for the app.
                        Please, fill all the blanks within the metadata.")),
        tabPanel("Growth curve checker", p("In this tab you will see a plot
                                           that contains all of your growth curves per well.
                                           Use it to check all the replicates and the success of
                                           your experiment."), plotOutput("growth_curve")),
        tabPanel("Results", p("Here you can view the stored results for each data calculated."),
                 selectInput("stored_sheet", "Select Stored Sheet", choices = NULL),
                 tabsetPanel(
                   tabPanel("Table summary",
                            p("Here its shown the first rows
                                               of all the calculated data, which you can download
                                               as an Excel file, using the button in the side panel."), 
                            tableOutput("stored_results_data")),
                   tabPanel("Growth Curves", plotOutput("stored_results_growth_curve")),
                   tabPanel("Lag Time Plot",
                            p("In this tab you can find your Lag Time Phase plot, with a dashed line which cut the lag phase, as well as the slope 
                              (red line) to calculate the lag time."),
                            plotOutput("stored_results_lag_plot")),
                   tabPanel("µmax Plot",
                            p("This is the plot of the maximum growth rate (µmax) ocurring in a culture media. The µmax is calculated via the derivate of the growth in its maximum point.
                            The plots shown here are a representation of that derivate, where the red dot is the maximum point."),
                            plotOutput("stored_results_max_percap_plot")),
                   tabPanel("Performance Plot",
                            p("Performance is another way to say maximum optical density.
                            Here its shown the plots, where the red dot shows the point of saturation of cells in the culture media."), 
                            plotOutput("stored_results_ODmax_plot"))
                 )
        ),
        tabPanel("Heatmaps by Plates",
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
  
  #Loggin system start
  log_text <- reactiveVal(paste(Sys.time(), "- System ready. Waiting for inputs..."))
  
  log_msg <- function(message, type = "INFO") {
    current_log <- log_text()
    new_entry <- paste0(format(Sys.time(), "%H:%M:%S"), " [", type, "]: ", message)
    log_text(paste(new_entry, current_log, sep = "\n"))
  }
  
  output$app_log <- renderText({
    log_text()
  })
  
  #Disable download buttons
  observe({
    if (processed_data()) {
      shinyjs::enable("download_data")
      shinyjs::enable("download_plots")
      shinyjs::enable("download_lag_plots")
      shinyjs::enable("download_max_percap_plot")
      shinyjs::enable("download_performance_plot")
      shinyjs::enable("download_all_heatmaps")
    } else {
      shinyjs::disable("download_data")
      shinyjs::disable("download_plots")
      shinyjs::disable("download_lag_plots")
      shinyjs::disable("download_max_percap_plot")
      shinyjs::disable("download_performance_plot")
      shinyjs::disable("download_all_heatmaps")
    }
  })
  
  #Reactive for Excel file and obtain sheet names
  sheet_names <- reactive({
    req(input$excel_file)
    tryCatch({
      excel_sheets(input$excel_file$datapath)
    }, error = function(e) {
      log_msg("Error reading Excel sheets. File might be corrupted.", "ERROR")
      return(NULL)
    })
  })
  
  #UI to select the Excel sheet
  output$sheet_selector <- renderUI({
    req(sheet_names())
    selectInput("selected_sheet", "Select Sheet", choices = sheet_names())
  })
  
  
  #Reactive for file upload
  raw_data <- reactive({
    req(input$file_type)
    
    tryCatch({
      data <- NULL
      log_msg(paste("Reading OD file:", input$file_type), "INFO")
      
      if (input$file_type == "excel_type") {
        req(input$excel_file, input$selected_sheet)
        data <- read_excel(input$excel_file$datapath, range = input$range_excel, sheet = input$selected_sheet) 
        data$Time <- as.numeric(as.duration(data$Time - first(data$Time)))/3600
        
      } else if (input$file_type == "csv_type") {
        req(input$csv_file)
        data <- read.csv(input$csv_file$datapath)
        data$Time <- data$Time %>% hms() %>% as.duration() %>% as.numeric()/3600
        
      } else if (input$file_type == "tsv_type") {
        req(input$tsv_file)
        data <- read.delim(input$tsv_file$datapath)
        data$Time <- data$Time %>% hms() %>% as.duration() %>% as.numeric()/3600
      }
      
      log_msg("OD Data loaded successfully.", "SUCCESS")
      return(data)
      
    }, error = function(e) {
      log_msg("Error reading OD file. Check format/columns.", "ERROR")
      # We do NOT show a popup here to avoid spamming, the Log is enough
      return(NULL)
    })
  })
  
  #Reactive for metadata file upload
  experimental_design <- reactive({
    req(input$metadata_file)
    
    tryCatch({
      log_msg("Reading Metadata file...", "INFO")
      metadata <- import_blockdesigns(input$metadata_file$datapath, block_names = input$metadata_block_names)
      
      if (nrow(metadata) == 0) stop("Empty metadata")
      
      log_msg("Metadata loaded successfully.", "SUCCESS")
      return(metadata)
      
    }, error = function(e) {
      log_msg("Error reading Metadata. Check format.", "ERROR")
      return(NULL)
    })
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
  
  output$growth_curve <- renderPlot({
    req(growth_curve())
    growth_curve()
  })
  
  #Kinectic derivatives calculations 
  deriv_cin <- eventReactive(input$process_data, {
    req(merge_raw(), input$metadata_block_names) 
    deriv <- merge_raw() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      mutate(deriv = calc_deriv(x = Time, y = Measurements, blank = input$blank))
    
    return(deriv)
  })
  
  percap <- eventReactive(input$process_data, {
    req(deriv_cin(), input$metadata_block_names)
    percap_cin <- deriv_cin() %>% group_by(Well, !!sym(input$metadata_block_names))
    if (input$noisy == "noisy_no") {
      percap_cin %>%
        mutate(deriv_percap = calc_deriv(x = Time, 
                                         y = Measurements,
                                         percapita = TRUE, 
                                         blank = input$blank))
      
    } else {
      percap_cin %>%
        mutate(deriv_percap = calc_deriv(x = Time, 
                                         y = Measurements,
                                         percapita = TRUE, 
                                         blank = input$blank,
                                         window_width_n = input$noisy_data)) 
    }
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
                                    deriv = deriv_percap, blank = input$blank))
    
    return(lag_data)
  })
  
  ##Plot lag
  lag_time_plot <- eventReactive(input$process_data, {
    req(lagtime(), doub_ti(), max_percap())
    lag_data <- lagtime()
    slope_data <- max_percap()
    ggplot(doub_ti(), aes(x = Time, y = log(Measurements))) +
      geom_line() +
      facet_wrap(~Well, nrow = 8, ncol = 12) + 
      geom_vline(data = lag_data, aes(xintercept = lag_time), lty = 2) +
      geom_abline(data = slope_data,
                  color = "red",
                  aes(slope = max_percap,
                      intercept = log(max_percap_dens) - max_percap * max_percap_time)) +
      labs(title = "Lag time of each strain", caption = "Vertical lines represent the time when the lag time ends")
  })
  
  ##max_percap
  max_percap <- eventReactive(input$process_data, {
    req(doub_ti(), input$metadata_block_names)
    max_percap_data <- doub_ti() %>% group_by(Well, !!sym(input$metadata_block_names)) %>%
      summarize(max_percap = max_gc(deriv_percap, na.rm = TRUE),
                max_percap_time = extr_val(Time, which_max_gc(deriv_percap)),
                max_percap_dens = extr_val(Measurements, which_max_gc(deriv_percap)),
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
      facet_wrap(~Well, nrow = 8, ncol = 12) +
      geom_point(data = max_percap_data,
                 aes(x = max_percap_time, y = max_percap),
                 size = 2, color = "red") +
      coord_cartesian(ylim = c(-1, NA)) +
      labs(title = "µmax of each strain")
  })
  
  ##OD max
  ODmax <- eventReactive(input$process_data, {
    req(doub_ti(), input$metadata_block_names)
    data_for_calc <- doub_ti()
    
    if (input$use_time_limit && !is.null(input$time_limit_val)) {
      data_for_calc <- data_for_calc %>% 
        filter(Time <= input$time_limit_val)
    }
    
    odmax_data <- data_for_calc %>% 
      group_by(Well, !!sym(input$metadata_block_names)) %>%
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
      facet_wrap(~Well, nrow = 8, ncol = 12) +
      geom_point(data = ODmax_data,
                 aes(x = max_time, y = max_dens),
                 size = 2, color = "red") +
      labs(title = "OD max reached by strain", y = "Optical Density at 600 nm")
  })
  
  ##AUC
  ##AUC (Con filtro de tiempo opcional)
  AUC <- eventReactive(input$process_data, {
    req(doub_ti(), ODmax()) 
    data_for_calc <- doub_ti()
    
    if (input$use_time_limit && !is.null(input$time_limit_val)) {
      data_for_calc <- data_for_calc %>% 
        filter(Time <= input$time_limit_val)
    }
    
    data_for_calc %>% 
      group_by(Well, !!sym(input$metadata_block_names)) %>% 
      summarize(auc = auc(x = Time, y = Measurements, blank = input$blank))
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
  
  
  #Shielding the main process events
  observeEvent(input$process_data, {
    
    #Check for Missing Inputs
    od_file_missing <- FALSE
    if (input$file_type == "csv_type" && is.null(input$csv_file)) od_file_missing <- TRUE
    if (input$file_type == "tsv_type" && is.null(input$tsv_file)) od_file_missing <- TRUE
    if (input$file_type == "excel_type" && is.null(input$excel_file)) od_file_missing <- TRUE
    
    if (od_file_missing) {
      showNotification("Missing Data: Please upload your OD measurements file.", type = "error")
      log_msg("Process ABORTED: Missing OD file.", "WARNING")
      return() 
    }
    
    if (is.null(input$metadata_file)) {
      showNotification("Missing Metadata: Please upload your Experimental Design file.", type = "error")
      log_msg("Process ABORTED: Missing Metadata file.", "WARNING")
      return() 
    }
    
    if (trimws(input$metadata_block_names) == "") {
      showNotification("Missing Label: Please specify the Plate Label.", type = "error")
      log_msg("Process ABORTED: Missing Plate Label.", "WARNING")
      return() 
    }
    
    log_msg("Inputs validated. Starting processing...", "INFO")
    
    #Safe Execution
    tryCatch({
      
      #Double check if files were read correctly by the reactives
      if (is.null(raw_data()) || is.null(experimental_design())) {
        log_msg("Process ABORTED: Invalid input files (Check previous errors).", "WARNING")
        return()
      }
      
      #Force calculation of the main dataframe
      final_df <- all_data() 
      
      if (nrow(final_df) == 0) {
        stop("Resulting dataframe is empty (Check if Well IDs match).")
      }
      
      #If we reach here, calculations were successful
      processed_data(TRUE)
      
      new_sheet_label <- input$new_sheet_name
      if (trimws(new_sheet_label) == "") {
        new_sheet_label <- paste0("Data_", format(Sys.time(), "%H%M%S"))
      }
      
      #Save results to the reactive list
      current_results <- list(
        data = final_df, 
        growth_curve = growth_curve(),
        lag_plot = lag_time_plot(),
        max_percap_plot = max_percap_plot(),
        ODmax_plot = ODmax_plot(),
        meta_name = input$metadata_block_names
      )
      
      stored_results[[new_sheet_label]] <- current_results
      
      updateSelectInput(session, "stored_sheet", choices = names(stored_results), selected = new_sheet_label)
      updateTextInput(session, "new_sheet_name", value = "")
      
      log_msg(paste("Processing SUCCESS! Saved as:", new_sheet_label), "SUCCESS")
      
      showModal(modalDialog(
        title = "Processing Complete",
        "Calculations finished successfully!",
        easyClose = T, 
        footer = modalButton("Dismiss")
      ))
      
    }, error = function(e) {
      #Error Handling (No crash)
      processed_data(FALSE)
      
      #Generic message for user
      msg_generico <- "Error in format, check your files"
      showNotification(msg_generico, type = "error")
      
      # Detailed message for log
      log_msg(paste("Processing FAILED:", e$message), "ERROR")
    })
  })

  
  #Heatmaps
  #Reactive to generate the kinetic parameters tables with a Common ID
  parameter_tables <- reactive({
    req(stored_results) 
    
    lag_table <- NULL
    AUC_table <- NULL
    odmax_table <- NULL
    max_percap_table <- NULL
    
    common_id <- "Common_ID"
    
    for (sheet_name in names(stored_results)) {
      dataset_list <- stored_results[[sheet_name]]
      data <- dataset_list$data
      
      original_col_name <- if(!is.null(dataset_list$meta_name)) dataset_list$meta_name else input$metadata_block_names
      
      if (!is.null(data) && nrow(data) > 0) {
        
        if (!(original_col_name %in% names(data))) {
          log_msg(paste("Warning for Heatmap: Column", original_col_name, "not found in sheet", sheet_name), "WARNING")
          next 
        }
        
        #Rename to common ID
        data <- data %>% rename(!!sym(common_id) := !!sym(original_col_name))
        
        lag_temp <- data %>% select(Well, !!sym(common_id), lag_time) %>%
          rename(value = lag_time)
        colnames(lag_temp)[3] <- sheet_name
        
        AUC_temp <- data %>% select(Well, !!sym(common_id), AUC) %>%
          rename(value = AUC)
        colnames(AUC_temp)[3] <- sheet_name
        
        odmax_temp <- data %>% select(Well, !!sym(common_id), ODmax) %>%
          rename(value = ODmax)
        colnames(odmax_temp)[3] <- sheet_name
        
        max_percap_temp <- data %>% select(Well, !!sym(common_id), µmax) %>%
          rename(value = µmax)
        colnames(max_percap_temp)[3] <- sheet_name
        
        if (is.null(lag_table)) {
          lag_table <- lag_temp
          AUC_table <- AUC_temp
          odmax_table <- odmax_temp
          max_percap_table <- max_percap_temp
        } else {
          lag_table <- merge(lag_table, lag_temp, by = c("Well", common_id), all = TRUE)
          AUC_table <- merge(AUC_table, AUC_temp, by = c("Well", common_id), all = TRUE)
          odmax_table <- merge(odmax_table, odmax_temp, by = c("Well", common_id), all = TRUE)
          max_percap_table <- merge(max_percap_table, max_percap_temp, by = c("Well", common_id), all = TRUE)
        }
      }
    }
    
    return(list(lag = lag_table, AUC = AUC_table, odmax = odmax_table, max_percap = max_percap_table))
  })
  
  #Function to calculate the average using the Common ID
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
    }
    
    result_matrix <- as.matrix(averaged_data)
    return(result_matrix)
  }
  
  #Reactive for averaged parameters
  averaged_parameters_by_species <- reactive({
    req(parameter_tables())
    
    species_col_internal <- "Common_ID"
    
    return(list(
      lag = average_by_species(parameter_tables()$lag, species_col_internal),
      AUC = average_by_species(parameter_tables()$AUC, species_col_internal),
      odmax = average_by_species(parameter_tables()$odmax, species_col_internal),
      max_percap = average_by_species(parameter_tables()$max_percap, species_col_internal)
    ))
  })
  
  #Function to plot Heatmaps with safety check
  draw_heatmap_logic <- function(data, parameter_name, species_label) {
    
    #Is there any data?
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No Data Available", cex = 1.2)
      return()
    }
    
    #Multiple sheets validation (2 or more)
    if (ncol(data) < 2) {
      plot.new()
      text(0.5, 0.5, "Insufficient Data for Heatmap\n\nYou need to process at least 2 sheets/files\nto compare them.", cex = 1.2, col = "darkgray")
      return()
    }
    
    #Ploting
    tryCatch({
      heatmap(data,
              main = paste("Heatmap of Average", parameter_name, "by Plate"),
              xlab = "Processed Sheets",
              ylab = species_label,
              col = viridis::viridis(256),
              margins = c(10, 10)
      )
    }, error = function(e) {
      #If heatmap fails
      plot.new()
      text(0.5, 0.5, paste("Error generating Heatmap:\n", e$message), col = "red")
    })
  }
  
  #Render heatmaps
  output$lag_heatmap_species <- renderPlot({
    data <- averaged_parameters_by_species()$lag
    req(data)
    draw_heatmap_logic(scale(data), "Lag Time (Z-score)", input$metadata_block_names)
  })
  
  output$AUC_heatmap_species <- renderPlot({
    data <- averaged_parameters_by_species()$AUC
    req(data)
    draw_heatmap_logic(scale(data), "AUC (Z-score)", input$metadata_block_names)
  })
  
  output$odmax_heatmap_species <- renderPlot({
    data <- averaged_parameters_by_species()$odmax
    req(data)
    draw_heatmap_logic(scale(data), "ODmax (Z-score)", input$metadata_block_names)
  })
  
  output$max_percap_heatmap_species <- renderPlot({
    data <- averaged_parameters_by_species()$max_percap
    req(data)
    draw_heatmap_logic(scale(data), "µmax (Z-score)", input$metadata_block_names)
  })

  #Show original data
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
  
  #####DOWNLOAD HANDLERS#####
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Kinetic_parameters_", input$metadata_block_names, "_", input$stored_sheet, ".xlsx", sep = "")
    },
    content = function(file) {
      req(input$stored_sheet, stored_results[[input$stored_sheet]]$data)
      write.xlsx(stored_results[[input$stored_sheet]]$data, file, row.names = FALSE)
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste("Growth_curve_plots_", input$metadata_block_names, "_", input$stored_sheet, ".pdf", sep = "")
    },
    content = function(file) {
      req(input$stored_sheet, stored_results[[input$stored_sheet]]$growth_curve)
      ggsave(file, plot = stored_results[[input$stored_sheet]]$growth_curve, device = "pdf", width = 15, height = 10)
    }
  )
  
  output$download_lag_plots <- downloadHandler(
    filename = function() {
      paste("lag_plots_", input$metadata_block_names, "_", input$stored_sheet, ".pdf", sep = "")
    },
    content = function(file) {
      req(input$stored_sheet, stored_results[[input$stored_sheet]]$lag_plot)
      ggsave(file, plot = stored_results[[input$stored_sheet]]$lag_plot, device = "pdf", width = 15, height = 10)
    }
  )
  
  output$download_max_percap_plot <- downloadHandler(
    filename = function() {
      paste("max_percap_plots_", input$metadata_block_names, "_", input$stored_sheet, ".pdf", sep = "")
    },
    content = function(file) {
      req(input$stored_sheet, stored_results[[input$stored_sheet]]$max_percap_plot)
      ggsave(file, plot = stored_results[[input$stored_sheet]]$max_percap_plot, device = "pdf", width = 15, height = 10)
    }
  )
  
  output$download_performance_plot <- downloadHandler(
    filename = function() {
      paste("Performance_plots_", input$metadata_block_names, "_", input$stored_sheet, ".pdf", sep = "")
    },
    content = function(file) {
      req(input$stored_sheet, stored_results[[input$stored_sheet]]$ODmax_plot)
      ggsave(file, plot = stored_results[[input$stored_sheet]]$ODmax_plot, device = "pdf", width = 15, height = 10)
    }
  )  
  
  output$download_all_heatmaps <- downloadHandler(
    filename = function() {
      paste("All_Heatmaps_", input$metadata_block_names, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(averaged_parameters_by_species())
      data_list <- averaged_parameters_by_species()
      species_label <- input$metadata_block_names
      
      pdf(file, width = 10, height = 10) 
      
      #Page 1: Lag_time
      if(!is.null(data_list$lag)) {
        draw_heatmap_logic(scale(data_list$lag), "Lag Time (Z-score)", species_label)
      }
      
      #Page 2: AUC
      if(!is.null(data_list$AUC)) {
        draw_heatmap_logic(scale(data_list$AUC), "AUC (Z-score)", species_label)
      }
      
      #Page 3: ODmax
      if(!is.null(data_list$odmax)) {
        draw_heatmap_logic(scale(data_list$odmax), "ODmax (Z-score)", species_label)
      }
      
      # Page 4: µmax
      if(!is.null(data_list$max_percap)) {
        draw_heatmap_logic(scale(data_list$max_percap), "µmax (Z-score)", species_label)
      }
      
      dev.off()
    }
  )
}

#Run the app
shinyApp(ui = ui, server = server)
