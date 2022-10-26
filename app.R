########### EY FORECASTING APP #################

# LIBRARIES ----
source(file = "00_scripts/libraries.R")
libraries()

# FUNCTIONS ----
source(file = "00_scripts/f_frequency_data.R")
source(file = "00_scripts/f_sliderInput2.R")
source(file = "00_scripts/f_read_data.R")
source(file = "00_scripts/f_select_data.R")
source(file = "00_scripts/f_count_rangeselector.R")
source(file = "00_scripts/f_plot_acf.R")

# Shiny settings ----
options(shiny.maxRequestSize=30*1024^2) 


# UI ELEMENTS ----

## 1. Header ----
header <- dashboardHeader(
    title = div(
        span(img(src    = "EY_Logo_Beam_RGB_White_Yellow.png",
                 width  = "12%",
                 height = "12%",
                 style = "vertical-align: top; margin-top: 2px;"
        )),
        span(tags$b("FORECASTING TOOL"),
             style = "font-size: 21px; font-family: EYInterstate, sans-serif; padding-bottom: 50px"
        )
    ),
    titleWidth = 300
)

## 2. Sidebar ----
sidebar <- dashboardSidebar(width = 300, 
                            
                            sidebarMenu(
                                menuItem(text     = tags$b("APP Description"),
                                         tabName  = "app_description",
                                         selected = TRUE,
                                         icon     = icon("readme")
                                ),
                                menuItem(text     = tags$b("Load Data"),
                                         tabName  = "app_load",
                                         icon     = icon("file-upload")
                                ),
                                menuItem(text     = tags$b("Data Exploration"),
                                         tabName  = "app_exploration",
                                         icon     = icon("search")
                                )
                            )
)

## 3. Control Bar ----
controlbar <- dashboardControlbar(
    disable = TRUE
)

body <- dashboardBody(
    
    shinyjs::useShinyjs(),
    
    useShinyalert(),
    
    chooseSliderSkin(
        skin = c("Flat"),
        color = "#ffe600"
    ),
    
    # * CSS ----
    tags$head(tags$style(HTML('/* logo */
                               .skin-blue .main-header .logo {background-color: #333333;}
                                
                               /* body */ 
                               .content-wrapper, .right-side {background-color: #f0f0f0;}

                               /* navbar (rest of the header) */
                               .skin-blue .main-header .navbar {background-color: #333333;}

                               /* main sidebar */
                               .skin-blue .main-sidebar {background-color: #333333;}
                              
                               .box.box-solid.box-primary>.box-header {background: #333333}

                               .box.box-solid.box-primary{background: #ffffff}
                              
                               div.box {
                                        text-align: left;
                                        border-style: solid;
                                        border-bottom-color: #ffffff;
                                        border-left-color:  #ffffff;
                                        border-right-color:#ffffff;
                                        border-top-color: #ffffff;
                                        border-bottom-width:5px;
                                        border-top-width:5px;
                                        border-left-width:5px;
                                        border-right-width:5px;
                               }
                               
                              .skin-blue .sidebar-menu>li.active>a {
                                border-left-color: #ffe600;}
                                
                              .skin-blue .sidebar-menu .treeview-menu>li>a {
                                color: #b0ac8b;}
                              
                              .skin-blue .sidebar-menu .treeview-menu>li.active>a, .skin-blue .sidebar-menu .treeview-menu>li>a:hover {
                                color: #ffe600;
                              }
                              
                              .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li.menu-open>a, .skin-blue .sidebar-menu>li:hover>a {
                                color: #ffe600;
                                background: #1e282c;
                              }
                                
                                
                              .navbar {
                                height: 60px;
                                min-height:25px !important;
                              }
                                
                              .navbar-nav > li > a, .navbar-brand {
                                padding-top:5px !important; 
                                padding-bottom:5px !important;
                                height: 25px;
                              }
                              
                              /* Switch */
                              .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-success,
                              .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-success {
                                background: #ffe600;
                                color: #2e2e38;
                              }
                              
                              /* Download handlers */
                              .download_button{
                                background-color: #ffe600;
                              }
                              
                              /* AppButton */
                              .btn-default .badge{
                                background-color:#ffe600!important;
                                color:#2e2e38!important;
                              }
                              .btn-app{
                                border: 1px solid #ffe600;
                              }
                              
                              /** Active tabs **/
                              /* Tabs */
                              .nav-tabs-custom>.nav-tabs>li.active {
                                border-top-color: #ffe600;
                              }
                              
                              a {
                                color: #2e2e38;
                              }
                              
                              .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                                color: #ffd900;
                                cursor: default;
                                background-color: #fff;
                                border: 1px solid #ffd900;
                                border-bottom-color: transparent;
                              }
                              
                              .pretty input:checked~.state.p-warning .icon,
                              .pretty input:checked~.state.p-warning .svg,
                              .pretty.p-toggle .state.p-warning .icon, .pretty.p-toggle .state.p-warning .svg {
                                background-color: #ffe600;
                              }
                              
                              /* width log_trans label */
                              .logtrans_label {
                                width: 181px;
                              }
                              
                              /* width log_trans label */
                              .smoother_label{
                                padding-left: 120px;
                              }
                                
                              /* sliders */
                              .irs--flat .irs-from, .irs--flat .irs-to, .irs--flat .irs-single {
                                color: black;
                                font-size: 11px;
                                background-color: #ffe600;
                              }
                              .irs-bar-edge, .irs-bar, .irs-single:after, .irs-from:after, .irs-to:after, .irs-slider {
                              filter: hue-rotate(-299.566563467492deg) saturate(64.1350210970464%) brightness(159.693626%);
                              }
                              
                              /* Progress bar */
                              .progress-bar{
                                background-color: #ffe600;
                              }
                              .shiny-notification{
                                border: 3px solid #ffe600;
                                border-radius: 9px;
                              }
                              
                              /* VerbatimTextOutput */
                              code, kbd, pre, samp {
                                font-family: EYInterstate, sans-serif;
                                font-weight: 400;
                              }
                              
                              .seasonality_plot_titles{
                                margin-left: 60px;
                                margin-bottom: 0px;
                              }
                              
                              .box-header.with-border {
                                border-bottom: 1px solid #ffe600;
                              }
                              
                              /* External/documentation links */
                              .ref_link{
                                color: #e6bc00
                              }'
    )
    )
    ),
    
    includeCSS("www/style.css"),
    
    tabItems(
        ### 4.1 app_description tab content ----
        tabItem(
            tabName = "app_description",
            class   = "container",
            
            fluidRow(
                div(
                    wellPanel(
                        style = "background-color: white;",
                        fluidRow(
                            column(
                                width = 12,
                                tags$h1(tags$img(src    = "EY_Logo_Beam_RGB-OffBlack-Yellow.png",
                                                 width  = "5%",
                                                 height = "5%",
                                                 style  = "vertical-align: bottom; padding-bottom: 7px"),
                                        tags$b("FORECASTING TOOL")
                                ),
                                br(),
                                p(HTML("The EY Forecasting Tool is an interactive user interface to explore, visualize and forecast time series data with a wide range of models at your disposal.")),
                                p(HTML("This application is designed with the idea that the user will navigate through it with the help of the side menu. The only requisite for the good functioning of its forecasting abbilities is to load the data and then select the preferred forecasting framework.")),
                                br(),
                                tags$h3(tags$b("LOAD DATA:")),
                                p(HTML("The user can upload a CSV file with one or multiple (grouped) time series data. The minimum requirements for the file to be correctly accepted by the application are:<br/>
                                       &nbsp&nbsp&nbsp- Must be a .csv file.<br/>
                                       &nbsp&nbsp&nbsp- The only accepted separators between values are: , and ;<br/>
                                       &nbsp&nbsp&nbsp- Must contain at least two variables: a date feature, with an appropriate date format, and the value to be forecasted by the models.<br/>
                                       &nbsp&nbsp- For hierarchical or groupped time series forecasting, there must be an id column contained in the dataset to identify the different time series.<br/>
                                       The date, value and, optionally, the id variables must be selected by the user prior to continuing to the modelling stage.<br/>
                                       Variable formats are shown for the user to see what they have inputted as well as the frequency of the date variable.")),
                                br(),
                                tags$h3(tags$b("EXPLORE DATA:")),
                                p(HTML("The tool provides many descriptive and interactive visualizations for exploring the uploaded data. The included features are:<br/>
                                       &nbsp&nbsp&nbsp- Data table to inspect the inputted values.<br/>
                                       &nbsp&nbsp&nbsp- Summary of the date variable, including: start/end dates, frequency and units of the data.<br/>
                                       &nbsp&nbsp&nbsp- Time series plot.<br/>
                                       &nbsp&nbsp&nbsp- ACF and PACF plots.<br/>
                                       &nbsp&nbsp&nbsp- Time series breakdown: seasonality, trend and errors plots.<br/>
                                       &nbsp&nbsp&nbsp- Seasonality plots dependent on the frequency of the time series.<br/>
                                       &nbsp&nbsp&nbsp- Anomaly detection plot.<br/>
                                       <b>Beware</b> that the time series breakdown and anomaly detection plots are not available for time series with missing values.")),
                                br(),
                                tags$h3(tags$b("FORECASTING MODELS:")),
                                p(HTML("The interface allows the user to compare different fitted time series models and their forecasts with the following algorithms:<br/>
                                       &nbsp&nbsp&nbsp- ARIMA: autoARIMA, ARIMABoost (with xgboost) and manual ARIMA.<br/>
                                       &nbsp&nbsp&nbsp- ML modelling: Prophet, ProphetBoost, xgboost, Random Forest.<br/>
                                       &nbsp&nbsp&nbsp- ML ensemble of the previously trained ML models.<br/>
                                       &nbsp&nbsp&nbsp- AutoML, powered by H2O.ai.<br/>
                                       &nbsp&nbsp&nbsp- Deep Learning: DeepAR and NBeats Ensemble.<br/>")),
                                p(HTML("To each model, the user must provide a horizon for the future predictions. This amount will correspond to a number of periods in the same frequecny as the date variable. This horizon period will also be used to split the data between the training and test set.<br/>")),
                                p(HTML("Each of the forecasting model menu items is subdivided into three tabs:<br/>
                                       &nbsp&nbsp&nbsp<b>1. Model training:</b><br/>
                                       &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp- Includes input selectors for the corresponding parameters for each of the algorithms.<br/>
                                       &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp- Forecast visualization on the testing set. Including median values and 95% confidence intervals.<br/>
                                       &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp- An interactive table is displayed including the accuracy metrics for the forecast on the testing set to assess the fit of the model to the data. Read more about forecasting accuracy metrics"),
                                  tags$a("here", href = "Forecast_KPIs.pdf", target="_blank", class = "accuracy_link"),
                                  HTML(".<br/>&nbsp&nbsp&nbsp<b>2. Forecast plot:</b><br/>
                                       &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp- Forecast visualization with the refitted model to the full dataset (train + test).<br/>
                                       &nbsp&nbsp&nbsp<b>3. Forecast table:</b><br/>
                                       &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp- Interactive table with all the trained models' predictions and their correspondent confidence intervals (95%).<br/>
                                       &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp- Forecast table downloader. The user is offered the possibility of downloading a CSV file with the forecasted values for all trained models. The file name is constructed as follows:"),
                                  tags$p("{forecast horizon periods}_{arima/ml/dl}_forecast_{original file name}.csv", style = "text-align:center;")
                                ),
                                style = "text-align:justify; font-size: 20px"
                            )
                        )
                    )
                )
            )
        ),
        
        ### 4.2 app_load tab content ----
        tabItem(tabName = "app_load",
                class = "fluid-container",
                fluidRow(
                    box(title="Upload data",width = 12,
                        column(width = 4,
                               fileInput(
                                   inputId  = "file_main",
                                   label    = "Upload time series dataset (.csv)",
                                   multiple = FALSE,
                                   accept   = c(".csv",
                                                'text/csv',
                                                'text/comma-separated-values')
                               )
                        ),
                        column(width = 6,offset = 2,
                               fluidRow(
                                   uiOutput("date_dropdown")
                               ),
                               fluidRow(
                                   uiOutput("value_dropdown")
                               ),
                               
                               fluidRow(
                                   shinyWidgets::prettyCheckbox(
                                       inputId = "id_checkbox",
                                       label = "Do you have multiple time series in your data?", 
                                       value = FALSE,
                                       status = "warning",
                                       icon   = icon("check")
                                   ),
                                   
                                   uiOutput("id_dropdown")
                               ),
                               
                               fluidRow(
                                   appButton(inputId = "explore",
                                             label = "Load variables",
                                             class = "pull-right",
                                             icon = icon("database"),
                                             dashboardBadge("Start",
                                                            color = "yellow")
                                   )
                               )
                        )
                    ),
                    
                    # column(width = 4,
                    #        box(width= NULL,
                    #            title="Missing values in the data",
                    #            verbatimTextOutput("text_missing")
                    #        )
                    #  )
                ),
                
                fluidRow(
                    column(width = 4,
                           box(width       =NULL,
                               title       ="Data preview",
                               collapsible = TRUE,
                               reactableOutput(outputId = "table_original")
                           )
                    ),
                    
                    column(width = 4,
                           box(width       = NULL,
                               title       = "Data summary",
                               collapsible = TRUE,
                               verbatimTextOutput("text_str"),
                               verbatimTextOutput("text_summary")
                           )
                    ),
                    
                    column(width = 4,
                           box(width       = NULL,
                               title       = "Date variable frequency",
                               collapsible = TRUE,
                               verbatimTextOutput("text_frequency")
                           )
                    )
                )
        ),
        ### 4.3 app_exploration tab content ----
        tabItem(tabName = "app_exploration",
                class = "fluid-container",
                fluidRow(
                    column(
                        width =12,
                        tabBox(width=NULL,
                               title="Data Exploration",
                               tabPanel(title = "Data table + Summary",
                                        
                                        wellPanel(style = "background-color: white;",
                                                  fluidRow(
                                                      reactableOutput(outputId = "ts_summary")
                                                  )
                                        ),
                                        br(), br(),
                                        
                                        wellPanel(style = "background-color: white;",
                                                  fluidRow(
                                                      reactableOutput(outputId = "table_ts")
                                                  )
                                        )
                               ),
                               
                               tabPanel(title = "Time series plot",
                                        wellPanel(
                                            style = "background: white",
                                            fluidRow(
                                                # box(width = 6,
                                                column(width = 2,
                                                       helpText("Logarithmic Transformation",
                                                                class = "logtrans_label")
                                                       
                                                ),
                                                column(width = 2,
                                                       shinyWidgets::switchInput(
                                                           inputId    = "ts_plot_log_trans",
                                                           onStatus   = "success",
                                                           offStatus  = "danger",
                                                           value      = FALSE,
                                                           width      = "auto"
                                                       )
                                                ),
                                                column(width = 2,
                                                       helpText("Smoother",
                                                                class = "smoother_label")
                                                ),
                                                column(width = 2,
                                                       shinyWidgets::switchInput(
                                                           inputId    = "ts_plot_smooth",
                                                           onStatus   = "success",
                                                           offStatus  = "danger",
                                                           value      = TRUE,
                                                           width      = "auto"
                                                       )
                                                ),
                                                column(width = 4,
                                                       shiny::sliderInput(
                                                           inputId = "ts_plot_smooth_span",
                                                           label   = "Smooth Span",
                                                           value   = 0.75,
                                                           min     = 0.05,
                                                           max     = 1,
                                                           ticks   = FALSE
                                                       )
                                                )
                                            ),
                                            fluidRow(
                                                plotlyOutput(outputId = "ts_plot")
                                            )
                                        )
                               ),
                               
                               tabPanel(title = "ACF/PACF",
                                        wellPanel(
                                            style = "background-color: white;",
                                            fluidRow(
                                                tags$h3("ACF",
                                                        class = "seasonality_plot_titles"),
                                                plotlyOutput(outputId = "ts_ACF")
                                            ),
                                            
                                            fluidRow(
                                                tags$h3("PACF",
                                                        class = "seasonality_plot_titles"),
                                                plotlyOutput(outputId = "ts_PACF")
                                            )
                                        )
                               ),
                               
                               tabPanel(title = "Time series breakdown",
                                        wellPanel(style = "background-color: white;",
                                                  fluidRow(
                                                      tags$h3("Seasonality",
                                                              class = "seasonality_plot_titles"),
                                                      plotlyOutput(outputId = "ts_season")
                                                  ),
                                                  
                                                  fluidRow(
                                                      tags$h3("Trend",
                                                              class = "seasonality_plot_titles"),
                                                      plotlyOutput(outputId = "ts_trend")
                                                  ),
                                                  
                                                  fluidRow(
                                                      tags$h3("Errors",
                                                              class = "seasonality_plot_titles"),
                                                      plotlyOutput(outputId = "ts_remainder")
                                                  )
                                        )
                               ),
                               
                               tabPanel(title = "Seasonality plot",
                                        uiOutput(outputId = "ts_seasonality")
                               ),
                               
                               tabPanel(title = "Anomaly detection",
                                        plotlyOutput(outputId = "ts_anomaly")
                               )
                        )
                    )
                )
        )
    )
)

## 5. Footer ----
footer <-  dashboardFooter(
    left = div(img(src="ey-wavespace-logo-png-transparent-png_small.png")
    ),
    right = "2021"
)

# UI ----
ui <- dashboardPage(header, sidebar, body, controlbar, footer)
# SERVER ----
server <- function(session, input, output) { 
    #____________________________________----
    #4.2 app_load TAB ----
    
    ##Reading dataset (main_data())----
    main_data <- reactive({
        req(input$file_main)
        if(is.error(expr = read_data(input$file_main))){
            sendSweetAlert(session = session,
                           title      = "Oops",
                           btn_labels = "OK",
                           text       = HTML("The delimiter of your .csv file is not permitted, permitted delimiters are: , and ;<br>Please submit a file with the appropriate format."),
                           type       = "error",
                           html       = TRUE,
                           showCloseButton = TRUE)
        }else{
            read_data(input$file_main)
        }
    })
    
    ##Date variable (date_var)----
    output$date_dropdown <- renderUI({
        selectInput(inputId   = "date_var",
                    label     = "Select the date/time variable:",
                    selected  = colnames(main_data())[1],
                    choices   = colnames(main_data()),
                    selectize = TRUE
        )
    })
    
    ##Value variable (value_var)----
    output$value_dropdown <- renderUI({
        selectInput(inputId   = "value_var",
                    label     = "Select the variable to be forecasted:",
                    selected  = colnames(main_data())[2],
                    choices   = colnames(main_data()),
                    selectize = TRUE
        )
    })
    
    ##ID variable (id_var)----
    output$id_dropdown <- renderUI({
        if(input$id_checkbox == 0){
            return(NULL)
        }
        
        else if(input$id_checkbox == 1){
            list(
                selectInput(
                    inputId   = "id_var",
                    label     = "Select the ID variable:",
                    choices   = colnames(main_data()),
                    selectize = TRUE
                )
            )
        }
    })
    
    ##Missing data (text_missing)----
    output$text_missing <- renderPrint({
        req(vars_data())
        statsNA(rv$data$Value)
    })
    
    ##Data Overview (table_original) ----
    output$table_original <-  renderReactable({
        req(main_data())
        rect_data <- reactable(main_data(),
                               defaultPageSize     = 5,
                               pageSizeOptions     = c(5, 10, 20, 50),
                               showPageSizeOptions = TRUE,
                               minRows             = 1,
                               sortable            = TRUE,
                               highlight           = TRUE,
                               defaultColDef       = colDef(
                                   footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
                               )
        )
        return(rect_data)
    })
    
    ##Data Summary (text_summary)----
    output$text_summary <- renderPrint({
        req(main_data())
        summary(main_data())
    })
    
    output$text_str <- renderPrint({
        req(main_data())
        str(main_data())
    })
    
    ##Date frequency (text_frequency) ----
    output$text_frequency <- renderPrint({
        req(vars_data())
        rv$ts_scale
    })
    
    ##Modified data (vars_data()) ----
    vars_data <- eventReactive(input$explore,{
        req(main_data())
        if (is.error(select_data(data = main_data(), input = input))
        ){sendSweetAlert(session = session,
                         title      = "Oops",
                         btn_labels = "OK",
                         text       = HTML("The date variable you have selected is not in a date format.<br>Please select a valid date for forecasting."),
                         type       = "error",
                         html       = TRUE,
                         showCloseButton = TRUE)
        }else{
            df <- select_data(data = main_data(), input = input)
            
            return(df)
        }
    })
    
    ##Data RV ----
    rv <- reactiveValues()
    
    observeEvent(eventExpr = input$explore, {
        req(vars_data())
        ### Data+VARS----
        rv$data       <- vars_data()
        rv$date_name  <- colnames(vars_data())[1]
        
        rv$value_name <- colnames(vars_data())[2]
        
        rv$ts_summary_tbl <- rv$data %>%
            # group_by(!! rv$group_name_expr) %>%
            tk_summary_diagnostics()
        
        rv$ts_scale <- rv$ts_summary_tbl$scale[[1]]
        
        print(rv$ts_scale)
        
        rv$median_nobs <- rv$ts_summary_tbl %>%
            pull(n.obs) %>%
            median()
        
        rv$lag_limit <- rv$median_nobs %>%
            `*`(0.4) %>%
            round()
        
        rv$horizon_recommended <- round(0.18 * rv$median_nobs)
        
    }, ignoreNULL = FALSE)
    
    ##Modelling RV----
    
    observeEvent(eventExpr = input$explore, {
        # ARIMA tibbles
        rv$arima_data_prepared_tbl       <- NULL
        rv$arima_future_tbl              <- NULL
        
        rv$arima_calibration_tbl         <- NULL
        rv$arima_accuracy_tbl            <- NULL
        rv$arima_forecast_tbl            <- NULL
        
        rv$arima_refit_tbl               <- NULL
        rv$arima_future_forecast_tbl     <- NULL
        
        # ML tibbles (for the outputs)
        rv$ml_accuracy_tbl               <- NULL
        rv$ml_forecast_tbl               <- NULL
        rv$ml_future_forecast_tbl        <- NULL
        
        #Submodels tibbles
        rv$ml_data_prepared_tbl          <- NULL
        rv$ml_future_tbl                 <- NULL
        
        rv$submodels_calibration_tbl     <- NULL
        rv$submodels_accuracy_tbl        <- NULL
        rv$submodels_forecast_tbl        <- NULL
        
        rv$submodels_refit_tbl           <- NULL
        rv$submodels_future_forecast_tbl <- NULL
        
        # Ensemble tibbles
        rv$ensemble_data_prepared_tbl    <- NULL
        rv$ensemble_future_tbl           <- NULL
        
        rv$ensemble_calib_tbl            <- NULL
        rv$ensemble_accuracy_tbl         <- NULL
        rv$ensemble_forecast_tbl         <- NULL
        
        rv$ensemble_refit_tbl            <- NULL
        rv$ensemble_future_forecast_tbl  <- NULL
        
        # AutoML tibbles
        rv$automl_data_prepared_tbl      <- NULL
        rv$automl_future_tbl             <- NULL
        
        rv$automl_calib_tbl              <- NULL
        rv$automl_accuracy_tbl           <- NULL
        rv$automl_forecast_tbl           <- NULL
        
        rv$automl_refit_tbl              <- NULL
        rv$automl_future_forecast_tbl    <- NULL
        
        #Deep Learning tibbles
        rv$dl_data_prepared_tbl          <- NULL
        rv$dl_future_tbl                 <- NULL
        
        rv$dl_calibration_tbl            <- NULL
        rv$dl_accuracy_tbl               <- NULL
        rv$dl_forecast_tbl               <- NULL
        
        rv$dl_refit_tbl                  <- NULL
        rv$dl_future_forecast_tbl        <- NULL
        
        
    },ignoreNULL = FALSE)
    
    ## Recommended horizon (horizon_recommended) ----
    output$auto_arima_horizon_recommended <- output$manual_arima_horizon_recommended <- output$ml_horizon_recommended <- output$ensemble_horizon_recommended <- output$auto_horizon_recommended <- output$dl_horizon_recommended <- renderText(
        paste("We recommend forecasting at least", {rv$horizon_recommended}, "periods for a better performance.")
    )
    
    #____________________________________----
    #4.3 app_exploration TAB ----
    
    ##Summary table (ts_summary)----
    output$ts_summary <- renderReactable({
        req(vars_data())
        data <- vars_data() %>% tk_summary_diagnostics(.date_var = Date) %>% select(1:5)
        rect_data <- reactable(data      = data,
                               highlight = TRUE,
                               columns = list(
                                   n.obs = colDef(align  = "left",
                                                  name   = "Nº observations"),
                                   start = colDef(format = colFormat(date = TRUE),
                                                  name   = "Start"),
                                   end   = colDef(format = colFormat(date = TRUE),
                                                  name   = "End"),
                                   units = colDef(name   = "Units"),
                                   scale = colDef(name   = "Frequency")
                               )
        )
        
        return(rect_data)
    })
    
    ##Data table (table_ts)----
    output$table_ts <-  renderReactable({
        req(vars_data())
        rect_data <- reactable(vars_data(),
                               columns = list(
                                   Date = colDef(format = colFormat(date = TRUE),
                                                 align  = "left"),
                                   Value = colDef(format = colFormat(digits     = 2,
                                                                     separators = TRUE), 
                                                  align  = "left")
                               ),
                               defaultPageSize     = 10,
                               pageSizeOptions     = c(5, 10, 20, 50),
                               showPageSizeOptions = TRUE,
                               minRows             = 1,
                               searchable          = TRUE,
                               sortable            = TRUE,
                               highlight           = TRUE,
                               defaultColDef = colDef(
                                   footer = function(values, name) htmltools::div(name, style = list(fontWeight = 600))
                               )
        )
        return(rect_data)
    })
    
    
    ##Plot Time Series (ts_plot)----
    output$ts_plot <- renderPlotly({
        req(vars_data())
        df <- vars_data()
        # Log Transformation
        if (input$ts_plot_log_trans) {
            df <- df %>%
                mutate(Value := log1p(Value))
        }
        
        g  <- df %>%
            plot_time_series(
                .date_var     = Date,
                .value        = Value,
                .smooth       = input$ts_plot_smooth,
                .smooth_span  = input$ts_plot_smooth_span,
                .smooth_color = "deeppink3",
                .smooth_size  = 0.5,
                .title        = FALSE,
                .interactive  = FALSE) +
            geom_line(color = "cornflowerblue") +
            scale_y_continuous(labels = scales::comma_format())
        
        ggplotly(g, dynamicTicks = TRUE) %>% 
            plotly::layout(
                xaxis = list(
                    rangeselector = list(
                        buttons = list(
                            list(
                                count    = 1 * count_rangeselector(freq = rv$ts_scale),
                                label    = "1",
                                step     = rv$ts_scale,
                                stepmode = "backward"),
                            list(
                                count    = 3 * count_rangeselector(freq = rv$ts_scale),
                                label    = "3",
                                step     = rv$ts_scale,
                                stepmode = "backward"),
                            list(
                                count    = 6 * count_rangeselector(freq = rv$ts_scale),
                                label    = "6",
                                step     = rv$ts_scale,
                                stepmode = "backward"),
                            list(
                                count = 12 * count_rangeselector(freq = rv$ts_scale),
                                label = "12",
                                step  = rv$ts_scale,
                                stepmode = "backward"),
                            list(
                                count = 1,
                                label = "YTD",
                                step  = "year",
                                stepmode = "todate"),
                            list(
                                step = "all"
                            )
                        ),
                        font = list(
                            family = "Arial",
                            color  = "#2E2E38"
                        ),
                        bgcolor     = "#f6f6fa",
                        activecolor = "#FFE600",
                        bordercolor = "#FFE600",
                        borderwidth = 1
                    ),
                    rangeslider = list(
                        type = "date"
                    )
                )
            )
    })
    
    ##Plot ACF/PACF (ts_ACF)----
    output$ts_ACF <- renderPlotly({
        req(vars_data())
        g <-   plot_acf(
            .data                  = vars_data(),
            .date_var              = Date,
            .value                 = Value,
            .show_white_noise_bars = TRUE,
            .point_color           = "darkslateblue",
            .title                 = FALSE,
            .interactive           = FALSE,
            .feature_set           = "acf") +
            geom_line(color = "cornflowerblue")
        
        ggplotly(g, dynamicTicks = TRUE) %>%
            rangeslider()
        
    })
    
    output$ts_PACF <- renderPlotly({
        req(vars_data())
        g <-   plot_acf(
            .data                  = vars_data(),
            .date_var              = Date,
            .value                 = Value,
            .show_white_noise_bars = TRUE,
            .point_color           = "darkslateblue",
            .title                 = FALSE,
            .interactive           = FALSE,
            .feature_set           = "pacf") +
            geom_line(color = "cornflowerblue")
        
        ggplotly(g, dynamicTicks = TRUE) %>%
            rangeslider()
        
        # plotly::layout(
        #     xaxis = list(
        #         rangeslider = list(autorange = TRUE)
        #     )
        # )
        
    })
    
    ##Plot stl diagnostics 3x(ts_stl)----
    output$ts_season <- renderPlotly({
        req(vars_data())
        result <- tryCatch(
            expr = {
                g <- plot_stl_diagnostics(
                    .data        = vars_data(),
                    .date_var    = Date,
                    .message     = FALSE,
                    .value       = Value,
                    .feature_set = c("season"),
                    .title       = FALSE,
                    .interactive = FALSE) +
                    geom_line(color = "cornflowerblue")
                return(ggplotly(g, dynamicTicks = TRUE))
                
            }, warning = function(cond){
                sendSweetAlert(session         = session,
                               title           = "Oops",
                               btn_labels      = "OK",
                               text            = HTML("This feature is not available for time series with missing values."),
                               type            = "error",
                               html            = TRUE,
                               showCloseButton = TRUE)
                
            }, error = function(cond){
                sendSweetAlert(session         = session,
                               title           = "Oops",
                               btn_labels      = "OK",
                               text            = HTML("This feature is not available for time series with missing values."),
                               type            = "error",
                               html            = TRUE,
                               showCloseButton = TRUE)
            }
        )
    })
    
    output$ts_trend <- renderPlotly({
        req(vars_data())
        result <- tryCatch(
            expr = {
                g <- plot_stl_diagnostics(
                    .data        = vars_data(),
                    .date_var    = Date,
                    .value       = Value,
                    .message     = FALSE,
                    .feature_set = c("trend"),
                    .title       = FALSE,
                    .interactive = FALSE) +
                    geom_line(color = "cornflowerblue")
                return(ggplotly(g, dynamicTicks = TRUE))
                
            }, warning = function(cond){
            }, error   = function(cond){
            }
        )
    })
    
    output$ts_remainder <- renderPlotly({
        req(vars_data())
        result <- tryCatch(
            expr = {
                g <- plot_stl_diagnostics(
                    .data        = vars_data(),
                    .date_var    = Date,
                    .value       = Value,
                    .message     = FALSE,
                    .feature_set = c("remainder"),
                    .title       = FALSE,
                    .interactive = FALSE) +
                    geom_line(color = "cornflowerblue") +
                    scale_y_continuous(labels = scales::comma_format())
                return(ggplotly(g, dynamicTicks = TRUE))
                
            }, warning = function(cond){
            }, error = function(cond){
            }
        )
    })
    
    ##Plot seasonalities (ts_seasonality)----
    output$ts_seasonality <- renderUI({
        req(vars_data())
        g <- plot_seasonal_diagnostics(.data               = vars_data(),
                                       .date_var           = Date,
                                       .value              = Value,
                                       .feature_set        = "auto")
        
        seasonal_panel = function(m){renderPlotly({m})}
        lapply(1:length(g$x$layout$annotations), function(i){
            seasonal_panel(
                ggplotly(plot_seasonal_diagnostics(.data               = vars_data(),
                                                   .date_var           = Date,
                                                   .value              = Value,
                                                   .feature_set        = g$x$layout$annotations[[i]]$text,
                                                   .geom_color         = "steelblue4",
                                                   .geom_outlier_color = "deeppink3",
                                                   .title              = FALSE,
                                                   .interactive        = FALSE
                ) + scale_y_continuous(labels = scales::comma_format())
                
                )
            )
        })
    })
    
    ##Plot anomalies (ts_anomaly)----
    output$ts_anomaly <- renderPlotly({
        req(vars_data())
        result <- tryCatch(
            expr = {
                g <- plot_anomaly_diagnostics(
                    .data        = vars_data(),
                    .date_var    = Date,
                    .value       = Value,
                    .anom_color  = "deeppink3",
                    .title       = FALSE,
                    .legend_show = FALSE,
                    .interactive = FALSE) +
                    geom_line(color = "cornflowerblue") +
                    scale_y_continuous(labels = scales::comma_format())
                return(ggplotly(g, dynamicTicks = TRUE))
                
            }, warning = function(cond){
                sendSweetAlert(session         = session,
                               title           = "Oops",
                               btn_labels      = "OK",
                               text            = HTML("This feature is not available for time series with missing values."),
                               type            = "error",
                               html            = TRUE,
                               showCloseButton = TRUE)
            }, error = function(cond){
                sendSweetAlert(session         = session,
                               title           = "Oops",
                               btn_labels      = "OK",
                               text            = HTML("This feature is not available for time series with missing values."),
                               type            = "error",
                               html            = TRUE,
                               showCloseButton = TRUE)
            })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
