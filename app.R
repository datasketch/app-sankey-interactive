# load in packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(V8)
library(shinydisconnect)
library(shinybusy)

# load in ds packages
library(dsmodules)
library(dspins)
library(shinyinvoer)
library(shinypanels)
library(shi18ny)
library(hotr)
library(parmesan)
library(paletero)
library(hgchmagic)
library(dsthemer)

webshot::install_phantomjs()

# Sys.setlocale("LC_ALL","C")

styles <- "

#ss-connect-dialog  a::before {
 background: #da1c95 !important;
  }
"

# Define UI for app ----
ui <- panelsPage(useShi18ny(),
                 styles = styles,
                 disconnectMessage(
                   text = "Tu sesión ha finalizado, si tienes algún problema trabajando con la app por favor contáctanos y cuéntanos qué ha sucedido // Your session has ended, if you have any problem working with the app please contact us and tell us what happened.",
                   refresh = "REFRESH",
                   background = "#ffffff",
                   colour = "#435b69",
                   size = 14,
                   overlayColour = "#2a2e30",
                   overlayOpacity = 0.85,
                   refreshColour = "#ffffff",
                   css = "padding: 4.8em 3.5em !important; box-shadow: 0 1px 10px 0 rgba(0, 0, 0, 0.1) !important;"
                 ),
                 busy_start_up(
                   loader = tags$img(
                     src = "img/loading_gris.gif",
                     width = 100
                   ),
                   #text = "Loading...",
                   mode = "auto",
                   #timeout = 3500,
                   color = "#435b69",
                   background = "#FFF"
                 ),
                 langSelectorInput("lang", position = "fixed"),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("dataInput")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       body = div(
                         div(
                           uiOutput("select_var"),
                           uiOutput("dataset")
                         )
                       )),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body =  uiOutput("viz")))



# Define server logic ----
server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en",
               availableLangs = c("en", "de", "es", "pt"))
  
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  
  observeEvent(lang(),{
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })
  
  output$dataInput <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 i_("input_data", lang()),
                 choices = choices,
                 selected =  "sampleData")
  })
  
  sample_data <- reactive({
    sm_f <- list(File_1 = "data/titanic_data.csv",
                 File_2 = "data/election_data.csv")
    names(sm_f) <- i_(c("sample_titanic_name", "sample_elections_name"), lang())
    sm_f
  })
  
  inputData <- tableInputServer("initial_data", 
                                sampleLabel = i_("sample_lb", lang()),
                                sampleFiles = sample_data(),
                                sampleSelected = names(reactive(sample_data()))[1],
                                
                                pasteLabel = i_("paste", lang()),
                                pasteValue = "",
                                pastePlaceholder = i_("paste_pl", lang()),
                                pasteRows = 5,
                                
                                uploadLabel = i_("upload_lb", lang()),
                                uploadButtonLabel = i_("upload_bt_lb", lang()),
                                uploadPlaceholder = i_("upload_pl", lang()),
                                
                                googleSheetLabel = i_("google_sh_lb", lang()),
                                googleSheetValue = "",
                                googleSheetPlaceholder = i_("google_sh_pl", lang()),
                                googleSheetPageLabel = i_("google_sh_pg_lb", lang()))
  
  output$dataset <- renderUI({
    if (is.null(inputData()))
      return()
    suppressWarnings(hotr("hotr_input", data = inputData(), options = list(height = 470)))
  })
  
  output$select_var <- renderUI({
    selectInput(inputId = "chooseColumns", label= i_("chooseColumns", lang()),
                choices = datasetColumnChoices(),
                selected = datasetColumnSelected(),
                multiple = TRUE)
  })
  
  data_fringe <- reactive({
    suppressWarnings( hotr::hotr_fringe(input$hotr_input))
  })
  
  dic_load <- reactive({
    data_fringe()$dic
  })
  
  data_load <- reactive({
    data <- data_fringe()$data
    names(data) <- dic_load()$label
    as.data.frame(data)
  })
  
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  
  datasetColumnChoices <- reactive({
    dic_load()$label
  })
  
  moreDataInfo <- reactive({
    data_load() %>% map_df( ~ (data.frame(
      n_distinct = n_distinct(.x),
      class = class(.x)
    )),
    .id = "variable") %>% filter(!class == "vctrs_vctr")
  })
  
  datasetColumnSelected <- reactive({
    possible_columns <- moreDataInfo() %>% filter(n_distinct <= 20) %>% distinct(variable) %>% pull()
    dic_cat <- dic_load() %>% filter(hdType %in% c("Cat", "Dat")) %>% filter(label %in% possible_columns)
    dic_cat$label[1:2]
  })
  
  dic_draw <- reactive({
    moreDataInfo() %>% filter(variable %in% input$chooseColumns)
  })
  
  fillValueSelected <- reactive({
    datasetColumnSelected()[1]
  })
  
  colourMethodChoices <- reactive({
    colour_method_choices <- list("colourpalette" = "colourpalette", "custom" = "custom")
    names(colour_method_choices) <- i_(names(colour_method_choices), lang())
    colour_method_choices
  })
  
  background <- reactive({
    dsthemer_get("datasketch")$background_color
  })
  
  colourPaletteChoices <- reactive({
    c("Accent", "Dark2", "Paired", "Pastel1",
      "Pastel2", "Set1", "Set2", "Set3", "Greys")
  })
  
  dataLabelChoices <- reactive({
    label <- c("node_name", "total", "percentage")
    names(label) <- i_(c("node_name", "total", "percentage"), lang())
    label
  })
  
  categoriesFill <- reactive({
    req(plot_data_orig())
    d <- plot_data_orig() %>% distinct() %>% as.data.frame()
    nodes_unique <- c()
    for(col in names(d)){
      nodes_unique <- c(nodes_unique, unique(d[,col]))
    }
    unique(nodes_unique)
  })
  
  categoriesMissingsEncode <- reactive({
    categoriesFill()[!is.na(categoriesFill())]
  })
  
  colourCustomChoices <- reactive({
    paletero::paletero_cat(categoriesFill(), palette = "Set1")
  }) 
  
  maxCustomChoices <- reactive({
    length(categoriesFill())
  })
  
  customColours <- reactive({
    req(input$colour_custom)
    colours <- input$colour_custom
    names(colours) <- sort(categoriesFill())
    colours
  })
  
  fillFlow <- reactive({
    flow <- c("from", "to")
    names(flow) <- i_(c("left_to_right", "right_to_left"), lang())
    flow
  })

  
  plot_data_orig <- reactive({
    req(input$chooseColumns)
    if(!any(input$chooseColumns %in% names(data_load()))) return()
    if(!all(dic_draw()$class %in% c("hd_Cat", "hd_Dat")) | any(dic_draw()$n_distinct > 20)) return()
    if(length(input$chooseColumns) < 2) return()
    d <- data_load() %>% select(input$chooseColumns)
    if(any(dic_draw()$class == "hd_Dat")){
      dat_cols <- dic_draw()[dic_draw()$class == "hd_Dat",]$variable
      d <- d %>% 
        mutate_at(vars(all_of(dat_cols)), ~homodatum::as_Cat(as.character(.)))
    }
    d
  })
  
  hasdataNA <- reactive({
    req(plot_data_orig())
    # req(input$code_as_na)
    cols_contain_na <- purrr::map_lgl(.x = plot_data_orig(),
                                      .f = function(.x) any(is.na(.x)))
    if(length(input$code_as_na > 0)){
      cols_contain_na <- c(cols_contain_na, TRUE)
    }
    any(cols_contain_na)
  })
  
  input_drop_na <- reactive({
    if(is.null(input$drop_na)){
      drop_na <- FALSE
    } else {
      drop_na <- input$drop_na
    }
    drop_na
  })
  
  plot_data <- reactive({
    req(plot_data_orig())
    d <- plot_data_orig()
    if(!is.null(input$code_as_na)){
      d <- d %>% purrr::map_df(function(.x) {
                            .x[.x %in% input$code_as_na] <- NA
                            .x
                            })
    }
    if(!is.null(input$drop_na)){
      if(input$drop_na){
        d <- d %>% filter(complete.cases(.))
      }
    }
    if(!is.null(input$na_label)){
      d[is.na(d)] <- input$na_label
    }
    d
  })
  
  hgch_viz <- reactive({
    req(input$chooseColumns)
    req(plot_data())
    palette <- input$palette
    if(input$colour_method == "colourpalette"){
      palette <- input$palette
    } else if(input$colour_method == "custom"){
      req(customColours())
      palette <- customColours()
    }
    if(is.null(palette)) return()
    # browser()
    opts <- dsvizopts::merge_dsviz_options(color_by = input$fillval, palette_colors = palette,
                                           title = input$title, subtitle = input$subtitle, caption = input$caption,
                                           background_color = input$background_color, dataLabels_type = input$dataLabel_type)
    
    if(!is.null(input$caption)){
      if(nchar(input$caption) > 0){
        opts <- c(opts, plot_margin_bottom = 55)
      }
    }
    
    viz <- do.call("hgch_sankey_CatCat", c(list(data = plot_data(), opts = opts
    )))
    
    viz
  })
  
  output$sankeyChart <- renderHighchart({
    if(is.null(hgch_viz())) return()
    hgch_viz()
  })
  
  output$viz <- renderUI({
    if(is.null(dic_draw()) | nrow(dic_draw()) == 0)return()
    if((!all(dic_draw()$class %in% c("hd_Cat", "hd_Dat")) | any(dic_draw()$n_distinct > 20)) | length(input$chooseColumns) < 2){
      v <- div(shinypanels::infomessage(type = "warning" , i_("cannot_plot", lang())),
               shinypanels::infomessage(type = "info" , i_("data_advice", lang())))
    } else {
      v <- highchartOutput("sankeyChart")
    }
    v
  })
  
  
  output$download <- renderUI({
    
    downloadDsUI("download_data_button",
                 display = "dropdown",
                 formats = c("html","jpeg", "pdf", "png"),
                 dropdownWidth = 170,
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 text = i_("download", lang()), 
                 dropdownLabel = i_("download_viz", lang()), 
                 getLinkLabel = i_("get_link", lang()), 
                 modalTitle = i_("get_link", lang()), 
                 modalButtonLabel = i_("gl_save", lang()), 
                 modalLinkLabel = i_("gl_url", lang()), 
                 modalIframeLabel = i_("gl_iframe", lang()),
                 nameLabel = i_("gl_name", lang()),
                 descriptionLabel = i_("gl_description", lang()),
                 sourceLabel = i_("gl_source", lang()),
                 sourceTitleLabel = i_("gl_source_name", lang()),
                 sourcePathLabel = i_("gl_source_path", lang()),
                 licenseLabel = i_("gl_license", lang()),
                 tagsLabel = i_("gl_tags", lang()),
                 tagsPlaceholderLabel = i_("gl_type_tags", lang()),
                 categoryLabel = i_("gl_category", lang()),
                 categoryChoicesLabels = i_("gl_no_category", lang())
                 )
  })
  
  par <- list(user_name = "test", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  
  observe({
    req(hgch_viz())
    user_name <- url_par()$inputs$user_name
    org_name <- url_par()$inputs$org_name
    if (is.null(user_name) & is.null(user_name)) return()
    downloadDsServer(id = "download_data_button",
                     element = reactive(hgch_viz()),
                     formats = c("html", "jpeg", "pdf", "png"),
                     errorMessage = i_("error_down", lang()),
                     elementType = "dsviz",
                     user_name = user_name,
                     org_name = org_name)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
