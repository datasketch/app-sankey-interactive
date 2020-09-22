# load in packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(titanic)
library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(V8)

# load in ds packages
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(shi18ny)
library(hotr)
library(parmesan)
library(paletero)
library(hgchmagic)
library(dsthemer)

# Define UI for app ----
ui <- panelsPage(useShi18ny(),
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
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(
                         langSelectorInput("lang", position = "fixed"),
                         highchartOutput("sankeyChart"),
                         shinypanels::modal(id = "download",
                                            title = ui_("download_plot"),
                                            uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = ui_("download_plot"), modal_id = "download")))



# Define server logic ----
server <- function(input, output) {
  
  i18n <- list(defaultLang = "en",
               availableLangs = c("en", "de", "es", "pt"))
  
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  
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
  
  datasetColumnSelected <- reactive({
    dic_load()$label[1:2]
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
  
  categoriesFill <- reactive({
    req(input$chooseColumns)
    d <- data_load() %>% select(input$chooseColumns) %>% distinct() %>% as.data.frame()
    nodes_unique <- c()
    for(col in input$chooseColumns){
      nodes_unique <- c(nodes_unique, unique(d[,col]))
    }
    nodes_unique
  })
  
  colourCustomChoices <- reactive({
    paletero::paletero_cat(categoriesFill(), palette = "Set1")
  }) 
  
  maxCustomChoices <- reactive({
    length(categoriesFill())
  })
  
  customColours <- reactive({
    colours <- input$colour_custom
    names(colours) <- sort(categoriesFill())
    colours
  })
  
  fillFlow <- reactive({
    flow <- c("from", "to")
    names(flow) <- i_(c("left_to_right", "right_to_left"), lang())
    flow
  })

  
  plot_data <- reactive({
    req(input$chooseColumns)
    data_load() %>% select(input$chooseColumns)
  })
  
  plot <- reactive({
    req(input$chooseColumns)
    palette = input$palette
    if(input$colour_method == "colourpalette"){
      palette <- input$palette
    } else if(input$colour_method == "custom"){
      palette <- customColours()
    }
    # browser()
    if(is.null(palette)) return()
    hgch_sankey_CatCat(plot_data(), color_by = input$fillval, palette_colors = palette,
                       title = input$title, subtitle = input$subtitle, caption = input$caption,
                       background_color = input$background_color)
  })
  
  output$sankeyChart <- renderHighchart({
    plot()
  })
  
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadImageUI("download_data_button", dw, formats = c("html","jpeg", "pdf", "png"))
  })
  
  downloadImageServer("download_data_button", element = plot(), lib = "highcharter", formats = c("html","jpeg", "pdf", "png"))
  
}

# Create Shiny app ----
shinyApp(ui, server)
