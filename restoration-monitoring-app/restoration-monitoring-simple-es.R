# TO DO:
#   - 
# NOTES:
#   - 

##############################  GLOBAL  ########################################
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  library(leaflet)
  library(plotly)
  library(shiny)
  library(shinyWidgets)
  library(htmlwidgets)
  library(bslib)
  library(bsicons)
  library(showtext)
  # library(shinycssloaders)
  # library(leaflet.extras2)
  library(thematic)
}
# ---------------------------- FUNCTIONS ---------------------------------------
# function to do the opposite of %in%
`%nin%` = Negate(`%in%`)

# ---------------------------- OPTIONS -----------------------------------------


# ------------------------- PREPROCESSING --------------------------------------
load("data/rm-preprocessed-es.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------

# bar graph
card2 <- card(
  full_screen = TRUE,
  height = "600px",
  card_header("Composición de los peces etiquetados"),
  card_body(plotlyOutput("fish_comp"))
)

# map
card4 <- card(
  full_screen = TRUE,
  card_header("Riqueza de especies por ubicación"),
  card_body(leafletOutput("map", height = "100%"),
            style = "padding: 0; height: 100%;")
)

# --------------------------- SIDEBAR ------------------------------------------
sidebar <- sidebar(
  width=350,
  helpText("Utilice las siguientes opciones para actualizar los datos mostrados."),
  sliderInput("years", label = "Rango de tiempo", min(seine_data$YEAR), max(seine_data$YEAR),
              value = c(min(seine_data$YEAR), max(seine_data$YEAR)), sep = "", round = TRUE, step = 1),
  checkboxGroupInput("seasons", "Estación", 
                     choiceNames = list(HTML("<b>Invierno</b> (enero, febrero, marzo)"),
                                        HTML("<b>Primavera</b> (abril, mayo, junio)"), 
                                        HTML("<b>Verano</b> (julio, agosto, septiembre)"), 
                                        HTML("<b>Otoño</b> (octubre, noviembre, diciembre)")), 
                     selected = list("Winter","Spring", "Summer", "Fall"),
                     choiceValues = list("Winter","Spring", "Summer", "Fall"))
) # end sidebar

# ----------------------------- MAIN -------------------------------------------
# automatically adjust ggplot themes
thematic::thematic_shiny(font = "auto")

# custom Mote theme
theme <- bs_theme(
  version = 5,
  bg = "#f4f4f4",
  fg = "#000000",
  primary = "#0054a6",
  secondary = "#00aae7"
)

ui <- tagList(
  # Custom full-width header bar
  tags$div(
    style = "background-color: #0054a6;color: white; padding: 10px 20px;
      display: flex; align-items: center; justify-content: space-between;",
    
    # Left side: logo and title
    tags$div(
      style = "display: flex; align-items: center;",
      img(src = "MoteMarineLaboratory_StackedLogo_White.png", style = "height: 30px; margin-right: 15px;"),
      tags$h4("Monitoreo de la restauración en la Reserva Robinson", style = "margin: 0;")
    ),
    
    # Right side: actionLink and GitHub link
    tags$div(
      style = "display: flex; align-items: center; gap: 15px;",
      actionLink("open_about", label = HTML('<i class="fas fa-info-circle"></i> Acerca de los datos'), style = "color: white; text-decoration: none;")
    )
  ),
  
  # Main page structure
  page_sidebar(
    theme = theme,
    sidebar = sidebar,
      layout_columns(
        col_widths = c(8, 4), 
        card4,  
        layout_column_wrap(
          width=1, heights_equal = "row",
          card2, 
          value_box(title = "Peces marcados", value = textOutput("tagged_fish"), 
                    showcase = icon("fish"), showcase_layout = "left center", theme = "text-primary"),
          value_box(title = "Jábegas", value = textOutput("seine_nums"),
                    showcase = bsicons::bs_icon("bucket"), showcase_layout = "left center", theme = "text-primary")
        )
     )
  )
)


##############################  SERVER  ########################################

server <- function(input, output, session) {
  
  # ---------------------------- LINKS -----------------------------------------
  # Open modal for about the data info 
  observeEvent(input$open_about, {
    showModal(modalDialog(
      title = "Acerca de los datos",
      HTML("
      <div style='line-height: 1.6; font-size: 16px;'>
      <p>Los datos que se muestran en esta aplicación se recopilaron mediante redes de arrastre en 
      la Reserva Robinson (2022-2025). Este proyecto forma parte del Programa de Investigación 
      para la Ecología y la Mejora Pesquera del Laboratorio Marino Mote.</p>
    </div>
      "),
      easyClose = TRUE,
      footer = modalButton("Cerca")
    ))
  })
  
  # ---------------------------- DATA ------------------------------------------
  
  # species richness
  rich_data <- reactive({
    seine_data %>%
      filter(YEAR >= input$years[1],
             YEAR <= input$years[2],
             Season %in% input$seasons) %>%
      group_by(Seine_ID) %>%
      add_count(Common_Name, name = "n") %>% 
      mutate(p = n / sum(n)) %>%
      reframe(
        Species_Richness = n_distinct(Common_Name),
        Shannon_Index = -sum(p * log(p)),
        Simpson_Index = 1 - sum(p^2),
        Water_Name = first(Water_Name),
        Seine_Latitude = first(Seine_Latitude),
        Seine_Longitude = first(Seine_Longitude),
        NETTIME = first(NETTIME),
        WQZONE = first(WQZONE),
        MINDEPTH = first(MINDEPTH),
        MAXDEPTH = first(MAXDEPTH),
        SUB1 = first(SUB1),
        SUB2 = first(SUB2),
        SHADE = first(SHADE)
      )
  })
  
  # ---------------------------- TEXT ------------------------------------------
  # print total observations for filtered data in ui
  comp_data <- reactive({
    composition_data %>%
      filter(
        Year >= input$years[1],
        Year <= input$years[2],
        Season %in% input$seasons
      )
  })
  output$tagged_fish <- renderText({
    format(sum(comp_data()$Frequency), big.mark = ",")
  })
  
  output$seine_nums <- renderText({
    if (length(input$seasons) > 0) {
      format(n_distinct(rich_data()$Seine_ID), big.mark = ",")
    } else {
      0
    }
  })
  
  # --------------------------- PLOTS ------------------------------------------

  # tagged fish by species for each sampling event
  output$fish_comp <- renderPlotly({
    composition_plot
  })
  
  # ---------------------------- MAPS ------------------------------------------
  # species richness map
  output$map <- renderLeaflet({
    rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Species_Richness, reverse = TRUE)
    icon <- makeAwesomeIcon(icon = "tower-broadcast", library = "fa", markerColor = "darkblue", iconColor = "#FFFFFF")
    leaflet(options = leafletOptions(minZoom = 16, maxZoom=18)) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.66670, lat = 27.50925, zoom = 17) %>%
      addAwesomeMarkers(data = antenna_loc, lng = ~Ant_Longitude, lat = ~Ant_Latitude, icon = icon) %>%
      leaflet::addLegend(pal = wbpal, data = seine_data, values = ~Water_Name, opacity = 1, title = HTML("Ubicación")) %>%
      leaflet::addLegend(pal = rpal, data = rich_data_all, values = ~Species_Richness, opacity = 1, title = HTML("Riqueza de<br>especies"))
  })
  
  observe({
    rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Species_Richness, reverse = TRUE)
    popper <- paste0("<strong>Riqueza de especies: </strong>", rich_data()$Species_Richness,
                     "<br><strong>Jábega ID: </strong>", rich_data()$Seine_ID)
    if (nrow(rich_data()) > 0 ) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(data = rich_data(), lng = ~Seine_Longitude, lat = ~Seine_Latitude, 
                         radius = ~Species_Richness/1.5, weight = 2, opacity = 0.75, fillOpacity = 0.75, 
                         color = ~wbpal(Water_Name), fillColor = ~rpal(Species_Richness), popup = popper) 
    } else {
      leafletProxy("map") %>%
        clearMarkers()
    }
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)