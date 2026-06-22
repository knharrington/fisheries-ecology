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
load("data/se-preprocessed-es.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------
# map
card1 <- card(
  full_screen = TRUE,
  height = "800px",
  card_header("Peces marcados liberados por ubicación"),
  card_body(leafletOutput("map", height = "100%"),
            style = "padding: 0; height: 100%;")
)
# bar graph
card2 <- card(
  full_screen = TRUE,
  card_header("Peces liberados en cada arroyo"),
  card_body(plotlyOutput("fish_bar"))
)


# --------------------------- SIDEBAR ------------------------------------------
sidebar <- sidebar(
  width=350, open="always",
  helpText("Utilice las siguientes opciones para actualizar los datos que se muestran en las figuras."),
  sliderInput("years", label = "Rango de tiempo", min(rels_yr$Year), max(rels_yr$Year),
              value = c(min(rels_yr$Year)), sep = "", round = TRUE, step = 1, animate=TRUE)
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
      tags$h4("Mejora de las poblaciones del róbalo", style = "margin: 0;")
    )
  ),
  
  # Main page structure
  page_sidebar(
    theme = theme,
    sidebar = sidebar,
    layout_columns(col_widths = c(8, 4), 
                   card1,  
                   card2)
  )
)

##############################  SERVER  ########################################

server <- function(input, output, session) {
  
  # ---------------------------- LINKS -----------------------------------------
  
  # ---------------------------- DATA ------------------------------------------

  release_points <- reactive({
    rels_yr %>%
      filter(
        Year == input$years
        # Year >= input$years[1],
        # Year <= input$years[2]
      )
  })
  
  # ---------------------------- TEXT ------------------------------------------
  
  # --------------------------- PLOTS ------------------------------------------
  
  # total fish released at each creek
  output$fish_bar <- renderPlotly({
    release_plot
  })
  
  # ---------------------------- MAPS ------------------------------------------
  # map
  creek_pal <- colorFactor("Paired", domain=sort(unique(rels_yr$Creek_System), decreasing=TRUE))
  rel_pal <-colorNumeric("Blues", domain=rels_yr$Num_Released)
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 9, maxZoom=12)) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      #addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.35, lat = 27.02, zoom = 10) %>%
      addLegend(pal = creek_pal, data = rels_yr, values = ~Creek_System, opacity=1, title = HTML("Sitios de liberación")) %>%
      addLegend(pal = rel_pal, data = rels_yr, values = ~Num_Released, opacity=1, title = HTML("Número de<br>peces liberados"))
  })
  
  observe({
    popper <- paste0("<strong>Sitio de liberación: </strong>", release_points()$Creek_System,
                     "<br><strong>Peces liberados: </strong>", format(release_points()$Num_Released, big.mark=","))
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data=release_points(), lat=~Latitude, lng=~Longitude, 
                       color=~creek_pal(Creek_System), fillColor=~rel_pal(Num_Released), radius = ~Num_Released^(1/3),
                       fillOpacity=1, opacity = 1, weight = 4,
                       popup=~popper)
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)