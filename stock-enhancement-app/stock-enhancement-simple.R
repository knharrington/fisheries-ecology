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
load("se-preprocessed.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------

photo_card <- card(
  class = "bg-secondary",
  card_header(h5("IN THE FIELD")),
  card_body(
    img(src="Snook Release 6_3_202622.jpg"),
    p("Mote scientists pick times and places where the snook are likely to survive based on research.", style = "font-size: 18px;"),
    p("Explore the graphic map to see how many we release in each location.", style = "font-size: 18px;")
  )
)

map_card <- card(
  #full_screen = TRUE,
  class = "bg-secondary",
  #height = "800px",
  card_header(h5("TAGGED FISH RELEASED BY LOCATION")),
  card_body(
    leafletOutput("map", height = "100%"),
    div(id = "translucent_panel",
        sliderInput("years", label = span("Year", style="font-size: 20px;"), min(rels_yr$Year), max(rels_yr$Year),
          value = c(min(rels_yr$Year)), sep = "", round = TRUE, step = 1, animate=TRUE)),
    #div(id = "snook_photo", img(src="CommonSnook.jpg", width="340px")),
    style = "padding: 0; height: 100%;"
    )
)

# bar graph
plot_card <- card(
  class = "bg-secondary",
  #full_screen = TRUE,
  card_header(h5("FISH RELEASED IN EACH CREEK")),
  card_body(plotlyOutput("fish_bar"))
)

# --------------------------- SIDEBAR ------------------------------------------

# ----------------------------- MAIN -------------------------------------------
# automatically adjust ggplot themes
thematic::thematic_shiny(font = "auto")

# custom Mote theme
theme <- bs_theme(
  version = 5,
  bg = "#02426A",
  fg = "#ffffff",
  primary = "#8bc7ca",
  secondary = "#34758A",
  heading_font = font_face(
      family = "Futura",
      src = "url('/fonts/FuturaExtraBold.ttf') format('truetype')",
      style="normal"
    ),
  base_font = font_face(
      family = "Tablet Gothic",
      src = "url('/fonts/TabletGothic.ttf') format('truetype')",
      style="normal"
    )
)

ui <- page_fillable(
  tags$style(HTML("
    #time_slider {
      position: absolute;
      bottom: 30px;
      left: 30px;
      z-index: 1000; /* Ensure input is above map */
    }
    
    #translucent_panel {
      position: absolute;
      bottom: 20px;
      left: 30px;
      background-color: rgba(2, 66, 106, 1.0); /* Opaque blue */
      padding: 20px;
      border-radius: 5px;
      z-index: 999; /* Ensure panel is below slider */
    }
    
    #snook_photo {
    position: absolute;
    bottom: 210px;
    left: 30px;
    z-index: 1001;
    }
    
    @font-face {
    font-family: 'Tablet Gothic';
    src: url('fonts/TabletGothic.ttf') format('truetype');
    font-weight: 400;
    font-style: normal;
    }
    
    @font-face {
    font-family: 'Futura';
    src: url('fonts/FuturaExtraBold.ttf') format('truetype');
    font-weight: 400;
    font-style: normal;
    }
    
    .info.legend.leaflet-control {
    font-family: 'Tablet Gothic', sans-serif !important;
    }
    
    /* font size for time slider */
    .irs-grid-text { font-size: 13px !important; }
    .irs-min, .irs-max { font-size: 13px !important; }
    .irs-single, .irs-from, .irs-to { font-size: 15px !important; }
    
  ")),
    theme = theme,
    #sidebar = sidebar,
    layout_columns(col_widths = c(3, 6, 3), 
                   photo_card,
                   map_card,  
                   plot_card)
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
    plot_ly(
      data = rels_yr, x = ~Year, y = ~Num_Released, color = ~Creek_System,
      type = "bar", colors="Paired",
      text = ~ paste(
        "<b>Year:</b>", Year,
        "<br><b>Release Site:</b>", Creek_System,
        "<br><b>Number of Fish Released:</b>", format(Num_Released, big.mark=",")
      ),
      marker = list(line = list(color = "grey30", width=0.5)),
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", gridcolor = "#cccccc", fixedrange = TRUE),
        yaxis = list(title = "Number of Fish Released", gridcolor = "#cccccc", fixedrange = TRUE),
        legend = list(title = list(text = "Release Site")),
        font = list(color="white", family="Tablet Gothic", size=14),
        hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displaylogo = FALSE,
        displayModeBar = FALSE,
        modeBarButtonsToRemove = c('toImage', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
      )
  })
  
  # ---------------------------- MAPS ------------------------------------------
  # map
  creek_pal <- colorFactor("Paired", domain=sort(unique(rels_yr$Creek_System), decreasing=TRUE))
  rel_pal <-colorNumeric(palette = blue_palette(7), domain=rels_yr$Num_Released)
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE, minZoom = 9, maxZoom=12)) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      #addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.45, lat = 27.02, zoom = 10) %>%
      setMaxBounds(lng1 = -81.5, lat1 = 26, lng2 = -83.5, lat2 = 28) %>%
      addLegend(pal = creek_pal, data = rels_yr, values = ~Creek_System, opacity=1, title = HTML("Release Sites")) %>%
      addLegend(pal = rel_pal, data = rels_yr, values = ~Num_Released, opacity=1, title = HTML("Number of Fish<br>Released"))
  })
  
  observe({
    popper <- paste0("<strong>Release Site: </strong>", release_points()$Creek_System,
                     "<br><strong>Fish Released: </strong>", format(release_points()$Num_Released, big.mark=","))
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