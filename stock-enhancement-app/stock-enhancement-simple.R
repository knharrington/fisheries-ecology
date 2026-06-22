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
# map
card1 <- card(
  #full_screen = TRUE,
  class = "bg-secondary",
  #height = "800px",
  card_header("Tagged Fish Released by Location"),
  card_body(
    leafletOutput("map", height = "100%"),
    div(id = "translucent_panel",
        sliderInput("years", label = "Year", min(rels_yr$Year), max(rels_yr$Year),
          value = c(min(rels_yr$Year)), sep = "", round = TRUE, step = 1, animate=TRUE)),
    div(id = "snook_photo", img(src="CommonSnook.jpg", width="340px")),
    style = "padding: 0; height: 100%;"
    )
)
# bar graph
card2 <- card(
  class = "bg-secondary",
  #full_screen = TRUE,
  card_header("Fish Released in Each Creek"),
  card_body(plotlyOutput("fish_bar"))
)

# --------------------------- SIDEBAR ------------------------------------------
# sidebar <- sidebar(
#   width=350, open="always",
#   helpText("Use the following selections to update the data displayed in the figures."),
#   sliderInput("years", label = "Time Range", min(rels_yr$Year), max(rels_yr$Year),
#               value = c(min(rels_yr$Year)), sep = "", round = TRUE, step = 1, animate=TRUE)
# ) # end sidebar

# ----------------------------- MAIN -------------------------------------------
# automatically adjust ggplot themes
thematic::thematic_shiny(font = "auto")

# custom Mote theme
theme <- bs_theme(
  version = 5,
  bg = "#02426A",
  fg = "#ffffff",
  primary = "#B5D675",
  secondary = "#34758A"
)

# ui <- tagList(
#   # Custom full-width header bar
#   tags$div(
#     style = "background-color: #0054a6;color: white; padding: 10px 20px;
#       display: flex; align-items: center; justify-content: space-between;",
#     
#     # Left side: logo and title
#     tags$div(
#       style = "display: flex; align-items: center;",
#       img(src = "MoteMarineLaboratory_StackedLogo_White.png", style = "height: 30px; margin-right: 15px;"),
#       tags$h4("Common Snook Stock Enhancement", style = "margin: 0;")
#     )
#   ),
#   
#   # Main page structure
#   page_sidebar(
#     theme = theme,
#     sidebar = sidebar,
#     layout_columns(col_widths = c(8, 4), 
#                    card1,  
#                    card2)
#   )
# )

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
      background-color: rgba(2, 66, 106, 0.9); /* Translucent white */
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
  ")),
    theme = theme,
    #sidebar = sidebar,
    layout_columns(col_widths = c(8, 4), 
                   card1,  
                   card2)
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
  rel_pal <-colorNumeric(palette = blue_palette(7), domain=rels_yr$Num_Released)
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE, minZoom = 9, maxZoom=12)) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      #addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.45, lat = 27.02, zoom = 10) %>%
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