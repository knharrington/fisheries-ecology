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
  library(thematic)
  library(reactable)
}
# ---------------------------- FUNCTIONS ---------------------------------------
# function to do the opposite of %in%
`%nin%` = Negate(`%in%`)

# ---------------------------- OPTIONS -----------------------------------------
# options(
#   reactable.theme = reactableTheme(
#     color = "hsl(0, 0%, 100%)", # white
#     backgroundColor = "hsl(195, 45%, 37%)"
#   )
# )

# ------------------------- PREPROCESSING --------------------------------------
load("rm-preprocessed.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------

# bar graph
plot_card <- card(
  #full_screen = TRUE,
  class = "bg-secondary",
  height = "600px",
  #card_header("Tagged Fish Composition"),
  card_header(h5("MOST COMMONLY TAGGED FISH SPECIES")),
  card_body(
    plotlyOutput("fish_comp")
  )
)

# map
map_card <- card(
  #full_screen = TRUE,
  class = "bg-secondary",
  card_header(h5("NUMBER OF SPECIES CAUGHT PER AREA AT ROBINSON PRESERVE")),
  card_body(leafletOutput("map", height = "100%"),
            div(id = "translucent_panel",
                p("See the species we found at different years and seasons", style = "font-size: 18px;"),
                layout_columns(
                  col_widths = c(7,5),
                  sliderInput("years", label = span("Time Range", style="font-size: 20px;"), min(seine_data$YEAR), max(seine_data$YEAR),
                            value = c(min(seine_data$YEAR), max(seine_data$YEAR)), sep = "", round = TRUE, step = 1),
                checkboxGroupInput("seasons", label = span("Season", style="font-size: 20px;"),
                                   choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
                                                      HTML("<b>Spring</b> (Apr, May, Jun)"),
                                                      HTML("<b>Summer</b> (Jul, Aug, Sep)"),
                                                      HTML("<b>Fall</b> (Oct, Nov, Dec)")),
                                   selected = list("Winter","Spring", "Summer", "Fall"),
                                   choiceValues = list("Winter","Spring", "Summer", "Fall"))
                )
                
            ), style = "padding: 0; height: 100%;")
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
    layout_columns(
      col_widths = c(8, 4), 
      layout_columns(
        col_widths = c(12),
        #row_heights = c(3,1),
        map_card#, input_card
      ),
  
      layout_column_wrap(
        width=1, heights_equal = "row",
        plot_card, 
        value_box(title = span("Total Number of Fish Tagged", style="font-family: 'Tablet Gothic'; font-size: 20px;"), 
                  value = span(textOutput("tagged_fish"), style="font-family: 'Tablet Gothic';"), 
                  showcase = icon("fish"), showcase_layout = "left center", theme = "secondary"),
        value_box(title = span("Nets for Catching and Releasing Fish", style="font-family: 'Tablet Gothic'; font-size: 20px;"), 
                  value = span(textOutput("seine_nums"), style="font-family: 'Tablet Gothic';"),
                  showcase = tags$img(src = "seine net.png", style = "max-height: 75px; width: auto;"), 
                  showcase_layout = "left center", theme = "secondary")
      )
    )
  )


##############################  SERVER  ########################################

server <- function(input, output, session) {
  
  # ---------------------------- LINKS -----------------------------------------

  
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
  
  # ---------------------------- TABLE -----------------------------------------  
  
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
    plot_ly(
      data = fish_tagged, x = ~Count, y = ~Common_Name, color = ~Common_Name, 
      type = "bar", colors=fish_pal,
      text = ~ paste(
        "<b>Species:</b>", Common_Name,
        "<br><b>Number of Unique Tags:</b>", Count
      ),
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        images = list(
          list(
            source = "common_snook.png",
            row=1, col=1, x=0, y=0.15,
            xanchor="right", 
            yanchor="top", 
            sizex=0.2, 
            sizey=0.2
          ),
          list(
            source = "striped_mullet.png",
            row=1, col=1, x=0, y=0.49,
            xanchor="right", 
            yanchor="top", 
            sizex=0.2, 
            sizey=0.2
          ),
          list(
            source = "ladyfish.png",
            row=1, col=1, x=0, y=0.82,
            xanchor="right", 
            yanchor="top", 
            sizex=0.2, 
            sizey=0.2
          )
        ),
        barmode = "stack",
        xaxis = list(title = "Number of Unique Tags", gridcolor = "#cccccc", fixedrange = TRUE, linecolor = "#cccccc"),
        yaxis = list(title = "", gridcolor = "#cccccc", fixedrange = TRUE, linecolor = "#cccccc"),
        showlegend = FALSE,
        #legend = list(title = list(text = "Common Name")),
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
  # species richness map
  output$map <- renderLeaflet({
    rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Species_Richness, reverse = TRUE)
    icon <- makeAwesomeIcon(icon = "tower-broadcast", library = "fa", markerColor = "darkblue", iconColor = "#FFFFFF")
    leaflet(options = leafletOptions(attributionControl = FALSE, minZoom = 16, maxZoom=18)) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.66765, lat = 27.50865, zoom = 17) %>%
      setMaxBounds(lng1 = -82.655, lat1 = 27.495, lng2 = -82.68, lat2 = 27.52) %>%
      #addAwesomeMarkers(data = antenna_loc, lng = ~Ant_Longitude, lat = ~Ant_Latitude, icon = icon) %>%
      leaflet::addLegend(pal = wbpal, data = seine_data, values = ~Water_Name, opacity = 1, title = HTML("Location")) %>%
      leaflet::addLegend(pal = rpal, data = rich_data_all, values = ~Species_Richness, opacity = 1, title = HTML("Number of<br>Species Caught"))
  })
  
  observe({
    rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Species_Richness, reverse = TRUE)
    popper <- paste0("<strong>Number of Species Caught: </strong>", rich_data()$Species_Richness,
                     "<br><strong>Seine ID: </strong>", rich_data()$Seine_ID)
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