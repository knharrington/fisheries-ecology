# TO DO:
#   - preprocessing needs its own script; use .RData object for dataset
# NOTES:
#   - 

##############################  GLOBAL  ########################################
#library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(leaflet)
library(plotly)
#library(ggsvg)
#library(ggimage)
#library(glue)
library(shiny)
library(shinyWidgets)
library(htmlwidgets)
library(bslib)
library(showtext)
library(shinycssloaders)
library(leaflet.extras2)
library(thematic)
library(progress)

# ---------------------------- FUNCTIONS ---------------------------------------
# function to do the opposite of %in%
`%nin%` = Negate(`%in%`)


# ---------------------------- OPTIONS -----------------------------------------


# ------------------------- PREPROCESSING --------------------------------------
load("data/preprocessed.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------
# Abacus plot
card1 <- card(
  full_screen = TRUE,
  height = "800px",
  card_header("Habitat Use"),
  card_body(plotlyOutput("abacus_plot"))
)
# bar graph
card2 <- card(
  full_screen = TRUE,
  card_header("Seined Fish Composition"),
  card_body(plotlyOutput("fish_comp"))
)
# line graph
card3 <- card(
  full_screen = TRUE,
  card_header("Species Richness Over Time"),
  card_body(plotlyOutput("sp_rich"))
)
# map
card4 <- card(
  full_screen = TRUE,
  card_header("Species Richness by Location"),
  card_body(leafletOutput("map", height = "100%"),
            style = "padding: 0; height: 100%;")
)

# --------------------------- SIDEBAR ------------------------------------------
sidebar <- sidebar(
  width=400,
  helpText("Use the following selections to update the data displayed in the plots."),
  pickerInput("select_species", label="Select Species", choices = c("Common snook", "Sheepshead", "Red drum"), 
               selected = "Common snook", #selectize = TRUE, multiple=TRUE,
               options = pickerOptions(container = "body", liveSearch = TRUE, actionsBox = TRUE,
                                       style = "btn-outline-primary",
                                       selectedTextFormat = "count > 2", countSelectedText = "{0} species selected"), width = "100%"),
  checkboxGroupInput("size", "Fish Size", 
                     choiceNames = list(HTML("<b>Large</b> (> 300 mm)"),
                                        HTML("<b>Medium</b> (150 mm - 300 mm)"), 
                                        HTML("<b>Small</b> (< 150 mm)")), 
                     selected = list("Large", "Medium", "Small"),
                     choiceValues = list("Large", "Medium", "Small")),
  hr(),
  helpText("Use the following selections to update the data displayed on the map."),
  sliderInput("years", label = "Time Range", min(seine_data$YEAR), max(seine_data$YEAR),
              value = c(min(seine_data$YEAR), max(seine_data$YEAR)), sep = "", round = TRUE, step = 1),
  checkboxGroupInput("seasons", "Season", 
                    choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
                                         HTML("<b>Spring</b> (Apr, May, Jun)"), 
                                         HTML("<b>Summer</b> (Jul, Aug, Sep)"), 
                                         HTML("<b>Fall</b> (Oct, Nov, Dec)")), 
                    selected = list("Winter","Spring", "Summer", "Fall"),
                    choiceValues = list("Winter","Spring", "Summer", "Fall"))#,
  #actionButton("update", "Update Map", icon=icon("refresh"), class="btn btn-primary", style = "color: white;")
) # end sidebar

# ----------------------------- MAIN -------------------------------------------
# automatically adjust ggplot themes
thematic::thematic_shiny(font="auto")

# custom Mote theme
theme <- bs_theme(
  version = 5,
  bg = "#f4f4f4",
  fg = "#000000",
  primary = "#0054a6",
  secondary = "#00aae7"#,
  #base_font = font_google("Roboto Condensed"),
  #heading_font = font_google("Roboto Condensed")
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
      tags$h4("Restoration Monitoring at Robinson Preserve", style = "margin: 0;")
    ),
    
    # Right side: actionLink and GitHub link
    tags$div(
      style = "display: flex; align-items: center; gap: 15px;",
      actionLink("open_about", label = HTML('<i class="fas fa-info-circle"></i> About the Data'), style = "color: white; text-decoration: none;"),
      tags$a(
        HTML('<i class="fab fa-github"></i> GitHub'),
        href = "https://github.com/knharrington",
        target = "_blank",
        style = "color: white; text-decoration: none;"
      )
    )
  ),
  
  # Main page structure
  page_sidebar(
    theme = theme,
    sidebar = sidebar,
    layout_columns(col_widths = c(2,5,5), # or layout_column_wrap()
      value_box(title = "Tagged Fish", value = textOutput("tagged_fish"), 
                showcase = icon("fish"), theme = "text-primary"),
      card2,  
      card3), 
    layout_columns(col_widths = c(8, 4), 
      card1,  
      card4)
  )
)


##############################  SERVER  ########################################

server <- function(input, output, session) {
  
  # ---------------------------- LINKS -----------------------------------------
  # Open modal for about the data info 
  observeEvent(input$open_about, {
    showModal(modalDialog(
      title = "About the Data",
      HTML("
      <div style='line-height: 1.6; font-size: 16px;'>
      <p>The data displayed in this app was collected through XXX (2016–2024). This effort is part of the Fisheries Ecology and Enhancement
      Research Program at Mote Marine Laboratory.</p>
      <p>
        For more information, please visit 
        <a href='https://www.mote.org' target='_blank' style='color: #00aae7; text-decoration: underline;'>
          mote.org.
        </a>
      </p>
    </div>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ---------------------------- DATA ------------------------------------------
  
  # habitat use
  hab_use <- reactive({
    pit_sample_day %>%
      filter(Common_Name %in% input$select_species, 
             Fish_Size %in% input$size)
  })
  
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
  
  # --------------------------- TABLES -----------------------------------------
  
  
  # --------------------------- PLOTS ------------------------------------------
  # abacus plot
  output$abacus_plot <- renderPlotly({
    plot_ly() %>%
      add_markers(
        data = hab_use(), x = ~Date, y = ~Tag_ID,
        color=~Water_Body, colors=dark2_pal
      ) %>%
      layout(
        xaxis = list(title = "Detection Date", gridcolor = "#cccccc"),
        yaxis = list(title = "Tag ID", gridcolor = "#cccccc"),
        legend = list(title = list(text = "Location")),
        hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)",  
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # tagged fish by species for each sampling event
  output$fish_comp <- renderPlotly({
    composition_plot
  })
  
  # species richness by waterbody
  output$sp_rich <- renderPlotly({
    rich_time_plot
  })
  
  # ---------------------------- MAPS ------------------------------------------
  output$map <- renderLeaflet({
    #wbpal <- colorFactor(palette="Dark2", domain=seine_data$Water_Name)
    rpal <- colorNumeric(palette="RdYlBu", domain=rich_data_all$Shannon_Index, reverse=TRUE)
    #icon <- makeAwesomeIcon(icon="tower-broadcast", library="fa", markerColor = "darkblue", iconColor = "#FFFFFF")
    
  if (nrow(rich_data()) >0 ) { 
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery")%>%
      setView(lng=-82.66670, lat=27.50980, zoom=16)%>%
      #addAwesomeMarkers(data=antenna_loc, lng=~Ant_Longitude, lat=~Ant_Latitude, popup=~Antenna, icon=icon) %>%
      addCircleMarkers(data=rich_data(), lng=~Seine_Longitude, lat=~Seine_Latitude, 
                       radius=~Shannon_Index*2.5, weight=2, opacity=0.75, fillOpacity=0.75, 
                       color=~wbpal(Water_Name), fillColor=~rpal(Shannon_Index)) %>%
      leaflet::addLegend(pal=wbpal, data=seine_data, values=~Water_Name, opacity=1, title=HTML("Location")) %>%
      leaflet::addLegend(pal=rpal, data=rich_data_all, values=~Shannon_Index, opacity=1, title=HTML("Shannon<br>Index"))
  } else {
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery")%>%
      setView(lng=-82.66670, lat=27.50980, zoom=16) %>%
      addMarkers(data=antenna_loc, lng~Ant_Longitude, lat=~Ant_Latitude)
  }
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)