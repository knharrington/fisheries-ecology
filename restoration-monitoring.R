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
load("data/rm-preprocessed.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------
# Abacus plot
card1 <- card(
  full_screen = TRUE,
  height = "800px",
  card_header("Habitat Use", 
              tooltip(
                bs_icon("info-circle"),
                "Up to 40 fish are randomly selected for display to keep the plot readable."
                )),
  card_body(plotlyOutput("abacus_plot"))
)
# bar graph
card2 <- card(
  full_screen = TRUE,
  card_header("Tagged Fish Composition"),
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
  width=350,
  helpText("Use the following selections to update the data displayed in the habitat use plot."),
  selectInput("select_species", label="Select Species", 
              choices = c("Common snook", "Striped mullet"), selected = "Common snook"), 
  checkboxGroupInput("size", "Fish Age Class", 
                     choiceNames = list(HTML("<b>Adult</b>"),
                                        HTML("<b>Subadult</b>"), 
                                        HTML("<b>Juvenile</b>")), 
                     selected = list("Adult", "Subadult", "Juvenile"),
                     choiceValues = list("Adult", "Subadult", "Juvenile")),
  hr(),
  helpText("Use the following selections to update the data displayed on the species richness map."),
  sliderInput("years", label = "Time Range", min(seine_data$YEAR), max(seine_data$YEAR),
              value = c(min(seine_data$YEAR), max(seine_data$YEAR)), sep = "", round = TRUE, step = 1),
  checkboxGroupInput("seasons", "Season", 
                    choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
                                         HTML("<b>Spring</b> (Apr, May, Jun)"), 
                                         HTML("<b>Summer</b> (Jul, Aug, Sep)"), 
                                         HTML("<b>Fall</b> (Oct, Nov, Dec)")), 
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
        href = "https://github.com/knharrington/fisheries-ecology",
        target = "_blank",
        style = "color: white; text-decoration: none;"
      )
    )
  ),
  
  # Main page structure
  page_sidebar(
    theme = theme,
    sidebar = sidebar,
    layout_columns(col_widths = c(2, 5, 5),
      layout_column_wrap(
        value_box(title = "Tagged Fish", value = textOutput("tagged_fish"), 
                  showcase = icon("fish"), showcase_layout = "top right", theme = "text-primary"),
        value_box(title = "Seines", value = textOutput("seine_nums"), 
                  showcase = bsicons::bs_icon("bucket"), showcase_layout = "top right", theme = "text-primary")
      ),
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
      <p>The data displayed in this app was collected through seining and PIT tag antenna arrays at Robinson Preserve
      (2022–2025). This effort is part of the Fisheries Ecology and Enhancement
      Research Program at Mote Marine Laboratory.</p>
      <p>
        For more information, please visit 
        <a href='https://mote.org/research/program/fisheries-ecology-enhancement/' target='_blank' style='color: #00aae7; text-decoration: underline;'>
          mote.org/research/program/fisheries-ecology-enhancement.
        </a>
      </p>
    </div>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ---------------------------- DATA ------------------------------------------
  
  # habitat use tag order for plotting (random sample)
  tags_order <- reactive({
    req(length(input$size) > 0)
    set.seed(123)
    pit_sample_day %>%
      filter(Common_Name %in% input$select_species, 
             Fish_Class %in% input$size) %>%
      group_by(Tag_ID) %>%
      summarise(First_Det = min(Date)) %>%
      slice_sample(n = 40) %>%
      arrange(desc(First_Det)) %>%
      pull(Tag_ID)
  })
  
  # habitat use dataset
  hab_use <- reactive({
    req(tags_order(), length(input$size) > 0)
    pit_sample_day %>%
      filter(Common_Name %in% input$select_species, 
             Fish_Class %in% input$size,
             Tag_ID %in% tags_order()) %>%
      mutate(Tag_ID = factor(Tag_ID, levels = tags_order()))
  })
  
  # habitat use segment creation
  hab_segments <- reactive({
    req(hab_use())
    hab_use() %>%
      arrange(Tag_ID, Date) %>%
      group_by(Tag_ID) %>%
      mutate(
        Segment_ID = cumsum(Water_Body != lag(Water_Body, default = first(Water_Body)))
      ) %>%
      group_by(Tag_ID, Segment_ID, Water_Body) %>%
      summarise(
        Common_Name = first(Common_Name),
        Fish_Size = first(Fish_Class),
        Start_Date = min(Date),
        End_Date = max(Date),
        .groups = "drop"
      )
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
  
  # ---------------------------- TEXT ------------------------------------------
  # print total observations for filtered data in ui
  tagged_fish_data <- reactive({
    pit_sample_day %>%
      filter(Common_Name %in% input$select_species, 
             Fish_Class %in% input$size)
  })
  output$tagged_fish <- renderText({
    format(n_distinct(tagged_fish_data()$Tag_ID), big.mark = ",")
  })
  
  output$seine_nums <- renderText({
    if (length(input$seasons) > 0) {
      format(n_distinct(rich_data()$Seine_ID), big.mark = ",")
    } else {
      0
    }
  })
  
  # --------------------------- PLOTS ------------------------------------------
  # abacus plot
  output$abacus_plot <- renderPlotly({
    req(hab_segments(), hab_use())
    plot_ly() %>%
      add_segments(
        data = hab_segments(),
        x = ~Start_Date, xend = ~End_Date,
        y = ~Tag_ID, yend = ~Tag_ID,
        color = ~Water_Body,
        colors = dark2_pal,
        hoverinfo = "none",
        line = list(width = 5)
      ) %>%
      add_markers(
        data = hab_use(), x = ~Date, y = ~Tag_ID,
        color = ~Water_Body, colors = dark2_pal, marker = list(size = 4),
        text = ~paste(
          "<b>Location:</b>", Water_Body,
          "<br><b>Detection Date:</b>", paste0(format(Date, format="%b %d, %Y")),
          "<br><b>Tag ID:</b>", Tag_ID),
        hoverinfo = "text", showlegend = FALSE
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
  # species richness map
  output$map <- renderLeaflet({
    rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Species_Richness, reverse = TRUE)
    icon <- makeAwesomeIcon(icon = "tower-broadcast", library = "fa", markerColor = "darkblue", iconColor = "#FFFFFF")
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.66670, lat = 27.50980, zoom = 16) %>%
      addAwesomeMarkers(data = antenna_loc, lng = ~Ant_Longitude, lat = ~Ant_Latitude, icon = icon) %>%
      leaflet::addLegend(pal = wbpal, data = seine_data, values = ~Water_Name, opacity = 1, title = HTML("Location")) %>%
      leaflet::addLegend(pal = rpal, data = rich_data_all, values = ~Species_Richness, opacity = 1, title = HTML("Species<br>Richness"))
  })
  
  observe({
    rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Species_Richness, reverse = TRUE)
    popper <- paste0("<strong>Species Richness: </strong>", rich_data()$Species_Richness,
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