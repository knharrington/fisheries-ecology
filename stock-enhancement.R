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
load("data/se-preprocessed.RData")

##############################  UI  ############################################

# ---------------------------- CARDS -------------------------------------------
# map
card1 <- card(
  full_screen = TRUE,
  height = "800px",
  card_header("Fish Released by Location"),
  card_body(leafletOutput("map", height = "100%"),
            style = "padding: 0; height: 100%;")
)
# bar graph
card2 <- card(
  full_screen = TRUE,
  card_header("Fish Detected at Each Antenna"),
  card_body(plotlyOutput("fish_bar"))
)
# abacus plot
card3 <- card(
  full_screen = TRUE,
  card_header("Fish Detections Over Time"),
  card_body(plotlyOutput("abacus_plot"))
)
# post-release survival
card4 <- card(
  full_screen = TRUE,
  card_header("Proportion of Fish Detected After Release"),
  card_body(plotlyOutput("fish_survival"))
)

# --------------------------- SIDEBAR ------------------------------------------
sidebar <- sidebar(
  width=350,
  helpText("Use the following selections to update the data displayed in the figures."),
  selectInput("select_event", label="Select Release Event", 
              choices = sort(unique(release_survival$Release_Event)), selected = sort(unique(release_survival$Release_Event))[[1]]), 
  # checkboxGroupInput("region", "Region", 
  #                    choiceNames = list(HTML("<b>Upper</b>"),
  #                                       HTML("<b>Lower</b>"), 
  #                                       HTML("<b>Other</b>")), 
  #                    selected = list("Upper", "Lower", "Misc"),
  #                    choiceValues = list("Upper", "Lower", "Misc")),
  checkboxGroupInput("size", "Fish Age Class", 
                     choiceNames = list(HTML("<b>Adult</b> > 340 mm"),
                                        HTML("<b>Subadult</b> 180 mm - 340 mm"), 
                                        HTML("<b>Juvenile</b> < 140 mm")), 
                     selected = list("Adult", "Subadult", "Juvenile"),
                     choiceValues = list("Adult", "Subadult", "Juvenile"))
  # hr(),
  # helpText("Use the following selections to update the data displayed on the species richness map."),
  # sliderInput("years", label = "Time Range", min(seine_data$YEAR), max(seine_data$YEAR),
  #             value = c(min(seine_data$YEAR), max(seine_data$YEAR)), sep = "", round = TRUE, step = 1),
  # checkboxGroupInput("seasons", "Season", 
  #                   choiceNames = list(HTML("<b>Winter</b> (Jan, Feb, Mar)"),
  #                                        HTML("<b>Spring</b> (Apr, May, Jun)"), 
  #                                        HTML("<b>Summer</b> (Jul, Aug, Sep)"), 
  #                                        HTML("<b>Fall</b> (Oct, Nov, Dec)")), 
  #                   selected = list("Winter","Spring", "Summer", "Fall"),
  #                   choiceValues = list("Winter","Spring", "Summer", "Fall"))
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
      tags$h4("Stock Enhancement at Phillippi Creek", style = "margin: 0;")
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
    layout_columns(col_widths = c(2, 5, 5),
      layout_column_wrap(
        value_box(title = "Fish Released", value = textOutput("fish_released"), 
                  showcase = icon("fish"), theme = "text-primary")
      ),
      card2,  
      card3), 
    layout_columns(col_widths = c(6, 6), 
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
      <p>The data displayed in this app was collected through PIT tag antenna arrays at Phillippi Creek in Sarasota, FL
      (2016–2024). This effort is part of the Fisheries Ecology and Enhancement
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
  
  # pit detection tag order for plotting (random sample)
  tags_order <- reactive({
    req(length(input$size) > 0)
    set.seed(123)
    pit_sample_day %>%
      filter(Release_Event %in% input$select_event,
             Fish_Class %in% input$size) %>%
      group_by(Tag_ID) %>%
      summarise(First_Det = min(Date)) %>%
      slice_sample(n = 40) %>%
      arrange(desc(First_Det)) %>%
      pull(Tag_ID)
  })
  
  # pit detection dataset
  detections <- reactive({
    req(tags_order(), length(input$size) > 0)
    pit_sample_day %>%
      filter(
        Release_Event %in% input$select_event,
        Fish_Class %in% input$size,
        Tag_ID %in% tags_order()
        ) %>%
      mutate(Tag_ID = factor(Tag_ID, levels = tags_order()))
  })
  
  # pit detection segment creation
  detect_segments <- reactive({
    req(detections())
    detections() %>%
      arrange(Tag_ID, Date) %>%
      group_by(Tag_ID) %>%
      mutate(
        Segment_ID = cumsum(Antenna != lag(Antenna, default = first(Antenna)))
      ) %>%
      group_by(Tag_ID, Segment_ID, Antenna) %>%
      summarise(
        Common_Name = first(Common_Name),
        Fish_Size = first(Fish_Class),
        Start_Date = min(Date),
        End_Date = max(Date),
        .groups = "drop"
      )
  })
  
  # proportion of fish detected post-release
  survival_data <- reactive({
    req(length(input$size) > 0)
    release_survival %>%
      filter(Release_Event %in% input$select_event,
             Fish_Class %in% input$size) %>%
      group_by(Release_ID) %>%
      mutate(
        Days_After_Release = as.integer(as.Date(Date) - as.Date(Release_Date))
      ) %>%
      group_by(Release_ID, Days_After_Release) %>%
      summarise(
        Fish_Detected = n_distinct(Tag_ID),
        Fish_Released = first(Fish_Released),
        Prop_Detected = Fish_Detected/Fish_Released,
        .groups = "drop"
      ) %>%
      arrange(Release_ID, Days_After_Release) %>%
      filter(Days_After_Release <= 60)
  })
  
  # bar graph: total fish seen at each antenna bar graph
  bar_order <- reactive({
    req(length(input$size) > 0)
    pit_sample_day %>%
      filter(Release_Event %in% input$select_event,
             Fish_Class %in% input$size) %>%
      group_by(Antenna) %>%
      summarise(Num_Fish = n_distinct(Tag_ID), .groups = "drop") %>%
      arrange(desc(Num_Fish)) %>%
      pull(Antenna)
  })
  
  fish_bar_data <- reactive({
    req(length(input$size) > 0)
    pit_sample_day %>%
      filter(Release_Event %in% input$select_event,
             Fish_Class %in% input$size) %>%
      group_by(Antenna) %>%
      summarise(Num_Fish = n_distinct(Tag_ID), .groups = "drop") %>%
      mutate(Antenna = factor(Antenna, levels = bar_order()))
  })
  
  # ---------------------------- TEXT ------------------------------------------
  # print total observations for filtered data in ui
  release_text <- reactive({
    release_survival %>%
      filter(
        Release_Event %in% input$select_event,
        Fish_Class %in% input$size
      )
  })
  
  output$fish_released <- renderText({
    format(n_distinct(release_text()), big.mark = ",")
  })
  
  # --------------------------- PLOTS ------------------------------------------
  # abacus plot
  output$abacus_plot <- renderPlotly({
    req(detections())
    plot_ly() %>%
      add_segments(
        data = detect_segments(),
        x = ~Start_Date, xend = ~End_Date,
        y = ~Tag_ID, yend = ~Tag_ID,
        color = ~Antenna,
        colors = set3_pal,
        hoverinfo = "none",
        line = list(width = 5),
        showlegend = TRUE
      ) %>%
      add_markers(
        data = detections(), x = ~Date, y = ~Tag_ID,
        color = ~Antenna, colors = set3_pal, #symbol = ~Antenna, symbols = antenna_symbols, 
        marker = list(size = 5), #line = list(width = 0.5, color="grey30")
        text = ~paste(
          "<b>Antenna:</b>", Antenna,
          "<br><b>Detection Date:</b>", paste0(format(Date, format="%b %d, %Y")),
          "<br><b>Tag ID:</b>", Tag_ID),
        hoverinfo = "text", showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Detection Date", gridcolor = "#cccccc"),
        yaxis = list(title = "Tag ID", gridcolor = "#cccccc"),
        legend = list(title = list(text = "Antenna")),
        hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # proportion of fish alive post-release over time
  output$fish_survival <- renderPlotly({
    req(survival_data(), nrow(survival_data() > 0))
    plot_ly(
      mode = "lines+markers", type = "scatter",
      data = survival_data(), x = ~Days_After_Release, y = ~Prop_Detected, color=~Release_ID, colors="Set1",
      marker = list(size = 6), line = list(width = 2),
      text = ~paste(
        "<b>Release Event:</b>", Release_ID,
        "<br><b>Days After Release:</b>", Days_After_Release,
        "<br><b>Proportion of Fish Detected:</b>", round(Prop_Detected, digits=2)),
      hoverinfo = "text", showlegend = TRUE
    ) %>%
      layout(
        xaxis = list(title = "Days After Release", gridcolor = "#cccccc"),
        yaxis = list(title = "Proportion of Fish Detected", gridcolor = "#cccccc"),
        legend = list(title = list(text = "Release Event")),
        hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # total fish seen at each antenna bar graph
  output$fish_bar <- renderPlotly({
    plot_ly(
      data = fish_bar_data(), x = ~Antenna, y = ~Num_Fish, color = ~Antenna,
      type = "bar", colors=set3_pal,
      text = ~ paste(
        "<b>Antenna:</b>", Antenna,
        "<br><b>Number of Fish Detected:</b>", Num_Fish
      ),
      marker = list(line = list(color = "grey30", width=0.5)),
      hoverinfo = "text",
      textposition = "none"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", gridcolor = "#cccccc"),
        yaxis = list(title = "Number of Fish Detected", gridcolor = "#cccccc"),
        legend = list(title = list(text = "Antenna")),
        hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # ---------------------------- MAPS ------------------------------------------
  # map
  release_points <- reactive({
    rel_points %>%
      filter(Release_Event %in% input$select_event)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      #addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -82.514139, lat = 27.298909, zoom = 13) %>%
      addCircleMarkers(data = antenna_loc, lng = ~Longitude, lat = ~Latitude,
                       fillColor = ~ant_pal(Antenna), fillOpacity = 1, color="#4D4D4D", weight = 1, opacity = 1) %>%
      addLegend(pal = ant_pal, values = set3_val, opacity = 1, title = "Antenna")
      # addMarkers(data = antenna_loc, lng = ~Longitude, lat = ~Latitude, icon = ~shape_icons[Antenna]) %>%
      # addLegend(title = "Antenna",
      #           colors = rep("transparent", length(shape_icons)),
      #           labels = mapply(
      #             function(name, path) {
      #               sprintf("<img src='%s' height='20' style='vertical-align: middle;'> %s", path, name)
      #             },
      #             names(shape_icons),
      #             icon_paths
      #           ),
      #           opacity = 1)
  })
  
  observe({
    rel_pal <- colorNumeric("Reds", domain = rel_points$Num_Fish)
    leafletProxy("map") %>%
      clearGroup("release_points") %>%
      addCircleMarkers(data = release_points(), lat=~Latitude, lng=~Longitude, group="release_points",
                 fillColor = ~rel_pal(Num_Fish))
  })
  
} # end server

##############################  RUN APP  #######################################

shinyApp(ui = ui, server = server)