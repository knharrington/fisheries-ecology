##############################  GLOBAL  ########################################
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  # library(ggsvg)
  # library(ggimage)
  # library(glue)
  # library(shiny)
  library(bslib)
  library(showtext)
  library(thematic)
  library(progress)
  library(leaflet)
  library(base64enc)
}
# ---------------------------- FUNCTIONS ---------------------------------------
# function to do the opposite of %in%
`%nin%` <- Negate(`%in%`)

# ------------------------- RELEASE DATA ---------------------------------------
release_raw <- fread("data/Hatchery.Release.Final.csv")

unique(release_raw$Release.System)

release_data <- release_raw %>%
  filter(Release.System == "PHILLIPPI CREEK", Common.Name == "Common Snook") %>%
  rename(
    Release_ID = Release.ID,
    Release_Site = Release.Site,
    Source_Tank = Source.Tank,
    FL_Tagging = FL.tagging,
    Tag_Date = TAG.DATE,
    Tag_Type = TAG.TYPE,
    Release_System = Release.System,
    Date_Time_Rel = Date.Time.Rel,
    Total_Time = Total.Time,
    Number_Hits = Number.Hits,
    Tag_Date_n = Tag.Date.n,
    Tag_Date_R = Tag.Date.R,
    Len_Diff = Len.diff,
    Holding_Time = Holding.time,
    Region_Init = Region.Init,
    Shoreline_Init = Shoreline.Init,
    Common_Name = Common.Name
  ) %>%
  select(-V1) %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Longitude = ifelse(Longitude > 0, -Longitude, Longitude),
    Release_Date = as.Date(Date_Time_Rel),
    Release_Event = substr(Release_ID, 1, 9)
  )

options(pillar.sigfig = 7)

rel_points <- release_data %>%
  mutate(
    Release_Site = case_when(
      Release_Site %in% c("PC Lower Mid", "PC Lower Shore") ~ "PC Lower",
      Release_Site %in% c("PC Upper Mid", "PC Upper Shore") ~ "PC Upper",
      TRUE ~ Release_Site
    )
  ) %>%
  group_by(Release_Event, Release_Site) %>%
  reframe(
    Longitude = mean(Longitude),
    Latitude = mean(Latitude),
    Num_Fish = n_distinct(Tag_ID),
    Rel_IDs = paste(unique(Release_ID), collapse = ", "),
    Rel_Dates = paste(unique(Date_Time_Rel), collapse = ", ")) %>%
  arrange(Latitude, Longitude)

rel_points %>% group_by(Release_Event) %>% summarise(Num_Fish=sum(Num_Fish))
rel_points2 <- rel_points %>% filter(Release_Event == "SRC2019-2")

pal <- colorNumeric(palette = "Blues", domain = rel_points2$Num_Fish, reverse = FALSE)
poppy <- paste0("<strong>Fish Released: </strong>", rel_points2$Num_Fish,
                "<br><strong>Release IDs: </strong>", rel_points2$Rel_IDs,
                "<br><strong>Release Dates: </strong>", rel_points2$Rel_Dates)
leaflet() %>%
  #addTiles() %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(data=rel_points2, lat=~Latitude, lng=~Longitude, 
                   radius=~sqrt(Num_Fish)/2, popup=poppy,
                   weight=2, fillOpacity=.75, fillColor=~pal(Num_Fish),
                   color="black") %>%
  addLegend(pal = pal, data = rel_points2, values = ~Num_Fish, opacity = 1, title = HTML("Fish<br>Released"))

n_distinct(release_data$Release_Date) # 40

post_rel <- release_data %>%
  group_by(Release_Date) %>%
  summarise(Num_Fish = n_distinct(Tag_ID))

# -------------------------- RECAP DATA ----------------------------------------
recap_raw <- fread("data/Hatchery.recaps.all.csv")

unique(recap_raw$Antenna)
unique(recap_raw$Region)
unique(recap_raw$Release.System)

antenna_names <- c("Riverview", "Brown", "Tucci2", "Tucci1", "Duncan", "Chapman", "Oskamp", "Byron", "Tanglewood", "SedimentTrapB", "Sperandeo")

recap_data <- recap_raw %>%
  filter(Release.System == "PHILLIPPI CREEK",
         Common.Name == "Common Snook",
         Antenna %in% antenna_names) %>%
  select(-V1, -TL..mm., -FL..mm., -Sex) %>%
  rename(
    Read_Date = ReadDate,
    Date_Time_Start = Date.Time.Start,
    Time_Duration = Time.Duration,
    Date_Time_End = Date.Time.End,
    FL_MM = FL,
    Date_Rel = Date.Rel,
    Release_ID = Release.ID,
    Release_System = Release.System,
    Release_Site = Release.Site,
    Region_Init = Region.Init,
    Shoreline_Init = Shoreline.Init,
    Source_Tank = Source.Tank,
    Tag_Type2 = TAG.TYPE,
    Date_Time_Rel = Date.Time.Rel,
    Common_Name = Common.Name,
    Tag_Date = TAG.DATE,
    Time_at_Large = Time.at.Large
  ) %>%
  mutate(
    Date_Time_Start = as.POSIXct(Date_Time_Start, format = "%Y-%m-%d %H:%M:%S"),
    Date_Time_End = as.POSIXct(Date_Time_End, format = "%Y-%m-%d %H:%M:%S"),
    Date_Time_Rel = as.POSIXct(Date_Time_Rel, format = "%Y-%m-%d %H:%M:%S"),
    Release_Date = as.Date(Date_Time_Rel),
    Release_Event = substr(Release_ID, 1, 9),
    Fish_Class = case_when(
      Common_Name == "Common Snook" & FL_MM >= 340 ~ "Adult",
      Common_Name == "Common Snook" & FL_MM >= 180 & FL_MM < 340 ~ "Subadult",
      Common_Name == "Common Snook" & FL_MM < 180 ~ "Juvenile",
      TRUE ~ "Unknown")
  )

n_distinct(recap_data$Release_Date) # 40
n_distinct(recap_data$Release_ID) # 50
unique(recap_data$Common_Name)

recap_data %>% group_by(Common_Name) %>% summarise(Count = n_distinct(Tag_ID))

pit_sample <- recap_data %>%
  group_by(Tag_ID) %>%
  arrange(Date_Time_Start) %>%
  mutate(
    Present = last(Date_Time_End) >= Date_Time_Rel + lubridate::days(30),
    Next_Ping = lead(Date_Time_Start),
    Next_Loop = lead(Loop),
    Previous_Ping = lag(Date_Time_Start),
    Previous_Loop = lag(Loop),
    Num_Detections = n()
  ) %>%
  filter(Num_Detections < 50000)
  #filter(Present == TRUE, Num_Detections >= 10, Num_Detections < 50000)

hist(pit_sample$Num_Detections)

# group by date to reduce points on abacus plot
pit_sample_day <- pit_sample %>%
  group_by(Tag_ID, Date) %>%
  summarise(
    Release_Event = first(Release_Event),
    Release_ID = first(Release_ID),
    Release_Date = first(Release_Date),
    Common_Name = first(Common_Name),
    Fish_Class = first(Fish_Class),
    Antenna = first(Antenna)
  )

pit_sample %>% group_by(Region, Antenna) %>% summarise(Fish_Detected = n_distinct(Tag_ID)) 

# pit detection tag order for plotting (random sample)
tags_order <- {
  set.seed(123)
  release_survival %>%
    filter(
      #Release_Event %in% input$select_event,
      Present == TRUE
    ) %>%
    group_by(Tag_ID) %>%
    summarise(Release_Date = first(Release_Date), First_Det = min(Date)) %>%
    slice_sample(n = 40) %>%
    arrange(desc(Release_Date), desc(First_Det)) %>%
    pull(Tag_ID)
}

# pit detection dataset
detections <- release_survival %>%
  filter(
    #Release_Event %in% input$select_event,
    Present == TRUE,
    Tag_ID %in% tags_order
  ) %>%
  mutate(Tag_ID = factor(Tag_ID, levels = tags_order))

# Define a symbol palette with enough markers and map to antenna
symbol_palette <- c("circle", "square", "diamond", "cross", "x", "triangle-up", "triangle-down", "bowtie", "hourglass", "star", "pentagon") 
antenna_names <- c("Riverview", "Brown", "Tucci2", "Tucci1", "Duncan", "Chapman", "Oskamp", "Byron", "Tanglewood", "SedimentTrapB", "Sperandeo")
antenna_symbols <- setNames(symbol_palette[1:11], antenna_names)

# test abacus plot
plot_ly() %>%
  add_markers(
    data = detections, x = ~Date, y = ~Tag_ID,
    color = ~Antenna, marker = list(size = 5),  
    text = ~paste(
      "<b>Antenna:</b>", Antenna,
      "<br><b>Detection Date:</b>", paste0(format(Date, format="%b %d, %Y")),
      "<br><b>Tag ID:</b>", Tag_ID),
    hoverinfo = "text", showlegend = TRUE, legendgroup = 'antenna'
  ) %>%
  add_markers(
    data = detections, x = ~Release_Date, y = ~Tag_ID,
    marker = list(size = 6, symbol="star", color = "grey", opacity=0.75),  
    text = ~paste(
      "<b>Release Date:</b>", paste0(format(Release_Date, format="%b %d, %Y")),
      "<br><b>Tag ID:</b>", Tag_ID),
    hoverinfo = "text", showlegend = TRUE, legendgroup = 'release', name = "Release Date"
  ) %>%
  layout(
    xaxis = list(title = "Date", gridcolor = "#cccccc"),
    yaxis = list(title = "Tag ID", gridcolor = "#cccccc"),
    legend = list(title = list(text = "Location"), tracegroupgap=50),
    hovermode = "closest",
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  )

# plotly symbol options
vals <- schema(F)$traces$scatter$attributes$marker$symbol$values
print(vals)

# bar graph: total fish seen at each antenna bar graph
bar_order <- pit_sample_day %>%
  group_by(Antenna) %>%
  summarise(Num_Fish = n_distinct(Tag_ID), .groups = "drop") %>%
  arrange(desc(Num_Fish)) %>%
  pull(Antenna)

fish_bar_data <- pit_sample_day %>%
  group_by(Antenna) %>%
  summarise(Num_Fish = n_distinct(Tag_ID), .groups = "drop") %>%
  mutate(Antenna = factor(Antenna, levels = bar_order))

set3_pal = c("Brown" = "#feffaf",
             "Byron" = "#b6de61",
             "Chapman" = "#86b1d5",
             "Duncan" = "#f7b35b",
             "Oskamp" = "#f8cde5",
             "Riverview" = "#95d3c7",
             "SedimentTrapB" = "#b880bf",
             "Sperandeo" = "#d9d9d9",
             "Tanglewood" = "#bebadb",
             "Tucci1" = "#f37f68",
             "Tucci2" = "#cfebc4")

# interactive plot
fish_bar_plot <- plot_ly(
  data = fish_bar_data, x = ~Antenna, y = ~Num_Fish, color = ~Antenna,
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

# post-release survival
release_events <- release_data %>%
  group_by(Release_Event) %>%
  mutate(Fish_Released_Total = n_distinct(Tag_ID)) %>%
  group_by(Release_ID) %>%
  summarise(
    Fish_Released = n_distinct(Tag_ID),
    Fish_Released_Total= first(Fish_Released_Total)
  )

recap_sample <- recap_data %>%
  group_by(Tag_ID) %>%
  arrange(Date_Time_Start) %>%
  mutate(
    Present = last(Date_Time_End) >= Date_Time_Rel + lubridate::days(14),
    Num_Detections = n()
  ) %>%
  filter(Num_Detections < 50000)

recap_sample_day <- recap_sample %>%
  group_by(Tag_ID, Date) %>%
  summarise(
    Release_Event = first(Release_Event),
    Release_ID = first(Release_ID),
    Release_Date = first(Release_Date),
    Common_Name = first(Common_Name),
    Fish_Class = first(Fish_Class),
    Antenna = first(Antenna),
    Present = first(Present)
  )

# join release data with recap data
release_survival <- recap_sample_day %>% left_join(release_events, by = join_by(Release_ID))

survival_data <- release_survival %>%
  #filter(Release_ID %in% c("SRC2016-3.1", "SRC2016-3.2", "SRC2016-3.3", "SRC2016-3.4")) %>%
  group_by(Release_Event) %>%
  mutate(
    Days_After_Release = as.integer(as.Date(Date) - as.Date(Release_Date))
  ) %>%
  group_by(Release_ID, Days_After_Release) %>%
  summarise(
    Release_Event = first(Release_Event),
    Fish_Detected = n_distinct(Tag_ID),
    Fish_Released = first(Fish_Released),
    Prop_Detected = Fish_Detected/Fish_Released,
    .groups = "drop"
  ) %>%
  arrange(Release_ID, Days_After_Release) %>%
  filter(Days_After_Release <= 60)

# release_event_names <- unique(survival_data$Release_Event)
# choice_list <- lapply(
#   split(survival_data$Release_ID, survival_data$Release_Event),
#   unique
# )

# interactive plot
fish_survival_plot <- plot_ly(
  mode = "lines+markers", type = "scatter",
  data = survival_data, x = ~Days_After_Release, y = ~Prop_Detected, color=~Release_ID,
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

# -------------------------- WATER DATA ----------------------------------------
# water_raw <- fread("data/Phillippi water height.csv")

# -------------------------- ANTENNA DATA --------------------------------------
antenna_loc <- fread("data/PC-antenna-locations.csv")

# shape_icons <- iconList(
#   Brown = makeIcon(iconUrl = "www/square.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Byron = makeIcon(iconUrl = "www/bowtie.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Chapman = makeIcon(iconUrl = "www/triangle-up.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Duncan = makeIcon(iconUrl = "www/x.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Oskamp = makeIcon(iconUrl = "www/triangle-down.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Riverview = makeIcon(iconUrl = "www/circle.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   SedimentTrapB = makeIcon(iconUrl = "www/star.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Sperandeo = makeIcon(iconUrl = "www/pentagon.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Tanglewood = makeIcon(iconUrl = "www/hourglass.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Tucci1 = makeIcon(iconUrl = "www/cross.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10),
#   Tucci2 = makeIcon(iconUrl = "www/diamond.svg", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 10)
# )
# 
# icon_paths <- sapply(shape_icons, function(x) x$iconUrl)
# icon_paths <- gsub("^www/", "", icon_paths)   # remove 'www/' for legend

# leaflet(data = antenna_loc) %>%
#   addTiles() %>%
#   addMarkers(
#     lng = ~Longitude,
#     lat = ~Latitude,
#     icon = ~shape_icons[Antenna]
#   ) %>%
#   addLegend(
#     position = "bottomright",
#     title = "Antenna",
#     colors = rep("transparent", length(shape_icons)),
#     labels = mapply(
#       function(name, path) {
#         sprintf("<img src='%s' height='20' > %s", path, name)
#       },
#       names(shape_icons),
#       icon_paths
#     ),
#     opacity = 1
#   )

set3_val = c("Brown",
             "Byron",
             "Chapman",
             "Duncan",
             "Oskamp",
             "Riverview",
             "SedimentTrapB",
             "Sperandeo",
             "Tanglewood",
             "Tucci1",
             "Tucci2")
# set3_hex = c("#feffaf",
#              "#b6de61",
#              "#86b1d5",
#              "#f7b35b",
#              "#f8cde5",
#              "#95d3c7",
#              "#b880bf",
#              "#d9d9d9",
#              "#bebadb",
#              "#f37f68",
#              "#cfebc4")
set3_pal = c("Brown" = "#FAC98D",
             "Byron" = "#C0F967",
             "Chapman" = "#4FAADD",
             "Duncan" = "#EE951E",
             "Oskamp" = "#F891EA",
             "Riverview" = "#476878",
             "SedimentTrapB" = "#CA52BA",
             "Sperandeo" = "#2166A2",
             "Tanglewood" = "#57386A",
             "Tucci1" = "#F78775",
             "Tucci2" = "#77AE11")

# ant_pal <- colorFactor(palette = set3_hex, domain = set3_val)

getColor <- function(df) {
  unname(sapply(df$Antenna, function(Antenna) {
    if (Antenna == "Brown") {
      "beige" # "#feffaf"
    } else if (Antenna == "Byron") {
      "lightgreen" # "#b6de61"
    } else if (Antenna == "Chapman") {
      "blue" # "#86b1d5"
    } else if (Antenna == "Duncan") {
      "orange" # "#f7b35b"
    } else if (Antenna == "Oskamp") {
      "pink" # "#f8cde5"
    } else if (Antenna == "Riverview") {
      "cadetblue" # "#95d3c7"
    } else if (Antenna == "SedimentTrapB") {
      "purple" # "#b880bf"
    } else if (Antenna == "Sperandeo") {
      "darkblue" # "#d9d9d9"
    } else if (Antenna == "Tanglewood") {
      "darkpurple" # "#bebadb"
    } else if (Antenna == "Tucci1") {
      "lightred" # "#f37f68"
    } else if (Antenna == "Tucci2") {
      "green" # "#cfebc4"
    } else {
      "grey"
    }
  }))
}

icons <- awesomeIcons(
  icon = "circle",
  library = "fa",
  iconColor = "#FFFFFF",
  markerColor = getColor(antenna_loc)
  
)

# test
leaflet(antenna_loc) %>%
  addTiles() %>%
  addAwesomeMarkers(~Longitude, ~Latitude, icon=icons)

# possible awesome icons colors
# c("red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", 
# "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", 
# "white", "gray", "lightgray", "black")

# pal <- colorFactor(palette="Paired", domain=antenna_loc$Antenna)
# pop <- paste0("<strong>Antenna: </strong>", antenna_loc$Antenna,
#               "<br><strong>Lat: </strong>", antenna_loc$Latitude,
#               "<br><strong>Lon: </strong>", antenna_loc$Longitude)
# leaflet(data=antenna_loc) %>%
#   addTiles() %>%
#   addCircleMarkers(lng=~Longitude, lat=~Latitude,
#                    radius=7, opacity=1, popup=pop,
#                    color=~pal(Antenna)) %>%
#   addLegend(pal=pal, values=~Antenna, opacity=1)

# Releases by year
rels_yr_raw <- fread("data/releases_by_year_creek.csv")

se_creek_loc <- fread("data/se_creek_locations.csv")
creek_loc <- se_creek_loc %>%
  rename(Creek_System = `Release Site`)

rels_yr <- rels_yr_raw %>%
  filter(Species == "Common Snook") %>%
  select(-Total) %>%
  pivot_longer(
    cols = `Tidy Island`:`Tippecanoe`,
    names_to = "Creek_System",
    values_to = "Num_Released"
  ) %>%
  filter(!is.na(Num_Released)) %>%
  left_join(creek_loc, by = join_by(Creek_System))

release_plot <- plot_ly(
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
    xaxis = list(title = "", gridcolor = "#cccccc"),
    yaxis = list(title = "Number of Fish Released", gridcolor = "#cccccc"),
    legend = list(title = list(text = "Release Site")),
    hovermode = "closest",
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  )

# --------------------------- SAVE DATA ----------------------------------------

save(release_survival, rel_points, antenna_loc, icons, set3_pal, release_plot, rels_yr, file = "data/se-preprocessed.RData")
