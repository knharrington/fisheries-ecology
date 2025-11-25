##############################  GLOBAL  ########################################
{
  library(tidyverse)
  library(data.table)
  library(lubridate)
  # library(ggsvg)
  # library(ggimage)
  # library(glue)
  library(shiny)
  library(bslib)
  library(showtext)
  library(thematic)
  library(progress)
  library(leaflet)
}
# ---------------------------- FUNCTIONS ---------------------------------------
# function to do the opposite of %in%
`%nin%` <- Negate(`%in%`)

# ------------------------- PIT TAG DATA ---------------------------------------

##### FOR MULTI-FILE DATA #####
# Get the file names of all the raw txt files saved in the folder of data to be imported (only txt files starting with RP)
ORMR.files <- list.files(path = "data/ORMR_data", pattern = "^RP_.*\\.txt$", full.names = TRUE)

# Use a loop to create a raw dataframe for each file in the folder to be imported and create columns in the dataframe to specify location, date, and Antenna
filenames <- as.vector(NA) # create a dummy vector used in the loop

{
  pb <- progress_bar$new(total = length(ORMR.files), format = " Processing [:bar] :percent elapsed: :elapsedfull")
  for (i in 1:length(ORMR.files)) {
    file_name <- str_sub(str_extract(ORMR.files[i], "data/ORMR_data[[:graph:]]+"), start = 6, end = -5)
    filenames[[i]] <- file_name

    # # make temporary files so as not to lose original data
    # file_clean <- tempfile()
    file_small <- tempfile()

    # remove bad characters and filter out "I" detections (does this before reading the file) REQUIRES GIT BASH
    # system2("tr", c("-d", "'\\000'"), stdin = ORMR.files[i], stdout = file_clean) # calls Unix command-line tool to translate/delete null bytes
    system2("grep", c("--text", "-v", "I", ORMR.files[i]), stdout = file_small)

    file_df <- read.table(file_small, header = FALSE, fill = TRUE, col.names = paste0("V", seq_len(16)))
    # file_df = read.table(ORMR.files[i], header=F, fill=T, col.names = paste0("V", seq_len(16))) # this one will throw warnings for NULL bytes

    # Delete temporary file after reading it
    unlink(file_small)

    # Extract info from file names in format: data/MS_YYYY-MM-DD_Habitat.txt
    file_df$System <- str_sub(str_extract(ORMR.files[i], "data/ORMR_data/[[:graph:]]+"), start = 16, end = 17)
    file_df$ReadDate <- str_sub(str_extract(ORMR.files[i], "data/ORMR_data/[[:graph:]]+"), start = 19, end = 25)
    file_df$Antenna <- str_sub(str_extract(ORMR.files[i], "data/ORMR_data/[[:graph:]]+"), start = 27, end = -5)
    assign(file_name, file_df, envir = .GlobalEnv)

    # advance the progress bar
    pb$tick()
  }
  pb$terminate()
}

# Identify the PIT tag data in each dataframe, merge them, and relabeled columns appropriately
dflist <- as.list(NA) # creates a dummy list that the loop below can fill with all the dataframes
PITlist <- as.list(NA) # creates a dummy list to fill with the PIT tag data within the list of dataframes
Errorlist <- as.list(NA)

for (i in 1:length(filenames)) {
  dflist[[i]] <- get(filenames[i])
  PITlist[[i]] <- dflist[[i]][which(dflist[[i]]$V1 == "S"), ]
  Errorlist[[i]] <- dflist[[i]][which(dflist[[i]]$V1 == "E"), ]
}

# Combine the list of dataframes into one dataframe
ORMR.raw <- droplevels(rbindlist(PITlist, fill = TRUE))
# delete and rename columns
ORMR.raw <- within(ORMR.raw, rm(V11, V12, V13, V14, V15, V16))
ORMR.raw <- ORMR.raw %>% rename(
  Code = V1, Date = V2, Time = V3, Time_Reference = V4, Duration = V5,
  Tag_Type = V6, Loop = V7, Tag_ID = V8, Site_Code = V9, Effective_Amps = V10
)
# add columns for loops and date/time info
ORMR.raw$Loop <- paste(ORMR.raw$Antenna, "-", ORMR.raw$Loop)
ORMR.raw$Date <- as.POSIXct(ORMR.raw$Date, format = "%Y-%m-%d")
# ORMR.raw$Time <- hms(ORMR.raw$Time)
ORMR.raw$Duration <- as.numeric(hms(ORMR.raw$Duration))
ORMR.raw$Date_Time <- ymd_hms(paste(ORMR.raw$Date, ORMR.raw$Time))

# clean up dates
ORMR.raw <- ORMR.raw %>%
  filter(!is.na(Date), Date > as.POSIXct("2022-01-01", format = "%Y-%m-%d"), Date < as.POSIXct("2026-01-01", format = "%Y-%m-%d"), Tag_Type %in% c("A", "R", "W"))

# check dates
hist(ORMR.raw$Date, breaks = "months", freq = TRUE)
sort(unique(ORMR.raw$Date))
max(ORMR.raw$Date)

# save to data table
pit_data <- as.data.table(ORMR.raw)

# save data to csv file
write_csv(pit_data, "data/pit_data_2025-10-14.csv")

########## PICK BACK UP HERE ##########
pit_data <- fread("data/pit_data_2025-10-14.csv") # > 5 million detections
tag_data <- fread("data/FWC_RP_Tagdata.csv")
seine_raw <- fread("data/Robinson Preserve Seine Sampling Y1, Y2, Y3, Y4.csv")

unique(tag_data$CommonName)
n_distinct(pit_data$Tag_ID) # 4504

# tag data cleaning to keep naming conventions consistent
tag_data <- tag_data %>%
  rename(Common_Name = CommonName, Species = Scientificname, Tag_ID = TagNo, Date_Time_Rel = Date.Time.Rel, Rel_Water_Body = Waterbody) %>%
  mutate(
    Date_Time_Rel = as.POSIXct(Date_Time_Rel, format = "%m/%d/%Y %H:%M")
  ) %>%
  select(Common_Name, Species, Tag_ID, Date_Time_Rel, SL_MM, FL_MM, TL_MM, Rel_Water_Body)

# investigating and fixing tag ID errors
n_distinct(tag_data$Tag_ID) # 283... so that means there are some duplicate entries? (288 obs) - remove for now
tag_data %>%
  group_by(Tag_ID) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
tag_data %>% filter(Tag_ID %in% c("900_226001074178", "900_226001074182", "900_226001074288", "900_226001074290", "900_226001525931")) # some duplicated, some measuring discrepancies
tag_data <- tag_data %>% filter(Tag_ID %nin% c("900_226001074178", "900_226001074182", "900_226001074288", "900_226001074290", "900_226001525931"))

# number of tagged fish per species
tag_data %>%
  group_by(Common_Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) # 233 snook, 30 sheepshead, 10 red drum, 9 tarpon...

# join detections and tag info (Note that there are more tags without species info than with) (should NOT be many-to-many join)
pittag_data <- pit_data %>% left_join(tag_data, by = join_by(Tag_ID))
pittag_data <- pittag_data %>% left_join(seine_tags_clean, by = join_by(Tag_ID)) # seine_tags_clean is created in the seine data subsection

# only keeping detections where the tag info is complete
pittag_data_comp <- pittag_data %>% filter(!is.na(Common_Name), !is.na(Date_Time_Rel), !is.na(Rel_Water_Body), !is.na(TL_MM))

tagged_species <- pittag_data_comp %>%
  group_by(Common_Name) %>%
  summarize(Num_Indv = n_distinct(Tag_ID)) %>%
  arrange(desc(Num_Indv))

# summary of tag detections
# tags <- pittag_data_comp %>%
#   group_by(Tag_ID) %>%
#   summarise(Num_Detections = n(),
#             Num_Dates = n_distinct(Date),
#             Num_Loops = n_distinct(Loop)) %>%
#   arrange(desc(Num_Detections))

# head(tags)
# boxplot(tags$Num_Detections)
# summary(tags$Num_Detections)

# # take a sample of most-detected fish for testing...
# sample_tags <- tags$Tag_ID[1:20]
# pit_sample <- pittag_data_comp %>% filter(Tag_ID %in% sample_tags)

# filter detections to fish that were present at least 14 days after release
keepers <- pittag_data_comp %>%
  group_by(Tag_ID) %>%
  arrange(Date_Time) %>%
  mutate(Present = last(Date_Time) >= Date_Time_Rel + lubridate::days(14)) # %>%
# filter(Present == TRUE)
died <- keepers %>%
  filter(Present == FALSE) %>%
  group_by(Tag_ID) %>%
  arrange(Date_Time) %>%
  reframe(
    Last_Date_Time = last(Date_Time),
    Date_Time_Rel = Date_Time_Rel
  ) %>%
  group_by(Tag_ID) %>%
  summarise(
    Date_Time_Rel = first(Date_Time_Rel),
    Last_Date_Time = first(Last_Date_Time)
  )

# filter to 3 species, keep fish present at least 14 days post-release and have at least 10 detections, assign water bodies to detections
pit_sample <- pittag_data_comp %>%
  filter(Common_Name %in% c("Common snook", "Striped mullet")) %>%
  group_by(Tag_ID) %>%
  arrange(Date_Time) %>%
  mutate(
    Fish_Class = case_when(
      Common_Name == "Striped mullet" & FL_MM >= 250 ~ "Adult",
      Common_Name == "Striped mullet" & FL_MM >= 150 & FL_MM < 250 ~ "Subadult",
      Common_Name == "Striped mullet" & FL_MM < 150 ~ "Juvenile",
      Common_Name == "Common snook" & FL_MM >= 340 ~ "Adult",
      Common_Name == "Common snook" & FL_MM >= 180 & FL_MM < 340 ~ "Subadult",
      Common_Name == "Common snook" & FL_MM < 180 ~ "Juvenile",
      TRUE ~ "Unknown"
    ),
    Present = last(Date_Time) >= Date_Time_Rel + lubridate::days(14),
    Water_Body = case_when(
      Loop %in% c("Nursery - A1", "Nursery - A3", "Nursery1 - A1", "Nursery2 - A1", "Nursery2 - A3", "Nursery3 - A1", "Nursery3 - A3") ~ "Drummer Bayou",
      Loop %in% c("Culverts - A1", "Culverts - A2", "Culverts - A4") ~ "Fish Tale Pond",
      Loop %in% c("NorthBridge - A1", "NorthBridge - A3") ~ "Pelican Bayou",
      Loop %in% c("Nursery - A2", "Nursery - A4", "Nursery1 - A2", "Nursery2 - A2", "Nursery2 - A4", "Nursery3 - A2", "Nursery3 - A4", "NorthBridge - A2", "MainBridge - A3") ~ "Oyster Bay",
      Loop %in% c("Culverts - A3", "MainBridge - A1", "MainBridge - A2") ~ "Out of Preserve",
      TRUE ~ "Unknown"
    ),
    Previous_WB = lag(Water_Body),
    Next_Ping = lead(Date_Time),
    Next_Loop = lead(Loop),
    Previous_Ping = lag(Date_Time),
    Previous_Loop = lag(Loop),
    WB_Btwn_Det = case_when(
      Water_Body == "Drummer Bayou" & Previous_WB == "Drummer Bayou" ~ "Drummer Bayou",
      Water_Body == "Fish Tale Pond" & Previous_WB == "Fish Tale Pond" ~ "Fish Tale Pond",
      Water_Body == "Pelican Bayou" & Previous_WB == "Pelican Bayou" ~ "Pelican Bayou",
      Water_Body == "Oyster Bay" & Previous_WB == "Oyster Bay" ~ "Oyster Bay",
      Water_Body == "Out of Preserve" & Previous_WB == "Out of Preserve" ~ "Out of Preserve",
      TRUE ~ "Unknown"
    ),
    Num_Detections = n()
    # WB_Btwn_Rel = case_when(
    #   Rel_Water_Body == "RA"
    # )
  ) %>%
  filter(Present == TRUE, Num_Detections >= 10)

# detections per species, detections per tag from the ones that are left size per species
pit_sample %>%
  group_by(Common_Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
pit_sample %>%
  group_by(Common_Name) %>%
  summarise(Count = n_distinct(Tag_ID)) %>%
  arrange(desc(Count)) # 105 snook, 10 sheepshead, 9 red drum
temp <- pit_sample %>%
  group_by(Tag_ID) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
pit_sample %>%
  group_by(Common_Name, Fish_Class) %>%
  summarise(Count = n_distinct(Tag_ID))
sort(unique(pit_data$Loop))

# group by date to reduce points on abacus plot
pit_sample_day <- pit_sample %>%
  group_by(Tag_ID, Date) %>%
  summarise(
    Common_Name = first(Common_Name),
    Fish_Class = first(Fish_Class),
    Water_Body = first(Water_Body)
  )

# abacus plot palette
dark2_pal <- c("Dogleg Lake" = "#399E76", "Drummer Bayou" = "#D15E00", "Fish Tale Pond" = "#7670B5", "Oyster Bay" = "#DE268B", "Pelican Bayou" = "#6DA602", "Out of Preserve" = "#E1AA00", "Unknown" = "#666666")

# ggplot(ab_samp) +
#   #geom_point(aes(x=Date_Time, y=Tag_ID, color=Water_Body)) +
#   geom_segment(aes(x=Date_Time, y=Tag_ID, color=Water_Body)) +
#   scale_color_manual(name="Water Body", values=dark2_pal) +
#   labs(x="Detection Date", y="Tag ID")

# arrange the tag order for the plot
tag_order <- {
  set.seed(123)
  pit_sample_day %>%
    filter(Common_Name == "Common snook", Fish_Class == "Adult") %>%
    group_by(Tag_ID) %>%
    summarise(First_Det = min(Date)) %>%
    slice_sample(n = 30) %>%
    arrange(desc(First_Det)) %>%
    pull(Tag_ID)
}

# because there are so many fish, need to take a sub-sample (either fish with longest time or random)
ab_samp <- pit_sample_day %>%
  filter(Common_Name == "Common snook", Fish_Class == "Adult", Tag_ID %in% tag_order) %>%
  mutate(Tag_ID = factor(Tag_ID, levels = tag_order))

# make a new segment when waterbody changes
segments <- ab_samp %>%
  arrange(Tag_ID, Date) %>%
  group_by(Tag_ID) %>%
  mutate(
    Segment_ID = cumsum(Water_Body != lag(Water_Body, default = first(Water_Body)))
  ) %>%
  group_by(Tag_ID, Segment_ID, Water_Body) %>%
  summarise(
    Common_Name = first(Common_Name),
    Fish_Class = first(Fish_Class),
    Start_Date = min(Date),
    End_Date = max(Date),
    .groups = "drop"
  )

# abacus plot - interactive
plot_ly() %>%
  add_segments(
    data = segments,
    x = ~Start_Date, xend = ~End_Date,
    y = ~Tag_ID, yend = ~Tag_ID,
    color = ~Water_Body,
    colors = dark2_pal,
    hoverinfo = "none",
    line = list(width = 5)
  ) %>%
  add_markers(
    data = ab_samp, x = ~Date, y = ~Tag_ID,
    color = ~Water_Body, colors = dark2_pal, marker = list(size = 4),
    text = ~ paste(
      "<b>Location:</b>", Water_Body,
      "<br><b>Detection Date:</b>", paste0(format(Date, format = "%b %d, %Y")),
      "<br><b>Tag ID:</b>", Tag_ID
    ),
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

# pit tag antenna locations
antenna_loc <- data.table(
  Antenna = c("Main Bridge", "North Bridge", "Culverts", "Nursery", "Nursery3"),
  Ant_Longitude = c(-82.66875, -82.670719, -82.667879, -82.666687, -82.665703),
  Ant_Latitude = c(27.506788, 27.510683, 27.512505, 27.506902, 27.507391)
)
# loop locations
# loops_loc <- data.table(
#   Loop = c("Culverts"),
#   Loop_Longitude = c(),
#   Loop_Latitude = c()
# )

# -------------------------- SEINE DATA ----------------------------------------

seine_raw <- fread("data/Robinson Preserve Seine Sampling Y1, Y2, Y3, Y4.csv")

unique(seine_raw$ZONE)
unique(seine_raw$SUBSITE)
n_distinct(seine_raw$`REL PIT NO`)
sort(unique(seine_raw$`REL PIT NO`))[1:100]

# preprocessing
seine_clean <- seine_raw %>%
  mutate(
    Date = as.POSIXct(strptime(DATE, "%d-%b-%y", tz = "EST")),
    Date_Time = as.POSIXct(strptime(paste(Date, NETTIME), "%Y-%m-%d%H:%M")),
    Month = month(Date),
    Month_Year = format(Date, format = "%m/%y"),
    DATE = as.POSIXct(strptime(DATE, format = "%d-%b-%y")),
    Date_Cond = format(DATE, "%Y%m%d"),
    Season = case_when(
      Month %in% c(1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9) ~ "Summer",
      Month %in% c(10, 11, 12) ~ "Fall"
    ),
    Water_Name = case_when(
      SUBSITE == "WB#1" ~ "Oyster Bay",
      SUBSITE == "WB#3" ~ "Pelican Bayou",
      SUBSITE == "WB#2" ~ "Drummer Bayou",
      SUBSITE == "WB#4" ~ "Fish Tale Pond",
      SUBSITE == "WB#8" ~ "Dogleg Lake",
      TRUE ~ "Out.Pres"
    ),
    Seine_ID = str_c(Date_Cond, "_", Water_Name, "_T", REP),
    Tag_ID = case_when(
      grepl("\\?\\?\\?", `REL PIT NO`) | `REL PIT NO` == "" ~ NA_character_,
      TRUE ~ `REL PIT NO`
    )
  )

# sort(unique(seine_raw$ZONE_clean))
# unique(seine_clean$Tag_ID)

# check for unmatched values
# seine.clean %>%
#   filter(is.na(Zone_Abbr)) %>%
#   distinct(ZONE, ZONE_clean) %>%
#   arrange(ZONE_clean)

# check for NAs
# seine_data %>% filter(is.na(Seine_ID))

# exploratory
n_distinct(seine_data$Seine_ID)
unique(sort(seine_data$`COMMON NAME`))
unique(seine_data$VERT)
unique(seine_data[seine_data$VERT == ""]$`COMMON NAME`)
# unique(sort(seine_data[seine_data$VERT %nin% c("Invertebrate","invertebrate")]$`COMMON NAME`))
unique(sort(seine_data[seine_data$VERT == TRUE]$Common_Name))

# cleaning for species information
seine_data <- seine_clean %>%
  mutate(
    Common_Name = case_when(
      `COMMON NAME` == "Atlantic\xa0butterfish" ~ "Atlantic butterfish",
      `COMMON NAME` == "Hogchoker" ~ "Hogchoker/sole",
      `COMMON NAME` == "Fat sleeper" ~ "Fat sleeper goby",
      `COMMON NAME` == "Redfin Needlefish" ~ "Redfin needlefish",
      TRUE ~ `COMMON NAME`
    ),
    VERT = case_when(
      `COMMON NAME` == "Hogchoker/sole" ~ TRUE,
      `COMMON NAME` == "Crested goby" ~ TRUE,
      `COMMON NAME` == "Blenny spp" ~ TRUE,
      `COMMON NAME` == "Pipefish spp" ~ TRUE,
      `COMMON NAME` == "Remora spp" ~ TRUE,
      `COMMON NAME` == "Nudibranch spp" ~ FALSE,
      VERT == "vertebrate" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  rename(
    Seine_Longitude = `EW COORD`, Seine_Latitude = `NS COORD`
  )

# exploratory seine map
# sample.seine <- seine_data[sample(nrow(seine_data), 1000, replace=FALSE),]
# wbpal <- colorFactor(palette="Set1", domain=sample.seine$Location)
# leaflet(data=sample.seine) %>%
#   addTiles() %>%
#   addCircleMarkers(lng=~Seine_Longitude, lat=~Seine_Latitude,
#                    radius=2, opacity=0.5,
#                    color=~wbpal(Location)) %>%
#   addLegend(pal=pal, values=~Location)

# tagged fish per species (snook=1392, mullet=916, ladyfish=620, etc...)
seine_data %>%
  filter(!is.na(Tag_ID)) %>%
  group_by(Common_Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# seine dataset but only for tagged fish (12 recaptured fish in here)
seine_tags <- seine_data %>%
  filter(!is.na(Tag_ID))
duplicates <- seine_tags %>%
  group_by(Tag_ID) %>%
  summarise(Count = n()) %>%
  filter(Count > 1) %>%
  pull(Tag_ID)

# tag data from seines to merge with tag data from ORMR detections
seine_tags_clean <- seine_tags %>%
  filter(Tag_ID %nin% duplicates) %>%
  select(Common_Name, SPECIES, Tag_ID, Date_Time, SL_MM, FL_MM, TL_MM, Water_Name) %>%
  rename(Date_Time_Rel = Date_Time, Rel_Water_Body = Water_Name, Species = SPECIES)

# species richness data
rich_data_all <- seine_data %>%
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
    SHADE = first(SHADE),
    Date = first(Date),
    Month_Year = first(Month_Year)
  )

# exploratory species richness map
# dark2_val <- c("Dogleg Lake", "Drummer Bayou", "Fish Tale Pond", "Out of Preserve", "Oyster Bay", "Pelican Bayou", "Unknown")
# dark2_hex <- c("#399E76", "#D15E00", "#7670B5", "#E1AA00", "#DE268B", "#6DA602",  "#666666")
dark2_val <- c("Dogleg Lake", "Drummer Bayou", "Fish Tale Pond", "Oyster Bay", "Pelican Bayou")
dark2_hex <- c("#399E76", "#D15E00", "#7670B5", "#DE268B", "#6DA602")
wbpal <- colorFactor(palette = dark2_hex, domain = dark2_val)
rpal <- colorNumeric(palette = "RdYlBu", domain = rich_data_all$Shannon_Index, reverse = TRUE)
leaflet(data = rich_data_all) %>%
  # addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    lng = ~Seine_Longitude, lat = ~Seine_Latitude,
    radius = ~ Shannon_Index * 2.5, weight = 2, opacity = 0.75, fillOpacity = 0.75,
    color = ~ wbpal(Water_Name), fillColor = ~ rpal(Shannon_Index)
  ) %>%
  addLegend(pal = wbpal, values = dark2_val, opacity = 1, title = HTML("Location")) %>%
  addLegend(pal = rpal, values = ~Shannon_Index, opacity = 1, title = "Shannon Index")


# line graph: species richness over time
# static plot
rich_time <- rich_data_all %>%
  mutate(Month_Year = parse_date_time(Month_Year, orders = c("%m/%y"))) %>%
  group_by(Water_Name, Month_Year) %>%
  summarise(
    Avg_Species_Richness = mean(Species_Richness),
    Avg_Shannon_Index = mean(Shannon_Index)
  )
ggplot(rich_time) +
  geom_line(aes(x = Date, y = Avg_Shannon_Index, color = Water_Name)) +
  scale_color_manual(name = "Water Body", values = dark2_pal) +
  labs(x = "Detection Date", y = "Mean Shannon Index")
# interactive plot
rich_time_plot <- plot_ly(
  data = rich_time, x = ~Month_Year, y = ~Avg_Species_Richness, color = ~Water_Name,
  colors = dark2_pal, type = "scatter", mode = "lines+markers",
  text = ~ paste(
    "<b>Location:</b>", Water_Name,
    "<br><b>Seine Month:</b>", paste0(format(Month_Year, format = "%b %Y")),
    "<br><b>Mean Species Richness:</b>", paste0(format(Avg_Species_Richness, digits = 3))
  ),
  hoverinfo = "text",
  line = list(width = 2), marker = list(size = 0.5, symbol = "circle", color = "transparent")
) %>%
  layout(
    xaxis = list(title = "", gridcolor = "#cccccc"),
    yaxis = list(title = "Mean Species Richness", gridcolor = "#cccccc"),
    legend = list(title = list(text = "Location")),
    hovermode = "closest",
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  )

# bar graph: number of fish by species sampled by sampling event
composition_data <- seine_tags %>%
  mutate(Month_Year = parse_date_time(Month_Year, orders = c("%m/%y"))) %>%
  group_by(Month_Year, Common_Name) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(Common_Name = fct_lump_n(Common_Name, n = 7, w = Frequency, other_level = "Other")) %>%
  mutate(Common_Name = fct_relevel(Common_Name, "Other")) %>%
  group_by(Month_Year, Common_Name) %>%
  summarise(Frequency = sum(Frequency), .groups = "drop")
# interactive plot
composition_plot <- plot_ly(
  data = composition_data, x = ~Month_Year, y = ~Frequency, color = ~Common_Name,
  type = "bar", # colors=~color_map
  text = ~ paste(
    "<b>Species:</b>", Common_Name,
    "<br><b>Seine Month:</b>", paste0(format(Month_Year, format = "%b %Y")),
    "<br><b>Frequency:</b>", Frequency
  ),
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "stack",
    xaxis = list(title = "", gridcolor = "#cccccc"),
    yaxis = list(title = "Frequency", gridcolor = "#cccccc"),
    legend = list(title = list(text = "Common Name")),
    hovermode = "closest",
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  )


# --------------------------- SAVE DATA ----------------------------------------

save(seine_data, rich_data_all, pit_sample_day, wbpal, dark2_pal, antenna_loc, rich_time_plot, composition_plot, file = "data/preprocessed.RData")
