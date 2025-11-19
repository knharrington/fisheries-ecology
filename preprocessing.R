##############################  GLOBAL  ########################################
{
#library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)
#library(ggsvg)
#library(ggimage)
#library(glue)
library(shiny)
library(bslib)
library(showtext)
library(thematic)
library(progress)
library(leaflet)
}
# ---------------------------- FUNCTIONS ---------------------------------------
# function to do the opposite of %in%
`%nin%` = Negate(`%in%`)


# ---------------------------- OPTIONS -----------------------------------------


# ------------------------- PIT TAG DATA ---------------------------------------

##### FOR MULTI-FILE DATA #####  
# Get the file names of all the raw txt files saved in the folder of data to be imported (only files starting with RP)
ORMR.files = list.files(path="data/ORMR_data", pattern="^RP_.*\\.txt$", full.names=T)

# Use a loop to create a raw dataframe for each file in the folder to be imported and create columns in the dataframe to specify location, date, and Antenna
filenames = as.vector(NA) # create a dummy vector used in the loop

{pb <- progress_bar$new(total=length(ORMR.files), format = " Processing [:bar] :percent elapsed: :elapsedfull")
  for (i in 1:length(ORMR.files)) {
    file_name <- str_sub(str_extract(ORMR.files[i], "data/ORMR_data[[:graph:]]+"),start=6, end=-5)
    filenames[[i]] <- file_name
    
    # # make temporary files so as not to lose original data
    #file_clean <- tempfile()
    file_small <- tempfile()
    
    # remove bad characters and filter out "I" detections (does this before reading the file) REQUIRES GIT BASH 
    #system2("tr", c("-d", "'\\000'"), stdin = ORMR.files[i], stdout = file_clean) # calls Unix command-line tool to translate/delete null bytes
    system2("grep", c("--text", "-v", "I", ORMR.files[i]), stdout = file_small)
    
    file_df <- read.table(file_small, header = FALSE, fill = TRUE, col.names = paste0("V", seq_len(16)))
    #file_df = read.table(ORMR.files[i], header=F, fill=T, col.names = paste0("V", seq_len(16))) # this one will throw warnings for NULL bytes
    
    # Delete temporary file after reading it
    unlink(file_small)
    
    # Extract info from file names in format: data/MS_YYYY-MM-DD_Habitat.txt
    file_df$System = str_sub(str_extract(ORMR.files[i], "data/ORMR_data/[[:graph:]]+"),start=16, end=17)
    file_df$ReadDate = str_sub(str_extract(ORMR.files[i], "data/ORMR_data/[[:graph:]]+"),start=19, end=25)
    file_df$Antenna = str_sub(str_extract(ORMR.files[i], "data/ORMR_data/[[:graph:]]+"),start=27, end=-5)
    assign(file_name, file_df, envir = .GlobalEnv)
    
    # advance the progress bar
    pb$tick()
  }
  pb$terminate()}

# Identify the PIT tag data in each dataframe, merge them, and relabeled columns appropriately
dflist =as.list(NA) # creates a dummy list that the loop below can fill with all the dataframes
PITlist = as.list(NA) # creates a dummy list to fill with the PIT tag data within the list of dataframes
Errorlist = as.list(NA)

for (i in 1:length(filenames)) {
  dflist[[i]] = get(filenames[i])
  PITlist[[i]] = dflist[[i]][which(dflist[[i]]$V1=='S'),]
  Errorlist[[i]] = dflist[[i]][which(dflist[[i]]$V1=='E'),]
}

ORMR.raw = droplevels(rbindlist(PITlist, fill=T)) # Combine the list of dataframes into one dataframe
# delete and rename columns
ORMR.raw = within(ORMR.raw, rm(V11, V12, V13, V14, V15, V16)) # delete empty columns
ORMR.raw = ORMR.raw %>% rename(Code = V1, Date = V2, Time = V3, Time_Reference = V4, Duration = V5, 
                               Tag_Type = V6, Loop = V7, Tag_ID = V8, Site_Code = V9, Effective_Amps = V10)

ORMR.raw$Loop = paste(ORMR.raw$Antenna, "-", ORMR.raw$Loop)

ORMR.raw$Date <- as.POSIXct(ORMR.raw$Date, format="%Y-%m-%d")
#ORMR.raw$Time <- hms(ORMR.raw$Time)
ORMR.raw$Duration <- as.numeric(hms(ORMR.raw$Duration))
ORMR.raw$Date_Time <- ymd_hms(paste(ORMR.raw$Date, ORMR.raw$Time))

# clean up dates
ORMR.raw <- ORMR.raw %>%
  filter(!is.na(Date), Date > as.POSIXct("2022-01-01", format="%Y-%m-%d"), Date < as.POSIXct("2026-01-01", format="%Y-%m-%d"), Tag_Type %in% c("A", "R", "W"))

# subset for date
hist(ORMR.raw$Date, breaks="months", freq=TRUE)
sort(unique(ORMR.raw$Date))
max(ORMR.raw$Date)
#ORMR.raw <- subset(ORMR.raw, Date >= "2023-10-01" & Date <= "2023-10-07")
# end_date <- 
# start_date <- as.POSIXct("2023", format="%Y-%m-%d")
# ORMR.raw <- subset(ORMR.raw, Date >= start_date & Date <= end_date)

# save to data table
pit_data <- as.data.table(ORMR.raw) 

# save data to csv file
write_csv(pit_data, "data/pit_data_2025-10-14.csv")
#pit_data <- fread("data/pit_data_2025-10-14.csv")

# -------------------------- SEINE DATA ----------------------------------------

seine.raw <- fread("data/Robinson Preserve Seine Sampling Y1, Y2, Y3, Y4.csv")

unique(seine.raw$ZONE)
unique(seine.raw$SUBSITE)

# preprocessing
seine.raw$Date = as.POSIXct(strptime(seine.raw$DATE, "%d-%b-%y", tz="EST"))
seine.raw$Date_Time = as.POSIXct(strptime(paste(seine.raw$Date, seine.raw$NETTIME),"%Y-%m-%d%H:%M"))
seine.raw$Month = month(seine.raw$Date)
seine.raw$Month_Year = format(seine.raw$Date, format="%m/%y")
seine.raw$DATE = as.POSIXct(strptime(seine.raw$DATE, format="%d-%b-%y"))
seine.raw$Date_Cond = format(seine.raw$DATE, "%Y%m%d")
seine.raw <- seine.raw %>%
  mutate(
    ZONE_clean = ZONE |> 
      str_replace_all('"', "") |>      # remove all quotes
      str_trim() |>                    # trim white space
      str_to_lower(),                  # lowercase 
    Season = case_when(
      Month %in% c(1,2,3) ~ "Winter",
      Month %in% c(4,5,6) ~ "Spring",
      Month %in% c(7,8,9) ~ "Summer",
      Month %in% c(10,11,12) ~ "Fall"
    )
  )
#sort(unique(seine.raw$ZONE_clean))

seine_data <- seine.raw %>%
  mutate(
    Zone_Abbr = case_when(
      ZONE_clean == "y"                              ~ "Y",
      ZONE_clean == "bottom of y"                    ~ "Bot.Y",
      ZONE_clean == "visitor center"                 ~ "VC",
      ZONE_clean == "pelican point"                  ~ "PP",
      ZONE_clean == "south end"                      ~ "S.end",
      ZONE_clean == "front of island"                ~ "Fr.isl",
      ZONE_clean == "boat ramp"                      ~ "Boat",
      ZONE_clean == "west nursery hole"              ~ "W.nurs",
      ZONE_clean == "main bridge"                    ~ "M.brdg",
      ZONE_clean == "inside island"                  ~ "In.isl",
      ZONE_clean == "north dead end"                 ~ "N.DE",
      ZONE_clean == "north bridge inside"            ~ "N.brdg.in",
      ZONE_clean == "central nursery hole"           ~ "Cent.nurs",
      ZONE_clean == "behind island bridge"           ~ "Bhnd.isl.brdg",
      ZONE_clean == "culvert antenna"                ~ "Cul.ant",
      ZONE_clean == "nest"                           ~ "Nest",
      ZONE_clean == "delta inner"                    ~ "Dlt.in",
      ZONE_clean == "middle 2"                       ~ "Mid.2",
      ZONE_clean == "delta outter"                   ~ "Dlt.out",
      ZONE_clean == "between main bridge and w nursery bridge"  ~ "btw.M&W.nurs.brdg",
      ZONE_clean == "behind island"                  ~ "Bhnd.isl",
      ZONE_clean == "y fork"                         ~ "Y.fork",
      ZONE_clean == "middle"                         ~ "Mid",
      ZONE_clean == "culvert"                        ~ "Cul",
      ZONE_clean == "delta"                          ~ "Dlt",
      ZONE_clean == "e end"                          ~ "E.end",
      ZONE_clean == "end"                            ~ "End",
      ZONE_clean == "lagoon"                         ~ "Lag",
      ZONE_clean == "lagoon e end"                   ~ "Lag.E.end",
      ZONE_clean == "n bridge"                       ~ "N.brdg",
      ZONE_clean == "n bridge behind island"         ~ "N.brdg.bhnd.isl",
      ZONE_clean == "near island"                    ~ "Near.isl",
      ZONE_clean == "near n bridge"                  ~ "Near.N.brdg",
      ZONE_clean == "near north bridge"              ~ "Near.N.brdg",
      ZONE_clean == "nest stairway"                  ~ "Nest.stair",
      ZONE_clean == "north end"                      ~ "N.end",
      ZONE_clean == "nursery behind island"          ~ "Nurs.bhnd.isl",
      ZONE_clean == "nursery hole"                   ~ "Nurs",
      ZONE_clean == "ramp"                           ~ "Ramp",
      ZONE_clean == "w side of island"               ~ "W.isl",
      TRUE ~ NA_character_
    ),
    Location = case_when(
      SUBSITE == "WB#1" ~ "M.Lag",
      SUBSITE == "WB#3" ~ "1.Acc.Nurs",
      SUBSITE == "WB#2" ~ "3.Acc.Nurs",
      SUBSITE == "WB#4" ~ "Culv",
      SUBSITE == "WB#8" ~ "Dog",
      TRUE ~ "Out.Pres"
    ),
    Seine_ID = str_c(Date_Cond, "_", Location, "_", Zone_Abbr, "_T", REP)
  )

# check for unmatched values
# seine.clean %>% 
#   filter(is.na(Zone_Abbr)) %>%
#   distinct(ZONE, ZONE_clean) %>%
#   arrange(ZONE_clean)

# check for NAs
#seine_data %>% filter(is.na(Seine_ID))

# exploratory
n_distinct(seine_data$Seine_ID)
unique(sort(seine_data$`COMMON NAME`))
unique(seine_data$VERT)
unique(seine_data[seine_data$VERT == ""]$`COMMON NAME`)
#unique(sort(seine_data[seine_data$VERT %nin% c("Invertebrate","invertebrate")]$`COMMON NAME`))
unique(sort(seine_data[seine_data$VERT == TRUE]$Common_Name))

# cleaning for species information
seine_data <- seine_data %>%
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
    ),
    Water_Name = case_when(
      Location == "1.Acc.Nurs" ~ "Pelican Bayou",
      Location == "3.Acc.Nurs" ~ "Drummer Bayou",
      Location == "Culv" ~ "Fish Tale Pond",
      Location == "Dog" ~ "Dogleg Lake",
      Location == "M.Lag" ~ "Oyster Bay"
    )#,
    # Fish_Size = case_when(
    #   TL_MM >= 300 ~ "Large",
    #   TL_MM >= 150 & TL_MM < 300 ~ "Medium",
    #   TL_MM < 150 ~ "Small"
    # )
  ) %>%
  rename(
    Seine_Longitude = `EW COORD`, Seine_Latitude = `NS COORD`
  )

# exploratory seine map
sample.seine <- seine_data[sample(nrow(seine_data), 1000, replace=FALSE),]
wbpal <- colorFactor(palette="Set1", domain=sample.seine$Location)
leaflet(data=sample.seine) %>% 
  addTiles() %>% 
  addCircleMarkers(lng=~Seine_Longitude, lat=~Seine_Latitude, 
                   radius=2, opacity=0.5, 
                   color=~wbpal(Location)) %>%
  addLegend(pal=pal, values=~Location)

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
    SHADE = first(SHADE)
  )

# exploratory species richness map
rpal <- colorNumeric(palette="RdYlBu", domain=rich_data$Shannon_Index, reverse=TRUE)
leaflet(data=rich_data) %>% 
  #addTiles() %>%
  addProviderTiles("Esri.WorldImagery")%>%
  addCircleMarkers(lng=~Seine_Longitude, lat=~Seine_Latitude, 
                   radius=~Shannon_Index*2, weight=1, opacity=0.75, fillOpacity=0.75, 
                   color=~wbpal(Location), fillColor=~rpal(Shannon_Index)) %>%
  addLegend(pal=rpal, values=~Shannon_Index, opacity=1, title="Shannon Index")

save(seine_data, rich_data_all, file="data/preprocessed.RData")
