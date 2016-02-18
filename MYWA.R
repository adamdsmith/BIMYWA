# Load in some utility functions
source("./R/utils.R")

# Packages needed; these will be installed if not present
toLoad = c("ggplot2", "plyr", "lubridate", "dplyr", "grid")
instant_pkgs(toLoad); rm(toLoad)

# Load RDS file of detections
mywa_det <- readRDS("./Data/2015_BIMYWA_globaltags.rds")

# Load MYWA experimental data
mywa_dat <- read.csv("./Data/mywa_experiment.csv", header = TRUE, stringsAsFactors = FALSE)

# Load antenna orientation data
antennas <- read.csv("./Data/antennas.csv", header = TRUE)

# Format dates of MYWA experimental data
mywa_dat <- mywa_dat %>%
  mutate(rel_ts = mdy_hms(paste(rel_date, paste0(rel_time, ":00")), tz = "America/New_York"),
         rel_date = mdy(rel_date))

# Filter out for each individual all detections prior to the release timestamp
# Takes a few seconds...lots of detections here
mywa_ids <- mywa_dat$tag_id
mywa_det <- ldply(mywa_ids, function(tag) {
  rel_time <- mywa_dat[mywa_dat$tag_id == tag, "rel_ts"]
  id_dat <- filter(mywa_det, id == tag, ts >= rel_time) %>%
    # Calculate days since release
    mutate(since_rel = difftime(ts, rel_time, units = "days"))
  id_dat
}, .id = NULL)

# Simplify detection data 
# Takes some time...sill lots of detections here
mywa_det <- mywa_det %>% 
  # keep only BI detections
  filter(site == "NBI1" | site == "bise") %>% 
  # Keep only certain columns
  select(id, ts, site, ant, since_rel, dbm, lat, lon, runID, runLen, posInRun) %>%
  # Create a date variable from the time stamp
  mutate(date = ymd(substr(ts, 1, 10)))

# Now for the fun...
# Group detections by tag ID and Block Island tower
# We'll keep all detections within 12 hours of the last detection at each tower
mywa_filt <- mywa_det %>% 
  # so NBI tower is on top of graph
  mutate(site = factor(site, levels = c("NBI1", "bise"))) %>%
  group_by(id) %>%
  filter(difftime(max(ts), ts, units = "hours") <= 12) %>%
  mutate(ant = as.integer(ant)) %>% #coerce some antenna labels to integer (some were character)
  left_join(., antennas, by = c("ant" = "port"))

# Here's a table of stopover lengths for convenience
stopover_lengths <- mywa_filt %>% group_by(id) %>% 
  summarize(stopover = round(as.numeric(max(since_rel)), 1)) %>%
  left_join(., mywa_dat, by = c("id" = "tag_id")) %>% as.data.frame() 

# Now you can plot departure info using the custom function `depart`
# All you do is give it a bird ID and how many of its final minutes on Block Island you want to summarize
# Number in each wedge is the number of detections in specified time period for that antenna
# Notice which tower is indicated on right sidebar

# Default is last detection
depart(504)

# Otherwise, specify number of minutes prior to last detection to include
depart(504, 5) # Last 5 minutes
depart(504, 60) # Last hour

# If you assign the function to an object, it stores a table containing some useful information
# summarizing the detections by tower and antenna direction
(bird504 <- depart(504, 15))
