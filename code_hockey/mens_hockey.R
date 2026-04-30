# load libraries
require(tidyverse)
require(ggbeeswarm)
require(lubridate)
require(ggstream)

# read in CSVs
BOS_23_24 <- read_csv("hockey_mens/BOS_23_24.csv")
BOS_24_25 <- read_csv("hockey_mens/BOS_24_25.csv")
BOS_25_26 <- read_csv("hockey_mens/BOS_25_26.csv")
MTL_23_24 <- read_csv("hockey_mens/MTL_23_24.csv")
MTL_24_25 <- read_csv("hockey_mens/MTL_24_25.csv")
MTL_25_26 <- read_csv("hockey_mens/MTL_25_26.csv")
TOR_23_24 <- read_csv("hockey_mens/TOR_23_24.csv")
TOR_24_25 <- read_csv("hockey_mens/TOR_24_25.csv")
TOR_25_26 <- read_csv("hockey_mens/TOR_25_26.csv")
OTT_23_24 <- read_csv("hockey_mens/OTT_23_24.csv")
OTT_24_25 <- read_csv("hockey_mens/OTT_24_25.csv")
OTT_25_26 <- read_csv("hockey_mens/OTT_25_26.csv")
VAN_23_24 <- read_csv("hockey_mens/VAN_23_24.csv")
VAN_24_25 <- read_csv("hockey_mens/VAN_24_25.csv")
VAN_25_26 <- read_csv("hockey_mens/VAN_25_26.csv")
NYR_23_24 <- read_csv("hockey_mens/NYR_23_24.csv")
NYR_24_25 <- read_csv("hockey_mens/NYR_24_25.csv")
NYR_25_26 <- read_csv("hockey_mens/NYR_25_26.csv")
SEA_23_24 <- read_csv("hockey_mens/SEA_23_24.csv")
SEA_24_25 <- read_csv("hockey_mens/SEA_24_25.csv")
SEA_25_26 <- read_csv("hockey_mens/SEA_25_26.csv")
MIN_23_24 <- read_csv("hockey_mens/MIN_23_24.csv")
MIN_24_25 <- read_csv("hockey_mens/MIN_24_25.csv")
MIN_25_26 <- read_csv("hockey_mens/MIN_25_26.csv")

# merge by team
BOS <- bind_rows(BOS_23_24, BOS_24_25, BOS_25_26)
MTL <- bind_rows(MTL_23_24, MTL_24_25, MTL_25_26)
TOR <- bind_rows(TOR_23_24, TOR_24_25, TOR_25_26)
OTT <- bind_rows(OTT_23_24, OTT_24_25, OTT_25_26)
VAN <- bind_rows(VAN_23_24, VAN_24_25, VAN_25_26)
NYR <- bind_rows(NYR_23_24, NYR_24_25, NYR_25_26)
SEA <- bind_rows(SEA_23_24, SEA_24_25, SEA_25_26)
MIN <- bind_rows(MIN_23_24, MIN_24_25, MIN_25_26)

# mutate to make home team column
colnames(BOS)[3] <- "home_team"
colnames(MTL)[3] <- "home_team"
colnames(TOR)[3] <- "home_team"
colnames(OTT)[3] <- "home_team"
colnames(VAN)[3] <- "home_team"
colnames(NYR)[3] <- "home_team"
colnames(SEA)[3] <- "home_team"
colnames(MIN)[3] <- "home_team"

BOS_at_home <- BOS %>% 
  mutate(home_team = ifelse(is.na(home_team), "BOS", "away")) %>%
  filter(home_team == "BOS") %>%
  select(-Notes)

MTL_at_home <- MTL %>% 
  mutate(home_team = ifelse(is.na(home_team), "MTL", "away")) %>%
  filter(home_team == "MTL")%>%
  select(-Notes)

TOR_at_home <- TOR %>% 
  mutate(home_team = ifelse(is.na(home_team), "TOR", "away")) %>%
  filter(home_team == "TOR") %>%
  select(-Notes)

OTT_at_home <- OTT %>% 
  mutate(home_team = ifelse(is.na(home_team), "OTT", "away")) %>%
  filter(home_team == "OTT") %>%
  select(-Notes)

VAN_at_home <- VAN %>% 
  mutate(home_team = ifelse(is.na(home_team), "VAN", "away")) %>%
  filter(home_team == "VAN") %>%
  select(-Notes)

NYR_at_home <- NYR %>% 
  mutate(home_team = ifelse(is.na(home_team), "NYR", "away")) %>%
  filter(home_team == "NYR") %>%
  select(-Notes)

SEA_at_home <- SEA %>% 
  mutate(home_team = ifelse(is.na(home_team), "SEA", "away")) %>%
  filter(home_team == "SEA") %>%
  select(-Notes)


MIN_at_home <- MIN %>% 
  mutate(home_team = ifelse(is.na(home_team), "MIN", "away")) %>%
  filter(home_team == "MIN") %>%
  select(-Notes)

# merge in case i change my mind
mens <- bind_rows(BOS_at_home, MTL_at_home, OTT_at_home, TOR_at_home, VAN_at_home,
                  NYR_at_home, SEA_at_home, MIN_at_home)
write.csv(mens, "mens.csv", row.names = FALSE)





