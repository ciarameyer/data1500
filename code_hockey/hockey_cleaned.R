# load libraries
require(tidyverse)
require(ggbeeswarm)
require(lubridate)
require(ggstream)
require(ggbump)

# LOADING WOMENS
s24 <- read_csv("24.csv")
s2425 <- read_csv("2425.csv")
s2526 <- read_csv("2526.csv")

s24 <- s24 %>%
  select(-c(13, 14, `Attendance Rank`)) %>%
  mutate(`Percent of Venue Cap` = parse_number(`Percent of Venue Cap`)) %>%
  mutate(Date = mdy(Date)) %>%
  mutate(id = row_number()) %>%
  mutate(season = "2023-24")
s2425 <- s2425 %>%
  select(-`Attendance Rank`) %>%
  mutate(`Percent of Venue Cap` = parse_number(`Percent of Venue Cap`)) %>%
  mutate(Date = mdy(Date)) %>%
  mutate(id = row_number()) %>%
  mutate(season = "2024-25")
s2526 <- s2526 %>%
  select(-`Attendance Rank`) %>%
  mutate(`Percent of Venue Cap` = parse_number(`Percent of Venue Cap`)) %>%
  mutate(Date = mdy(Date)) %>%
  mutate(id = row_number()) %>%
  filter(`Percent of Venue Cap` != 0) %>%
  mutate(season = "2025-26")

full <- bind_rows(s24, s2425, s2526) %>%
  mutate(id = row_number()) %>%
  mutate(country = case_when(
    Venue %in% c(
      "Coca-Cola Coliseum",
      "TD Place",
      "Place Bell",
      "Scotiabank Arena",
      "Bell Centre",
      "Videotron Centre",
      "Rogers Arena",
      "Rogers Place",
      "Scotiabank Centre",
      "Canada Life Centre",
      "Scotiabank Saddledome",
      "Canadian Tire Centre"
    ) ~ "Canada",
    
    TRUE ~ "USA"
  ))

# WOMENS ANALYSIS
no_takeover <- full %>%
  mutate(Takeover = case_when(
    str_detect(Notes, regex("Takeover Tour", ignore_case = TRUE)) ~ "yes",
    TRUE ~ "no"
  )) %>%
  filter(Takeover != "yes") %>%
  mutate(month_year = floor_date(Date, "month"))

no_takeover$`Home Team` <- factor(
  no_takeover$`Home Team`,
  levels = c("New York", "Boston", "Minnesota", "Toronto", "Ottawa", "Montréal", "Vancouver", 
             "Seattle")
)

smoothed <- no_takeover %>%
  arrange(`Home Team`, Date) %>%
  group_by(`Home Team`, month = floor_date(Date, "month")) %>%
  summarise(games_in_month = n(),
            attendance_over_month = mean(`Published Attendance`, na.rm = TRUE)) %>%
  arrange(`Home Team`, month) %>%
  mutate(id = case_when(
    month == "2024-01-01" ~ 1,
    month == "2024-02-01" ~ 2,
    month == "2024-03-01" ~ 3,
    month == "2024-04-01" ~ 4,
    month == "2024-05-01" ~ 5,
    month == "2024-11-01" ~ 6,
    month == "2024-12-01" ~ 7,
    month == "2025-01-01" ~ 8,
    month == "2025-02-01" ~ 9,
    month == "2025-03-01" ~ 10,
    month == "2025-04-01" ~ 11,
    month == "2025-05-01" ~ 12,
    month == "2025-11-01" ~ 13,
    month == "2025-12-01" ~ 14,
    month == "2026-01-01" ~ 15,
    month == "2026-02-01" ~ 16,
    month == "2026-03-01" ~ 17,
    month == "2026-04-01" ~ 18
  ))

smoothed$`Home Team` <- factor(
  smoothed$`Home Team`,
  levels = c("Vancouver", "Seattle", "New York", "Boston", "Minnesota", "Toronto",
             "Ottawa", "Montréal")
)

# CHART 1
ggplot(smoothed, aes(x = id, y = attendance_over_month, 
                     fill = `Home Team`)) +
  geom_area() +
  theme_minimal() +
  geom_vline(xintercept = 6) +
  geom_vline(xintercept = 13) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Expansion teams are driving increased attendance at PWHL games",
       subtitle = "Total published attendance at PWHL games since the league's creation in 2024",
       caption = "PWHL via Jenness Wayne") 

# get start and end totals
values <- smoothed %>%
  group_by(id) %>%
  summarise(average_total = sum(attendance_over_month))

# MENS ANALYSIS
mens <- read_csv("mens.csv") 
mens <- mens %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)
mens <- mens %>%
  mutate(id = row_number()) %>%
  mutate(season = case_when(
    Date >= "2023-10-10" & Date <= "2024-04-18" ~ "2023-24",
    Date >= "2024-10-08" & Date <= "2025-04-17" ~ "2024-25",
    Date >= "2025-10-07" & Date <= "2026-04-25" ~ "2025-26")) %>%
  mutate(`Home Team` = case_when(
    home_team == "MTL" ~ "Montréal",
    home_team == "NYR" ~ "New York",
    home_team == "OTT" ~ "Ottawa",
    home_team == "TOR" ~ "Toronto",
    home_team == "SEA" ~ "Seattle",
    home_team == "MIN" ~ "Minnesota",
    home_team == "BOS" ~ "Boston",
    home_team == "VAN" ~ "Vancouver",
  ))

mens_by_team_by_season <- mens %>%
  group_by(season, `Home Team`) %>%
  summarize(average_attendance = sum(Att.)/n()) %>%
  mutate(gender = "men") %>%
  group_by(season) %>%
  mutate(rank = min_rank(average_attendance))

womens_by_team_by_season <- no_takeover %>%
  group_by(`Home Team`, season) %>%
  summarize(average_attendance = sum(`Published Attendance`)/n()) %>%
  mutate(gender = "women") %>%
  group_by(season) %>%
  arrange(average_attendance) %>%
  mutate(rank = min_rank(average_attendance)) %>%
  mutate(rank = case_when(season == "2023-24" ~ rank + 2,
                          season == "2024-25" ~ rank + 2,
                          season == "2025-26" ~ rank))

by_team_by_season <- bind_rows(womens_by_team_by_season, mens_by_team_by_season)
by_team_by_season <- by_team_by_season %>%
  mutate(`Home Team` = (case_when(gender =="men" ~ paste(`Home Team`, "(M)"),
                                  gender == "women" ~ paste(`Home Team`, "(W)")
  )))

# CHART 2
ggplot(by_team_by_season, aes(x = season, y = rank, color = `Home Team`)) +
  geom_bump(linewidth = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~gender) +
  theme_minimal() +
  labs(title = "For women's hockey, Seattle has become a hotspot --- despite relatively low men's attendance",
       subtitle = "NHL and PWHL teams ranked by average regular season home game attendance across the past 3 seasons",
       caption = "NHL via Hockey Reference and PWHL via Jenness Wayne")

# CHART 3

full <- full %>%
  mutate(Takeover = case_when(
    str_detect(Notes, regex("Takeover Tour", ignore_case = TRUE)) ~ "yes",
    TRUE ~ "no"
  ))

by_home_team <- no_takeover %>%
  group_by(`Home Team`) %>%
  summarize(average_attendance = sum(`Published Attendance`)/n())

ggplot(full, aes(x = id, y = `Published Attendance`)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 73) +
  geom_vline(xintercept = 163) +
  theme_minimal() +
  labs(title = "The PWHL has seen steady attendance growth over the past 3 seasons",
       subtitle = "Published attendance at PWHL games since Jan. 2024",
       caption = "PWHL via Jenness Wayne") 


