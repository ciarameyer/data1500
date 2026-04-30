# load libraries
require(tidyverse)
require(ggbeeswarm)
require(lubridate)
require(ggstream)

s24 <- read_csv("24.csv")
s2425 <- read_csv("2425.csv")
s2526 <- read_csv("2526.csv")

s24 <- s24 %>%
  select(-c(13, 14, `Attendance Rank`)) %>%
  mutate(`Percent of Venue Cap` = parse_number(`Percent of Venue Cap`)) %>%
  mutate(Date = mdy(Date)) %>%
  mutate(id = row_number()) %>%
  mutate(season = "2024")
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

ggplot(full, aes(x = id, y = `Percent of Venue Cap`, color = `Home Team`)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

by_day_analysis <- full %>%
  group_by(`Day of Week`) %>%
  summarize(average_attendance = sum(`Published Attendance`)/n())

by_home_team2 <- full %>%
  group_by(`Home Team`) %>%
  summarize(average_attendance = sum(`Percent of Venue Cap`)/n())

takeover <- full %>%
  mutate(Takeover = case_when(
    str_detect(Notes, regex("Takeover Tour", ignore_case = TRUE)) ~ "yes",
    TRUE ~ "no"
  )) %>%
  group_by(Takeover) %>%
  summarize(average_attendance = sum(`Published Attendance`)/n())

no_takeover <- full %>%
  mutate(Takeover = case_when(
    str_detect(Notes, regex("Takeover Tour", ignore_case = TRUE)) ~ "yes",
    TRUE ~ "no"
  )) %>%
  filter(Takeover != "yes") %>%
  mutate(month_year = floor_date(Date, "month"))

by_home_team <- no_takeover %>%
  group_by(`Home Team`) %>%
  summarize(average_attendance = sum(`Published Attendance`)/n())

ggplot(no_takeover, aes(x = id, y = `Published Attendance`, color = `Home Team`)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "New York and Seattle have seen the sharpest increase in game attendance over the past season",
       subtitle = "Percent of venue size filled at PWHL games, excluding the Takeover Tour",
       caption = "PWHL via Jenness Wayne") 

no_takeover$`Home Team` <- factor(
  no_takeover$`Home Team`,
  levels = c("New York", "Boston", "Minnesota", "Toronto", "Ottawa", "Montréal", "Vancouver", 
             "Seattle")
)

ggplot(no_takeover, aes(x = id, y = `Published Attendance`, 
                        fill = `Home Team`)) +
  geom_stream(type = "ridge", bw = 0.75) +
  geom_vline(xintercept = 73) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Expansion teams are driving increased attendance at PWHL games",
       subtitle = "Total published attendance at PWHL games since the league's creation in 2024",
       caption = "PWHL via Jenness Wayne") 

mens <- read_csv("mens.csv") 
mens <- mens %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)
mens <- mens %>%
  mutate(id = row_number()) %>%
  mutate(season = case_when(
    Date >= "2023-10-10" & Date <= "2024-04-18" ~ "2023-24",
    Date >= "2024-10-08" & Date <= "2025-04-17" ~ "2024-25",
    Date >= "2025-10-07" & Date <= "2026-04-25" ~ "2025-26"))
mens_by_team_by_season <- mens %>%
  group_by(season, home_team) %>%
  summarize(average_attendance = sum(Att.)/n())




