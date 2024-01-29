library(tidyverse)
library(nflverse)
library(rvest)
library(janitor)
library(gt)
library(gtExtras)
library(vroom)



# working code
nflreadr::load_pbp(2022) %>% 
  distinct(game_id, away_score, home_score, game_date, roof) %>% 
  mutate(day_of_the_week = lubridate::wday(game_date,label = TRUE),
         total_score = away_score + home_score
  ) %>% 
  group_by(roof) %>% 
  summarise(average_total_score = mean(total_score))





################# Dollars per Yard Thrown Historically ####################

# Pull earning from over the cap using rvest
over_the_cap <- read_html("https://overthecap.com/career-earnings")

otc_tables <- over_the_cap %>%
  html_table(fill = TRUE)

nfl_career_earnings_otc <- otc_tables[[1]]

# aggregate career yards
nflreadr::load_pbp(2022) %>% 
  group_by(season,passer,passer_id) %>% 
  summarise(total_passing_yards = sum(passing_yards, na.rm= T))


# Load player stats goes back to 1999, lucky me, that's when I really started to pay attention to the league
nflreadr::load_player_stats(seasons=T) %>% 
  glimpse()

# pull career yards for QBs (need to remove players with less than 100 completions in a season)
career_yards <- nflreadr::load_player_stats(seasons=T) %>% 
  filter(position == "QB", !is.na(player_display_name )) %>%
  group_by(player_display_name ) %>%
  summarize(
    passing_yards = sum(passing_yards, na.rm = TRUE),
    total_cmp = sum(completions, na.rm = TRUE)
    ) %>% 
  ungroup() %>% 
  filter(total_cmp >= 100) %>% 
  arrange(desc(passing_yards))

# need to filter out marino and aikman, but want to avoid manual removal
QB_Filter <- nflreadr::load_player_stats(seasons=T) %>% 
  filter(position == "QB", !is.na(player_display_name )) %>% 
  group_by(player_display_name,recent_team) %>% 
    summarise(n_seasons = n_distinct(season)) %>% 
  ungroup() %>% 
  filter(n_seasons > 2)

# add primary team 
primary_team_add <- nflreadr::load_player_stats(seasons=T) %>% 
  filter(position == "QB", !is.na(player_display_name )) %>% 
  group_by(player_display_name,recent_team) %>% 
  summarise(n_seasons = n_distinct(season)) %>% 
  filter(n_seasons == max(n_seasons)) %>% 
  distinct(player_display_name, .keep_all = TRUE) %>%  #break ties for players who had similar amount of years %>% 
  rename(primary_team = recent_team)

# combine all the data sources
which_QB_has_it_easiest <- left_join(career_yards, nfl_career_earnings_otc, by = c("player_display_name"="Player")) %>% 
  janitor::clean_names() %>% 
  mutate(career_earnings_numeric = as.numeric(gsub("[$,]","", career_earnings )),
         earnings_per_yard = round(career_earnings_numeric/passing_yards,0) 
         ) %>% 
  select(player_display_name,career_earnings_numeric,passing_yards,  earnings_per_yard) %>% 
  arrange(desc(earnings_per_yard)) %>% 
  inner_join(QB_Filter) %>% 
  inner_join(primary_team_add)







# pull in the team logos



teams <- "https://github.com/nflverse/nflfastR-data/raw/master/teams_colors_logos.rds"
team_df <- readRDS(url(teams)) %>% 
  select(team_abbr, team_logo_espn)

# grab one headshot per player
headshot_list <- nflreadr::load_rosters(1999:2022) %>%
  group_by(full_name, gsis_id) %>% 
  mutate(index = row_number()) %>%
  ungroup() %>% 
  group_by(full_name) %>% 
  mutate(index_flag = if_else(max(index) == index, 1,0)) %>% 
  ungroup() %>% 
  filter(index_flag == 1) %>% 
  select(full_name, headshot_url)

load_rosters(1999:2022) %>% 
  group_by(full_name) %>% 
  summarise(headshot_count = n_distinct(headshot_url),
            team_count = n_distinct(team)) %>% 
  arrange((desc(headshot_count)))

custom_format <- function(x) {
  paste0("$", format(x / 1000, nsmall = 1), "k")
}

# create the gt table 
which_QB_has_it_easiest_gt <- which_QB_has_it_easiest %>% 
  left_join(team_df, by = c("primary_team" = "team_abbr" )) %>% 
  left_join(headshot_list, by = c("player_display_name" = "full_name" )) %>% 
  distinct(player_display_name, .keep_all = T) %>% 
  select(player_display_name,headshot_url, career_earnings_numeric,passing_yards,
         earnings_per_yard, primary_team,team_logo_espn) %>% 
  arrange(desc(earnings_per_yard)) %>% 
  slice_head(n = 10) %>% # pipe to the great tables function  
  gt::gt() %>% 
  gt::tab_header(
    title = md("**Clipboard Holders: Which QB Earned Easy Money?**"),
    subtitle = md("*1999-2023  |  Earnings per Yard Passed*")) %>% 
  tab_stubhead(label = "Quarterback") %>%
  gt_img_rows(team_logo_espn , height = 40) %>% 
  gt_img_rows(headshot_url, height = 25)  %>% 
  cols_label(
    player_display_name = "Quarterback",
    headshot_url = "",
    career_earnings_numeric = "Career Earnings",
    passing_yards = "Career Passing Yards",
    earnings_per_yard = "Earnings Per Yard",
    primary_team = "Primary Team",
    team_logo_espn = "Team Logo") %>%
  cols_move_to_start(
    columns = headshot_url) %>%
  cols_align(align = "center", columns = c("player_display_name")) %>% 
  gt_color_box(earnings_per_yard, domain = 5000:30000,
               palette = "ggsci::blue_material",
               format = custom_format) %>% 
  fmt_currency(
    columns = vars(career_earnings_numeric),   # Specify the column(s) to format
    currency = "USD",         # Specify the currency symbol
    decimals = 0                # Specify the number of decimal places
  ) %>%
  fmt_number(
    columns = vars(passing_yards),  # Specify the column(s) to format
    use_seps = TRUE ,
    decimals = 0 # Enable thousands separators
  ) %>% 
  gtExtras::gt_theme_538() %>% 
  tab_source_note(
    source_note = md("*Special thanks to friends and family for the idea and Brad J. Congelio for his textbook on NFL analytics*"))  %>% 
  tab_footnote(
    footnote  = "Table created by: Tyler Otto"
  )


gtsave(which_QB_has_it_easiest_gt, "which_QB_has_it_easiest_gt.html" )

gtsave(which_QB_has_it_easiest_gt, "which_QB_has_it_easiest_gt.png" )
  
