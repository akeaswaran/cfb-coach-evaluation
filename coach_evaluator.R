library(cfbfastR)
library(dplyr)

years <- range(2014, 2020)

pbp_data <- data.frame()
progressr::with_progress({
    future::plan("multisession")
    pbp_data <- cfbfastR::load_cfb_pbp(years)
})

coach_data <- read.csv("./data/coaches.csv")
coach_data$full_name <- paste(coach_data$first_name, coach_data$last_name)

coach_data <- coach_data %>%
    group_by(school, year) %>%
    slice(rep(1:n(), games)) %>%
    mutate(
        week = 1:n()
    ) %>%
    ungroup() %>%
    select(school, year, week, full_name)

# coach_data %>%
#     count(year, school, week) %>%
#     filter(n > 1)

# pbp with coach names assigned for pos_team and def_pos_team
full_pbp <- left_join(pbp_data, coach_data, by=c("year" = "year", "pos_team" = "school", "week" = "week")) %>%
    rename(pos_team_coach = full_name)
full_pbp <- left_join(full_pbp, coach_data, by=c("year" = "year", "def_pos_team" = "school", "week" = "week")) %>%
    rename(def_pos_team_coach = full_name)

full_pbp < full_pbp %>%
    filter(
        !is.na(pos_team_coach)
        & !is.na(def_pos_team_coach)
    )

# first down passing
first_down_passing <- full_pbp %>%
    filter(
        down == 1
    ) %>%
    group_by(pos_team_coach) %>%
    count(pass) %>%
    mutate(
        pct = n / sum(n),
        total = sum(n)
    ) %>%
    filter(
        pass == 1
    ) %>%
    rename(
        passes = n,
        plays = total
    ) %>%
    select(
        pos_team_coach, passes, plays, pct
    )

# win prob in 1-score games
close_game_win_prob <- full_pbp %>%
    group_by(game_id) %>%
    filter(
        game_play_number == max(game_play_number)
    ) %>%
    ungroup() %>%
    group_by(pos_team_coach) %>%
    filter(
        abs(pos_team_score - def_pos_team_score) <= 8
    ) %>%
    mutate(
        winning = (pos_team_score > def_pos_team_score)
    ) %>%
    count(winning) %>%
    mutate(
        pct = n / sum(n),
        total = sum(n)
    ) %>%
    filter(
        winning == TRUE
    ) %>%
    rename(
        wins = n,
        games = total
    ) %>%
    select(
        pos_team_coach, wins, games, pct
    )

# fourth down data
fd_data <- data.frame()
for (yr in years) {
    file_url <- paste0("https://github.com/Kazink36/cfb_fourth_down/raw/main/data/fd_pbp_",yr,".RDS")
    tmp <- readRDS(gzcon(url(file_url)))
    fd_data <- bind_rows(fd_data, tmp)
}
full_fd_data <- left_join(fd_data, coach_data, by=c("season" = "year", "pos_team" = "school", "week" = "week")) %>%
    rename(pos_team_coach = full_name) %>%
    filter(
        !is.na(choice)
        & choice != "Penalty"
        & choice != ""
    )

# strength > 1.5 && "Go"
fd_decisions <- full_fd_data %>%
    filter(
        strength >= 0.015
        & recommendation == "Go for it"
    ) %>%
    group_by(pos_team_coach) %>%
    count(choice) %>%
    mutate(
        pct = n / sum(n),
        total = sum(n)
    ) %>%
    filter(
        choice == "Go for it"
    ) %>%
    rename(
        obvious_go = n,
        opps = total
    ) %>%
    select(
        pos_team_coach, obvious_go, opps, pct
    )

