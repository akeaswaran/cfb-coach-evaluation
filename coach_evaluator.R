library(cfbfastR)
library(tidyverse)
library(stringr)
library(ggpubr)
library(factoextra)
library(ggthemes)
library(cluster)

years <- 2014:2020

pbp_data <- data.frame()
progressr::with_progress({
    future::plan("multisession")
    pbp_data <- cfbfastR::load_cfb_pbp(years)
})

pbp_data <- pbp_data %>%
    filter(
        !is.na(offense_conference)
        & !is.na(defense_conference)
    )

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

full_pbp <- full_pbp %>%
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

# "obvious go" -> strength > 1.5 && "Go" rec
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

# Recruiting Points
recruiting_data <- data.frame()
for (yr in years) {
    tmp = cfbd_recruiting_team(yr)
    recruiting_data <- bind_rows(recruiting_data, tmp)
}

recruiting_data$week <- 1
recruiting_data <- left_join(recruiting_data, coach_data, by=c("year" = "year", "team" = "school", "week" = "week")) %>%
    rename(coach = full_name)
recruiting_data <- recruiting_data %>%
    filter(
        !is.na(coach)
    )

recruiting_summary <- recruiting_data %>%
    mutate(
        points = as.numeric(points)
    ) %>%
    group_by(coach) %>%
    summarise(average_class_score = mean(points), average_rank = mean(rank))

# Draft/Development
# https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvDraftCoach.csv
dvoe_data <- read.csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvDraftCoach.csv")
dvoe_data <- dvoe_data %>%
    select(-X, -X.1)

# Big Coach Summary Dataset
composite_coach_data <- left_join(recruiting_summary, fd_decisions, by=c("coach" = "pos_team_coach"))
composite_coach_data <- left_join(composite_coach_data, close_game_win_prob, by=c("coach" = "pos_team_coach"))
composite_coach_data <- left_join(composite_coach_data, first_down_passing, by=c("coach" = "pos_team_coach"))
composite_coach_data <- composite_coach_data %>%
    rename(
        "avg_croot_points" = average_class_score,
        "obvious_go_pct" = pct.x,
        "one_score_game_win_pct" = pct.y,
        "first_down_pass_rate" = pct
    )


composite_coach_data <- composite_coach_data %>%
    mutate(
       coach = str_trim(coach)
    ) %>%
    select(
        coach,
        obvious_go_pct,
        avg_croot_points,
        one_score_game_win_pct,
        first_down_pass_rate
    )

composite_coach_data <- left_join(composite_coach_data, dvoe_data, by=c("coach" = "Coach"))
composite_coach_data <- composite_coach_data %>%
    drop_na() %>%
    select(-Draft.Value, -Exp..Draft.Value)
write.csv(composite_coach_data, "./data/coaching_summary.csv")

# K means time
# resources:
# - https://uc-r.github.io/kmeans_clustering#elbow
# - https://www.datanovia.com/en/blog/k-means-clustering-visualization-in-r-step-by-step-guide/
set.seed(1990)
coach_clusters <- scale(composite_coach_data %>% select(-coach))

# Silhouette method of identifying K
# fviz_nbclust(coach_clusters, kmeans, method = "silhouette")
#
# Gap Statistic method of identifying K
# gap_stat <- clusGap(coach_clusters, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
# fviz_gap_stat(gap_stat)

# Elbow method to identify K
fviz_nbclust(coach_clusters, kmeans, method = "wss")

# Potential K-values:
# Elbow = 4
# Silhouette = 10
# gap stat = 1

# Percentage of variance explained by dimensions
# eigenvalue <- round(get_eigenvalue(res.pca), 1)
# variance.percent <- eigenvalue$variance.percent
# head(eigenvalue)

# Actually do clusters
res.km <- kmeans(coach_clusters, 4, nstart = 25)
print(res.km)
# K-means clusters showing the group of each individuals
res.km$cluster
# # Dimension reduction using PCA
res.pca <- prcomp(coach_clusters,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

# Looking at components
# src: https://juliasilge.com/blog/best-hip-hop/
organized_pca <- data.frame(res.km$centers)
organized_pca <- cbind(cluster = rownames(organized_pca), organized_pca)
rownames(organized_pca) <- 1:nrow(organized_pca)
organized_pca <- melt(organized_pca) %>%
    rename(
        component = variable
    ) %>%
    mutate(
        component = as.factor(component)
    ) %>%
    mutate(
        value = as.numeric(value)
    )

organized_pca %>%
    mutate(components = reorder_within(component, abs(value), cluster)) %>%
    ggplot(aes(abs(value), components, fill = value > 0)) +
    geom_col() +
    facet_wrap(~cluster, scales = "free_y") +
    scale_y_reordered() +
    labs(
        x = "Absolute value of contribution",
        y = NULL, fill = "Positive?"
    )

ind.coord <- ind.coord %>%
    rename(
        "Obvious Go Rate" = "Dim.1",
        "Avg Croot Class Points" = "Dim.2",
        "One-Score Win Pct" = "Dim.3",
        "1st Down Pass Rate" = "Dim.4",
        "DVOE" = "Dim.5"
    )

# note: the cluster assignment will change every time you run the KMeans stuff
ind.coord <- ind.coord %>%
    mutate(
        cluster_title = case_when(
            cluster == 3 ~ "Aggressive / Not Clutch",
            cluster == 4 ~ "Conservative / Poor Talent",
            cluster == 1 ~ "Conversative on 4th / Clutch",
            cluster == 2 ~ "Good at Development / Good Talent",
        )
    )

# Create plot
ggscatter(
    ind.coord, x = "Obvious Go Rate", y = "Avg Croot Class Points",
    color = "cluster_title", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
    size = 1.5,  legend = "right", ggtheme = theme_fivethirtyeight(),
    xlab = paste0("Obvious Go Rate"),
    ylab = paste0("Avg Recruiting Class Strength")
) +
    stat_mean(aes(color = cluster_title), size = 4) +
    theme(axis.title = element_text()) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    labs(
        title = "Classifying Coaches",
        subtitle = "How can we put coaches into game management boxes?",
        caption = "Data from @cfbfastR. Chart made by Akshay Easwaran (@akeaswaran)."
    )

# -------------------------

# Optimizing Axes selection Programmatically
# Prompt: for each pair of dimensions X and Y possible, find the center of each cluster in that cross section then calc the median. Then select the X, Y pair with the highest median value
#
# 1. Find all pairs of dimensions
# 2. create list of median centers
# 3. For each pair of dimensions:
#   - for each cluster:
#       - create tmp list
#       - grab center point from res.km$centers using dimensions and cluster index
#       - store center point into tmp list
#       - find median point of tmp list
#       - store median point into median centers list
# 4. Find max of median centers list


# find all pairs of dimensions
# dim_combos <- combn(colnames(res.km$centers), 2)
# centers <- res.km$centers
# mdn_centers <- data.frame()
# colnames(mdn_centers) <- c("cluster","x_dim", "y_dim", "x_mdn_val", "y_mdn_val")
#
# for (cluster in 1:4) {
#     print(paste0("Cluster ", cluster))
#     x_values = c()
#     y_values = c()
#     for (c in 1:cols) {
#         dim_1 = dim_combos[1, c]
#         dim_2 = dim_combos[2, c]
#
#         x = centers[cluster, dim_1]
#         print(paste0("Dim 1 (", dim_1, "): ", x))
#         x_values = c(x_values, x)
#
#         y = centers[cluster, dim_2]
#         print(paste0("Dim 2 (", dim_2, "): ", y))
#         y_values = c(y_values, y)
#     }
#
#     x_mdn = median(x_values)
#     y_mdn = median(y_values)
#     print(paste0("Cluster ", cluster, " Median calculation: (", x_mdn, ", ", y_mdn,")"))
#
#     x_mdn_col_idx = match(x_mdn, x_values)
#     y_mdn_col_idx = match(y_mdn, y_values)
#     print(paste0("Cluster ", cluster, " Median Indices: (", x_mdn_col_idx, ", ", y_mdn_col_idx,")"))
#
#     x_mdn_col = dim_combos[1, x_mdn_col_idx]
#     y_mdn_col = dim_combos[2, y_mdn_col_idx]
#     print(paste0("Cluster ", cluster, " Median Dimensions: (", x_mdn_col, ", ", y_mdn_col,")"))
#
#     tmp = data.frame(cluster, x_mdn_col, y_mdn_col, x_mdn, y_mdn)
#     names(tmp) = c("cluster","x_dim", "y_dim", "x_mdn_val", "y_mdn_val")
#     mdn_centers <- rbind(mdn_centers, tmp)
# }
#
