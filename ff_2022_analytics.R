
#what bs thinks the yahoo market is underrating
# final_df$bs_yhavg <- final_df$Average - final_df$Avg.Salary
# 
# #what bs thinks yahoo is underrating
# final_df$bs_yhproj <- final_df$Average - final_df$Proj.Salary

final_df <- final_df |>
  mutate(bs_yhavg = Average - Avg.Salary,
         bs_yhproj = Average - Proj.Salary)

#Build a roster plan 

Position.Mat <- dummy(final_df[, "Pos"])

colnames(Position.Mat) <- levels(factor(final_df[, "Pos"]))

f.obj <- final_df[, "Average"]

f.con <- t(cbind(Salary = final_df[, "Avg.Salary"], Position.Mat))

colnames(f.con) <- final_df$Player

f.dir <- rep(0, nrow(f.con))
f.rhs <- rep(0, nrow(f.con))

f.dir[1] <- "<="
f.rhs[1] <- 192

#QB, RB, TE, WR
f.dir[2:nrow(f.con)] <- c( "=", "=", "=", "=")
f.rhs[2:nrow(f.con)] <- c( 1, 2, 1, 4)


opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
picks <- final_df[which(opt$solution == 1), ]

# sum(picks$Avg.Salary)
# sum(picks$Average)
# sum(picks$Proj.Salary)
# sum(picks$FPProj.Salary)

knitr::kable(picks[c(1:3,5,7)], format = 'pipe')


###dplyr version of analysis below
#split final df by position group
#determine clusters by generating elbow plots
#run the kmeans per group to get clusters
#add clusters back to df, attach to greater df?
pos_cluster_plots <- final_df |>
  select(c(1,2,5,7,23:25)) |>
  group_by(Pos)  |>
  group_map(~ fviz_nbclust(x = .x[c(2:5)], FUNcluster = kmeans, method = "wss") +
              labs(title = .y$Pos)) 
pos_cluster_plots
########cluster kmeans similarity#####

# qb_df <- final_df[which(final_df$Pos=='QB'),c(1,2,5,7,23:25)]
# rb_df <- final_df[which(final_df$Pos=='RB'),c(1,2,5,7,23:25)]
# wr_df <- final_df[which(final_df$Pos=='WR'),c(1,2,5,7,23:25)]
# te_df <- final_df[which(final_df$Pos=='TE'),c(1,2,5,7,23:25)]
# 
# qb_df <- qb_df %>% remove_rownames %>% column_to_rownames(var="Player")
# rb_df <- rb_df %>% remove_rownames %>% column_to_rownames(var="Player")
# wr_df <- wr_df %>% remove_rownames %>% column_to_rownames(var="Player")
# te_df <- te_df %>% remove_rownames %>% column_to_rownames(var="Player")
# 
# qb_clust <- fviz_nbclust(x = qb_df, kmeans, method = "wss")

pos_clusters <- final_df %>%
  select(c(2,5,7,23:25)) %>%
  group_split(Pos) %>%
  map(`[`,c('Average', 'Avg.Salary')) %>%
  map(~ kmeans(.x, 4, 20))


final_df$cluster <- c(pos_clusters[[1]]$cluster, pos_clusters[[2]]$cluster,
                      pos_clusters[[3]]$cluster, pos_clusters[[4]]$cluster)

# qb_df_cluster <- kmeans(qb_df, centers = 5, nstart = 20)
# rb_df_cluster <- kmeans(rb_df, centers = 3, nstart = 20)
# wr_df_cluster <- kmeans(wr_df, centers = 6, nstart = 20)
# te_df_cluster <- kmeans(te_df, centers = 3, nstart = 20)
# 
# qb_df$cluster <- factor(qb_df_cluster$cluster)
# rb_df$cluster <- factor(rb_df_cluster$cluster)
# wr_df$cluster <- factor(wr_df_cluster$cluster)
# te_df$cluster <- factor(te_df_cluster$cluster)
# 
# qb_centroids <- data.frame(cluster = factor(seq(1:5)),
#                         Average = qb_df_cluster$centers[,'Average'],
#                         Avg.Salary = qb_df_cluster$centers[,'Avg.Salary'],
#                         FPProj.Salary = qb_df_cluster$centers[,'FPProj.Salary']
#                         )
# rb_centroids <- data.frame(cluster = factor(seq(1:3)),
#                            Average = rb_df_cluster$centers[,'Average'],
#                            Avg.Salary = rb_df_cluster$centers[,'Avg.Salary'],
#                            FPProj.Salary = rb_df_cluster$centers[,'FPProj.Salary']
# )
# wr_centroids <- data.frame(cluster = factor(seq(1:6)),
#                            Average = wr_df_cluster$centers[,'Average'],
#                            Avg.Salary = wr_df_cluster$centers[,'Avg.Salary'],
#                            FPProj.Salary = wr_df_cluster$centers[,'FPProj.Salary']
# )
# te_centroids <- data.frame(cluster = factor(seq(1:3)),
#                            Average = te_df_cluster$centers[,'Average'],
#                            Avg.Salary = te_df_cluster$centers[,'Avg.Salary'],
#                            FPProj.Salary = te_df_cluster$centers[,'FPProj.Salary']
# )
# 
# 
# 
# cluster_df <- do.call(rbind, list(qb_df, rb_df, wr_df, te_df))
# 
# cluster_player_df <- cbind(rownames(cluster_df), data.frame(cluster_df, row.names=NULL))
# 
# colnames(cluster_player_df)[1] <- "Player"
# 
# cluster_df$cluster <- as.character(cluster_df$cluster)
# 
# player_count <- seq_along(picks$Player)
# 
# player_pick_list <- list()
# 
# for (i in player_count) {
#   
#   player <- subset(cluster_player_df, Player == picks$Player[i])$Player
#   print(player)
#   player_cluster <- subset(cluster_player_df, Player == picks$Player[i])$cluster
#   player_position <- subset(cluster_player_df, Player == picks$Player[i])$Pos
#   print(player_position)
#   player_cluster_match <- subset(cluster_player_df, cluster == player_cluster & Pos == player_position)
#   player_cluster_match$PlayerMatched <- player
#   print(player_cluster_match)
#   player_pick_list[[length(player_pick_list) + 1]] <- player_cluster_match
# }
# 
# names(player_pick_list) <- c("qb_list", "rb1_list", "rb2_list", "te_list", 
#                              "wr1_list", "wr2_list", "wr3_list", "wr4_list")
# 
# list2env(player_pick_list, envir = .GlobalEnv)
# 
# player_pick_df <- do.call(rbind,player_pick_list)

#########

player_pick_func <- function(df) {
  
  player_count <- seq_along(picks$Player)
  
  player_pick_list <- list()
  
  for (i in player_count) {
    
    player <- subset(df, Player == picks$Player[i])$Player
    print(player)
    player_cluster <- subset(df, Player == picks$Player[i])$cluster
    player_position <- subset(df, Player == picks$Player[i])$Pos
    print(player_position)
    player_cluster_match <- subset(df, cluster == player_cluster & Pos == player_position)
    player_cluster_match$PlayerMatched <- player
    print(player_cluster_match)
    player_pick_list[[length(player_pick_list) + 1]] <- player_cluster_match
  }
  
  names(player_pick_list) <- c("qb_list", "rb1_list", "rb2_list", "te_list", 
                               "wr1_list", "wr2_list", "wr3_list", "wr4_list")
  
  list2env(player_pick_list, envir = .GlobalEnv)
  
  player_pick_df <- do.call(rbind,player_pick_list)
  
}


tewsst <- player_pick_func(final_df)
tewsstf <- tewsst |>
  group_by(PlayerMatched) |>
  top_n(n = 5, wt = bs_yhavg) |>
  mutate(mx = max(Avg.Salary)) |> 
  arrange(Pos,  desc(mx), PlayerMatched) |>
  select(c(27, 1:3, 7, 24))

tewsstf |>
  group_by(PlayerMatched, Pos) |>
  mutate(across(where(is.numeric), round, 1)) |>
  rename(`Avg. Y!` =  Avg.Salary) |>
  rename(`BS/Y!` =  bs_yhavg) |>
  gt() |>
  tab_style(style = list(cell_text(weight = "bold",
                                   font = "Arial"),
                    cell_fill(color = "mediumspringgreen"),
                    "font-variant: small-caps;"),
            locations = cells_row_groups()) |>
  tab_style(style = list(cell_text(weight = "normal",
                                   font = "Arial"),
                         cell_fill(color = "lightsteelblue1"),
                         "font-variant: small-caps;",
                         cell_borders(
                           sides = c("top", "bottom"),
                           color = "mintcream",
                           weight = px(2),
                           style = "solid"
                         )),
            locations = cells_body()) |>
  tab_header( title = md("The lpSolve powered roster plan"),
    subtitle = "green = optimal picks | blue = most similar options | @Sebasp41" )
