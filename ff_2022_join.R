# #Join using fuzzyjoin on Name and Player columns
# 
# bs_yh_df <- bs_df %>%
#   stringdist_left_join(yh_df, by = c(Name = "Name"), max_dist = 3)
# 
# bs_yh_df <- bs_yh_df[ which(bs_yh_df$Average != 1), ]
# 
# bs_yh_nf_df <- bs_yh_df %>%
#   stringdist_left_join(nf_df, by = c(Name.x = "Player"), max_dist = 3)
# 
# bs_yh_nf_fp_df <- bs_yh_nf_df %>%
#   stringdist_left_join(fp_df, by = c(Name.x = "Name"), max_dist = 3)
# 
# #final df subset
# 
# final_df <- bs_yh_nf_fp_df[c(12,2:5,10,11,13:27,29)]
# 
# #Post subset cleanup for the final_df
# 
# final_df$Player <- str_replace_all(final_df$Player, "[\r\n]" , "")


# final_df <- bs_df |>
#   stringdist_left_join(yh_df, by = c(Name = "Name"), max_dist = 3) |>
#   drop_na("Name.y") |>
#   filter(Average != 1) |>
#   stringdist_left_join(nf_df, by = c(Name.x = "Player"), max_dist = 3) |>
#   stringdist_left_join(fp_df, by = c(Name.x = "Name"), max_dist = 3) |>
#   select(c(12, 2:5, 10, 11, 13:27, 29)) |>
#   mutate(Player = str_replace_all(string = Player, pattern = "[\r\n]" , replacement = ""))


final_df <- bs_df |>
  stringdist_left_join(yh_df, by = c(Name = "Name"))|>
  drop_na("Name.y") |>
  stringdist_left_join(nf_df, by = c(Name.y = "Player"), max_dist = 2) |>
  drop_na("Player") |>
  stringdist_left_join(fp_df, by = c(Name.x = "Name"), max_dist = 3) |>
  filter(Average > 0 ) |>
  select(c(12, 2:5, 10, 11, 13:27, 29))
