#Clean Yahoo data

# yh_df$Name <- str_replace_all(yh_df$Name, c("No new player Notes" = "", "Player Note" = "",
#                                             "New" = "", "PUP" = "", "NFI" = "",
#                                             "Jr." = "", "III" = ""))
# 
# yh_df <-separate(data = yh_df, col = Name, into = "Name", sep = "-(?=[^-]+$)")
# 
# yh_df$Name <- trimws(yh_df$Name, which = c("both"))
# 
# yh_df <- separate(yh_df, Name, into = c("Name"), sep = " - ")
# 
# yh_df <- separate(yh_df, Name, into = c("Name", "Team"), sep = " (?=[^ ]+$)")
# 
# yh_df$Proj.Salary <- as.numeric(gsub("[\\$,]", "", yh_df$Proj.Salary))
# 
# yh_df$Avg.Salary <- as.numeric(gsub("[\\$,]", "", yh_df$Avg.Salary))

yh_df <- yh_list %>%
  map(as.data.frame) %>%
  bind_rows()

yh_df <-yh_df |>
  mutate(Name = str_replace_all(Name, c("No new player Notes" = "", "Player Note" = "",
                                              "New" = "", "PUP" = "", "NFI" = "",
                                              "Jr." = "", "III" = "")),
         Avg.Salary = str_replace_all(Avg.Salary, "-","0")) |>
  separate(Name, into = "Name", sep = "-(?=[^-]+$)")  |>
  mutate(Name = str_trim(Name, side = "both"))  |>
  separate(Name, into = "Name", sep = " - ")  |>
  separate(Name, into = c("Name", "Team"), sep = " (?=[^ ]+$)")  %>% 
  mutate(across(.cols = c("Proj.Salary", "Avg.Salary"),
           .fns = parse_number)) |>
  select(c(1,3,4))

#Yahoo subset
# yh_df <- yh_df[c(1,3,4)]
# 
# yh_df <- yh_df[ which(yh_df$Avg.Salary !=1), ]

#Clean numberFire data

# names(nf_table) <- nf_table[1,]
# 
# nf_table <- nf_table[-1,]
#
# nf_df <- subset(nf_table, select=c(1:3,6:16))
# 
# nf_df <- nf_df |>
#   separate(`C/A`, c("Comp", "PAtt"), sep = "/") |>
#   separate(CI, c("Lower", "Upper"), sep = "-")
# 
# nf_df[c(2:16)] <- sapply(nf_df[c(2:16)],as.numeric)
# 
# nf_df$Player <-word(string = nf_df$Player, 1,2, sep=" ")
# 
# nf_df$Player[nf_df$Player == "Amon-Ra St."] <- "Amon-Ra St. Brown"

nf_df <- nf_table %>%
  set_names(., nm = nf_table[1,]) %>%
  .[-1, ] |>
  subset(select=c(1:3,6:16)) |>
  separate(`C/A`, c("Comp", "PAtt"), sep = "/") |>
  separate(CI, c("Lower", "Upper"), sep = "-") |>
  mutate(across(.cols = c(2:16),
                .fns = as.numeric),
         Player = word(Player, 1, 2, sep = " "),
         Player = str_replace_all(Player, "[\r\n]" , ""),
         Player = recode(Player, "Amon-Ra St."= "Amon-Ra St. Brown",
                          "A.J. Dillon" = "AJ Dillon",
                          "D.J. Moore" = "DJ Moore",
                          "D.K. Metcalf" = "DK Metcalf",
                          "D.J. Chark"= "DJ Chark"))

#Clean Fantasy Football Pros data

# colnames(fp_df) <- c('Name', 'FPProj.Salary')
# 
# fp_df$Name <- str_replace(fp_df$Name, " \\s*\\([^\\)]+\\)", "")
# 
# fp_df$Name <- str_replace_all(fp_df$Name, c("INJ" = "", "Jr." = "",
#                                             "II" = "", "III" = "", " I" = ""))
# 
# fp_df$Name[fp_df$Name == "Ken Walker"] <- "Kenneth Walker"

fp_df <- fp_df |>
  mutate(X2 = str_replace_all(X2, " \\s*\\([^\\)]+\\)", ""),
         X2 = str_replace_all(X2, c("INJ" = "", "Jr." = "",
                                             "II" = "", "III" = "", " I" = "")),
         X2 = replace(X2, X2 == "Ken Walker", "Kenneth Walker")) |>
  rename(c(Name = X2, FPProj.Salary = X4)) |>
  filter(FPProj.Salary >= 0)

#fp_df <- fp_df[ which(!fp_df$FPProj.Salary <0), ]

#Clean BeerSheets

bs_df <- bs_table |>
  separate(Tm.Bye, c("Team", "Bye Week"), sep = "/") |>
  mutate(Name = replace(Name, Name == "Amon-Ra St", "Amon-Ra St. Brown"), 
         Name = replace(Name, Name == "Ken Walker", "Kenneth Walker")) |>
  select(c(1:8))

#BeerSheets subset
# bs_df <- bs_df[c(1:8)]

# bs_df$Name[bs_df$Name == "Ken Walker"] <- "Kenneth Walker"
# 
# bs_df$Name[bs_df$Name == "Amon-Ra St"] <- "Amon-Ra St. Brown"





