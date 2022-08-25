library(ggrepel)
library(hrbrthemes)
library(ggplot2)

#exploratory data analysis
#RB
ggplot(final_df[which(final_df$Pos=='RB'),], aes(Proj.Salary, Average, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()

#WR
ggplot(final_df[which(final_df$Pos=='WR'),], aes(Proj.Salary, Average, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()

#TE
ggplot(final_df[which(final_df$Pos=='TE'),], aes(Proj.Salary, Average, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()

#QB
ggplot(final_df[which(final_df$Pos=='QB'),], aes(Proj.Salary, Average, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()


#analytics
ggplot(final_df[which(final_df$Pos=='WR'),], aes(Avg.Salary, Average, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()

#sorted bs over yh proj rating
ggplot(final_df[which(final_df$Pos=='QB'),], aes(bs_yhavg, bs_yhproj, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()

#beer sheets vs fp
ggplot(final_df, aes(FPProj.Salary, FP, label=Player))+
  geom_point()+
  stat_smooth()+
  geom_text_repel()

#KMEANS plot

#####

picks$picks <- "picks"

final_picks_df <- final_df |>
  left_join(picks, by = "Player")  |>
  select(c(1:26, 51)) |>
  rename(Pos = Pos.x, Bye_week = `Bye Week.x`, Team = Team.x, Average = Average.x,
         Proj.Salary = Proj.Salary.x, Avg.Salary = Avg.Salary.x, FP = FP.x,
         Lower = Lower.x, Upper = Upper.x, Comp = Comp.x, PAtt = PAtt.x, 
         PYds = Yds.x, PTDs = TDs.x, PInts = Ints.x, RAtt = Att.x, RYds = Yds.1.x, RTDs = TDs.1.x,
         Rec = Rec.x, Tgt = Tgt.x, RecYds = Yds.2.x, RecTDs = TDs.2.x, FPProj.Salary = FPProj.Salary.x,
         bs_yhavg = bs_yhavg.x, bs_yhproj = bs_yhproj.x) |>
  filter(Pos == final_df[which(final_df$Player == picks$Player)]$Pos & cluster == final_df[which(final_df$picks == "picks"),]$cluster)


final_df |>
  left_join(picks, by = "Player") |>
  group_by(Pos.x) |>
  group_map(~ ggplot(.) + 
              aes(x = Avg.Salary.x, y= Average.x, color = factor(cluster), label = Player) +
              geom_point(size = 3) +
              geom_text_repel(data = .x,
                              aes(x = Avg.Salary.x, y = Average.x, color = factor(cluster), label = Player),
                              nudge_y = .2,
                              check_overlap = TRUE) +
              scale_color_discrete("Dark2") + 
              labs(title = paste(.y$Pos.x, "cluster")) +
              theme_ipsum() + 
              theme(legend.position = "none"))



