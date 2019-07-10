load("gls2017.RData")
load("pls2017.RData")
library(data.table)

#

str(pls[[1]])
str(pls[[2]])

head(pls[[1]]$players)

#

players2017 <-  data.frame()
playing_positions <- data.frame()
teams17 <- data.frame()
leagues17 <- data.frame()

length(pls)

## 
for( i in 1:length(pls)) {
  # add the df's 
  players2017 <- rbind(players2017, pls[[i]]$players)
  playing_positions <- rbind(playing_positions, pls[[i]]$playing_positions)
  teams17 <- rbind(teams17, pls[[i]]$teams17)
  leagues17 <- rbind(leagues17, pls[[i]]$leagues17)
  
  ## 
  players2017 <- unique(players2017)
  playing_positions <- unique(playing_positions)
  teams17 <- unique(teams17)
  leagues17 <- unique(leagues17)
}

head(players2017)


library(data.table)
games2017 <- list()
## games list
for (i in 1:length(names(gls2017[[1]]))) {
  name <- names(gls2017[[1]])[[i]]
  ## 
  print(name)
  ## 
  all_dfs <- lapply(gls2017, function (x) { x[[name]] })
 
  df <- rbindlist(all_dfs)  
  games2017[[name]] <- df
}


library(dplyr)

## 
players_17 <- players2017 %>% select(id, active, draft_overall_pick, first_name, last_name, bats, height, salary, position_abbreviation, team_id, slug, years_of_experience)

## 
players_17$pitcher <- (players_17$position_abbreviation %in% c('RP', 'SP', 'P'))
players_17$outfield <- players_17$position_abbreviation %in% c('RF', 'LF', 'OF', 'CF')
players_17$base <- players_17$position_abbreviation %in% c('1B', '2B', '3B', 'SS')
players_17$hitter <- players_17$position_abbreviation %in% c('PH', 'DH', 'PR')
players_17$catcher <- players_17$position_abbreviation %in% c('C')

## all our pitchers' information from the
pitchers_17 <- unique(players_17[players_17$pitcher,])

View(games2017$game_logs)
game_logs_17 <- games2017$game_logs

head(game_logs_17)

## here, I use dplyr to GROUP our game data by each player
## and then use SUMMARISE to create statistics for each player
library(dplyr)
pitcher_stats_17 <- game_logs_17 %>%
  select(player_id, strike_percentage, pitcher_hits, pitcher_walks, walks) %>%
  group_by(player_id) %>%
  dplyr::summarise(mean_strike_pct=mean(strike_percentage, na.rm=T),
                   mean_hits=mean(pitcher_hits, na.rm=T),
                   mean_walks=mean(pitcher_walks, na.rm=T),
                   mean_walks2=mean(walks, na.rm=T))

View(pitcher_stats_17)
View(pitchers_17)


merged17 <- merge(pitchers_17, pitcher_stats_17, by.x="id", by.y="player_id")


merged_all_y_17<- merge(pitchers_17, pitcher_stats_17, by.x="id", by.y="player_id", all.y=T)

View(merged17)
View(merged_all_y_17)


pitchers_17 <- pitchers %>%
  merge(pitcher_stats_17, by.x="id", by.y="player_id") %>%
  filter(salary > 0,
         mean_strike_pct > 0)


help(merge)


# Now creating Players list labeled playerstats_17 
library(magrittr)
library(dplyr)
playerstats_17 <- game_logs_17 %>%
  dplyr::select(player_id, batting_average, hits, home_runs, runs_batted_in, runs, slugging_percentage) %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(mean_batting_average=mean(batting_average, na.rm=T),
                   total_hits=sum(hits, na.rm=T),
                   mean_hits=mean(hits,na.rm=T),
                   total_home_runs=sum(home_runs, na.rm = T),
                   mean_home_runs=mean(home_runs, na.rm=T),
                   total_runs=sum(runs,na.rm=T),
                   mean_runs=mean(runs, na.rm=T),
                   total_runs_batted_in=sum(runs_batted_in, na.rm=T),
                   mean_runs_batted_in=mean(runs_batted_in, na.rm=T))
                   
#Merging
merged_players17 <- merge(players_17, playerstats_17, by.x="id", by.y="player_id")

# All graph
library(ggplot2)
ggplot(data = merged_players17, aes(x = salary, y = mean_batting_average )) + geom_point() + ggtitle("Batting Average V. Salary") + xlab("Salary") + ylab("Batting Average")

#Corrplot of all players
library(corrplot)
all_plot <- merged_players17%>% select(mean_batting_average, total_runs_batted_in, total_hits, total_home_runs, total_runs, salary)
all_cor <- cor(all_plot, use="complete.obs")
all_cor
corrplot(all_cor, method = "circle")
## 
merged_all_players_17<- merge(players_17, playerstats_17, by.x="id", by.y="player_id", all.y=T)

#With Salary filter
nrow(merged_all_players_17)
salary_17 <- na.omit(merged_all_players_17)
nrow(na.omit(merged_all_players_17))

#players filtered from pitchers
library(dplyr)
pos_players <- salary_17 %>% filter(pitcher == 'FALSE')

#Now to remove the outliers with "0' Salaries as well
library(dplyr)
pos_players17 <- pos_players %>% filter(salary >"0")

#Now for the means of position players' stats
data(pos_players17)
mean(pos_players17[["mean_batting_average"]])
mean(pos_players17[["total_hits"]])
mean(pos_players17[["total_home_runs"]])
mean(pos_players17[["total_runs_batted_in"]])
mean(pos_players17[["salary"]])
mean(pos_players17[["years_of_experience"]])

#Salary versus mean BA
library(ggplot2)
ggplot(data = pos_players17, aes(x = salary, y = mean_batting_average )) + geom_point() + ggtitle("Batting Average V. Salary") + xlab("Salary") + ylab("Batting Average")
# salary versus home runs
ggplot(data = pos_players17, aes(x = salary, y = total_home_runs )) + geom_point()

#salary versus mean RBI
ggplot(data = pos_players17, aes(x = salary, y = mean_runs_batted_in )) + geom_point()

#salary versus total RBI
ggplot(data = pos_players17, aes(x = salary, y = total_runs_batted_in )) + geom_point()

#salary versus years of experience
ggplot(data = pos_players17, aes(x= salary, y = years_of_experience)) + geom_point()
#BA versus YOE
ggplot(data = pos_players17, aes(y= mean_batting_average, x = years_of_experience)) + geom_point()


#corrplot
library(corrplot)
Cplot1 <- pos_players17 %>% select(mean_batting_average, total_runs_batted_in, total_hits, total_home_runs, total_runs, salary)
corr <- cor(Cplot1, use="complete.obs")
corr

corrplot(corr, method = "circle")

#corrplot with experience
library(corrplot)
Cplot1 <- pos_players17 %>% select(mean_batting_average, total_runs_batted_in, total_hits, total_home_runs, total_runs, years_of_experience, salary)
corr <- cor(Cplot1, use="complete.obs")
corr

corrplot(corr, method = "circle")


