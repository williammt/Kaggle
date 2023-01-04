# install.packages("tidyverse")
# install.packages("openxlsx")

library(tidyverse)

chess <- read.csv("C:\\Users\\William.Tang\\OneDrive - Prometric Inc\\Documents\\Kaggle Datasets\\archive\\chess.csv")
#players with GM titles on chess.com
id_games <- read.csv("C:\\Users\\William.Tang\\OneDrive - Prometric Inc\\Documents\\Kaggle Datasets\\archive\\id_games.csv")
#indonesian players
im_games <- read.csv("C:\\Users\\William.Tang\\OneDrive - Prometric Inc\\Documents\\Kaggle Datasets\\archive\\im_games.csv")
#players with IM titles

#time_control is in seconds
str(chess)

table(chess$rated)

hist(chess$white_rating)
# hist(chess$black_rating)

table(chess$time_control)[order(table(chess$time_control),decreasing = TRUE)]
# most popular times are blitz, bullet, hyperbullet, 3+1, 3+2, 5 min, rapid, lightning, 1+1
# we will observe these

unique(chess$white_result)
completegames <- c("win","timeout","resigned","repetition","checkmated","insufficient","agreed",
                   "timevsinsufficient","stalemate","abandoned")

popularmodes <- chess %>% 
  filter(time_control %in% c("300","60","30","180+1","180+2","300","600","10","1+1")) %>% 
  mutate(White_WLD = case_when(white_result %in% c("win") ~ "W",
                         white_result %in% c("timeout","resigned","checkmated","abandoned") ~ "L",
                         white_result %in% c("repetition","insufficient","agreed","stalemate","timevsinsufficent") ~ "D"),
         Black_WLD = case_when(White_WLD == "W" ~ "L",
                               White_WLD == "L" ~ "W",
                               White_WLD == "D" ~ "D"))
#bullet most popular. being 1 min games (2 min tops) people can just replay
  
averageratings <- popularmodes %>% 
  group_by(time_control) %>% 
  summarize(AverageWhiteRatings = mean(white_rating),
            WhiteSummary = list(summary(white_rating)),
            GamesCount = n(),
            MoreLowerRatedPlayers = (mean(white_rating) < median(white_rating))) %>% 
  arrange(desc(GamesCount))


gamespergm <- popularmodes %>% 
  group_by(gm_username,time_control) %>% 
  summarize(GamesPerModePerGM = n())

gmmostgames <- gamespergm %>% 
  ungroup() %>% 
  group_by(time_control) %>% 
  filter(GamesPerModePerGM == max(GamesPerModePerGM))
#benji with the most 5 min games

whitewinpercentage <- prop.table(table(popularmodes$White_WLD))
