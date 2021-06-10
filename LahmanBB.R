library(dplyr)
library(plyr)
library(class)
library(ggplot2)
library(caret)
library(lda)
library(tree)
library(Lahman)

data(People)
summary(People)
Players = People[People$birthYear > 1900, ]
Players = subset(Players, select = c(playerID, birthYear, nameFirst, nameLast, weight, height, bats, throws, debut, finalGame, retroID, bbrefID))
length(unique(Players$playerID))

data(Batting)
summary(Batting)
Batters = Batting[Batting$yearID > 1919, ]
Batters =Batters %>% group_by(playerID, sum(G))

data(Pitching)
summary(Pitching)
Pitchers = Pitching[Pitching$yearID > 1919, ]

data(Salaries)
summary(Salaries)

data(Fielding)
summary(Fielding)
Fielders = Fielding[Fielding$yearID > 1919, ]

data(AwardsPlayers)
summary(AwardsPlayers)
PlayerAwards = AwardsPlayers[AwardsPlayers$yearID > 1919, ]

data(HallOfFame)
summary(HallOfFame)
HOF = HallOfFame[HallOfFame$category == "Player", ]

data(Appearances)
summary(Appearances)
Appear = Appearances[Appearances$yearID > 1919, ]

