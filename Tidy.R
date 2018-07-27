#Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(caret)
library(randomForest)
library(gbm)
library(knitr)

#Load datasets
compSeason <- read.csv('RegularSeasonCompactResults.csv')
detSeason <- read.csv('RegularSeasonDetailedResults.csv')
teams <- read.csv('teams.csv')
season <- read.csv('seasons.csv')
compTour <- read.csv('TourneyCompactResults.csv')
detTour <- read.csv('TourneyDetailedResults.csv')
seeds <- read.csv('TourneySeeds.csv')

#1. Data Wrangling 
#Join seeds and team names for easier viewing
#Split Seeds column into region and actual seed
seeds <- seeds %>% left_join(teams, by = c('Team' = 'Team_Id')) %>% 
  mutate(Region = substr(Seed,1,1)) %>% mutate(Seed = as.numeric(substr(Seed,2,3))) 
seeds <- seeds[, c(1,5,2:4)]#Reorder Seeds columns

#Joining Compact Tourney with Seeds to add Team names and seeds for the season
#We also make new columns Team1 and Team 2 to reorder matchups so one columns is not exclusively wins or losses
#Rename columns to correspond correctly
compTour <- compTour %>% left_join(seeds, by = c('Season' = 'Season', 'Wteam' = 'Team')) %>%
  left_join(seeds, c('Season' = 'Season', 'Lteam' = 'Team')) %>%
  mutate(Team1 = ifelse(Wteam > Lteam, Wteam, Lteam), Team2 = ifelse(Wteam > Lteam, Lteam, Wteam)) %>%
  rename(Seed2 = Seed.x, Seed1 = Seed.y, Winner = Team_Name.x, Loser = Team_Name.y, Region2 = Region.x, Region1 = Region.y) 
compTour <- compTour[,c(1:3,11,9:10,4:5,14,12:13,6:8,15:16)] # Reorder columns
compTour$Team1Win <- ifelse(compTour$Wteam == compTour$Team1,1,0) #Assign binary 1/0 for Win/Lose


#Repeat for all large datasets CompSeason, detSeason, detTourney
#For the Season datasets, filter out the games where only non-seeded teams played
compSeason <- compSeason %>% left_join(seeds, c('Season' = 'Season', 'Wteam' = 'Team')) %>% 
  left_join(seeds, c('Season' = 'Season', 'Lteam' = 'Team')) %>%
  rename(Seed2 = Seed.x, Seed1 = Seed.y, Winner = Team_Name.x, Loser = Team_Name.y, Region2 = Region.x, Region1 = Region.y) %>%
  filter(xor(xor(!is.na(Seed2),  !is.na(Seed1)), !is.na(Seed2) & !is.na(Seed1))) #Filter out matches between non-seeded teams
compSeason <- compSeason[,c(1:3,11,9:10,4:5,14,12:13,6:8)]

detTour <- detTour %>% left_join(seeds, by = c('Season' = 'Season', 'Wteam' = 'Team')) %>%
  left_join(seeds, by = c('Season' = 'Season', 'Lteam' = 'Team')) %>%
  mutate(Team1 = ifelse(Wteam > Lteam, Wteam, Lteam), Team2 = ifelse(Wteam > Lteam, Lteam, Wteam)) %>%
  rename(Seed2 = Seed.x, Seed1 = Seed.y, Winner = Team_Name.x, Loser = Team_Name.y, Region2 = Region.x, Region1 = Region.y)
detTour <- detTour[,c(1:3,37,35:36,4,9:21,5,40,38:39,6,22:34,7:8,41:42)]
detTour$Team1Win <- ifelse(detTour$Wteam == detTour$Team1, 1, 0)

detSeason <- detSeason %>% left_join(seeds, by = c('Season' = 'Season', 'Wteam' = 'Team')) %>%
  left_join(seeds, by = c('Season' = 'Season', 'Lteam' = 'Team')) %>% 
  rename(Seed2 = Seed.x, Seed1 = Seed.y, Winner = Team_Name.x, Loser = Team_Name.y, Region2 = Region.x, Region1 = Region.y) %>%
  filter(xor(xor(!is.na(Seed2),  !is.na(Seed1)), !is.na(Seed2) & !is.na(Seed1))) #Filter out matches between non-seeded teams
detSeason <- detSeason[,c(1:3,37,35:36,4,9:21,5,40,38:39,6,22:34,7:8)]

#2. Calculating average statistics for use in model
#Split Season data into only winners and losers
compSeasonW <- compSeason %>% select(1:7,13:14) %>%
  rename(Team_ID = Wteam, Team = Winner, Seed = Seed2, Score = Wscore, Region = Region2) %>%
  mutate(Win = 1)
compSeasonL <- compSeason %>% select(1:2,8:14) %>%
  rename(Team_ID = Lteam, Team = Loser, Seed = Seed1, Score = Lscore, Region = Region1) %>%
  mutate(Win = 0)
#Bind the Win and Lose datasets that were split but by row and filter out stats for non-seeded teams
compSeason2 <- bind_rows(compSeasonW,compSeasonL) %>% filter(!is.na(Seed))

#Name vectors to convert column names so they can be joined
x <- c('Team_ID','Team','Region', 'Seed','Score','Fgm','Fga','Fgm3','Fga3',"Ftm", "Fta","Or", "Dr", "Ast",
       "To", "Stl", "Blk","Pf", "Wloc")
y <- c("Wteam",  "Winner", 'Region2', "Seed2",  "Wscore", "Wfgm",   "Wfga",   "Wfgm3",  "Wfga3",  "Wftm",   "Wfta",   "Wor",    "Wdr",
       "Wast",   "Wto",    "Wstl",   "Wblk",   "Wpf",    "Wloc")
z <- c("Lteam",  "Loser",  'Region1', "Seed1",  "Lscore", "Lfgm",   "Lfga",   "Lfgm3",  "Lfga3",  "Lftm",   "Lfta",   "Lor",    "Ldr",
       "Last",   "Lto",    "Lstl",   "Lblk",   "Lpf",    "Wloc")

#Rename columns and add win/lose binary
detSeasonW <- detSeason %>% select(1:20,39:40) %>% 
  rename_at(vars(y), ~x) %>% 
  mutate(Win = 1)
detSeasonL <- detSeason %>% select(1:2,21:40) %>% 
  rename_at(vars(z), ~x) %>%
  mutate(Win = 0)
#Bind season datasets by row and filter out non-seeded teams
detSeason2 <- bind_rows(detSeasonW,detSeasonL) %>% filter(!is.na(Team))


#Find the win percentage of each seeded team for each season in compact season data
df2 <- compSeason2 %>% group_by(Season, Team) %>% count(Team, Team_ID, Win) %>% spread(Win, n, fill = 0)
colnames(df2)[c(4,5)] <- c('Lose','Win') #Rename columns so they aren't '0' or '1'
df2 <- df2 %>% mutate(WinPct = Win / (Win + Lose)) #Calculate Win Percentae
df2 <- df2[, -c(2,4,5)] #Drop unnecessary columns
#Add win percentage to Team1 of compact tourney data set
compTour <- compTour %>% left_join(df2, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(compTour)[18] <- 'Team1WinPct' #Rename column for use
compTour$Team1Win <- as.factor(compTour$Team1Win)

#Calculate average team statistics for each team in the season
df2 <- detSeason2 %>% group_by(Season, Team_ID) %>% 
  summarise_at(.vars = c( "Score",   "Fgm",     "Fga",    "Fgm3",    "Fga3",    "Ftm",     "Fta", 
                          "Or",      "Dr",      "Ast",     "To",      "Stl",    "Blk",     "Pf"), mean)

#Join the average stats to the detailed tourney data and rename the column names to differentiate
detTour2 <- detTour %>% left_join(df2, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(detTour2)[44:57] <- paste('Team1Avg', colnames(detTour2)[44:57], sep = '')
detTour2 <- detTour2 %>% left_join(df2, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(detTour2)[58:71] <- paste('Team2Avg', colnames(detTour2)[58:71], sep = '')
#Drop statistics of the actual matches that occurred
detTour2 <- detTour2[, -c(7:20,25:38)]
detTour2$Team1Win <- as.factor(detTour2$Team1Win)

#New dataframe for Detailed Tourney with differences in team stats for reducing dimensionality
detTourT1 <- detTour2[,c(16:29)]
detTourT2 <- detTour2[,c(30:43)]
detTour3 <- detTourT1 - detTourT2
colnames(detTour3) <- paste('Diff', c( "Score",   "Fgm",     "Fga",    "Fgm3",    "Fga3",    "Ftm",     "Fta", 
                                       "Or",      "Dr",      "Ast",     "To",      "Stl",    "Blk",     "Pf")
                            , sep = '')
finalTour <- bind_cols(detTour2,detTour3)
finalTour <- finalTour[,c(1:15,44:57)]

#3. Build 2017 Tourney Data to predict the 2017 tourney
seeds2017 <- read.csv('TourneySeeds.csv') %>% filter(Season == 2017) %>% mutate(Seed = substr(Seed,1,3))
seeds2017 <- seeds2017[-c(11,18,50,63),]
slots <- read.csv('TourneySlots.csv')
slots$Slot <- as.character(slots$Slot)
slots <- slots %>% filter(Season == 2017)
slots <- slots[5:67,]
slots <- slots %>% mutate(Region1 = substr(Strongseed,1,1)) %>% mutate(Seed1 = as.numeric(substr(Strongseed,2,3))) %>%
  mutate(Region2 = substr(Weakseed,1,1)) %>% mutate(Seed2 = as.numeric(substr(Weakseed,2,3)))
slots$nextRound1 <- NA
slots$nextRound2 <- NA
slots <- slots %>% left_join(seeds2017, by = c('Season' = 'Season', 'Strongseed' = 'Seed')) %>% 
  left_join(seeds2017, by = c('Season' = 'Season', 'Weakseed' = 'Seed')) %>% rename(Team1 = Team.x, Team2 = Team.y)

#Calculate statistics from season 2017
season2017 <- detSeason2 %>% filter(Season == 2017)
season2017 <- season2017 %>% group_by(Season, Team_ID) %>% 
  summarise_at(.vars = c( "Score",   "Fgm",     "Fga",    "Fgm3",    "Fga3",    "Ftm",     "Fta", 
                          "Or",      "Dr",      "Ast",     "To",      "Stl",    "Blk",     "Pf"), mean)
#Add the season statistics to the tourney and rename as before
r1 <- slots[1:32,] %>% left_join(season2017, by = c('Season' = 'Season', 'Team1' = 'Team_ID'))
colnames(r1)[13:26] <- paste('Team1Avg', colnames(r1)[13:26], sep = '')
r1 <- r1 %>% left_join(season2017, by = c('Season' = 'Season', 'Team2' = 'Team_ID'))
colnames(r1)[27:40] <- paste('Team2Avg', colnames(r1)[27:40], sep = '')
r1 <- cbind(r1, r1[,13:26] - r1[,27:40])
colnames(r1)[41:54] <- paste('Diff', colnames(season2017)[3:16], sep='')

#Drop unecessary dataframes
rm(compSeasonL,compSeasonW,detSeasonL,detSeasonW,df2, detTourT1,detTour3,detTourT2)

