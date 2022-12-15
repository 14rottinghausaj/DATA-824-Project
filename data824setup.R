library(cfbfastR)
library(pacman)
library(tictoc)

Sys.setenv(CFBD_API_KEY = "4zRuaObad/tH7xcqcbaGWdxlO9S+6Hw9Eq8soBpaDwsDLGkR49GyvhoAvYz1CToZ")

library(dplyr)
library(tidyr)

tictoc::tic()
lines <- data.frame()
progressr::with_progress({
  lines <- cfbfastR::cfbd_betting_lines(year = 2022)
})
tictoc::toc()

dat <- lines %>% filter(home_conference == "Big 12" & away_conference == "Big 12", provider == "Bovada", week != 14) %>% arrange(week, desc(game_id))

View(lines)

teams <- dat %>% distinct(home_team) %>% rename(team = home_team)

GetTeamData <- function(team, dat){
dat$spread <- as.numeric(dat$spread)
games <- dat %>% filter(home_team == team | away_team == team) %>%
  mutate(teamName = team,
         game = row_number(),
         favorite = ifelse(substring(formatted_spread, 1, nchar(team)+2) == paste0(team," -"),'F', 'U'), 
         opponent = ifelse(home_team == team, away_team, home_team),
         homeaway = ifelse(home_team == team, 'Home', 'Away'),
         teamscore = ifelse(home_team == team, home_score, away_score),
         oppscore = ifelse(home_team == team, away_score, home_score),
         bookSpread = ifelse(favorite == 'F', -abs(as.numeric(spread)),abs(as.numeric(spread))),
         actualSpread = oppscore - teamscore,
         moneyline = ifelse(homeaway == 'Home', home_moneyline, away_moneyline),
         ML_outcome = ifelse(teamscore > oppscore, "W", "L"),
         spread_outcome = ifelse(favorite == 'F', ifelse(oppscore - teamscore < bookSpread, "W", "L"), ifelse(oppscore - teamscore > bookSpread, "L", "W")),
         OU_outcome = ifelse(over_under > home_score + away_score, "U", ifelse(over_under < home_score + away_score, "O","P"))) %>%
  select(game, teamName, opponent, homeaway, favorite, teamscore, oppscore, bookSpread,actualSpread, over_under,moneyline, ML_outcome, spread_outcome, OU_outcome) %>%
  mutate_if(is.numeric, round, 1)

return(games)
         
}

test <- GetTeamData("Kansas State", dat)
test

GetResultsData <- function(dat){
  Record = paste0(dat %>% filter(ML_outcome == 'W') %>% count(), "-", dat %>% filter(ML_outcome == 'L') %>% count())
  ATS = paste0(dat %>% filter(spread_outcome == 'W') %>% count(), "-", dat %>% filter(spread_outcome == 'L') %>% count())
  OU = paste0(dat %>% filter(OU_outcome == 'O') %>% count(), "-", dat %>% filter(OU_outcome == 'U') %>% count(), "-",dat %>% filter(OU_outcome == 'P') %>% count())
  
  m <- matrix(ncol = 2, nrow = 3, byrow = T, c("Record", Record, "ATS", ATS, "OU", OU))
  
  return(data.frame("Wager Type" = m[,1], "Record" = m[,2]))
}

Payout <- function(odds){
  if(odds > 0){
    pay = (odds/100) * 100
  } else{
    pay = 100*(100/-odds)
  }
  return(pay)
}
test
GetPayouts <- function(dat){
  MLpayouts = matrix(ncol = 1, nrow = nrow(dat))
  ATSpayouts = matrix(ncol = 1, nrow = nrow(dat))
  OVpayouts = matrix(ncol = 1, nrow = nrow(dat))
  UNpayouts = matrix(ncol = 1, nrow = nrow(dat))
  
  for(i in 1:nrow(dat)){
    MLpayouts[i,1] = ifelse(dat[i,]$ML_outcome == "W", Payout(dat[i,]$moneyline), -100)
    ATSpayouts[i,1] = ifelse(dat[i,]$spread_outcome == "W", 100*(100/110), -100)
    OVpayouts[i,1] = ifelse(dat[i,]$OU_outcome == "O", 100*(100/110), ifelse(dat[i,]$OU_outcome == "U",-100, 0))
    UNpayouts[i,1] = ifelse(dat[i,]$OU_outcome == "U", 100*(100/110), ifelse(dat[i,]$OU_outcome == "O",-100, 0))
  }
  
  m <- matrix(ncol = 2, nrow = 4, byrow = T, data = c("ML", round(sum(MLpayouts[,1]),2), "ATS", round(sum(ATSpayouts[,1]),2), "Over", round(sum(OVpayouts[,1]),2), "Under", round(sum(UNpayouts[,1]),2)))
  
  return(data.frame("Bet Type" = m[,1], "Payout" = m[,2]))
}

test
GetResultsData(test)
GetPayouts(test)
saveRDS(teams,"C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/teams.RData" )
saveRDS(dat, "C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/project_data.RData")
saveRDS(GetTeamData, "C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/team_filter.RData")
saveRDS(GetPayouts, "C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/totalpayouts.RData")
saveRDS(Payout, "C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/payout.RData")
saveRDS(GetResultsData, "C:/Users/14rot/OneDrive/Documents/Masters Program/DATA 824/Final Project/DATA_824_Project/results.RData")
