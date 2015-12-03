# Yankee Swamp model

# Rules:
# 1) N players each bring one gift
# 2) Each round, a player can choose to pick a new gift or steal someone else's gift
# 3) If a person steals a gift, the steal-ee makes the same choice
# 4) Gifts can not be stolen more than three times each
# 5) Gifts cannot be stolen more than once per round
#
# Assumptions:
# 1) Gifts have an underlying value that is the same for all players
# 2) Gifts have a specific value to each individual player
# 3) Players can perfectly assess both a gift's value to them and the underlying value
# 4) Players steal the gift of maximal utility to them
#
# Possible strategies:
# 1) Player steals with probability p = (number of gifts taken) / N (naive)
# 2) Player always steals most valuable gift available
# 3) Player always steals second-most-valuable gift available
# 4) Player never steals
# 5) Player steals if any stealable gift has value (to them) greater than estimated underlying value of average gift
# 6) Player steals about-to-be unstealable gift if one available greater than estimated underlying value of avg gift
# 7) Player steals the (N - P/2)th-best gift, where N is number of players and P is draft position.

require(magrittr)
require(dplyr)
require(tidyr)
require(ggplot2)
set.seed(538)

# Variables
n.play <- 15 # Number of players
max.steal <- 3 # Maximum number of times a gift can be stolen
v <- .1 # Variance in tastes

strat1 <- function(){
  prob <- nrow(stealable)/available
  runif(1)<prob
}

strat5 <- function(){
  expected <- gifts %>%
    filter(opened==1) %>%
    summarize(mean(underlying.value)) %>%
    as.numeric()
  if (max(stealable$specific)>expected) TRUE
  else FALSE
}

strat6 <- function(){
  expected <- gifts %>%
    filter(opened==1) %>%
    summarize(mean(underlying.value)) %>%
    as.numeric()
  locks <- stealable %>%
    filter(steals==2)
  if (max(locks$specific)>expected) TRUE
  else FALSE
}

will.steal <- function(p){
  if (p==1) f <- strat1()
  if (p==2) f <- TRUE
  if (p==3) f <- TRUE
  if (p==4) f <- FALSE
  if (p==5) f <- strat5()
  if (p==6) f <- strat6()
  f
}

chooser <- function(p){
  if (p==1 | p==2 | p==5) {choice <- stealable$gift.no[which(stealable$specific==max(stealable$specific))]}
  if (p==3) {choice <-  
               ifelse(nrow(stealable)>1,
                      stealable$gift.no[which(stealable$specific==max(stealable$specific[stealable$specific!=max(stealable$specific)]))],
                      stealable$gift.no[which(stealable$specific==max(stealable$specific))])}
  if (p==6) {locks <- stealable %>% filter(steals==2)
  choice <- locks$gift.no[which(locks$specific==max(locks$specific))]}
  choice
}

result <- data.frame(player.no=1:n.play)
cumulative <- data.frame()

# MEGA LOOP
# This loop runs multiple games.
# Only thing that stays constant is the baseline rules.

for (games in 1:5){
  
  # Set up gifts
  gifts <- data.frame(gift.no = 1:n.play,steals=rep(0,times=n.play),opened=rep(0,times=n.play),underlying.value=runif(n.play))
  
  # Set up players
  # Each player has a strategy, chosen at random
  players <- data.frame(player.no=1:n.play,strategy=rep(sample(1:6,n.play,replace=T)))
  
  # Set up matrix of player-specific gift values
  values <- data.frame(gifts=1:n.play)
  for (i in 1:n.play){
    values[c(i+1)] <- sapply(gifts$underlying.value,function(x)x*runif(1,min=1-v,max=1+v))
    names(values)[i+1] <- paste0("player_",i)
  }
  
  # Game play
  who.has <- players
  who.has$gift <- NA
  #game <- data.frame(player.no=1:n.play) # This will track the full game
  total.rounds <- 0
  last.stolen <- 0
  
  for (i in 1:n.play){
    player <- i
    
    # Internal loop for swapping
    for (j in 1:1000){
      # Starting situation
      strategy <- players$strategy[which(players$player.no==player)]
      
      unopened <- gifts %>%
        filter(opened==0) %>%
        select(gift.no)
      if (nrow(unopened)==0) break
      
    
      
      # Need df of stealable gifts, with value to potential stealer attached
      stealable <- values[c(1,player+1)]
      names(stealable)[2] <- "specific"
      stealable <- gifts %>%
        filter(opened==1,steals<3,!gift.no==last.stolen) %>%
        left_join(stealable,by=c("gift.no"="gifts"))
      
      available <- nrow(stealable)+nrow(unopened)
      
      if (nrow(stealable)==0) steal <- FALSE
      else steal <- will.steal(strategy)
      
      if (steal){
        # If player steals
        choice <- chooser(strategy)  # This is the gift they steal
        last.stolen <- choice # this can't be stolen again
        newplayer <- who.has$player.no[which(who.has$gift==choice)] # player stolen from becomes new player
        who.has$gift[which(who.has$player.no==player)] <- choice # assign gift to stealer
        who.has$gift[which(who.has$player.no==newplayer)] <- NA
        gifts$steals[which(gifts$gift.no==choice)] <- gifts$steals[which(gifts$gift.no==choice)] +1 # add to number of times gift has been stolen
        print(paste0("Player ",player," steals gift #",choice," from Player ",newplayer))
        player <- newplayer
        total.rounds <- total.rounds+1
        #game[c(total.rounds+1)] <- who.has$gift
        #names(game)[total.rounds+1] <- paste0("round_",total.rounds)
      }
      else {
        # if player opens
        choice <- unopened$gift.no[1] # takes first available gift
        gifts$opened[gifts$gift.no==choice] <- 1 # gift has now been opened
        who.has$gift[who.has$player.no==player] <- choice # assign gift
        total.rounds <- total.rounds+1
        print(paste0("Player ",player," opens gift #",choice))
        last.stolen <- 0 # new turn, so reset this
        #game[c(total.rounds+1)] <- who.has$gift
        #names(game)[total.rounds+1] <- paste0("round_",total.rounds)
        break # No switching needed
      }
      
    }
  }
  
  # Result we care about is player position and utility
  result[c(games+1)] <- mapply(function(x)values[who.has$gift[x],x+1],result$player.no)
  names(result)[games+1] <- paste0("game_",games)
  who.has$result <- sapply(who.has$player.no,function(x)result[x,c(ncol(result))])
  who.has$game <- games
  cumulative <- rbind(cumulative,who.has)
}

result %>%
  gather(game,value,-player.no) %>%
  ggplot(.,aes(player.no,value,colour=game))+geom_line()

cumulative %>%
  group_by(strategy) %>%
  summarize(score=mean(result)) %>%
  ggplot(.,aes(strategy,score))+geom_line()