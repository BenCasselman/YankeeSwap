# Yankee Swamp model

# Inspired to some degree by this approach by MaxGhenis: https://github.com/analyzestuff/posts/blob/master/white_elephant/white_elephant.R

# Rules:
# 1) N players each bring one gift
# 2) Each round, a player can choose to pick a new gift or steal someone else's gift
# 3) If a person steals a gift, the steal-ee makes the same choice
# 4) Gifts can not be stolen more than a fixed number of times each
# 5) Players can't "steal back" a gift that was just stolen
#
# Assumptions:
# 1) Gifts have an underlying value that is the same for all players (but not known to them)
# 2) Gifts have a specific value to each individual player
# 3) Players can perfectly assess a gift's value to them, but NOT the underlying value (might be interesting to play with this assumption)
# 4) Goal for each player is to maximize value of final gift
#
# Possible strategies:
# 1) Player steals with probability p = (number of gifts taken) / N (naive)
# 2) Player always steals most valuable gift available
# 3) Player always steals second-most-valuable gift available (if only one gift is available, player steals that one)
# 4) Player never steals
# 5) Player steals if any stealable gift has value (to them) greater than estimated underlying value of average gift
# 6) Player steals about-to-be unstealable gift if one available greater than estimated underlying value of avg gift
# 7) Ghosh-Mahdian: Player steals if best available gift has value > theta

require(magrittr)
require(dplyr)
require(tidyr)
require(ggplot2)
set.seed(538)

# INITIAL SETUP OF FUNCTIONS

strat1 <- function(){
  prob <- nrow(stealable)/available
  runif(1)<prob
}

strat5 <- function(){
  w <- values[c(1,player+1)]
  names(w)[2] <- "specific"
  w <- gifts %>%
    filter(opened==1) %>%
    left_join(w,by=c("gift.no"="gifts"))
  expected <- w %>%
    summarize(mean(specific)) %>%
    as.numeric()
  if (max(stealable$specific)>expected) TRUE
  else FALSE
}

strat6 <- function(){
  w <- values[c(1,player+1)]
  names(w)[2] <- "specific"
  w <- gifts %>%
    filter(opened==1) %>%
    left_join(w,by=c("gift.no"="gifts"))
  expected <- w %>%
    summarize(mean(specific)) %>%
    as.numeric()
  locks <- stealable %>%
    filter(steals==max.steals-1)
  if (max(locks$specific)>expected) TRUE
  else FALSE
}

# Strategy 7 based off Ghosh-Mahdian
# Paper: http://www.arpitaghosh.com/papers/gift1.pdf
theta <- data.frame(player.no=n.play:1)
theta$theta[1] <- 0.5
for (x in 1:(n.play-1)){
  theta$theta[x+1] <- theta$theta[x]-((theta$theta[x])^2)/2}

strat7 <- function(){
  if (max(stealable$specific)>=theta$theta[which(theta$player.no==player)]) TRUE
  else FALSE
}


will.steal <- function(p){
  if (p==1) f <- strat1()
  if (p==2) f <- TRUE
  if (p==3) f <- TRUE
  if (p==4) f <- FALSE
  if (p==5) f <- strat5()
  if (p==6) f <- strat6()
  if (p==7) f <- strat7()
  f
}

# Function for selecting which gift to steal.
# For most strategies, it's just to take highest-value available gift.
chooser <- function(p){
  if (p==1 | p==2 | p==5 | p==7) {choice <- stealable$gift.no[which(stealable$specific==max(stealable$specific))]}
  if (p==3) {choice <-  
               ifelse(nrow(stealable)>1,
                      stealable$gift.no[which(stealable$specific==max(stealable$specific[stealable$specific!=max(stealable$specific)]))],
                      stealable$gift.no[which(stealable$specific==max(stealable$specific))])}
  if (p==6) {locks <- stealable %>% filter(steals==2)
             choice <- locks$gift.no[which(locks$specific==max(locks$specific))]}
  choice
}

################################################################
#
#
# START RUNNING FROM HERE
#
#
#

# Variables
n.play <- 15 # Number of players
max.steals <- 3 # Maximum number of times a gift can be stolen
v <- .1 # Variance in tastes
iterations <- 500 # How many times to play?
extra <- FALSE # Does first person get an extra shot at the end?


result <- data.frame(player.no=1:n.play)
cumulative <- data.frame()

# MEGA LOOP
# This loop runs multiple games.
# Only thing that stays constant is the baseline rules.

for (games in 1:iterations){
  print(paste0("GAME ",games))
  
  # Set up gifts
  gifts <- data.frame(gift.no = 1:n.play,steals=rep(0,times=n.play),opened=rep(0,times=n.play),underlying.value=runif(n.play))
  
  # Set up players
  # Each player has a strategy, chosen at random
  players <- data.frame(player.no=1:n.play,strategy=rep(sample(1:7,n.play,replace=T)))
  
  # Set up matrix of player-specific gift values
  values <- data.frame(gifts=1:n.play)
  for (i in 1:n.play){
    values[c(i+1)] <- sapply(gifts$underlying.value,function(x)x*runif(1,min=1-v,max=1+v))
    names(values)[i+1] <- paste0("player_",i)
  }
  
  # Game play
  who.has <- players
  who.has$gift <- NA
  #game <- data.frame(player.no=1:n.play) # This will track the full game. Switched off when running multiple iterations.
  total.rounds <- 0
  last.stolen <- 0
  last.round <- 0
  
  for (i in 1:n.play){
    player <- i
    
    # Internal loop for swapping
    for (j in 1:1000){
      # Starting situation
      strategy <- players$strategy[which(players$player.no==player)]
      
      unopened <- gifts %>%
        filter(opened==0) %>%
        select(gift.no)
      #       if (nrow(unopened)==0) break
      
      # Need df of stealable gifts, with value to potential stealer attached
      stealable <- values[c(1,player+1)]
      names(stealable)[2] <- "specific"
      stealable <- gifts %>%
        filter(opened==1,steals<max.steals,!gift.no==last.stolen) %>%
        left_join(stealable,by=c("gift.no"="gifts"))
      
      available <- nrow(stealable)+nrow(unopened)
      
      if (nrow(stealable)==0) steal <- FALSE # if there's nothing to steal, then open
      else steal <- will.steal(strategy)  
          
      if (steal){
        # If player steals
        choice <- chooser(strategy)  # This is the gift they steal
        last.stolen <- choice # this can't be stolen again
        newplayer <- who.has$player.no[which(who.has$gift==choice)] # player stolen from becomes new player
        who.has$gift[which(who.has$player.no==player)] <- choice # assign gift to stealer
        who.has$gift[which(who.has$player.no==newplayer)] <- NA
        gifts$steals[which(gifts$gift.no==choice)] <- gifts$steals[which(gifts$gift.no==choice)] +1 # add to number of times gift has been stolen
        print(paste0("Player ",player,", following strategy #",strategy,", steals gift #",choice," from Player ",newplayer))
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
        print(paste0("Player ",player,", following strategy #",strategy,", opens gift #",choice))
        last.stolen <- 0 # new turn, so reset this
        if (choice == n.play & extra==TRUE) {
          last.round <- 1 # Under "extra round" rules, if this was the last gift, then this will be the last round        
          break} 
        else break # No switching needed
        #game[c(total.rounds+1)] <- who.has$gift
        #names(game)[total.rounds+1] <- paste0("round_",total.rounds)
      }
      
    }
    # Extra round
    # Key difference is it's a SWAP
    if (last.round==1){
      player <- 1
      swap <- who.has$gift[1] # What is Player 1's CURRENT gift
      
      stealable <- values[c(1,player+1)]
      names(stealable)[2] <- "specific"
      stealable <- gifts %>%
        filter(opened==1,steals<max.steals) %>% # No "last stolen" here since it's a separate "turn"
        left_join(stealable,by=c("gift.no"="gifts"))
      
      choice <- chooser(1)  # This is the gift they steal. It will ALWAYS be the best available gift.
      
      newplayer <- who.has$player.no[which(who.has$gift==choice)] # player stolen from becomes new player
      who.has$gift[which(who.has$player.no==player)] <- choice # assign gift to stealer
      who.has$gift[which(who.has$player.no==newplayer)] <- swap # assign swap to stealee      
      
      # Still add to these counts for tracking purposes
      gifts$steals[which(gifts$gift.no==choice)] <- gifts$steals[which(gifts$gift.no==choice)] +1 # add to number of times gift has been stolen
      print(paste0("Player 1 steals gift #",choice," from Player ",newplayer,". Player ",newplayer," gets gift #",swap," in return."))
      total.rounds <- total.rounds+1
      
    }
  }
  
  # Result we care about is player position and utility
  result[c(games+1)] <- mapply(function(x)values[who.has$gift[x],x+1],result$player.no)
  names(result)[games+1] <- paste0("game_",games)
  who.has$result <- sapply(who.has$player.no,function(x)result[x,c(ncol(result))]) # Find player-specific value
  who.has <- gifts %>%
    select(gift.no,underlying.value) %>%
    left_join(who.has,.,by=c("gift"="gift.no"))
  who.has$game <- games
  cumulative <- rbind(cumulative,who.has)
  
}

cumulative %>%
  group_by(strategy) %>%
  summarize(score=mean(result)) %>%
  ggplot(.,aes(factor(strategy),score))+geom_bar(stat="identity")+
  ggtitle("Value by Strategy")

cumulative %>%
  group_by(player.no) %>%
  summarize(score=mean(result)) %>%
  ggplot(.,aes(player.no,score))+geom_line()+
  ggtitle("Value by order of draw")

cumulative %>%
  filter(player.no<6) %>%
  group_by(strategy) %>%
  summarize(score=mean(result)) %>%
  ggplot(.,aes(factor(strategy),score))+geom_bar(stat="identity")+
  ggtitle("Value by strategy for early drawers")
cumulative %>%
  filter(player.no>=6,player.no<11) %>%
  group_by(strategy) %>%
  summarize(score=mean(result)) %>%
  ggplot(.,aes(factor(strategy),score))+geom_bar(stat="identity")+
  ggtitle("Value by strategy for middle drawers")
cumulative %>%
  filter(player.no>=11) %>%
  group_by(strategy) %>%
  summarize(score=mean(result)) %>%
  ggplot(.,aes(factor(strategy),score))+geom_bar(stat="identity")+
  ggtitle("Value by strategy for late drawers")

# save(cumulative,result,file="initial_results.RData")

# Some early takeaways:
# - Never changing is a terrible strategy.
# - So, somewhat surprisingly, is the "take the second best" strategy
# - Best strategy is to take best available, if it's bestter that expected value of pile
# - Changing variance of preferences affects shape but not overall takeaway
# - Going later is a big advantage
# - Allowing more swaps makes the game take forever
# - Strategy is less important later in the game