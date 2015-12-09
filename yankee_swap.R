# Yankee Swamp model
#
# What is the best strategy for a Yankee Swap (also known as a white elephant exchange)
# Analysis conducted for FiveThirtyEight
#
# Inspired partly by this approach by Max Ghenis: https://github.com/analyzestuff/posts/blob/master/white_elephant/white_elephant.R
# Thanks also to Max for his helpful coding edits.
#
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
# 7) Same as #5 but factor in knowledge of gift player brought.
# 8) Ghosh-Mahdian: Player steals if best available gift has value > theta

require(magrittr)
require(dplyr)
require(tidyr)
require(ggplot2)
set.seed(538)

# INITIAL SETUP OF FUNCTIONS

strat1 <- function() {
  prob <- nrow(stealable) / available
  runif(1) < prob
}

strat5 <- function() {
  w <- values[c(1, player+1)]
  names(w)[2] <- "specific"
  w <- gifts %>%
    filter(opened == 1) %>%
    left_join(w, by=c("gift.no" = "gifts"))
  expected <- w %>%
    summarize(mean(specific)) %>%
    as.numeric()
  return(max(stealable$specific) > expected)
}

strat6 <- function() {
  w <- values[c(1, player+1)]
  names(w)[2] <- "specific"
  w <- gifts %>%
    filter(opened == 1) %>%
    left_join(w,by = c("gift.no" = "gifts"))
  expected <- w %>%
    summarize(mean(specific)) %>%
    as.numeric()
  locks <- stealable %>%
    filter(steals == max.steals - 1)
  return(max(locks$specific) > expected) 
}

strat7 <- function() {
  w <- values[c(1, player+1)]
  names(w)[2] <- "specific"
  w <- gifts %>%
    filter(opened == 1 | brought == player) %>%
    left_join(w,by = c("gift.no" = "gifts"))
  expected <- w %>%
    summarize(mean(specific)) %>%
    as.numeric()
  return(max(stealable$specific) > expected)
}

# Strategy 8 based off Ghosh-Mahdian
# Paper: http://www.arpitaghosh.com/papers/gift1.pdf
theta <- data.frame(player.no = n.play:1)
theta$theta[1] <- 0.5
for (x in 1:(n.play - 1)){
  theta$theta[x+1] <- theta$theta[x] - ((theta$theta[x])^2) / 2
}

strat8 <- function() {
  if (max(stealable$specific) >= theta$theta[which(theta$player.no == player)]) TRUE
  else FALSE
}


will_steal <- function(p) {
  return(switch(p,
                strat1(),  # 1
                TRUE,  # 2
                TRUE,  # 3
                FALSE,  # 4
                strat5(),  # 5
                strat6(),  # 6
                strat7(),  # 7
                strat8() # 8
  ))
}

# Function for selecting which gift to steal.
# For most strategies, it's just to take highest-value available gift.
chooser <- function(p) {
  # If strategy 3 and more than one stealable gift, get 2nd best gift.
  if (p == 3 && nrow(stealable) > 1) {
    return(stealable[order(stealable$specific, decreasing = T), "gift.no"][2])
  }
  # If strategy 1, 2, 5, 7, or 3 (and now only one gift), steal best gift.
  if (p %in% c(1, 2, 5, 7,8, 3)) {
    return(stealable[order(stealable$specific, decreasing = T), "gift.no"][1])
  }
  if (p == 6) {
    locks <- stealable %>% filter(steals == 2)
    return(locks$gift.no[which(locks$specific == max(locks$specific))])
  }
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
v <- .9 # Variance in tastes
iterations <- 10 # How many times to play?
extra <- FALSE # Does first person get an extra shot at the end?

cumulative <- data.frame()

# MEGA LOOP
# This loop runs multiple games.
# Only thing that stays constant is the baseline rules.

for (games in 1:iterations){
  print(paste0("GAME ", games))
  
  # Set up gifts. Also assign who brought which gift, for Strategy #7
  gifts <- data.frame(gift.no = 1:n.play, 
                      brought = sample(1:n.play,replace=F),
                      steals=rep(0, times = n.play),
                      opened=rep(0,times = n.play),
                      underlying.value = runif(n.play))
  
  # Set up players
  # Each player has a strategy, chosen at random. 
  players <- data.frame(player.no = 1:n.play,
                        strategy = rep(sample(1:8, n.play, replace=T)))
  
  # Set up matrix of player-specific gift values
  values <- data.frame(gifts=1:n.play)
  for (i in 1:n.play){
    values[c(i+1)] <- sapply(gifts$underlying.value, function(x)x*runif(1,min=1-v,max=1+v))
    names(values)[i+1] <- paste0("player_", i)
  }
  
  # Game play
  who_has <- players
  who_has$gift <- NA
  #game <- data.frame(player.no=1:n.play) # This will track the full game. Switched off when running multiple iterations.
  total.rounds <- 0
  last.stolen <- 0
  last.round <- 0
  
  for (i in 1:n.play){
    player <- i
    
    # Internal loop for swapping
    for (j in 1:1000){ # Capped at 1,000 swaps in one turn (shouldn't ever get there)
      # Starting situation
      strategy <- players$strategy[which(players$player.no == player)]
      
      # Need df of unopened gifts
      unopened <- gifts %>%
        filter(opened==0) %>%
        select(gift.no)
      
      # Need df of stealable gifts, with value to potential stealer attached
      stealable <- values[c(1,player+1)]
      names(stealable)[2] <- "specific"
      stealable <- gifts %>%
        filter(opened==1,steals<max.steals,!gift.no==last.stolen) %>%
        left_join(stealable,by=c("gift.no"="gifts"))
      
      available <- nrow(stealable) + nrow(unopened) # for use in strategy 1
      
      if (nrow(stealable) == 0) {
        steal <- FALSE # if there's nothing to steal, then open
      } else steal <- will_steal(strategy) # Otherwise use strategy to determine whether to open or steal
      
      if (steal) {
        
        # If player steals
        choice <- chooser(strategy)  # This is the gift they steal
        last.stolen <- choice # this can't be stolen again
        newplayer <- who_has$player.no[which(who_has$gift == choice)] # player stolen from becomes new player
        who_has$gift[which(who_has$player.no == player)] <- choice # assign gift to stealer
        who_has$gift[which(who_has$player.no == newplayer)] <- NA
        gifts$steals[which(gifts$gift.no == choice)] <- 
          gifts$steals[which(gifts$gift.no == choice)] + 1 # add to number of times gift has been stolen
        
        # This will print results of each turn. Helpful for debugging but unnecessary.
        print(paste0("Player ", player, ", following strategy #", strategy, ", steals gift #", choice, " from Player ", newplayer))
        
        player <- newplayer
        total.rounds <- total.rounds + 1
        # game[c(total.rounds+1)] <- who_has$gift
        # names(game)[total.rounds+1] <- paste0("round_",total.rounds)        
      } else {
        
        # if player opens
        choice <- unopened$gift.no[1] # takes first available gift
        gifts$opened[gifts$gift.no == choice] <- 1 # gift has now been opened
        who_has$gift[who_has$player.no == player] <- choice # assign gift
        total.rounds <- total.rounds + 1
        
        # This will print results of each turn. Helpful for debugging but unnecessary.
        print(paste0("Player ", player, ", following strategy #", strategy, ", opens gift #", choice))
        
        last.stolen <- 0 # new turn, so reset this
        if (choice == n.play & extra == TRUE) {
          last.round <- 1 # Under "extra round" rules, if this was the last gift, then this will be the last round        
          break
        } else break # No switching needed so don't go back through the switching loop
        #game[c(total.rounds+1)] <- who_has$gift
        #names(game)[total.rounds+1] <- paste0("round_",total.rounds)
      }
      
    }
    
    # Extra round if extra == TRUE
    # Key difference is it's a SWAP
    if (last.round == 1){
      player <- 1
      swap <- who_has$gift[1] # What is Player 1's CURRENT gift
      
      # Same basic code as above
      stealable <- values[c(1, player + 1)]
      names(stealable)[2] <- "specific"
      stealable <- gifts %>%
        filter(opened == 1,steals < max.steals) %>% # No "last stolen" here since it's a separate "turn"
        left_join(stealable, by = c("gift.no" = "gifts"))
      
      choice <- chooser(1)  # This is the gift they steal. It will ALWAYS be the best available gift.
      
      newplayer <- who_has$player.no[which(who_has$gift == choice)] # player stolen from becomes new player
      who_has$gift[which(who_has$player.no == player)] <- choice # assign gift to stealer
      who_has$gift[which(who_has$player.no == newplayer)] <- swap # assign swap to stealee      
      
      # Still add to these counts for tracking purposes
      gifts$steals[which(gifts$gift.no == choice)] <- gifts$steals[which(gifts$gift.no == choice)] + 1 # add to number of times gift has been stolen
      print(paste0("Player 1 steals gift #", choice, " from Player ", newplayer, ". Player ", newplayer, " gets gift #", swap, " in return."))
      total.rounds <- total.rounds + 1
      
    }
  }
  
  # Track results from all games
  who_has$result <- mapply(function(x)values[who_has$gift[x], x + 1], who_has$player.no)
  who_has <- gifts %>%
    select(gift.no,underlying.value) %>%
    left_join(who_has, ., by=c("gift" = "gift.no"))
  who_has$game <- games
  cumulative <- rbind(cumulative, who_has) # keeps cumulative list of all games and results
  
}

# save(cumulative, result, file="results.RData")

#############################################################################
#
# END MODEL
# BEGIN ANALYSIS

# Results by strategy
cumulative %>%
  group_by(strategy) %>%
  summarize(score = mean(result)) %>%
  ggplot(., aes(factor(strategy), score)) + geom_bar(stat = "identity") +
  ggtitle("Value by Strategy") + xlab("Strategy") + ylab("Score")

# Results by player order
cumulative %>%
  group_by(player.no) %>%
  summarize(score = mean(result)) %>%
  ggplot(., aes(player.no,score)) + geom_line() +
  ggtitle("Value by order of draw") + xlab("Player position") + ylab("Score")

# Results by strategy and player order
cumulative %>%
  filter(player.no < 6) %>% # Early drawers
  group_by(strategy) %>%
  summarize(score = mean(result)) %>%
  ggplot(., aes(factor(strategy), score))+geom_bar(stat = "identity")+
  ggtitle("Value by strategy for early drawers") + xlab("Strategy") + ylab("Score")
cumulative %>%
  filter(player.no >= 6,player.no < 11) %>% # Middle drawers
  group_by(strategy) %>%
  summarize(score = mean(result)) %>%
  ggplot(., aes(factor(strategy), score)) + geom_bar(stat = "identity")+
  ggtitle("Value by strategy for middle drawers") + xlab("Strategy") + ylab("Score")
cumulative %>%
  filter(player.no >= 11) %>% # Late drawers
  group_by(strategy) %>%
  summarize(score=mean(result)) %>%
  ggplot(., aes(factor(strategy),score)) + geom_bar(stat = "identity")+
  ggtitle("Value by strategy for late drawers") + xlab("Strategy") + ylab("Score")

# Regressions
# Basic regression
regress <- lm(result ~ player.no + factor(strategy), data = cumulative)
summary(regress)

# Add interaction term
regress <- lm(result ~ factor(strategy) * factor(player.no), data = cumulative)
summary(regress)
