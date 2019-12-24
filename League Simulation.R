# Q1
rnorm_round = function(n, mean, sd) {
  round(rnorm(n, mean, sd),0)
}


rnorm_round(n, mean,sd)


# data put up
library(dplyr)
library(stringr)
df= read.csv("sample.csv")
colnames(df)[1] <- "Team"
df$Team <- trimws(df$Team, which = c("right"))

#q2
sim = function(a,b) {
  # connect Home team data
  A1 = df %>% filter(Team==a) %>% .$A
  D1 = df %>% filter(Team==a) %>% .$D
  H1 = df %>% filter(Team==a) %>% .$H
  R1 = df %>% filter(Team==a) %>% .$R
  
  # Away team 
  A2 = df %>% filter(Team==b) %>% .$A
  D2 = df %>% filter(Team==b) %>% .$D
  H2 = df %>% filter(Team==b) %>% .$H
  R2 = df %>% filter(Team==b) %>% .$R
  # Formula for rnorm_round
  exp = (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
  sd = 1/R1+1/R2
  result = rnorm_round(1,exp, sd)
  
  while (result == 0) {
    result = rnorm_round(1,exp, sd)
  }
  result
}



A1 = df %>% filter(Team=="Jersey Lions") %>% .$A
A1
sim("Jersey Lions", "Brooklyn Rats")

#q3
#install.packages("gtools")
library(gtools)
?permutations



# permutation of getting team


# permutation
# from the data
df= read.csv("sample.csv")
df$Team <- trimws(df$Team, which = c("right"))
# assign team
team = df$Team
game_circle = permutations(8,2, team)
game_circle
single_season_sim = function(){
  # using permutation for game schedule
  
  # output for win and point spread
  lionsWin = 0
  lionsps = 0
  catsWin = 0
  catsps = 0
  tigersWin = 0
  tigersps = 0
  dogsWin = 0
  dogsps = 0
  foxesWin = 0
  foxesps = 0
  bearsWin = 0
  bearsps = 0
  ducksWin = 0
  ducksps =0
  ratsWin =0
  ratsps =0
  # permutation is for half of season, so using for loop to repeat
  for (j in 1:2){
    # 56 match up for half of season
    for (i in 1:56) {
      result = sim(game_circle[i], game_circle[i+56])
      if (result > 0){
        if(game_circle[i] == "Jersey Lions"){
          lionsWin = lionsWin + 1
          lionsps = lionsps + result
        } else if (game_circle[i] == "Westchester Cats"){
          catsWin = catsWin + 1
          catsps = catsps + result
        } else if(game_circle[i] =="Long Island Tigers"){
          tigersWin = tigersWin + 1
          tigersps = tigersps + result
        } else if (game_circle[i] =="Staten Island Dogs"){
          dogsWin = dogsWin + 1
          dogsps = dogsps + result
        } else if(game_circle[i] =="The Bronx Foxes"){
          foxesWin = foxesWin + 1
          foxesps = foxesps + result
        } else if(game_circle[i] == "Queens Bears"){
          bearsWin = bearsWin + 1
          bearsps = bearsps + result
        } else if (game_circle[i] =="Manhattan Ducks"){
          ducksWin = ducksWin + 1
          ducksps = ducksps + result
        } else {
          ratsWin= ratsWin + 1
          ratsps = ratsps + result
        } 
      } else{
        if(game_circle[i + 56] == "Jersey Lions"){
          lionsWin = lionsWin + 1
          lionsps = lionsps + (-result)
        } else if (game_circle[i + 56] == "Westchester Cats"){
          catsWin = catsWin + 1
          catsps = catsps + (-result)
        } else if(game_circle[i + 56] =="Long Island Tigers"){
          tigersWin = tigersWin + 1
          tigersps = tigersps + (-result)
        } else if (game_circle[i + 56] =="Staten Island Dogs"){
          dogsWin = dogsWin + 1
          dogsps = dogsps + (-result)
        } else if(game_circle[i + 56] =="The Bronx Foxes"){
          foxesWin = foxesWin + 1
          foxesps = foxesps + (-result)
        } else if(game_circle[i+ 56] == "Queens Bears"){
          bearsWin = bearsWin + 1
          bearsps = bearsps + (-result)
        } else if (game_circle[i+56] =="Manhattan Ducks"){
          ducksWin = ducksWin + 1
          ducksps = ducksps + (-result)
        } else {
          ratsWin= ratsWin + 1
          ratsps = ratsps +(-result)
        }
      }
    }
  }
  team =c("lions", "cats", "tigers", "dogs", "foxes" , "bears" , "ducks", "rats")
  resWin = c(lionsWin, catsWin, tigersWin, 
          dogsWin, foxesWin , bearsWin , 
          ducksWin , ratsWin)
  resps = c(lionsps,catsps, tigersps, 
            dogsps, foxesps , bearsps, 
            ducksps , ratsps)       
  list(team = team, resWin=resWin, resps=resps)
}

# simulate 100,000 season
out = single_season_sim()


out


season_result 
# Issue
# 1. one team should come out
# 2. after 10 sims, number is returned not team name
# simulating 100,000
nsim = 100000
season_winner = character(nsim)
lionsrank = numeric(nsim)
catsrank = numeric(nsim)
tigersrank = numeric(nsim)
dogsrank = numeric(nsim)
foxesrank = numeric(nsim)
bearsrank = numeric(nsim)
ducksrank = numeric(nsim)
ratsrank = numeric(nsim)
counter = 0
for (n in 1:nsim){
  out1= single_season_sim()
  # winner
  
  # sort
  team_sort = out1$team[order(-out1$resWin, -out1$resps, out1$team)]
  # get winner
  season_winner[n] = team_sort[1]

  # rank
  lionsrank[n] = which(team_sort=="lions")
  catsrank[n] = which(team_sort=="cats")
  tigersrank[n]= which(team_sort=="tigers")
  dogsrank[n] = which(team_sort=="dogs")
  foxesrank[n] = which(team_sort=="foxes")
  bearsrank[n] = which(team_sort=="bears")
  ducksrank[n] = which(team_sort=="ducks")
  ratsrank[n] = which(team_sort=="rats")
  
  counter = counter + 1
  print(counter)
}


winner_table = table(season_winner)

winner_table
sum(winner_table)
lionsrank
season_winner


# create a columns for average rank by dividing nsim

season_rank = data.frame(lionsrank, catsrank,
                          tigersrank, dogsrank, foxesrank, bearsrank, ducksrank,ratsrank)


write.csv(winner_table, 'winner_table.csv', row.names = FALSE)
write.csv(season_rank,'season_rank.csv', row.names = FALSE)

library(ggplot2)
sort_winner_table = sort(prop.table(winner_table), decreasing = TRUE)
write.csv(sort_winner_table, 'sort_winner_table.csv', row.names = FALSE)
barplot(sort_winner_table,
        main="Percentage that each team wins the league simulation",
        xlab="Team",
        ylab="Percentage",
        col = "darkred")

season_rankA = data.frame(lionrank=lionsrank, catsrank = catsrank,
                         tigersrank=tigersrank,dogsrank= dogsrank, 
                         foxesrank = foxesrank, bearsrank = bearsrank, 
                         ducksrank=ducksrank,ratsrank = ratsrank)
barplot(colMeans(season_rank), 
     names.arg =c("lions", "cats", "tigers", "dogs", "foxes" , "bears" , "ducks", "rats"))
warnings()
season_rankB = data.frame(avg=round(colMeans(season_rankA),2))
season_rankB$team = c("Lions", "Cats", "Tigers", "Dogs", "Foxes" , "Bears" , "Ducks", "Rats")
season_rankB

ggplot(season_rankB)+aes(x=reorder(team,avg), y=avg, label=avg)+
  geom_bar(stat="identity",fill="#336699")+
  ggtitle('Average rankings of each team')+
  labs(y = "Percentage",
       x = "Team")+
  geom_text(size = 5, position = position_stack(vjust = 0.8))+
  theme(axis.text=element_text(size=10),
        plot.title = element_text(hjust = 0.5,face = "bold"))



season_rankC= read.csv("season_rank.csv")

# q4
team_name = df$Team
team_name
team1 = team_name[which(season_rankC[1,]==1)]
team8 = team_name[which(season_rankC[1,]==8)]


team1win = 0
team8win = 0
first_res=sim(team1, team8)
if (first_res > 0){
  team1win = team1win+1
} else {
  team8win = team8win +1
}

second_res = sim(team1, team8)
if (second_res > 0){
  team1win = team1win+1
} else {
  team8win = team8win +1
} 

third_res = sim(team8, team1)
if (third_res > 0){
  team8win = team8win+1
} else {
  team1win = team1win +1
}
team1win
team8win
if (team1win <= 2 & team8win <=2){
 fourth_res = sim(team8, team1)
  if (fourth_res > 0){
    team8win = team8win+1
  } else {
    team1win = team1win +1
  }
}
team1win
team8win
if (team1win <= 2 & team8win <=2){
  fifth_res = sim(team1, team8)
  if (fifth_res > 0){
    team1win = team1win+1
  } else {
    team8win = team8win +1
  }
}
  
team1win
team8win
  
  
  
  






while (Home <= 2 & Away <=2){
  first_res=sim(team1, team8)
  if (first_res > 0){
    Home = Home + 1
  } else {
    Away = Away + 1
  }
}
if (Home == 3){
  winnerA = team1
} else{
  winnerA = team8
}















nsim = 100000
cham = character(nsim)
counter = 0
for (i in 1:nsim){
  # assign team
  team1 = team_name[which(season_rankC[i,]==1)]
  team2 = team_name[which(season_rankC[i,]==2)]
  team3 = team_name[which(season_rankC[i,]==3)]
  team4 = team_name[which(season_rankC[i,]==4)]
  team5 = team_name[which(season_rankC[i,]==5)]
  team6 = team_name[which(season_rankC[i,]==6)]
  team7 = team_name[which(season_rankC[i,]==7)]
  team8 = team_name[which(season_rankC[i,]==8)]
  # first matchi
  team1win = 0
  team8win = 0
  
  first_res=sim(team1, team8)
  if (first_res > 0){
    team1win = team1win +1
  } else {
    team8win = team8win +1
  }
  
  second_res = sim(team1, team8)
  if (second_res > 0){
    team1win = team1win +1
  } else {
    team8win = team8win +1
  } 
  
  third_res = sim(team8, team1)
  if (third_res > 0){
    team8win = team8win +1
  } else {
    team1win = team1win +1
  }
  
  if (team1win <= 2 & team8win <=2){
    fourth_res = sim(team8, team1)
    if (fourth_res > 0){
      team8win = team8win+1
    } else {
      team1win = team1win +1
    }
  }
  if (team1win <= 2 & team8win <=2){
    fifth_res = sim(team1, team8)
    if (fifth_res > 0){
      team1win = team1win+1
    } else {
      team8win = team8win +1
    }
  }

  if (team1win == 3){
    winnerA = team1
  } else{
    winnerA = team8
  }
  
  # Second match
  team4win = 0
  team5win = 0
  first_res=sim(team4, team5)
  if (first_res > 0){
    team4win = team4win +1
  } else {
    team5win = team5win +1
  }
  
  second_res = sim(team4, team5)
  if (second_res > 0){
    team4win = team4win +1
  } else {
    team5win = team5win +1
  } 
  
  third_res = sim(team5, team4)
  if (third_res > 0){
    team5win = team5win +1
  } else {
    team4win = team4win +1
  }
  
  if (team4win <= 2 & team5win <=2){
    fourth_res = sim(team5, team4)
    if (fourth_res > 0){
      team5win = team1win+1
    } else {
      team4win = team4win +1
    }
  }
  if (team4win <= 2 & team5win <=2){
    fifth_res = sim(team4, team5)
    if (fifth_res > 0){
      team4win = team4win+1
    } else {
      team5win = team5win +1
    }
  }
  if (team4win == 3){
    winnerB = team4
  } else{
    winnerB = team5
  }
  # Third match
  team3win = 0
  team6win = 0
  first_res=sim(team3, team6)
  if (first_res > 0){
    team3win = team3win +1
  } else {
    team6win = team6win +1
  }
  
  second_res = sim(team3, team6)
  if (second_res > 0){
    team3win = team3win +1
  } else {
    team6win = team6win +1
  } 
  
  third_res = sim(team6, team3)
  if (third_res > 0){
    team6win = team6win +1
  } else {
    team3win = team3win +1
  }
  
  if (team3win <= 2 & team6win <=2){
    fourth_res = sim(team6, team3)
    if (fourth_res > 0){
      team6win = team6win+1
    } else {
      team3win = team3win +1
    }
  }
  if (team3win <= 2 & team6win <=2){
    fifth_res = sim(team3, team6)
    if (fifth_res > 0){
      team3win = team3win+1
    } else {
      team6win = team6win +1
    }
  }
  if (team3win == 3){
    winnerC = team3
  } else{
    winnerC = team6
  }
  # Fourth match
  team2win = 0
  team7win = 0
  first_res=sim(team2, team7)
  if (first_res > 0){
    team2win = team2win +1
  } else {
    team7win = team7win +1
  }
  
  second_res = sim(team2, team7)
  if (second_res > 0){
    team2win = team2win +1
  } else {
    team7win = team7win +1
  } 
  
  third_res = sim(team7, team2)
  if (third_res > 0){
    team7win = team7win +1
  } else {
    team2win = team2win +1
  }
  
  if (team2win <= 2 & team7win <=2){
    fourth_res = sim(team7, team2)
    if (fourth_res > 0){
      team7win = team7win+1
    } else {
      team2win = team2win +1
    }
  }
  if (team2win <= 2 & team7win <=2){
    fifth_res = sim(team2, team7)
    if (fifth_res > 0){
      team2win = team2win+1
    } else {
      team7win = team7win +1
    }
  }
  if (team2win == 3){
    winnerD = team2
  } else{
    winnerD = team7
  }
  ## second round
  # first match
  winnerAwin = 0
  winnerBwin = 0
  first_res=sim(winnerA, winnerB)
  if (first_res > 0){
    winnerAwin = winnerAwin +1
  } else {
    winnerBwin = winnerBwin +1
  }
  
  second_res = sim(winnerA, winnerB)
  if (second_res > 0){
    winnerAwin = winnerAwin +1
  } else {
    winnerBwin = winnerBwin +1
  } 
  
  third_res = sim(winnerB, winnerA)
  if (third_res > 0){
    winnerBwin = winnerBwin +1
  } else {
    winnerAwin = winnerAwin +1
  }
  
  if (winnerAwin <= 2 & winnerBwin <=2){
    fourth_res = sim(winnerB, winnerA)
    if (fourth_res > 0){
      winnerBwin = winnerBwin +1
    } else {
      winnerAwin = winnerAwin +1
    }
  }
  if (winnerAwin <= 2 & winnerBwin <=2){
    fifth_res = sim(winnerA, winnerB)
    if (fifth_res > 0){
      winnerAwin = winnerAwin +1
    } else {
      winnerBwin = winnerBwin +1
    }
  }
  if (winnerAwin == 3){
    winnerE = winnerA
  } else{
    winnerE = winnerB
  }
  # second match
  winnerCwin = 0
  winnerDwin = 0
  first_res=sim(winnerC, winnerD)
  if (first_res > 0){
    winnerCwin = winnerCwin +1
  } else {
    winnerDwin = winnerDwin +1
  }
  
  second_res = sim(winnerC, winnerD)
  if (second_res > 0){
    winnerCwin = winnerCwin +1
  } else {
    winnerDwin = winnerDwin +1
  } 
  
  third_res = sim(winnerD, winnerC)
  if (third_res > 0){
    winnerDwin = winnerDwin +1
  } else {
    winnerCwin = winnerCwin +1
  }
  
  if (winnerCwin <= 2 & winnerDwin <=2){
    fourth_res = sim(winnerD, winnerC)
    if (fourth_res > 0){
      winnerDwin = winnerDwin +1
    } else {
      winnerCwin = winnerCwin +1
    }
  }
  if (winnerCwin <= 2 & winnerDwin <=2){
    fifth_res = sim(winnerC, winnerD)
    if (fifth_res > 0){
      winnerCwin = winnerCwin +1
    } else {
      winnerDwin = winnerDwin +1
    }
  }
  if (winnerCwin == 3){
    winnerF = winnerC
  } else{
    winnerF = winnerD
  }
  # Final Match
  winnerEwin = 0
  winnerFwin = 0
  first_res=sim(winnerE, winnerF)
  if (first_res > 0){
    winnerEwin = winnerEwin +1
  } else {
    winnerFwin = winnerFwin +1
  }
  
  second_res = sim(winnerE, winnerF)
  if (second_res > 0){
    winnerEwin = winnerEwin +1
  } else {
    winnerFDwin = winnerFwin +1
  } 
  
  third_res = sim(winnerF, winnerE)
  if (third_res > 0){
    winnerFwin = winnerFwin +1
  } else {
    winnerEwin = winnerEwin +1
  }
  
  if (winnerEwin <= 2 & winnerFwin <=2){
    fourth_res = sim(winnerF, winnerE)
    if (fourth_res > 0){
      winnerFwin = winnerFwin +1
    } else {
      winnerEwin = winnerEwin +1
    }
  }
  if (winnerEwin <= 2 & winnerFwin <=2){
    fifth_res = sim(winnerE, winnerF)
    if (fifth_res > 0){
      winnerEwin = winnerEwin +1
    } else {
      winnerFwin = winnerFwin +1
    }
  }
  if (winnerEwin == 3){
    cham[i] = winnerE
  } else{
    cham[i]= winnerF
  }
  counter = counter + 1
  print(counter)
}
cham
table(cham)
playoff = table(cham)
write.csv(playoff, 'playoff.csv', row.names = FALSE)
library(dplyr)
library(ggplot2)
playoff_result= read.csv("playoff.csv")
playoff_result= playoff_result %>% mutate(Percentage = round((Freq/100000)*100,2))


ggplot(playoff_result) + aes(x=reorder(cham,Percentage), y=Percentage,label=Percentage)+
  geom_bar(stat="identity",fill="#33FF66") + coord_flip()+
  ggtitle("Best-of-three Playoff Average")+
  labs(y = "Percentage",
       x = "Team") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme(axis.text=element_text(size=8),
        plot.title = element_text(hjust = 0.5,face = "bold"))
