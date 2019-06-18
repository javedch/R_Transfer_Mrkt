library(transformr)
library(dplyr)
library(rvest)
library(stringr)
library(lubridate)
library(reshape2)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)

source('scrape_goals.r')

Player_URLs <- c(
  'https://www.transfermarkt.com/lionel-messi/alletore/spieler/28003/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/minute/0/torart/0/plus/1',
  'https://www.transfermarkt.com/sergio-aguero/alletore/spieler/26399/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/minute/0/torart/0/plus/1',
  'https://www.transfermarkt.com/cristiano-ronaldo/alletore/spieler/8198/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/minute/0/torart/0/plus/1',
  'https://www.transfermarkt.com/wayne-rooney/alletore/spieler/3332/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/minute/0/torart/0/plus/1'
  )


Players <- rbind(
  scr_gls(Player_URLs[1]),
  scr_gls(Player_URLs[2]),
  scr_gls(Player_URLs[3]),
  scr_gls(Player_URLs[4])
)



#Messi <- scr_gls(Player_URLs[1])
#Aguero <- scr_gls(Player_URLs[2])
#Ronaldo <- scr_gls(Player_URLs[3])
#Rooney <- scr_gls(Player_URLs[4])

Date_Plot <- 
  ggplot(Players,aes(x=dates_conv,y=count,colour = Player)) +
  geom_line() +
  labs(x = "Date", y = "No. of Goals")

Age_Plot <-
  ggplot(Players,aes(colour = Player)) +
  geom_line(aes(age,count)) +
  labs(x = "Age", y = "No. of Goals")

Date_Plot + transition_reveal(dates_conv)
Age_Plot + transition_reveal(age)
