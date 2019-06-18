# library(rvest)
# library(stringr)
# library(lubridate)
# library(reshape2)
# library(tidyr)
# library(ggplot2)
# library(dplyr)

scr_gls <- function(url) {
#  url <-
#    'https://www.transfermarkt.com/cristiano-ronaldo/alletore/spieler/8198/saison//verein/0/liga/0/wettbewerb//pos/0/trainer_id/0/minute/0/torart/0/plus/1'
  
  page_html <- read_html(url)
  player <- c(
    html_nodes(page_html, xpath = '//*[@id="main"]/div[8]/div/div[1]/div[1]/div/div[1]/h1/b') %>% html_text(),
    html_nodes(page_html, xpath = '//*[@id="main"]/div[8]/div/div[1]/div[2]/div/div[1]/h1/b') %>% html_text()
    )
  
  p_dob <- c(
    html_nodes(page_html,
               xpath = '//*[@id="main"]/div[8]/div/div[1]/div[2]/div/div[1]/p[1]/span[2]') %>% html_text(),
    html_nodes(page_html,
               xpath = '//*[@id="main"]/div[8]/div/div[1]/div[3]/div/div[1]/p[1]/span[2]') %>% html_text()
    
    )
  p_dob <- gsub("[\r\n\t]", "", p_dob)
  p_dob <- as.POSIXct(p_dob,format='%b %d, %Y')
  gls_table <- 
    html_nodes(page_html, 
               xpath = '//*[@id="main"]/div[10]/div/div/div[4]/table') %>% html_table(fill = TRUE)
  gls_table <- gls_table[[1]]
  gls_table <- data.frame(gls_table)
  gls_table <- gls_table[-nrow(gls_table),]
  gls_table <- gls_table[!grepl("Season",gls_table$Competition),]
  keeps <- c('Competition.1','Date', 'vs..1','Pos.','Minute','Type.of.goal','Provider')
  gls_table <- gls_table[keeps]
  gls_table[gls_table==""] <- NA
  gls_table <- gls_table %>% fill(Competition.1,Date,vs..1,Pos.)
  gls_table$vs..1 <- gsub("\\([^)]*\\)", "", gls_table$vs..1)
  rownames(gls_table) <- seq(length=nrow(gls_table))
  gls_table$dates_conv <- as.POSIXct(gls_table$Date,format='%m/%d/%y')
  gls_table$Player <- player
  gls_table$dob <- p_dob
  gls_table$age <- as.numeric((gls_table$dates_conv - gls_table$dob) / 365.25)
  gls <- gls_table %>%
    group_by(Player) %>%
    mutate(count = seq(n()))
  return(gls)
}


#plot(gls$dates_conv,gls$count)
#ggplot(gls) + geom_line(aes(dates_conv,count), group = Player)
