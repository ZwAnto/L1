install.packages(c("rvest","stringr"))

Sys.setenv(http_proxy="zscaler-paris.pj.fr:80")

library(rvest)
library(stringr)

fetch_l1_calendar <- function(saison=100){

calendar=NULL # initialisaiton du data.frame de sortie

# boucle sur les journ�e
for (j in 1:38){
  
  # cr�ation de l'url de requete
  url <- paste0("http://www.lfp.fr/ligue1/competitionPluginCalendrierResultat/changeCalendrierJournee?sai=",saison,"&jour=",j)
  
  # extraction du code html
  site <- read_html(url)
 
  # nombre de jour dans la journ�e
  site %>% 
    html_node("body") %>%
    as.character() %>%
    str_count("<h4>") -> n_day
  
  # pour chaque jour r�cup�ration de la date et des match
  for (i in 1:n_day){
    
    # xpath match
    xpath<-paste0('//*[@id="tableaux_rencontres"]/div/table[',i,']')
    
    # xpath date
    xpath2<-paste0('//*[@id="tableaux_rencontres"]/div/h4[',i,']')
      
    # r�cup�ration des matchs
    site %>% 
      html_node("body") %>%
      html_node(xpath=xpath) %>%
     html_table -> temp
    
    # r�cup�ration de la date
    site %>% 
      html_node("body") %>%
      html_node(xpath=xpath2) %>%
      html_text -> temp2
    
    calendar <- rbind(calendar,cbind(temp2,temp))
 
  }
}

# supression des variables inutiles
calendar <- calendar[,c(1,2,3,5,7)]

# renomage des colones
colnames(calendar) <- c("date","time","home","score","away")

# s�paration du score en deux variables
calendar <- cbind(calendar,str_split_fixed(calendar$score," - ",n = 2))

# supression de l'ancienne colone de score
calendar <- calendar[,-4]

# renomage des colones de scores
colnames(calendar)[c(5,6)] <- c("home_score","away_score")

return(calendar)
}

##################
#   Saison
#
# 100 : 2016/2017
# 84  : 2015/1016
# 83  : 2014/2015
# ....
#
##################

fetch_l1_calendar(saison=100)