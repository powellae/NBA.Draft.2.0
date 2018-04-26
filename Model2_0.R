#---- NBA Draft Model 2.0 ----
#Last Updated: April 23, 2018
#Author: Alexander Powell
#
#
#
#----------------------------#

#Packages
library(plyr)
library(dplyr)
library(data.table)
library(randomForest)
library(e1071)
library(tidyverse)

#Global Functions
multmerge <- function(path){
  filenames=list.files(path = path, full.names = TRUE)
  rbindlist(lapply(filenames, fread))
}

#Data----

CBB_yby <- read.csv("Documents/Analytics/NBA Draft Model 2_0/br_college.csv") #year-by-year college statistics 2000-2018
CBB_rsci <- read.csv("Documents/Analytics/NBA Draft Model 2_0/br_rsci.csv") #year-by-year rsci rankings through 2017


##NBA Salary Data
SalaryCap <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_SalaryCap_85_to_18.csv") #NBA salary cap history
Salaries <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_Salaries_90_to_18.csv") #player contracts 1992-present (with gaps)

#Data Cleaning----

##CBB Data Cleaning
#Merging Kenpom Data (changing team names)
kenpom <- multmerge("Documents/Analytics/NBA Draft Model 1_0/kenpom")
kenpom <- kenpom[,c("Season", "Team", "AdjTempo", "AdjOE", "AdjDE")]
for(i in 1:nrow(kenpom)){
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "North Carolina", "UNC", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Saint Joseph's", "St Joseph's", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Saint Mary's", "St Mary's", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Oklahoma St.", "Oklahoma St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Arizona St.", "Arizona St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Florida St.", "Florida St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Michigan St.", "Michigan St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Ohio St.", "Ohio St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Colorado St.", "Colorado St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "North Carolina St.", "N.C. State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Eastern Washington", "Eastern Wash.", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Kansas St.", "Kansas St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Fresno St.", "Fresno St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Iowa St.", "Iowa St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Morehead St.", "Morehead St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "San Diego St.", "San Diego St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Washington St.", "Washington St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Murray St.", "Murray St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Norfolk St.", "Norfolk St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Weber St.", "Weber St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Long Beach St.", "LBSU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Western Carolina", "WCU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Western Kentucky", "WKU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Missouri St.", "Missouri St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Oregon St.", "Oregon St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Georgia St.", "Georgia St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Boise St.", "Boise St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Montana St.", "Montana St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Fort Wayne", "IPFW", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Penn St.", "Penn St", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Wichita St.", "Wichita State", kenpom$Team[i])
}

#Advanced Statistics
CBB_yby$Poss <- CBB_yby$FGA + CBB_yby$TOV + 0.44*CBB_yby$FTA
CBB_yby$Ast.Pos <- CBB_yby$AST / CBB_yby$Poss
CBB_yby$Tov.Pos <- CBB_yby$TOV / CBB_yby$Poss
CBB_yby$Stl.G <- CBB_yby$STL / CBB_yby$G
CBB_yby$Blk.G <- CBB_yby$BLK / CBB_yby$G
CBB_yby$TS <- CBB_yby$PTS / (CBB_yby$FGA + 0.44*CBB_yby$FTA)
CBB_yby$FTA.G <- CBB_yby$FTA / CBB_yby$G
CBB_yby$Three.G <- CBB_yby$X3PA / CBB_yby$G
CBB_yby$Two.G <- CBB_yby$X2PA / CBB_yby$G
CBB_yby$Three.Per <- ifelse(CBB_yby$X3PA == 0, 0, CBB_yby$X3P / CBB_yby$X3PA)
CBB_yby$Two.Per <- ifelse(CBB_yby$X2PA == 0, 0, CBB_yby$X2P / CBB_yby$X2PA)
CBB_yby$FTA.FGA <- CBB_yby$FTA / CBB_yby$FGA
CBB_yby$Three.FGA <- CBB_yby$X3PA / CBB_yby$FGA
CBB_yby$OR.G <- CBB_yby$ORB / CBB_yby$G
CBB_yby$DR.G <- CBB_yby$DRB / CBB_yby$G
CBB_yby$PF.G <- CBB_yby$PF / CBB_yby$G
CBB_yby$Poss <- CBB_yby$Poss / CBB_yby$G

#Fix no-year players
CBB_yby <- within(CBB_yby, Class[Player == 'Devonte\' Graham' & Class == ''] <- 'SR')
CBB_yby <- within(CBB_yby, Class[Player == 'Deandre Hunter' & Class == ''] <- 'FR')
CBB_yby <- within(CBB_yby, Class[Player == 'Jaylen Adams' & Class == ''] <- 'SR')
CBB_yby <- within(CBB_yby, Class[Player == 'Josh Okogie' & Class == ''] <- 'SO')
CBB_yby <- within(CBB_yby, Class[Player == 'Jae\'Sean Tate' & Class == ''] <- 'SR')
CBB_yby <- within(CBB_yby, Class[Player == 'Austin Wiley' & Class == ''] <- 'FR')

#Fix years to match kenpom and merge
CBB_yby$Season <- as.numeric(gsub("\\-.*", "", CBB_yby$Season)) + 1
colnames(kenpom)[2] <- "School"
CBB_yby <- merge(CBB_yby, kenpom, by = c("Season", "School"))

#Group-by and weights
CBB_yby <- CBB_yby %>% 
  group_by(Player, School) %>%
  dplyr::mutate(Numb = row_number())

#CBB_yby$Wgts <- ifelse(CBB_yby$Class == "FR" | CBB_yby$Class == "Fr", 1, 
#                       ifelse(CBB_yby$Class == "SO" | CBB_yby$Class == "So", 2,
#                              ifelse(CBB_yby$Class == "JR" | CBB_yby$Class == "Jr", 3,
#                                     ifelse(CBB_yby$Class == "SR" | CBB_yby$Class == "Sr", 4, 4))))

CBB <- CBB_yby[,c(2,4:6,8,13,15,17,27:46)]
for(i in 1:nrow(CBB)){ #Weighting individual seasons
  if((i %% 1000) == 0){
    print(i/60000) #tracking progress
  }
  for(j in 6:27){
    CBB[i,j] <- CBB$Numb[i] * CBB[i,j]
  }
}

CBB_career <- CBB[,c(1:2,4:28)] %>%
  group_by(Player, Pos, School) %>%
  dplyr::mutate(G = sum(G), X2PA = sum(X2PA), X3PA = sum(X3PA), FTA = sum(FTA), Poss = sum(Poss),
         Ast.Pos = sum(Ast.Pos), Tov.Pos = sum(Tov.Pos), Stl.G = sum(Stl.G), Blk.G = sum(Blk.G),
         TS = sum(TS), FTA.G = sum(FTA.G), Three.G = sum(Three.G), Two.G = sum(Two.G), Three.Per = sum(Three.Per),
         Two.Per = sum(Two.Per), FTA.FGA = sum(FTA.FGA), Three.FGA = sum(Three.FGA), OR.G = sum(OR.G),
         DR.G = sum(DR.G), PF.G = sum(PF.G), Numb = max(Numb), adjTempo = sum(AdjTempo), 
         adjOE = sum(AdjOE), adjDE = sum(AdjDE)) %>%
  dplyr::select(Player, Pos, School, Numb, G, X2PA, X3PA, FTA, Poss, Ast.Pos, Tov.Pos, Stl.G,
                Blk.G, TS, FTA.G, Three.G, Two.G, Three.Per, Two.Per, FTA.FGA, Three.FGA, OR.G, DR.G, 
                PF.G, adjTempo, adjOE, adjDE) %>%
  unique()

for(i in 1:nrow(CBB_career)){
  divisor <- ifelse(CBB_career$Numb[i] == 1, 1, ifelse(CBB_career$Numb[i] == 2, 3, 
                                                          ifelse(CBB_career$Numb[i] == 3, 6, 10)))
  print(i/nrow(CBB_career)) #tracking progress
  for(j in 6:27){
    CBB_career[i,j] <- CBB_career[i,j] / divisor
  }
}

#RSCI Draft
CBB_rsci <- CBB_rsci %>%
  dplyr::select(Player, Draft, RSCI)

CBB_rsci$Player <- gsub("\\(.*", "", CBB_rsci$Player)
CBB_rsci$Player <- gsub("(^\\s+)|(\\s+$)", "", CBB_rsci$Player)
CBB_rsci$RSCI <- as.numeric(gsub("T", "", CBB_rsci$RSCI))

#NBA Draft Data
draft <- multmerge("Documents/Analytics/NBA Draft Model 1_0/draft data") %>%
  dplyr::select(Player, Pos, Age, HT, WT, WS)

#function to turn height/wingspan string into inches
height <- function(hT){
  feet <- gsub("\\'.*", "", hT)
  inches <- gsub(".*'", "", hT)
  inches <- substr(inches, start = 1, stop = 2)
  inches <- gsub("\\\".*", "", inches)
  height.in <- as.numeric(feet)*12 + as.numeric(inches)
  return(height.in)
}

draft$HT <- height(draft$HT)
draft$WS <- height(draft$WS)

#Sorting positions
draft$Pos2 <- ifelse(draft$Pos == "PG/SG", "PG", 
                         ifelse(draft$Pos == "SF/PF", "Wing",ifelse(draft$Pos == "SG", "Wing",
                                                                        ifelse(draft$Pos == "SF", "Wing",ifelse(draft$Pos == "SG/SF", "Wing", 
                                                                                     ifelse(draft$Pos == "PG", "PG", "Big"))))))
#imputes missing wingspan using height
impute <- function(data, ht){
  df <- na.omit(data) %>% filter(HT == ht)
  return(median(df$WS))
}
#impute wingspan for draft data set
for(i in 1:nrow(draft)){
  if(is.na(draft$WS[i])){
    draft$WS[i] <- impute(draft, draft$HT[i])
  }
}

#Merge RSCI and Draft data
draft <- merge(draft, CBB_rsci, by = "Player", all.x = TRUE)
draft$RSCI <- ifelse(draft$RSCI <= 25, "Top25", ifelse(draft$RSCI <= 50, "Top50", ifelse(draft$RSCI <= 75, "Top75", ifelse(draft$RSCI <= 100, "Top100", "NotRanked"))))

#merge with college data and final clean
#divide into test, train, and prediction
#build model
#test & analyze model
#data set for 2018
#predict 2018 draft


##NBA Salary Data Cleaning
Per.Cap <- merge(Salaries, SalaryCap, by.y = "Year", by.x = "season_end", all.x = TRUE)
Per.Cap$Per.Cap <- Per.Cap$salary / Per.Cap$Salary.Cap

NBA <- Per.Cap %>%
  group_by(player) %>%
  #filter(Per.Cap == max(Per.Cap)) 
  dplyr::mutate( fifth = nth(season_end, 5, order_by = season_end, default = 0) ) %>%
  filter(season_end == fifth)

NBA <- NBA[!duplicated(NBA[,c(2,9)]),]
NBA <- NBA[,c(1:4, 7:9)]

#Merge NBA Salary and Draft/RSCI
NBA <- merge(NBA, draft, by.x = "player", by.y = "Player", all.x = TRUE)

NBA <- ddply(NBA, .(Pos), function(x) transform(x, percentile=ecdf(x$Per.Cap)(x$Per.Cap)))

##Build final data set
Final <- merge(NBA, CBB_career, by.x = "player", by.y = "Player") %>%
  filter(season_end > 2000 & !(is.na(Pos.x)))


