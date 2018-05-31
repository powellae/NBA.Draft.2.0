#---- NBA Draft Model 2.0 ----
#Last Updated: April 26, 2018
#Author: Alexander Powell
#Data Processing -- Final Data set: "Final"
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
CBB_rsci <- read.csv("Documents/Analytics/NBA Draft Model 2_0/br_rsci2.csv") #year-by-year rsci rankings through 2017


##NBA Salary Data
SalaryCap <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_SalaryCap_85_to_18.csv") #NBA salary cap history
Salaries <- read.csv("Documents/Analytics/NBA Draft Model 1_0/NBA_Salaries_90_to_18.csv") #player contracts 1992-present (with gaps)

#Data Cleaning----

##CBB Data Cleaning
#Merging Kenpom Data (changing team names)
kenpom <- multmerge("Documents/Analytics/NBA Draft Model 1_0/kenpom")
kenpom <- kenpom[,c("Season", "Team", "AdjTempo", "AdjOE", "AdjDE")]
for(i in 1:nrow(kenpom)){
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "SMU", "Southern Methodist", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Miami FL", "Miami (FL)", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "UNLV", "Nevada-Las Vegas", kenpom$Team[i])
  #kenpom$Team[i] <- ifelse(kenpom$Team[i] == "North Carolina", "UNC", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Saint Joseph's", "St Joseph's", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Saint Mary's", "St Mary's", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Oklahoma St.", "Oklahoma State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Arizona St.", "Arizona State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Florida St.", "Florida State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Michigan St.", "Michigan State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Ohio St.", "Ohio State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Colorado St.", "Colorado State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "North Carolina St.", "North Carolina State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Eastern Washington", "Eastern Wash.", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Kansas St.", "Kansas State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Fresno St.", "Fresno State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Iowa St.", "Iowa State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Morehead St.", "Morehead State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "San Diego St.", "San Diego State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Washington St.", "Washington State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Murray St.", "Murray State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Norfolk St.", "Norfolk State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Weber St.", "Weber State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Long Beach St.", "Long Beach State", kenpom$Team[i])
  #kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Western Carolina", "WCU", kenpom$Team[i])
  #kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Western Kentucky", "WKU", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Missouri St.", "Missouri State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Oregon St.", "Oregon State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Georgia St.", "Georgia State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Boise St.", "Boise State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Montana St.", "Montana State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Fort Wayne", "IPFW", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Penn St.", "Penn State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "Wichita St.", "Wichita State", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "USC", "Southern California", kenpom$Team[i])
  kenpom$Team[i] <- ifelse(kenpom$Team[i] == "South Dakota St.", "South Dakota State", kenpom$Team[i])
  
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
CBB_yby <- within(CBB_yby, Pos[Player == 'Mikal Bridges' & School == 'Villanova'] <- 'F')


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
CBB_rsci <- read.csv("Documents/Analytics/NBA Draft Model 2_0/br_rsci2.csv") #year-by-year rsci rankings through 2017
CBB_rsci <- CBB_rsci %>%
  dplyr::select(Player, Draft, RSCI)

#CBB_rsci$Player <- as.character(CBB_rsci$Player)
#CBB_rsci$Player <- gsub("\\(.*", "", CBB_rsci$Player)
#CBB_rsci$Player <- gsub("(^\\s+)|(\\s+$)", "", CBB_rsci$Player)
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
  filter(season_end > 2000 & !(is.na(Pos.x))) %>%
  unique()

Final <- Final[!(Final$player=="David Lee" & Final$School == "Jacksonville"),]
Final <- Final[!(Final$player=="DeMarre Carroll" & Final$School == "Vanderbilt"),]
Final <- Final[!(Final$player=="Derrick Williams" & Final$School == "Richmond"),]
Final <- Final[!(Final$player=="Devin Harris" & Final$School == "Jacksonville"),]
Final <- Final[!(Final$player=="Ekpe Udoh" & Final$School == "Michigan"),]
Final <- Final[!(Final$player=="David Lee" & Final$School == "Jacksonville"),]
Final <- Final[!(Final$player=="Elliot Williams" & Final$School == "Duke"),]
Final <- Final[!(Final$player=="Emeka Okafor" & Final$School == "Western Illinois"),]
Final <- Final[!(Final$player=="Isaiah Thomas" & Final$School == "Northern Arizona"),]
Final <- Final[!(Final$player=="James Johnson" & Final$School == "Liberty"),]
Final <- Final[!(Final$player=="James Johnson" & Final$School == "Louisiana Tech"),]
Final <- Final[!(Final$player=="James Johnson" & Final$School == "Quinnipiac"),]
Final <- Final[!(Final$player=="Jason Thompson" & Final$School == "Old Dominion"),]
Final <- Final[!(Final$player=="James Johnson" & Final$School == "Western Carolina"),]
Final <- Final[!(Final$player=="James Johnson" & Final$School == "San Diego State"),]
Final <- Final[!(Final$player=="Jordan Crawford" & Final$School == "Indiana"),]
Final <- Final[!(Final$player=="Jordan Hamilton" & Final$School == "Lehigh"),]
Final <- Final[!(Final$player=="Jordan Hill" & Final$School == "Seattle"),]
Final <- Final[!(Final$player=="Jordan Hill" & Final$School == "Wisconsin"),]
Final <- Final[!(Final$player=="Marcus Thornton"),]
Final <- Final[!(Final$player=="Mike Scott" & Final$School == "Eastern Kentucky"),]
Final <- Final[!(Final$player=="Mike Scott" & Final$School == "Fairleigh Dickinson"),]
Final <- Final[!(Final$player=="Mike Scott" & Final$School == "Idaho"),]
Final <- Final[!(Final$player=="Reggie Jackson" & Final$School == "Duquesne"),]
Final <- Final[!(Final$player=="Reggie Jackson" & Final$School == "Murray State"),]
Final <- Final[!(Final$player=="Ryan Anderson"),]
Final <- Final[!(Final$player=="James Johnson" & Final$School == "Liberty"),]
Final <- Final[!(Final$player=="Ryan Gomes" & Final$School == "Mount St. Mary's"),]
Final <- Final[!(Final$player=="Tristan Thompson" & Final$School == "North Texas"),]
Final <- Final[!(Final$player=="Anthony Davis" & Final$School == "Austin Peay"),]
Final <- Final[!(Final$player=="James Johnson" & Final$RSCI == "Top75"),]
Final <- Final[!(Final$player=="Markieff Morris" & Final$RSCI == "Top100"),]
Final <- Final[!(Final$player=="Anthony Davis" & Final$School == "Iowa State"),]
Final <- Final[!(Final$player=="James Anderson" & Final$School == "Lehigh"),]
Final <- Final[!(Final$player=="Kevin Martin" & Final$School == "Wagner"),]
Final <- Final[!(Final$player=="Toney Douglas" & Final$School == "Auburn"),] #transfer
Final <- Final[!(Final$player=="Marvin Williams" & Final$School == "Lipscomb"),]
Final <- Final[!(Final$player=="Marvin Williams" & Final$School == "Murrary State"),]

Final$RSCI <- ifelse(is.na(Final$RSCI), "NR", Final$RSCI)
Final$Draft <- NULL
Final <- Final %>% unique()
Final$RSCI <- as.factor(Final$RSCI)

write.csv(Final, "Documents/Analytics/NBA Draft Model 2_0/Final.csv")
#Prospects----
Prospects <- read.csv("Documents/Analytics/NBA Draft Model 2_0/DraftProspect.csv")
Prospects <- merge(Prospects, CBB_career, by.x = c("PLAYER", "TEAM"), by.y = c("Player", "School"), all.x = TRUE) %>%
  filter(YEAR != "International")

Prospects$Pos2 <- ifelse(Prospects$POS == "PG/SG", "PG", 
                     ifelse(Prospects$POS == "SF/PF", "Wing",ifelse(Prospects$POS == "SG", "Wing",
                                                                ifelse(Prospects$POS == "SF", "Wing",ifelse(Prospects$POS == "SG/SF", "Wing", 
                                                                                                        ifelse(Prospects$POS == "PG", "PG", "Big"))))))
Prospects$POS <- NULL
Prospects$Pos <- NULL

Prospects$HT <- as.numeric(gsub("\\/.*", "", Prospects$HT))*12 + as.numeric(gsub("*.\\/", "", Prospects$HT))
Prospects <- merge(Prospects, CBB_rsci[,c(1,3)], by.x = "PLAYER", by.y = "Player", all.x = TRUE)
Prospects$RSCI <- ifelse(Prospects$RSCI <= 25, "Top25", ifelse(Prospects$RSCI <= 50, "Top50", ifelse(Prospects$RSCI <= 75, "Top75", ifelse(Prospects$RSCI <= 100, "Top100", "NR"))))
Prospects$RSCI <- ifelse(is.na(Prospects$RSCI), "NR", Prospects$RSCI)
Prospects$RSCI <- as.facotr(Prospects$RSCI)

write.csv(Prospects, "Documents/Analytics/NBA Draft Model 2_0/Prospects.csv")
