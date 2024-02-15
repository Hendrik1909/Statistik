library("readxl")
dat <- read_excel("Beispiel_Datensatz_Repräsentativbefragung_NEU.xlsx", sheet = 2)


simulateData <- function(rohdaten, n = 100, seednr = 161){
  
  # dataframe erstellen
  fragen         <- colnames(rohdaten)
  anzahl_fragen  <- ncol(rohdaten)
  data           <- as.data.frame(matrix(ncol = anzahl_fragen, nrow = n))
  colnames(data) <- fragen
  
  
  ### fragen beantworten
  set.seed(seednr)
  data$Geschlecht  <- sample(c("m", "w"), replace = TRUE, size = n)
  data$Altergruppe <- sample(c("14-29", "30-44", "45-59", "60+"), replace = TRUE, size = n)
  
  
  ## Alter an Altersgruppe (ag) anpassen:
  i.ag14             <- which(data$Altergruppe == "14-29")
  n.ag14             <- length(i.ag14)
  data$Alter[i.ag14] <- sample(14:29, replace = TRUE, size = n.ag14)
  
  i.ag30             <- which(data$Altergruppe == "30-44")
  n.ag30             <- length(i.ag30)
  data$Alter[i.ag30] <- sample(30:44, replace = TRUE, size = n.ag30)  
  
  i.ag45             <- which(data$Altergruppe == "45-59")
  n.ag45             <- length(i.ag45)
  data$Alter[i.ag45] <- sample(45:59, replace = TRUE, size = n.ag45)  
  
  # hier: maximal alter 75, veränderbar!
  i.ag60             <- which(data$Altergruppe == "60+")
  n.ag60             <- length(i.ag60)
  data$Alter[i.ag60] <- sample(60:75, replace = TRUE, size = n.ag60)
  
  data$Abschluss <- sample(c("Studium", "Abi", "mittl. Abschluss", "Hauptschule", "kein Abschluss"), replace = TRUE, size = n)
  data$`Sportliche Aktivität in den letzten 12 Monaten` <- "Ja"
  
  
  ## jetzt sportarten (sa), aber unique
  sportarten        <- c("Fußball", "Handball", "Schneesport", "Yoga", "Eishockey", "Tennis", "Biken", "Joggen")
  data$`Sportart 1` <- sample(sportarten, replace = TRUE, size = n, prob = c(.2, .15, .08, .1, .11, .1, .125, .135))
  
  data$`Sportart 2`         <- sample(c(sportarten, NA), replace = TRUE, size = n, prob = c(.18, .13, .06, .08, .09, .08, .105, .115, .16))
  i.sa12                    <- which(data$`Sportart 1` == data$`Sportart 2`)
  data$`Sportart 2`[i.sa12] <- NA
  
  data$`Sportart 3` <- sample(c(sportarten, NA), replace = TRUE, size = n, prob = c(.18, .13, .06, .08, .09, .08, .105, .115, .16))
  # NA wenn gleiche Sportart wie zuvor:
  i.sa13 <- which(data$`Sportart 1` == data$`Sportart 3`)
  i.sa23 <- which(data$`Sportart 2` == data$`Sportart 3`)
  # NA wenn 2. Sportart schon NA
  i.sa2NA                                       <- which(is.na(data$`Sportart 2`))
  data$`Sportart 3`[c(i.sa13, i.sa23, i.sa2NA)] <- NA
  
  
  ## jetzt organisationsformen (of) :
  of <- c("Verein", "Betriebs-; Schuls-; Hochschulsport", "Sonstiges", "selbst/nicht organisiert")
  # Sportart 1
  data$`Organisationsform A - Sportart 1`          <- sample(of, replace = TRUE, size = n, prob = c(.4, .15, .2, .25))
  data$`Organisationsform B - Sportart 1`          <- sample(c(of, NA), replace = TRUE, size = n)
  i.ofAB1                                          <- which(data$`Organisationsform A - Sportart 1` == data$`Organisationsform B - Sportart 1`)
  data$`Organisationsform B - Sportart 1`[i.ofAB1] <- NA
  
  # Sportart 2
  data$`Organisationsform A - Sportart 2`          <- sample(of, replace = TRUE, size = n, prob = c(.4, .15, .2, .25))
  data$`Organisationsform B - Sportart 2`          <- sample(c(of, NA), replace = TRUE, size = n)
  i.ofAB2                                          <- which(data$`Organisationsform A - Sportart 2` == data$`Organisationsform B - Sportart 2`)
  data$`Organisationsform B - Sportart 2`[i.ofAB2] <- NA
  # aber entferne, falls sportart 2 gar nicht vorhanden:
  data$`Organisationsform A - Sportart 2`[i.sa2NA] <- NA
  data$`Organisationsform B - Sportart 2`[i.sa2NA] <- NA
  
  # Sportart 3
  data$`Organisationsform A - Sportart 3`          <- sample(of, replace = TRUE, size = n, prob = c(.4, .15, .2, .25))
  data$`Organisationsform B - Sportart 3`          <- sample(c(of, NA), replace = TRUE, size = n)
  i.ofAB3                                          <- which(data$`Organisationsform A - Sportart 3` == data$`Organisationsform B - Sportart 3`)
  data$`Organisationsform B - Sportart 3`[i.ofAB3] <- NA
  # aber entferne, falls sportart 3 gar nicht vorhanden:
  i.sa3NA                                          <- which(is.na(data$`Sportart 3`) == TRUE)
  data$`Organisationsform A - Sportart 3`[i.sa3NA] <- NA
  data$`Organisationsform B - Sportart 3`[i.sa3NA] <- NA
  
  
  ## jetzt regelmäßigkeit (spalten 15 - 20 (sa1A, sa1B, sa2A,...))
  for(i in 15:20) data[, i] <- sample(1:20, replace = TRUE, size = n)
  
  # sa1A immer vorhanden, rest maybe nicht:
  i.sa1BNA <- which(is.na(data$`Organisationsform B - Sportart 1`))
  i.sa2ANA <- which(is.na(data$`Organisationsform A - Sportart 2`))
  i.sa2BNA <- which(is.na(data$`Organisationsform B - Sportart 2`))
  i.sa3ANA <- which(is.na(data$`Organisationsform A - Sportart 3`))
  i.sa3BNA <- which(is.na(data$`Organisationsform B - Sportart 3`))
  
  data[i.sa1BNA, 16] <- NA
  data[i.sa2ANA, 17] <- NA
  data[i.sa2BNA, 18] <- NA
  data[i.sa3ANA, 19] <- NA
  data[i.sa3BNA, 20] <- NA
  
  
  # jetzt Anzahl der verletzungen, gesamt (maximal 6!):
  data[, 21] <- sample(0:6, replace = TRUE, size = n, prob=c(0.3, 0.3, 0.2, 0.1, 0.05, 0.03, 0.02))
  verletzungsanzahl <- data[, 21]
  
  # erstmal alle einzelnen Anzahlen auf 0 setzen und dann schritt für schritt ergänzen:
  data[, 22:27] <- 0
  
  # restliche "antworten" vorbereiten:
  monate <- 1:12
  behandelt <- c("Ja", "Nein")
  koerperregion <- c("Kopf", "Obere Extremitäten", "Rumpf/Torso", "Untere Extremitäten")
  genauer_kopf  <- c("Kopf", "Hals")
  genauer_oben  <- c("Schulter", "Oberarm", "Ellenbogen", "Unterarm", "Handgelenk", "Hand")
  genauer_rumpf <- c("Brust", "Bauch", "Rücken", "Innere Organe")
  genauer_unten <- c("Leiste", "Hüfte", "Gesäß", "Oberschenkel", "Knie", "Unterschenkel", "Knöchel", "Fuß")
  art <- as.factor(1:14) # codierung siehe word dokument
  unfaehig <- c("Ja", "Nein", NA)
  studierunf <- 1:14
  arbeitsunf <- 1:28
  sportpause <- 1:31
  # jetzt jede verletzung pro person einzeln durchgehen:
  
  for(i in 1:n){ # Personenebene
    if(verletzungsanzahl[i] != 0){
      for(j in 1:verletzungsanzahl[i]){ # Verletzungsebene
        # wo (sportart/organisationsform) landet die j-te Verletzung?
        
        k <- sample(22:27, size = 1)
        if(k %in% 22:23) data[i, 18 + 11*j] <- data$`Sportart 1`[i]
        if(k %in% 24:25) data[i, 18 + 11*j] <- data$`Sportart 2`[i]
        if(k %in% 26:27) data[i, 18 + 11*j] <- data$`Sportart 3`[i]
        
        if(k == 22) data[i, 17 + 11*j] <- data$`Organisationsform A - Sportart 1`[i]
        if(k == 23) data[i, 17 + 11*j] <- data$`Organisationsform B - Sportart 1`[i]
        if(k == 24) data[i, 17 + 11*j] <- data$`Organisationsform A - Sportart 2`[i]
        if(k == 25) data[i, 17 + 11*j] <- data$`Organisationsform B - Sportart 2`[i]
        if(k == 26) data[i, 17 + 11*j] <- data$`Organisationsform A - Sportart 3`[i]
        if(k == 27) data[i, 17 + 11*j] <- data$`Organisationsform B - Sportart 3`[i]
        
        # wenn die verletzung in NA landet, packe sie in SA 1 OF A (einfachste loesung):
        if(is.na(data[i, 17 + 11*j]) == TRUE){
          data[i, 17 + 11*j] <- data$`Organisationsform A - Sportart 1`[i]
          data[i, 18 + 11*j] <- data$`Sportart 1`[i]
          data[i, 22] <- data[i, 22] + 1
        }else{
          data[i, k] <- data[i, k] + 1
        }
        
        
        data[i, 19 + 11*j] <- sample(monate, size = 1, prob = c(0.2, 0.18, 0.16, 0.12, 0.1, 0.08, 0.06, 0.04, 0.02, 0.02, 0.01, 0.01)) # vor wievielen Monaten?
        data[i, 20 + 11*j] <- sample(behandelt, size = 1) # Behandelt?
        data[i, 21 + 11*j] <- sample(koerperregion, size = 1) # wo?
        
        # wo genau?
        if(data[i, 21 + 11*j] == "Kopf") data[i, 22 + 11*j]                <- sample(genauer_kopf, size = 1) 
        if(data[i, 21 + 11*j] == "Obere Extremitäten") data[i, 22 + 11*j]  <- sample(genauer_oben, size = 1) 
        if(data[i, 21 + 11*j] == "Rumpf/Torso") data[i, 22 + 11*j]         <- sample(genauer_rumpf, size = 1) 
        if(data[i, 21 + 11*j] == "Untere Extremitäten") data[i, 22 + 11*j] <- sample(genauer_unten, size = 1) 
        
        # art
        data[i, 23 + 11*j] <- sample(art, size = 1)
        
        # unfähigkeit
        
        data[i, 24 + 11*j] <- sample(unfaehig, size = 1)
        if(is.na(data[i, 24 + 11*j]) == FALSE){
          if(data[i, 24 + 11*j] == "Ja"){
            data[i, 25 + 11*j] <- sample(arbeitsunf, size = 1)
            data[i, 26 + 11*j] <- sample(studierunf, size = 1)
          }
          
          if(data[i, 24 + 11*j] == "Nein"){
            data[i, 25 + 11*j] <- 0
            data[i, 26 + 11*j] <- 0
          }
        }else{
          data[i, 25 + 11*j] <- NA
          data[i, 26 + 11*j] <- NA
        }
        data[i, 27 + 11*j] <- sample(sportpause, size = 1)
        
      }
    }
    else{
      data[i, 21:27] <- 0
      data[i, 28:93] <- NA
    }
  }
  
  
  # nochmal die Nullen in NAs durch NAs ersetzen:
  data[i.sa1BNA, 23] <- NA
  data[i.sa2ANA, 24] <- NA
  data[i.sa2BNA, 25] <- NA
  data[i.sa3ANA, 26] <- NA
  data[i.sa3BNA, 27] <- NA
  
  return(data)  
}

#Funktion in Dataframe umwandeln/abspeichern
x <- simulateData(dat, n=1000)

#als Datensatz abspeichern 
save(x, file = "Beispieldatensatz.RData")
