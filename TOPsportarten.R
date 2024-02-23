library(tidyr) # gather
library(stringr) # string bearbeitung
library(tigerstats) # rowperc
load("Beispieldatensatz.RData") # erstellt mit: Erstellung Beispieldatensatz.R

##### NEU: durchschnittl. anzahl sportarten 
mean(rowSums(!is.na(x[, 6:8])))


# sportarten in eine variable zusammenfassen:
y <- gather(x, key = "Sportart 1-3", value ="Sportart Gesamt", 6:8)

# haeufigkeiten aller sportarten:
sort(table(y$`Sportart Gesamt`), decreasing = TRUE)

###### NEU: Hauptsportart:
y$Expositionszeit_SA[y$`Sportart 1-3` == "Sportart 1"] <- rowSums(y[y$`Sportart 1-3` == "Sportart 1", 12:13], na.rm = TRUE)
y$Expositionszeit_SA[y$`Sportart 1-3` == "Sportart 2"] <- rowSums(y[y$`Sportart 1-3` == "Sportart 2", 14:15], na.rm = TRUE)
y$Expositionszeit_SA[y$`Sportart 1-3` == "Sportart 3"] <- rowSums(y[y$`Sportart 1-3` == "Sportart 3", 16:17], na.rm = TRUE)

hauptSA <- NULL
zweitSA <- NULL
drittSA <- NULL
for(i in 1:nrow(x)){
  hauptSA[i] <- sort(y$Expositionszeit_SA[y$ID == i], decreasing = TRUE)[1]
  zweitSA[i] <- sort(y$Expositionszeit_SA[y$ID == i], decreasing = TRUE)[2]
  drittSA[i] <- sort(y$Expositionszeit_SA[y$ID == i], decreasing = TRUE)[3]
}
SAs <- data.frame(hauptSA, zweitSA, drittSA)
rm(hauptSA, zweitSA, drittSA)# aufraeumen
colSums(SAs)
colMeans(SAs)
rowPerc(colMeans(SAs))


#### jetzt gekoppelt mit organisationsform. benutze wieder ursprungsdatensatz x:
x$SA1OFA <- paste0(x$`Sportart 1`, " (", x$`Organisationsform A - Sportart 1`, ")")
x$SA1OFB <- paste0(x$`Sportart 1`, " (", x$`Organisationsform B - Sportart 1`, ")")

# NAs einsetzen, da sonst z.B. Handball (NA) moeglich:
x$SA1OFB[which(is.na(x$`Organisationsform B - Sportart 1`))] <- NA

# alles fuer den rest wiederholen:
x$SA2OFA <- paste0(x$`Sportart 2`, " (", x$`Organisationsform A - Sportart 2`, ")")
x$SA2OFB <- paste0(x$`Sportart 2`, " (", x$`Organisationsform B - Sportart 2`, ")")
x$SA3OFA <- paste0(x$`Sportart 3`, " (", x$`Organisationsform A - Sportart 3`, ")")
x$SA3OFB <- paste0(x$`Sportart 3`, " (", x$`Organisationsform B - Sportart 3`, ")")

x$SA2OFA[which(is.na(x$`Organisationsform A - Sportart 2`))] <- NA
x$SA2OFB[which(is.na(x$`Organisationsform B - Sportart 2`))] <- NA
x$SA3OFA[which(is.na(x$`Organisationsform A - Sportart 3`))] <- NA
x$SA3OFB[which(is.na(x$`Organisationsform B - Sportart 3`))] <- NA

# mit gather zusammenfassen:
z <- gather(x, key = "Sportart 1-3 (OF)", value = "Sportart (OF) Gesamt", 95:100)
sort(table(z$`Sportart (OF) Gesamt`), decreasing = TRUE)

# prozentual:
prop.table(table(z$`Sportart (OF) Gesamt`))
# gerundet
round(prop.table(table(z$`Sportart (OF) Gesamt`)), digits = 4) * 100

# jetzt nochmal trennen
z$`Sportart Gesamt` <- str_remove(z$`Sportart (OF) Gesamt`, pattern = "\\ .*")
z$`OF Gesamt` <- str_extract(z$`Sportart (OF) Gesamt`, pattern = "\\(.*")

###### NEU Vereinszahl######
z$Vereinszahl <- NULL
z$Expositionszeit <- NULL

for(i in 1:nrow(x)){
  # vereinszahl:
  z$Vereinszahl[z$ID == i] <- sum(z$`OF Gesamt`[z$ID == i] == "(Verein)", na.rm = TRUE)
  # expositionszeit:
  z$Expositionszeit[z$ID == i] <- NA
  z$Expositionszeit[z$ID == i][1] <- unique(z[z$ID == i, 15])
  z$Expositionszeit[z$ID == i][2] <- unique(z[z$ID == i, 16])
  z$Expositionszeit[z$ID == i][3] <- unique(z[z$ID == i, 17])
  z$Expositionszeit[z$ID == i][4] <- unique(z[z$ID == i, 18])
  z$Expositionszeit[z$ID == i][5] <- unique(z[z$ID == i, 19])
  z$Expositionszeit[z$ID == i][6] <- unique(z[z$ID == i, 20])
}
z$Vereinszahl[unique(z$ID)]
prop.table(table(z$Vereinszahl[unique(z$ID)]))
prop.table(table(z$Vereinszahl[unique(z$ID)])[-1])


# tabelle nach SA und OF:
xtabs(~`OF Gesamt` + `Sportart Gesamt`, data = z)
rowPerc(xtabs(~`OF Gesamt` + `Sportart Gesamt`, data = z)) 

# tabellen nach geschlecht 
xtabs(~ `OF Gesamt` + `Sportart Gesamt`+ Geschlecht, data = z)
# ODER einzeln (fuer rowperc notwendig), z.B.:
xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w", ])
rowPerc(xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w", ]))

# tabellen nach altersgruppe 
xtabs(~ `OF Gesamt` + `Sportart Gesamt`+ Altergruppe, data = z)

# beides
xtabs(~ `OF Gesamt` + `Sportart Gesamt`+ Geschlecht + Altergruppe, data = z)
# ODER z.B.:
xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w" & z$Altergruppe == "60+", ])

#### NEU Expositionszeiten 
xtabs(Expositionszeit ~ `Sportart Gesamt`, data = z)
# auf alle Sportler:
xtabs(Expositionszeit ~ `Sportart Gesamt`, data = z) / nrow(x)
# in der jeweiligen sportart:
aggregate(Expositionszeit ~ `Sportart Gesamt`, data = z, FUN = mean, na.rm = TRUE) 
# alle moeglichen gruppen (rest Hausaufgabe!):
aggregate(Expositionszeit ~ Geschlecht, data = z, FUN = mean, na.rm = TRUE) 
aggregate(Expositionszeit ~ Altergruppe, data = z, FUN = mean, na.rm = TRUE) 
aggregate(Expositionszeit ~ `OF Gesamt`, data = z, FUN = mean, na.rm = TRUE)
aggregate(Expositionszeit ~ `OF Gesamt` + `Sportart Gesamt` + Geschlecht + Altergruppe, data = z, FUN = mean, na.rm = TRUE)
