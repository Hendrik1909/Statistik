library(tidyr) # gather
library(stringr) # string bearbeitung
library(tigerstats) # rowperc
load("Beispieldatensatz.RData")

# sportarten in eine variable zusammenfassen:
y <- gather(x, key = "Sportart 1-3", value ="Sportart Gesamt", 6:8)

# haeufigkeiten aller sportarten:
sort(table(y$`Sportart Gesamt`), decreasing = TRUE)

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
z <- gather(x, key = "Sportart 1-3 (OF)", value = "Sportart (OF) Gesamt", 94:99)
sort(table(z$`Sportart (OF) Gesamt`), decreasing = TRUE)

# prozentual:
prop.table(table(z$`Sportart (OF) Gesamt`))
# gerundet
round(prop.table(table(z$`Sportart (OF) Gesamt`)), digits = 4) * 100

# jetzt nochmal trennen -> um tabllen nach ges und alter zu machen 
z$`Sportart Gesamt` <- str_remove(z$`Sportart (OF) Gesamt`, pattern = "\\ .*")
z$`OF Gesamt` <- str_extract(z$`Sportart (OF) Gesamt`, pattern = "\\(.*")

# tabelle nach SA und OF:
xtabs(~`OF Gesamt` + `Sportart Gesamt`, data = z)
# tabelle nach SA und OF -> in %
rowPerc(xtabs(~`OF Gesamt` + `Sportart Gesamt`, data = z))

# tabellen nach geschlecht 
xtabs(~ `OF Gesamt` + `Sportart Gesamt`+ Geschlecht, data = z)
# ODER einzeln (fuer rowperc notwendig -> für in % Angaben), z.B.:
xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w", ])
rowPerc(xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w", ]))
xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "m", ])
rowPerc(xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "m", ]))

# tabellen nach altersgruppe 
xtabs(~ `OF Gesamt` + `Sportart Gesamt`+ Altergruppe, data = z)
# z.B. für eine Altergruppe -> % Angaben
xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Altergruppe == "14-29", ])
rowPerc(xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Altergruppe == "14-29", ]))

# beides
xtabs(~ `OF Gesamt` + `Sportart Gesamt`+ Geschlecht + Altergruppe, data = z)
# ODER genau z.B. für w & 60+:
xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w" & z$Altergruppe == "60+", ])
#ODER für %-Angaben
rowPerc(xtabs(~ `OF Gesamt` + `Sportart Gesamt`, data = z[z$Geschlecht == "w" & z$Altergruppe == "60+", ]))
