##### vorher ausfuehren: 1. Erstellung_Beispieldatensatz.R 
#####                    2. Expositionszeiten.R
 
library(lme4) # gemischte modelle, glmer
library(MASS) # rnegbin

# weil kein datensatz gebaut werden soll, machen wirs einfach:
z$verletzt_bullshit <- sample(0:1, size = 6000, replace = TRUE) 

logreg <- glmer(data=z, verletzt_bullshit ~ Geschlecht + Alter  + (1|ID), 
                family = binomial)

# bei konvergenzproblemen: control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5))
summary(logreg)

z$verletzungen_bullshit <- rpois(6000, lambda = 0.5)
poireg <- glmer(data=z, verletzungen_bullshit ~ Geschlecht + Alter + `Sportart Gesamt`  + (1|ID), family = poisson)
summary(poireg)

z$verletzungen_bullshit2 <- rnegbin(6000, mu = 2, theta = 1000)
negbinreg <- glmer.nb(data=z, verletzungen_bullshit2 ~ Geschlecht + Alter + `Sportart Gesamt`  + (1|ID))
summary(negbinreg)

# fuer sportpause: quasi-stetig, andere methoden evtl. besser. Aber: Normalverteilung an sich nicht moeglich,
# da <0 nicht moeglich
