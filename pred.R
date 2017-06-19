source("func.r")
source("prep.r")

library(MASS)

#library(earth)
#library(caret)
preProcess(data[, 14:29], method = c("PCA"))

#mHome <- glm(FTHG ~ SFH * SAA * GFH * GAA * STFH * STAA , data, family="poisson")
#mAway <- glm(FTAG ~ SAH * SAH * GAH * GFA * STAH * STFA, data, family="poisson")
mHome <- glm.nb(FTHG ~ SFH * SAA * GFH * GAA * STFH * STAA , data)
mAway <- glm.nb(FTAG ~ SAH * SAH * GAH * GFA * STAH * STFA, data)
pHome <- predict(mHome, type = "response")
pAway <- predict(mAway, type = "response")

#home1 <- dpois(0:8,pHome[1])
#away1 <- dpois(0:8,pAway[1])
#predHG <- which.max(home1) - 1
#predAG <- which.max(away1) - 1

#oo <- outer(home1,away1)
#rownames(oo) <- 0:8
#colnames(oo) <- 0:8
#round(oo,digits=3)

data <- mutate(data, pGH = NA, pGA = NA)
for(i in 1:nrow(data)) {
  data$pGH[i] <- which.max(dpois(0:8,pHome[i])) - 1
  data$pGA[i] <- which.max(dpois(0:8,pAway[i])) - 1
}

ergebnis <- sum((data$pGH == data$FTHG) & (data$pGA == data$FTAG))/nrow(data)
differenz <- sum((data$pGH - data$pGA) == (data$FTHG - data$FTAG))/nrow(data)
data$pRes <- "H"
data[data$pGH < data$pGA, "pRes"] <- "A"
data[data$pGH == data$pGA, "pRes"] <- "D"
tendenz <- sum(data$pRes == data$FTR)/nrow(data)

# extent = 10, cutoff = 6, slope = 0.004
print("Fabo:")
print(paste0("Ergebnis: ", round(ergebnis,2)))
print(paste0("Differenz: ", round(ergebnis,2)))
print(paste0("Tendenz: ", round(tendenz,2)))

print("Kicktipp-Katharina:")
print("Ergebnis: 0,1")
print("Differenz: 0,15")
print("Tendenz: 0,41")
