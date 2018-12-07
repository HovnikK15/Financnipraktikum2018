require(readr)

#1.naloga
#a)
platina <- read_csv("Vaje4/Platina.csv", locale=locale(encoding="cp1250"))
platina <- platina[c(126:1),c(5)]
platina$Close <- as.numeric(gsub("\\$", "", platina$Close))

#b)
casovna <- ts(platina)
graf_platina <- ts.plot(casovna, xlab = "Leto", ylab = "Vrednost platine v evrih",
                        main = "Graf vrednosti platine")

#2.naloga
#a)
G <- function(vrsta, k) {
  dolzina <- length(vrsta)
  glajenje_vrednosti <- c()
  for (i in 1:(dolzina-k)) {
    glajenje_vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zglajena_vrsta <- ts(glajenje_vrednosti)
  return(zglajena_vrsta)
}

#b)
zglajena_vrsta_8 <- G(casovna, 8)
dolzina <- length(casovna)
napoved8 <- sum(casovna[(dolzina-8+1):dolzina])/8

#c)
graf2 <- ts.plot(casovna, zglajena_vrsta_8, xlab = "Leto",
                 ylab = "Vrednost platine", lwd = 2:1, col = 1:10)

#d)
SKN <- function(vrsta, zglajena_vrsta, k) {
  T <- length(vrsta)
  delna_vsota <- 0
  for (i in (k+1): T) {
    delna_vsota <- delna_vsota + (vrsta[i] - zglajena_vrsta[i-k])^2
  }
  napaka <- (1/(T-k))*delna_vsota
  return(napaka)
}

Napaka8 <- SKN(casovna, zglajena_vrsta_8, 8)


#e)
zglajena_vrsta_16 <- G(casovna, 16)
napoved16 <- sum(casovna[(dolzina-16+1):dolzina])/16
Napaka16 <- SKN(casovna, zglajena_vrsta_16, 16)

zglajena_vrsta_24 <- G(casovna, 24)
napoved24 <- sum(casovna[(dolzina-24+1):dolzina])/24
Napaka24 <- SKN(casovna, zglajena_vrsta_24, 24)


par(mfrow = (c(2,2)))
graf2 <- ts.plot(casovna, zglajena_vrsta_8, xlab = "Leto", ylab = "Vrednost platine",
                 main = "Drseče povprečje reda 8", lwd = 2:1, col = 1:10)
graf3 <- ts.plot(casovna, zglajena_vrsta_16,xlab = "Leto", ylab = "Vrednost platine",
                 main = "Drseče povprečje reda 16", lwd = 2:1, col = 1:10)
graf4 <- ts.plot(casovna, zglajena_vrsta_24,xlab = "Leto", ylab = "Vrednost platine",
                 main = "Drseče povprečje reda 24", lwd = 2:1, col = 1:10)
par(mfrow = (c(1,1)))


#3.naloga
#a)
EG <- function(vrsta, alpha) {
  glajena_vrsta <- c(vrsta[1])
  dolzina <- length(vrsta)
  for (i in 2:dolzina) {
    glajena_vrsta[i] <- alpha*vrsta[i] + (1-alpha)*glajena_vrsta[i-1]
  }
  return(ts(glajena_vrsta))
}
#b)
alfa = 0.2
zglajena_vrsta_alfa <- EG(casovna, alfa)

graf6 <- ts.plot(casovna, zglajena_vrsta_alfa,xlab = "Leto",
                 ylab = "Vrednost platine",main = "Eksponentno glajenje",
                 lwd = 2:1, col = 1:10)

#c)
SKN_o <- function(vrsta, alfa) {
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alfa)
  for (i in 1:(dolzina-1)) {
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina -1))
}

opt_alfa <- optimize(SKN_o, c(0,1), vrsta = casovna)

#d)
zglajen_alfa_o <- EG(casovna, opt_alfa$minimum)

graf7 <- ts.plot(casovna, zglajen_alfa_o, ylab = "EUR", main = "Eksponentsno glajenje,
                 minimalen MSE",lwd = 3:1, col = 10:1)

