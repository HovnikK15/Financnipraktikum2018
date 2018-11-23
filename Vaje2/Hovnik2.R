# FINANČNI PRAKTIKUM 
# 2. VAJE: KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM

# vzorec 1
library(actuar)
# 1a
vzorec <- scan("vzorec1.txt")

# nariši histogram 
histogram1 <- hist(vzorec, col = "light blue", xlab = "Visina odskodnine", 
                   xlim = c(0, 2), main="Histogram odskodnin")

#1b
parametri <- mde(vzorec, pexp, start = list(rate=1), measure = "CvM") 
ocena <- parametri$estimate[1]
ocena1 <- as.numeric(ocena)
razdalja <- parametri$distance[1]

#1c
histogram2 <-hist(vzorec, probability = TRUE, xlab = "Visina dohodnine", col = "light blue", 
                  xlim = c(0, 2), ylim = c(0, 2.5))
curve(dexp(x, ocena),add = TRUE, from = 0, to = 9, col = "blue", type = "l", lwd = 2)
legend("topright", legend = "eksponentna porazdelitev", col="blue", lwd = 2)



histogram3 <- plot(ecdf(vzorec), main = "Porazdelitvena funkcija odskodnin", 
                   ylab = "Porazdelitvena funkcija", xlab = "Visina odskodnine")
curve(pexp(x, ocena), add = TRUE, from = 0,to = 9, col = "blue", type = "l", lwd = 2)
legend(0.45, 0.6, legend = c("empiricna porazd.", "eksponentna porazd."), 
       col=c("black", "blue"), lwd = 2, box.lty=0)

#1d
velikost  <- 25
verjetnost <- 1/2

upanje_Yi <- 1 / ocena # 1 / lambda = 0.337
upanje_N <- velikost * verjetnost # n * p = 12.5

upanje_S <- upanje_Yi * upanje_N # = 4.22

varianca_Yi <- 1 / (ocena ^ 2) # 1 / lambda^2 = 0.114
varianca_N <- velikost * verjetnost * (1 - verjetnost) # n*p*(1-p) = 6.25

# Var(S) = Var(Yi) * E(N) + E(Yi)^2 * Var(N)
varianca_S <- varianca_Yi * upanje_N + (upanje_Yi)^2 * varianca_N # = 2.13

#2a
h <- 0.25 # korak
n <- 20 
diskretizacija <- discretize(pexp(x, ocena), from = 0, to = n * h,
                             step = h, method = "rounding")

#2b
graf_porazdelitve_Y <- plot(stepfun(seq(0, (n-1) * h, h), diffinv(diskretizacija)), 
                            do.points = FALSE, col = "orange", lwd = 2, xlab = "x", 
                            ylab = "Porazdelitvena funkcija", main = "Eksponentna porazdelitev")
curve(pexp(x,ocena),add=TRUE, from = 0,to = 9, col = "black")

#2c
h <- 0.25
n <- 100 
diskretizacija <- discretize(pexp(x,ocena),from=0, to=n*h, 
                             step = h, method ="rounding" )

porazdelitvena <- aggregateDist(method = "recursive",
                                model.freq = "binom",
                                model.sev = diskretizacija,
                                x.scale = h,
                                size = 25,
                                prob = 1/2,
                                maxit = 1000)

plot(porazdelitvena)

#2d
vrednosti <- knots(porazdelitvena)
verjetnosti <- diff(porazdelitvena)

# upanje_S_nova <- mean(porazdelitvena) # = 4.12164164145619 
# varianca_S_nova <- sum(( vrednosti * h - upanje_S_nova)^2 * verjetnosti) # = 9.69489882650343

S_upanje1 <- sum( vrednosti * verjetnosti) # = 4.12164164145619 
S_upanje_kvadrat1 <- sum( vrednosti^2 * verjetnosti) # = 19.2151719732542
S_varianca1 <- S_upanje_kvadrat1 - (S_upanje1)^2 # = 2.22724215266853

#3a
vrednosti_vse <- 10000
n <- 25
p <- 1/2

stevec = 1
S <- list()
vrednosti_N <- rbinom(vrednosti_vse, n, p)
for (i in vrednosti_N) {
  k =rexp(i,ocena)
  S[stevec] = sum(k)
  stevec = stevec + 1
}

#3b 
S1 <- as.numeric(S)
povprecje_S1 <- mean(S1) # = 4.23906809774774 
varianca_S1 <- var(S1) # = 2.14157694741955



#3c
graf1 <- plot(porazdelitvena)
graf2 <- plot(ecdf(S1), col = 'green', add = TRUE, lwd = 3)
legend('bottomright', legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'green'), lty = 1:1, bty = "n", lwd = 3)

