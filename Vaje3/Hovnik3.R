#3. Vaje FP
library(dplyr)
library(combinat)
library(Rlab)
S0 = 50
u = 1.05
d = 0.95
U = 5
R = 0.03 
T = 3
S_0 = c(50.00, 50.00, 50.00, 50.00, 50.00)
S_1 = c(52.50, 52.50, 47.50, 47.50, 52.50)
S_2 = c(49.88, 55.12, 49.88, 45.12, 49.88)
S_3 = c(52.37, 57.88, 47.38, 47.38, 52.37)
S_4 = c(49.75, 60.78, 45.01, 49.75, 54.99)
S_5 = c(52.24, 63.81, 42.76, 52.24, 57.74)

izplacilo_X = c(0,0,0,0,0) 
izplacilo_Y = c(0,0,0,0,0) 

tabela1 <- data.frame(S_0, S_1, S_2, S_3, S_4, S_5, izplacilo_X, izplacilo_Y)

for (i in 1:5){
  l = max( max(tabela1[i,(T+1):(U+1)])  -   max(tabela1[i, 1:T]), 0) 
  tabela1$izplacilo_X[i] <- l
}
for (i in 1:5){
  k = max( min(tabela1[i,(T+1):(U+1)])  -   min(tabela1[i, 1:T]), 0) 
  tabela1$izplacilo_Y[i] <- k
}

#1b

#funkcija izplačilo sprejme parameter vrsta tj. vektor plačil, recimo vrstica v tabeli 1, 
#ter parameter call ali put tj. a jo izvršimo ali ne
izplacilo <- function(vrsta, T, type = c("call", "put") ) {
  if (type == "call") {
    return( max( max(vrsta[(T+1):length(vrsta)])  -   max(vrsta[1:T]), 0))}
  else {
    return( max( min(vrsta[(T+1):length(vrsta)])  -   min(vrsta[1:T]), 0) )}
}

#2a
binomski <- function(S0,u,d,U,R,T,type){
  q = (1+R-d)/(u-d)
  razpleti <- hcube(rep(2,U), translation = -1) #drevo stanj 1 pomeni, da gremo navzgor, 0 navzdol
  razpleti_1 <- d**(1-razpleti) * u**(razpleti)
  k <- rowSums(razpleti) #vektor, ki za vsako vrstico pove kolikokrat je up
  vektor_verjetnosti_koncnih_stanj <- q^k *(1-q)^(U-k)
  
  razpleti_1 <- t(apply( razpleti_1, 1, cumprod))
  vrednosti <- cbind(S0, S0*razpleti_1)
  
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x,T,type))
  E <- sum(izplacila * vektor_verjetnosti_koncnih_stanj)
  
  return (E/(1+R)^U)
}

#2b

monte <- function(S0, u, d, U, R, T, type, N){
  q = (1+R-d)/(u-d)
  stanja <- matrix(rbinom(U*N,1,q),N,U) 
  stanja_1 <- d**(1-stanja) * u**(stanja)
  
  k <- rowSums(stanja) #vektor, ki za vsako vrstico pove kolikokrat je up
  vektor_verjetnosti_koncnih_stanj <- q^k *(1-q)^(U-k)
  
  stanja_1 <- t(apply(stanja_1, 1, cumprod))
  vrednosti <- cbind(S0, S0*stanja_1)
  
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x,T,type))
  E= sum(izplacila)/ length(izplacila)
  return (E/(1+R)^U)
  
}

#simuliranje vrednosti
monte(50,1.05,0.9,10,0.03,5,"call",100)
monte(70, 1.05, 1   ,  7,    0, 5, "put",2000)
monte(90, 1.15, 0.8 , 10, 0.01, 3, "call",50000)

#3a

N1 <- c()
N2 <- c()
N3 <- c()

M <- 100 

for (i in c(1:M)){
  N1 <- c(N1,monte(60,1.05,0.95,15,0.01,8,"put",10) )
  N2 <- c(N2,monte(60,1.05,0.95,15,0.01,8,"put",100) )
  N3 <- c(N3,monte(60,1.05,0.95,15,0.01,8,"put",1000) )
}
cena_binomske <- binomski(60,1.05,0.95,15,0.01,8,"put") #cena premije dobljena z binomksim modelom

min <- floor(min(c(N1,N2,N3))) 
max <- ceiling(max(c(N1,N2,N3))) 


#histogram N1
pov.N1 <- mean(N1) #povprečje vrednosti N1
odklon.N1 <- sqrt(var(N1)) #standardni odklon vrednosti N1
x1_odklon_desno <- cena_binomske + odklon.N1
x1_odklon_levo <- cena_binomske - odklon.N1

histogram1 <-hist(N1,breaks = 20,
                  main = "Monte Carlo: N=10",
                  xlab = "Premija",
                  xlim = c(min, max),
                  col ="yellow")
abline(v= pov.N1, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0 = 0, x1= x1_odklon_desno, col= "green", length = 0.1 )
arrows(x0 = cena_binomske, y0 = 0, x1= x1_odklon_levo, col= "green", length = 0.1 )

legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))


#histogram N2
pov.N2 <- mean(N2) #povprečje vrednosti N1
odklon.N2 <- sqrt(var(N2)) #standardni odklon vrednosti N1
x2_odklon_desno <- cena_binomske + odklon.N2
x2_odklon_levo <- cena_binomske - odklon.N2


histogram2 <-hist(N2,breaks = 20,
                  main = "Monte Carlo: N=100",
                  xlab = "Premija",
                  xlim = c(min, max),
                  col ="yellow")
abline(v= pov.N2, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0 = 0, x1= x2_odklon_desno, col= "green", length = 0.1 )
arrows(x0 = cena_binomske, y0 = 0, x1= x2_odklon_levo, col= "green", length = 0.1 )


legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

#histogram N3
pov.N3 <- mean(N3) #povprečje vrednosti N1
odklon.N3 <- sqrt(var(N3)) #standardni odklon vrednosti N1
x3_odklon_desno <- cena_binomske + odklon.N3
x3_odklon_levo <- cena_binomske - odklon.N3


histogram3 <-hist(N3,breaks = 20,
                  main = "Monte Carlo: N=1000",
                  xlab = "Premija",
                  xlim = c(min, max),
                  col ="yellow")
abline(v= pov.N3, col = "green")
abline (v = cena_binomske, col = "red", lty = "dashed")
arrows(x0 = cena_binomske, y0 = 0, x1= x3_odklon_desno, col= "green", length = 0.1 )
arrows(x0 = cena_binomske, y0 = 0, x1= x3_odklon_levo, col= "green", length = 0.1 )
legend('topright', 
       legend = c('Monte Carlo', 'Analiza modela'),
       col = c('green', 'red'),
       cex=0.8,
       lty=c("solid","dashed"))

