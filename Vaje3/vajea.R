# FINANČNI PRAKTIKUM
# 3.NALOGA : VREDNOTENJE EKSOTIČNIH OPCIJ

library(combinat)
library(Rlab)

# 1.vaja
# a)
W = c(1,2,3,4,5,6)

pot1 = c(50,52.5,49.88,47.38,45.01,47.26)
K1 <- sum((pot1*W)) / sum(W)
izplaciloX1 <- max(pot1[6] - K1,0) # 0
izplaciloY1 <- max(K1 - pot1[6],0) # 0.4909524

pot2 = c(50.00,52.50, 55.12, 57.88, 60.78, 63.81)
K2 <- sum((pot2*W)) / sum(W)
izplaciloX2 <- max(pot2[6] - K2,0) # 4.827143
izplaciloY2 <- max(K1 - pot2[6],0) # 0

pot3 = c(50.00, 47.50, 49.88, 47.38, 45.01, 42.76)
K3 <- sum((pot3*W)) / sum(W)
izplaciloX3 <- max(pot3[6] - K3,0) # 0
izplaciloY3 <- max(K3 - pot3[6],0) # 3.229048

pot4 = c(50.00, 47.50, 45.12, 47.38, 45.01, 47.26)
K4 <- sum((pot4*W)) / sum(W)
izplaciloX4 <- max(pot4[6] - K4,0) # 0.6652381
izplaciloY4 <- max(K4 - pot4[6],0) # 0

pot5 = c(50.00, 52.50, 49.88, 52.37, 54.99, 52.24)
K5 <- sum((pot5*W)) / sum(W)
izplaciloX5 <- max(pot5[6] - K5,0) # 0
izplaciloY5 <- max(K5 - pot5[6],0) # 0.2604762

# b)

izplacilo <- function(vrsta, W, type){
  K <- sum(vrsta*W)/sum(W)
  if(type == "call") {
    return(max(vrsta[length(vrsta)] - K, 0))
  }
  else {
    return(max(K - vrsta[length(vrsta)], 0))
  }
}


# 2. Vaja

# a)

S0 = 50
u = 1.05
d = 0.95
T = 5
R = 0.03  
W = c(1, 2, 3, 4, 5, 6)


binomski<- function(S0,u,d,R,T,W,type) {
  kocka1 <- hcube(c(rep(2,T))) - 1
  kocka2 <- 2 - hcube(c(rep(2, T)))
  kocka <- u^kocka1 * d^kocka2
  matrika <- cbind(S0, kocka)
  moznosti <- t(apply(matrika, 1, cumprod))
  izpl <- (apply(moznosti, 1, izplacilo,W = W, type =type))
  q <- (1 + R - d)/(u - d)
  st_u <- rowSums(kocka1)
  st_d <- T - st_u
  verjetnosti <- (q ^st_u) *((1-q)^st_d)
  premija <- sum(izpl * verjetnosti) / (1 + R)^T
  return(premija)
  
}

# b)