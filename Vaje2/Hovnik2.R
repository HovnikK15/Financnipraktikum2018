library(actuar)
library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(rmarkdown)
library(tidyr)
library(digest)
library(httr)
library(sp)
library(maptools)

vzorec <- scan("Vaje2/vzorec1.txt")

#1.a)
hist(vzorec, col = 'blue', xlab="Visina odškodnine", main = 'Histogram odskodnin')
visina <- mde(vzorec, pexp, start = list(rate=1), measure = "CvM") 
#mde je funkcija, ki nam za vzorec izračuna višino
lambda <- visina$estimate[1] #to je naša lambda
razdalja <- visina$distance[1]
#c)
hist(vzorec, probability = TRUE, xlab = "Višina odškodnine")
curve(dexp(x, ocena, razdalja), add = TRUE, From = 0, to = 9, 
      col = 'red')
legend("topright", "eksponentna porazdelitev", fill="red")
#d)
n = 25
p = 0.5

#N <- dbinom(x, size = n, prob = p, log = FALSE)
EN <- n*p #UPANJE N
EY<- 1/lambda  #Upanje Y
ES <- EN*EY
varN = n*p*(1-p)
varY = 1/(lambda)^2
EY2 <- varY + (EY)^2
varS <- varY * EN + EY2 * var(N)
EY
ES

#2. naloga
kr neki