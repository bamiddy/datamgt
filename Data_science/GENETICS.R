d <- subset(ChickWeight, Time==10)  
hist(d$weight, xlim = c(0,300), freq = F)
#curve(dnorm(x, mean = 230, sd= 50), add= T,col="black", lwd=2)       
#curve(dnorm(x, mean = 200, sd= 40), add= T,col="black", lwd=2)  
#curve(dnorm(x, mean = 170, sd= 30), add= T,col="black", lwd=2)
#curve(dnorm(x, mean = 130, sd= 26), add= T,col="black", lwd=2)       
#curve(dnorm(x, mean = 110, sd= 30), add= T,col="black", lwd=2)  
#curve(dnorm(x, mean = 90, sd= 40), add= T, col="black", lwd=2)
#curve(dnorm(x, mean = 90, sd= 40), add= T, col="black", lwd=2)

mu <- mean(d$weight)
s <- sd(d$weight)
curve(dnorm(x, mean = mu, sd= s), add= T, col="red", lwd=5)
pnorm(x, mean = mu, sd= s)

pnorm(100, 107.8, 3.4)
1-pnorm(115, 107.8, 3.4)

SEM = s/sqrt(length(d$weigh))
SEM

pnorm(7000, 8000, sqrt(250000))
curve(pnorm, from = -4, to = 4)
curve(dnorm, from = -4, to = 4, add = T)

y <- rnorm(1000, 10, 5)
m <- mean(y)
s <- sd(y)

z = (y - m)/s
hist(z)

qnorm(0.025)
qnorm(0.05)
qnorm(0.975)

j = c(2.5, 3.1,2.6, 2.2, 3.3, 2.9, 2, 2.2, 2, 1.4)
s = sd(j)
qnorm(0.995)


d = chickwts
head(d)
dw <- d$weight
so = sd(dw)
m= mean(dw)
l = length(dw)

LL = m - (1.96*s)/sqrt(l)

UL = m + (1.96*s)/sqrt(l)

LL
UL
m
mean(ChickWeight$weight)

#fOR SMALL SAMPLE SIZE
smp = sample(dw, 15)
s = sd(smp)
m= mean(smp)
l = length(smp)

ll = m - (1.96*s)/sqrt(l)

Ul = m + (1.96*s)/sqrt(l)

vd =var(dw)
E = 0.5
n = ((1.96)^2*(vd))/E^2
n


fcr = c(3.08, 2.4, 2.56, 3.25, 2.74, 3.61, 2.80, 3.04, 2.37, 3.42, 2.82,
        2.86, 2.23, 3.08, 2.63, 2.93, 2.66, 2.23, 2.24, 3.07, 2.97, 3.59,
        2.96, 2.78, 2.70, 3.44, 2.69, 2.22, 3.31, 2.92, 3.08, 3.51, 2.78)

m = mean(fcr)
m
s =sd(fcr)
s
l = length(33)
z = (m - 3)/(s/sqrt(l))
z

pa = c(240, 237, 266, 215, 228, 237, 245)
pb = c(250, 240, 267, 210, 224, 243, 265)

t.test(pa, pb, paired = TRUE)

library(readr)
#data = readr::read_delim("http://merlin.up.poznan.pl/~mcszyd/dyda/Experimental-Design/swine-data.txt", delim = "\t")

m = mean(data$CarcassL)
va = var(data$CarcassL)
va
z1 = qnorm( 0.975)
z2 = qnorm(0.999)

n = (2*va*(z1+z2)^2)/0.5^2
n

pla = c(250, 271, 243, 252, 266, 272, 293, 296, 301, 298, 310, 286, 306, 309)
Ben = c(258, 285, 245, 250, 268, 278, 280, 305, 319, 308, 320, 293, 305, 313)

t.test(pla, Ben, alternative =  "less", paired = TRUE)

data = read.table("http://merlin.up.poznan.pl/~mcszyd/dyda/Experimental-Design/swine-data.txt", head= TRUE)

# For 

pl <-  subset(data, Breed == "PL")
plw <-  subset(data, Breed == "PLW")
L990 <-  subset(data, Breed == "L990")

pl3 = pl$BFT3 
pl4 = pl$BFT4

t.test(pl3, pl4, paired = TRUE)

plw3 = plw$BFT3 
plw4 = plw$BFT4

t.test(plw3, plw4, paired = TRUE)

L9903 = L990$BFT3 
L9904 = L990$BFT4

t.test(L9903, L9904, paired = TRUE)

pnorm(1.96)
qnorm(0.975)
1-pnorm(1.96)
