x <- seq(0,pi,length.out = 1000)
n <- (pi-x)
funct <- (x*n)
max(x*n)
match(min(x**n),x**n)
plot(n, funct)
plot
funct[369]
x[369]
x[370]
pi/2*pi/2
hist(funct)
L <- 2.5
k <- c(0,1, 2, 3, 4)
pmf <- ((L**k)*(exp(-L)))/factorial(k)
pmf
help(hist)


ftoc <- function(tempinf) {
  celsius <- (tempinf - 32)*5/9
  return(celsius)
  }

ftok <-function(tempinf) {
  celsius <- (tempinf-32)*5/9
  kelvin <- (celsius+213.15) 
  return(kelvin)
  }

ftok(32)



CAGR <- function(beginning, ending, time) {
  values <- ((ending/beginning)**(1/time) - 1)
return(values)
  }

CAGR(1000,100000,40) * 100


x <- 10
f1 <- function(x){
  function() {
    x + 10
  }
}  
f1()
