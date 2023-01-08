#This was my first script produced with Professor Wood, to try to better comprehend various distributions. In this script, a normal, log normal, poisson, and binomial distribution were produced. 

#par
par(mfrow=c(2,2), font=4, bg="black", 
    col.axis="white", col.lab="white",col.main="white",col.sub="white", font=3)
curve(dnorm(x,0,1),xlim=c(-3,3), 
      main="Normal Distribution", xlab="x", ylab="Frequency",col="red")
curve(dlnorm(x,meanlog=0,sdlog=1), xlim=c(0,5),
      main="Log Normal Distribution", xlab="x", ylab="Frequency",col="orange")
plot(y=dpois(0:16,lambda=6),x=0:16,type="p",
     main="Poisson Distribution", xlab="x", ylab="Frequency",col="yellow")
plot(y=dbinom(0:5,size=5,prob=0.15),x=0:5,
     main="Binomial Distribution", xlab="x", ylab="Frequency",col="green")

#split.screen
# split.screen(curve(dnorm(x,0,1),xlim=c(-3,3)))
# curve(dlnorm(x,meanlog=0,sdlog=1), xlim=c(0,5)))
# curve(dpois(x,lambda=6),xlim=c(0,15),n=16, add=TRUE)
# curve(dbinom(x,size=5,prob=0.65),xlim=c(0,),n=16, add=TRUE)
# screen, erase = TRUE))
