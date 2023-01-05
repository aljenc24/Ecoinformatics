#This script is to explore and understand the Bird data. Looking at the abundance, size, and biomass data and their most fitting distributions
site<-read.csv("./Raw Bird Data/SiteVariables.csv")
hist(site$Bird_Average)
hist(log(site$Bird_Average))
plot(x=site$Bird_Average,y=rnorm(nrow(site),0,0.1),ylim=c(-1,1), main = "Bird Average",ylab = "Variable",xlab="Average Bird Sitings Per Site")
par(mfrow=c(1,2))

#Abundance Histogram and Plot
abundance<-read.csv("./Raw Bird Data/SampleAbundance.csv")
hist(abundance$Total,main="Species Abundance", ylab = "Frequency",xlab="Abundance")
plot(x=abundance$Total,main="Species Abundance",ylab = "Frequency",xlab="Abundance")
#Axes need to be flipped, frequency is on bottom and abundance is on y axis

biomass<-read.csv("./Raw Bird Data/SampleBiomass.csv")
hist(biomass$Total.mass,main="Biomass",ylab = "Frequency",breaks=50,xlab="Total Mass",xlim=c(0,5))

#BodySize
bodysize<-read.csv("./Raw Bird Data/SampleBodySize.csv")
hist(bodysize$Size.Class..5mm, xlim=c(0,300), breaks = 200, main="Frequency of Invertebrates under 5mm",xlab="Number of Invertebrates (<5mm)")

hist(bodysize$Size.Class..5mm.1)
