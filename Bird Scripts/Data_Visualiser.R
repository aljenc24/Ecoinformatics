#This script is to explore and understand the Bird data. Looking at the abundance, size, and biomass data and their most fitting distributions

site<-read.csv("./Raw Bird Data/SiteVariables.csv")
par(mfrow=c(1,1))
hist(site$Bird_Average)
hist(log(site$Bird_Average))
plot(x=site$Bird_Average,y=rnorm(nrow(site),0,0.1),ylim=c(-1,1), main = "Bird Average",ylab = "Variable",xlab="Average Bird Sitings Per Site")
par(mfrow=c(1,2))

#Abundance Histogram and Plot
abundance<-read.csv("./Raw Bird Data/SampleAbundance.csv")
hist(abundance$Total,main="Species Abundance", ylab = "Frequency",xlab="Abundance")
plot(x=abundance$Total,main="Species Abundance",y=rnorm(nrow(abundance),0,0.1),ylab = "",xlab="Abundance", yaxt="n")
#Axes need to be flipped, frequency is on bottom and abundance is on y axis

#Abundance Data Analysis
mean(abundance$Total)
# Total Mean = 167.97
sd(abundance$Total)
# Standard Deviation = 265.09

#Biomass Histogram
biomass<-read.csv("./Raw Bird Data/SampleBiomass.csv")
hist(biomass$Total.mass,main="Biomass",ylab = "Frequency",breaks=50,xlab="Total Mass",xlim=c(0,5))
#DISCOVERY--> Delete anything w/ negative note, handle negative mass values, recalculate ALL total mass

par(mfrow=c(1,1), bg="white",col.axis="black",col.lab="black",col.main="black",col.sub="black")
hist(newbiomass$Total.mass,main="Biomass",ylab = "Frequency",breaks=50,xlab="Total Mass",xlim=c(0,5))

#Body Size Histograms
bodysize<-read.csv("./Raw Bird Data/SampleBodySize.csv")
hist(bodysize$Size.Class..5mm/bodysize$Total.Number, xlim=c(0,1),breaks=100, main="Proportion of Invertebrates under 5mm",xlab="Proportion of Invertebrates (<5mm)")
hist(bodysize$Size.Class..5mm.1/bodysize$Total.Number, xlim=c(0,1),breaks=100, main="Proportion of Invertebrates Over 5mm",xlab="Proportion of Invertebrates (>5mm)")
