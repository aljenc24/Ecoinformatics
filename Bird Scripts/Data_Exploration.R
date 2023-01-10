require(pracma)

#   1. ABUNDANCE EXPLORATION

#   --> I want to plot the effect of treatment type on abundance --> Boxplot
newabundance <-read.csv("./Cleaned Bird Data/cleanedabundance.csv")

head(newabundance)

boxplot(Total~Treatment, data = newabundance, ylim=c(0,500))

split_newabundance <- split(newabundance, newabundance$Treatment)
split_newabundance
control_samples <- split_newabundance[[1]]  # Contains only rows with "control" in the "group" column
treatment_samples <- split_newabundance[[2]]  # Contains only rows with "treatment1" in the "group" column

#   --> I want to explore the effect of site on abundance --> Boxplot
boxplot(Total~Site, data = control_samples,  ylim=c(0,600))
##Site DOES effect total control abundance
boxplot(Total~Site, data = treatment_samples)
##Site DOES effect total treatment abundance
# fits our assumptions for poisson distribution, 
#sites with higher mean have higher variation

#---------------------------------------------------------------------------------------------

#---> Hello Zach! Here is my code for producing the collective graphic that you were
#     suggesting I make but it keeps coming back with the response 
#     -- Error in plot.new() : figure margins too large


par(mfrow=c(13,1))
x.scale = range(control_samples$Total) #<-----What should I put here so that it will properly reflect the max and min values of the order abundances not the total
boxplot(Total~Orthoptera, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Hemiptera, data = control_samples, xlim=x.scale,  xlab="")
boxplot(Total~Lepidoptera, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Diptera, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Hymenoptera, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Coleoptera, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Thysanoptera, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Amphipoda, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Snail, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Isopoda, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Spider, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Pseudoscorpionida, data = control_samples, xlim=x.scale, xlab="")
boxplot(Total~Unknown, data = control_samples,xlim=x.scale,xlab="")

par(mfrow=c(13,1))
x.scale = range(treatment_samples$Total) #<-----What should I put here so that it will properly reflect the max and min values of the order abundances not the total
boxplot(Total~Orthoptera, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Hemiptera, data = treatment_samples, xlim=x.scale,  xlab="")
boxplot(Total~Lepidoptera, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Diptera, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Hymenoptera, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Coleoptera, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Thysanoptera, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Amphipoda, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Snail, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Isopoda, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Spider, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Pseudoscorpionida, data = treatment_samples, xlim=x.scale, xlab="")
boxplot(Total~Unknown, data = treatment_samples,xlim=x.scale,xlab="")

#---------------------------------------------------------------------------------------------

# --> Solar Days and the Period
boxplot(Total~Period, data = control_samples, ylim = c(0,600))
time.range = range(newabundance$SolarDay)
par(mfrow=c(1,2))
plot(Total~SolarDay, data = control_samples, col="black", ylim=c(0,600), xlim=time.range)
plot(Total~SolarDay, data = treatment_samples, col="black", ylim=c(0,800),xlim=time.range)

#To see how many samples were collected for every given time period
aggregate(control_samples$Total,by=list(control_samples$Period), FUN="length")
aggregate(treatment_samples$Total,by=list(treatment_samples$Period), FUN="length")

# unique(newabundance$Treatment)
# treat.effect <-unique(newabundance$Treatment)
# control <-treat.effect[c(1)]
# exclosure <-treat.effect[c(2)]

#Another Approach
# abundance.control<-newabundance[newabundance$treatment=="control",]
# abundance.treatment<-newabundance[newabundance$treatment=="exclosure",]



#   2. BIOMASS EXPLORATION

newbiomass <-read.csv("./Cleaned Bird Data/cleanedbiomass.csv")

#   --> I want to examine the effect of Site on Total --> Boxplot
par(mfrow=c(1,1))
boxplot(nthroot(Total.mass,n=3)~Site, data = newbiomass, ylim=c(0,1.5))

#   --> I want to examine the abundances of genus in controlled and enclosed environments --> boxplots


#   3. BODYSIZE EXPLORATION --> Currently this is Zach's Problem

newbodysize <-read.csv("./Cleaned Bird Data/cleanedbodysize.csv")

#   --> I want to examine the effect of treatment type on abundance


