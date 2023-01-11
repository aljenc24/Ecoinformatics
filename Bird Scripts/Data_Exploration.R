require(pracma)
require(car)
require(multcomp)

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

#------------------------ABUNDANCE PLOTS & Histograms---------------------------------------------------------------------

#---> Hello Zach! Here is my code for producing the collective graphic that you were
#     suggesting I make but it keeps coming back with the response 
#     -- Error in plot.new() : figure margins too large

### [Zach] This figure you're trying to make will be too big for poor R studio to
###  show in it's little window. Let's make it as an external .png for now and look at it there.
###  I made a folder called 'Rough Figures' where I'll store the graph.

png(height=10,width=7.5,pointsize=8,units="in",res=900,file="./Rough Figures/abun_taxa.png")
### [Zach] the above line starts creating the png image. any graphics code that follows
###  will affect that image file and NOT show up in your graphics window.
###  At the end of the graphics code, we use the 'dev.off()' function to finish creating
###  the png image, finalize it, and close it

par(mfrow=c(13,1),mar=c(0,0,0.5,0),oma=c(4.5,4.5,0,0.5))
#x.scale <- range(control_samples[,c("Orthoptera","Hemiptera","Lepidoptera","Diptera",
                                  #  "Hymenoptera","Coleoptera","Thysanoptera","Amphipoda",
                                  #  "Snail","Isopoda","Spider","Pseudoscorpionida")])

x.scale <- c(0,250)

### [Zach] added x.scale method above, having 'range' select all individual count columns
breaknum=0:10000*2
hist(control_samples$Orthoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Orthoptera")
hist(control_samples$Hemiptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Hemiptera")
hist(control_samples$Lepidoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Lepidoptera")
hist(control_samples$Diptera,xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Diptera")
hist(control_samples$Hymenoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Hymenoptera")
hist(control_samples$Coleoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Coleoptera")
hist(control_samples$Thysanoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Thysanoptera")
hist(control_samples$Amphipoda, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Amphipoda")
hist(control_samples$Snail,xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Snail")
hist(control_samples$Isopoda, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Isopoda")
hist(control_samples$Spider, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Spider")
hist(control_samples$Pseudoscorpionida, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Pseudoscorpioniida")
hist(control_samples$Unknown, xlim=x.scale, xlab="",main="",ylab="", breaks=breaknum)
title(line=-1, "Unknown")
mtext(outer=T,"Abundance",side=1, line=3)
mtext(outer=T,"Frequency",side=2, line=3)
### [Zach] now keep adding your histograms for the other taxa
### [Zach] on your bottom panel, remove the 'xaxt="n"' argument

dev.off()

# !!!Notes for Graphics produced... 
# 1. Outlier points must be addressed to fix the x-axis spread (it is how it is :( )
# 2. y axis lines and tick marks have disappeared (Check)


# --> follow the same steps for Treatment...

png(height=10,width=7.5,pointsize=8,units="in",res=900,file="./Rough Figures/abun_taxa_treatment.png")
par(mfrow=c(13,1),mar=c(0,0,0.5,0),oma=c(4.5,4.5,0,0.5))
x.scale <- c(0,250)
breaknum=0:10000*2
hist(treatment_samples$Orthoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Orthoptera")
hist(treatment_samples$Hemiptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Hemiptera")
hist(treatment_samples$Lepidoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Lepidoptera")
hist(treatment_samples$Diptera,xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Diptera")
hist(treatment_samples$Hymenoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Hymenoptera")
hist(treatment_samples$Coleoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Coleoptera")
hist(treatment_samples$Thysanoptera, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Thysanoptera")
hist(treatment_samples$Amphipoda, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Amphipoda")
hist(treatment_samples$Snail,xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Snail")
hist(treatment_samples$Isopoda, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Isopoda")
hist(treatment_samples$Spider, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Spider")
hist(treatment_samples$Pseudoscorpionida, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Pseudoscorpionida")
hist(treatment_samples$Unknown, xlim=x.scale, xlab="Treatment Type",main="",ylab="", breaks=breaknum)
title(line=-1, "Unknown")
mtext(outer=T,"Abundance",side=1, line=3)
mtext(outer=T,"Frequency",side=2, line=3)
dev.off()

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

#-------------------------------ABUNDANCE LINEAR MODELING--------------------------------------------------------------

# --> new abundance --> Linear Regression for the Effect of SITE ON ABUNDANCE
### [Zach] For funzies... (x independence assumption violated)

#a. Control Samples
control_samples$Site<-as.factor(control_samples$Site)
model.1<-glm(Total~0+Site, data=control_samples, family=poisson())
summary(model.1)
Anova(model.1) #Chisqr=325913 & p-value<2.2e-16
#b. Treatment Samples
treatment_samples$Site<-as.factor(treatment_samples$Site)
model.2<-glm(Total~0+Site, data=treatment_samples, family=poisson())
summary(model.2)
Anova(model.2) #Chisqr=223630 & p-value<2.2e-16

#  [Data Confusion area Start]>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

### ??? Why is it that when I run a. Hemiptera and b. Diptera the coefficient table is arranged differently.
# in a. it says hemiptera at each site but in b it's every entry with no reference to site

# --> new abundance --> Linear Regression for the Effect of ORDER ON ABUNDANCE
# a. Hemiptera
control_samples$Hemiptera<-as.factor(control_samples$Hemiptera)
model.H<-glm(Total~0+Hemiptera, data=control_samples, family=poisson())
summary(model.H)
Anova(model.H) #Chisqr=325913 & p-value<2.2e-16

# b. Diptera
control_samples$Diptera<-as.factor(control_samples$Diptera)
model.D<-glm(Total~0+Diptera, data=control_samples, family=poisson())
summary(model.D)
Anova(model.D) #Chisqr=344768 & p-value<2.2e-16

#  [Data Confusion area End]>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# c. Amphipoda
control_samples$Amphipoda<-as.factor(control_samples$Amphipoda)
model.A<-glm(Total~0+Amphipoda, data=control_samples, family=poisson())
summary(model.A)
Anova(model.A) #Chisqr=343695 & p-value<2.2e-16

# Spider
control_samples$Spider<-as.factor(control_samples$Spider)
model.S<-glm(Total~0+Spider, data=control_samples, family=poisson())
summary(model.S)
Anova(model.S) #Chisqr=348457 & p-value<2.2e-16

#glm(y~x*z)

# --> new abundance --> Looking at the effect of Solar day on Abundance
newabundance$SolarDay<-as.factor(newabundance$SolarDay)
model.Solar<-glm(Total~0+SolarDay, data=newabundance, family=poisson())
summary(model.Solar)
Anova(model.Solar) #Chisqr=578352 & p-value<2.2e-16

# ???--> Looking more Specifically at the Effect of Solar day and specific orders?


#Conclusions...
# site does indeed impact abundance --> look at p-value and chi sqr.
# Treatment has slightly greater impact on chi-square for control samples
# 4 orders above have a large impact on the total abundance
# Solar Day has an effect on Total abundance

#---------------------------------------------------------------------------------------------

#   2. BIOMASS EXPLORATION

newbiomass <-read.csv("./Cleaned Bird Data/cleanedbiomass.csv")

split_newbiomass <- split(newbiomass, newbiomass$EC)
split_newbiomass
control_mass_samples <- split_newbiomass[[1]]  
treatment_mass_samples <- split_newbiomass[[2]]

#Looking at the effect of Control and Exclosure on Total Mass
par(mfrow=c(1,2), mar=c(1,4,2,2))
boxplot(Total.mass~EC, data = control_mass_samples,  ylim=c(0,1), ylab="Total Mass", main="Control")
boxplot(Total.mass~EC, data = treatment_mass_samples,  ylim=c(0,1),yaxt="n", main="Exclosure", ylab="")
### Higher SD in Exclosure but big overlap in control and exclosure groups

#warning()

newbiomass$EC<-as.factor(newbiomass$EC)
model.ECmass<-glm(Total.mass~0+EC, data=newbiomass)
summary(model.ECmass)
Anova(model.ECmass) #Chi.sqr=74.447 & p-value<2.2e-16

#- biomass —> cubed root & normal distribution, use GLM but don't put family

png(height=10,width=7.5,pointsize=8,units="in",res=900,file="./Rough Figures/biomass_taxa_treatment.png")
par(mfrow=c(5,1),mar=c(0,0,0.5,0),oma=c(4.5,4.5,0,0.5))
x.scale <- c(0,250)
breaknum=0:100*2
hist(control_mass_samples$Amphi.mass, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Amphibian Biomass")
hist(control_mass_samples$Spider.mass, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Spider Biomass")
hist(control_mass_samples$Hemi.mass, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Hemiptera Biomass")
hist(control_mass_samples$Snail.mass,xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum, col="red")
title(line=-1, "Snail Biomass")
hist(control_mass_samples$Other.mass, xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Other Biomass")

newbiomass$<-as.factor(control_samples$Site)
model.1<-glm(Total~0+Site, data=control_samples, family=poisson())
summary(model.1)
Anova(model.1)


#   --> I want to examine the effect of Site on Total --> Boxplot

par(mfrow=c(1,1))
boxplot(nthroot(Total.mass,n=3)~Site, data = newbiomass, ylim=c(0,1.5), ylab="Cubed Root Total Mass")



#   3. BODYSIZE EXPLORATION --> Currently this is Zach's Problem

newbodysize <-read.csv("./Cleaned Bird Data/cleanedbodysize.csv")

#   --> I want to examine the effect of treatment type on abundance


