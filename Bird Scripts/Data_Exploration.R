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
par(mfrow=c(1,2), mar=c(4.5,4.5,0.5,0.5))
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

#- biomass —> cubed root & normal distribution, use GLM but don't put family

png(height=10,width=7.5,pointsize=8,units="in",res=900,file="./Rough Figures/biomass_taxa_control.png")
par(mfrow=c(5,1),mar=c(0,0,0.5,0),oma=c(4.5,4.5,0,0.5))
x.scale <- c(0,2)
breaknum=0:100000*0.05
hist(nthroot(control_mass_samples$Amphi.mass, n=3), xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Amphibian Biomass")
hist(nthroot(control_mass_samples$Spider.mass, n=3), xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Spider Biomass")
hist(nthroot(control_mass_samples$Hemi.mass, n=3), xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Hemiptera Biomass")
hist(nthroot(control_mass_samples$Snail.mass, n=3),xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Snail Biomass")
hist(nthroot(control_mass_samples$Other.mass, n=3), xlim=x.scale, xlab="",main="",ylab="", breaks=breaknum)
title(line=-1, "Other Biomass")
dev.off()

png(height=10,width=7.5,pointsize=8,units="in",res=900,file="./Rough Figures/biomass_taxa_treatment.png")
par(mfrow=c(5,1),mar=c(0,0,0.5,0),oma=c(4.5,4.5,0,0.5))
x.scale <- c(0,2)
breaknum=0:100000*0.05
hist(nthroot(treatment_mass_samples$Amphi.mass, n=3), xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Amphibian Biomass")
hist(nthroot(treatment_mass_samples$Spider.mass, n=3), xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Spider Biomass")
hist(nthroot(treatment_mass_samples$Hemi.mass, n=3), xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Hemiptera Biomass")
hist(nthroot(treatment_mass_samples$Snail.mass, n=3),xlim=x.scale, xlab="",main="",ylab="",xaxt="n", breaks=breaknum)
title(line=-1, "Snail Biomass")
hist(nthroot(treatment_mass_samples$Other.mass, n=3), xlim=x.scale, xlab="",main="",ylab="", breaks=breaknum)
title(line=-1, "Other Biomass")
dev.off()

#   --> I want to examine the effect of Site on Total --> Boxplot

par(mfrow=c(1,1))
boxplot(nthroot(Total.mass,n=3)~Site, data = newbiomass, ylim=c(0,1.5), ylab="Cubed Root Total Mass")



#   3. BODYSIZE EXPLORATION --> Currently this is Zach's Problem

newbodysize <-read.csv("./Cleaned Bird Data/cleanedbodysize.csv")

#   --> I want to examine the effect of treatment type on abundance


