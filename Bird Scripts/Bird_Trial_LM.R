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

### <--- tests for the chi sqr value for treatment on biomass
#? is this correct if I havent specified 
newbiomass$EC<-as.factor(newbiomass$EC)
model.ECmass<-glm(nthroot(Total.mass,n=3)~0+EC, data=newbiomass)
summary(model.ECmass)
Anova(model.ECmass) #Chi.sqr=1071.4 & p-value<2.2e-16

#- biomass —> cubed root & normal distribution, use GLM but don't put family

#a. Control Samples
control_mass_samples$Site<-as.factor(control_mass_samples$Site)
model.masscsite<-glm(nthroot(Total.mass,n=3)~0+Site, data=control_mass_samples)
summary(model.masscsite)
Anova(model.masscsite) #Chisqr=1368.1 & p-value<2.2e-16

#b. Treatment Samples
treatment_mass_samples$Site<-as.factor(treatment_mass_samples$Site)
model.masstsite<-glm(nthroot(Total.mass,n=3)~0+Site, data=treatment_mass_samples, family=poisson())
summary(model.masstsite)
Anova(model.masstsite) #Chisqr=88.151 & p-value=1.103e-15
warnings() #ERROR MESSEGE>>>>>>>>>>>>>>>>>>>>>>>>>

#Looking at the effect of Solar day on Abundance
newbiomass$SolarDay<-as.factor(newbiomass$SolarDay)
model.massS<-glm(nthroot(Total.mass,n=3)~0+SolarDay, data=newbiomass)
summary(model.massS)
Anova(model.massS) #Chisqr=2178.7 & p-value<2.2e-16

#Is there a relationship between Snail Mass and Solar day
newbiomass$SolarDay<-as.factor(newbiomass$SolarDay)
model.snail.massS<-glm(nthroot(Snail.mass,n=3)~0+SolarDay, data=newbiomass)
summary(model.snail.massS)
Anova(model.snail.massS)
Anova(model.masscsite) #Chisqr=532.17 & p-value<2.2e-16

