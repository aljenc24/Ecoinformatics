require(car)
require(multcomp)
require(corrplot)

cleanedabundance<-read.csv("./Cleaned Bird Data/cleanedabundance.csv")

#         ABUNDANCE

###   Abundance~Taxa
stacked.abun<-read.csv("./Cleaned Bird Data/cleanedabundance_stacked.csv")
head(stacked.abun)
stacked.abun$Taxa=as.factor(stacked.abun$Taxa)
model.orderabun<-glm(Abundance~0+Taxa, data=stacked.abun, family=poisson())
summary(model.orderabun)
Anova(model.orderabun)
order.test=glht(model.orderabun,linfct=mcp(Taxa="Tukey"))
cld(order.test)
# plot(Abundance~Taxa, data=stacked.abun)

###   Abundance~Taxa+Site
model.abunsite<-glm(Abundance~0+Taxa+Site, data=stacked.abun, family=poisson())
summary(model.abunsite)
Anova(model.abunsite)
par(mfrow=c(1,1))
plot(Abundance~Taxa+Site, data = stacked.abun)

abunsite.test=glht(model.abunsite,linfct=mcp(Taxa="Tukey"))
cld(abunsite.test)

###   Abundance~Taxa*Site
model.abunsiteint<-glm(Abundance~0+Taxa*Site, data=stacked.abun, family=poisson())
summary(model.abunsiteint)
Anova(model.abunsiteint)
# => Significant interaction between Taxa and Site

###   Abundance~Taxa*Site+SolarDay+Treatment
model.abunsitesol<-glm(Abundance~0+Taxa*Site+Period+Treatment, data=stacked.abun, family=poisson())
summary(model.abunsitesol)
Anova(model.abunsitesol)

### Interactions between Treatment and other variables (Site, Taxa, Solar Day) on Abundance>>>>>>>>>>>>>>>>>

###   Abundance~Treatment*Taxa
model.treat.taxa<-glm(Abundance~0+Treatment*Taxa, data=stacked.abun, family=poisson())
summary(model.treat.taxa)
Anova(model.treat.taxa)
# par(mfrow=c(2,2))
# plot(model.treat.taxa)
# par(mfrow=c(1,1))

###   Abundance~Treatment*Site
model.treat.site<-glm(Abundance~0+Treatment*Site, data=stacked.abun, family=poisson())
summary(model.treat.site)
Anova(model.treat.site)
# par(mfrow=c(2,2))
# plot(model.treat.site)
# par(mfrow=c(1,1))

###   Abundance~Treatment*SolarDay
model.treat.sol<-glm(Abundance~0+Treatment*SolarDay, data=stacked.abun, family=poisson())
summary(model.treat.sol)
Anova(model.treatsol)
# par(mfrow=c(2,2))
# plot(model.treat.sol)
# par(mfrow=c(1,1))


#         BIOMASS

### What Kind of Effect does Exclosure have on Abundance >>>>>>>>>>>>>>>>>

stacked.biomass<-read.csv("./Cleaned Bird Data/cleanedbiomass_stacked.csv")
head(stacked.biomass)
stacked.biomass$Taxa=as.factor(stacked.biomass$Taxa)
model.orderbiom<-glm(Mass~0+Taxa, data=stacked.biomass, family=poisson())
# 
# plot(Mass~Taxa*Site, data=stacked.biomass)



##Abundance Based on Taxa
head(stacked.abun)
jittered.taxa<-as.numeric(stacked.abun$Taxa)+runif(n=nrow(stacked.abun), min = -0.2, max = 0.2)
par(mfrow=c(1,1), mar=c(9,4.5,0.5,0.5))
plot(Abundance~jittered.taxa, data=stacked.abun, ylim=c(0,200), col=rgb(0,0,0,0.2), pch=16, xlab="",xaxt="n")
axis(side=1, at=unique(as.numeric(stacked.abun$Taxa)), labels=unique(stacked.abun$Taxa),las=2)

model.orderabun<-glm(Abundance~0+Taxa, data=stacked.abun, family=poisson())
summary(model.orderabun)
points(y=exp(coef(model.orderabun)), x=c(1:length(unique(stacked.abun$Taxa))), col="red", pch=8)
model.sol<-glm(Total~SolarDay, data=cleanedabundance, family=poisson())
summary(model.sol)
coef.sol<-coef(model.sol)
plot(Total~SolarDay, data=cleanedabundance, ylim=c(0,600))
curve(exp(coef.sol["(Intercept)"]+coef.sol["SolarDay"]*x), add = T, col="red")
#Once again, red is the model, black is the data

#Running a glm with squared term --> quadratic
cleanedabundance$SqSolarDay<-cleanedabundance$SolarDay^2
quad.model.sol<-glm(Total~SqSolarDay+SolarDay, data=cleanedabundance, family=poisson())
summary(quad.model.sol)
coef.sqsol<-coef(quad.model.sol)
plot(Total~SolarDay, data=cleanedabundance, ylim=c(0,600))
curve(exp(coef.sqsol["(Intercept)"]+coef.sqsol["SolarDay"]*x+coef.sqsol["SqSolarDay"]*x^2), add = T, col="red")
# Produced a curve model for the effect of solar day on abundance. 

Anova(quad.model.sol)
#site specific intercept, global effect of solar day and sq solar day (same beta (b) and gamma (c) coefficients for all sites)
quad.site.model.sol<-glm(Total~0+Site+SqSolarDay+SolarDay, data=cleanedabundance, family=poisson())
summary(quad.site.model.sol)
#site specific intercept, site specific effect of solar day and sq solar day (different b and c coefficients per site)
crazy.quad.site.model.sol<-glm(Total~0+Site+Site:SqSolarDay+Site:SolarDay, data=cleanedabundance, family=poisson())
summary(crazy.quad.site.model.sol)

#To plot data only from one site: data= cleanedabundance[cleanedabundance$Site="Site Name"]

# HOMEWORK GOAL: Make 8 plots for each site 

png(height=11,width=7,pointsize=8,units="in",res=900,file="./Rough Figures/Site.Solar.Models.png")
par(mfrow=c(4,2), mar=c(4,4,0.5,0.5))
x.scale= c(160,260)
#   1. Asparagus
Asp.Abun=cleanedabundance[cleanedabundance$Site=="Asparagus",] 
Asp.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Asp.Abun, family=poisson())
coef.asp<-coef(Asp.Abun.model)
plot(Total~SolarDay, data=Asp.Abun, xlim=x.scale, xlab="Solar Day", ylab="Total Abundance")
curve(exp(coef.asp["(Intercept)"]+coef.asp["SolarDay"]*x+coef.asp["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Asparagus")

#   2. Barn Island
Bar.Abun=cleanedabundance[cleanedabundance$Site=="Barn Island",] 
Bar.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Bar.Abun, family=poisson())
coef.bar<-coef(Bar.Abun.model)
plot(Total~SolarDay, data=Bar.Abun, xlim=x.scale, xlab="Solar Day", ylab="Total Abundance")
curve(exp(coef.bar["(Intercept)"]+coef.bar["SolarDay"]*x+coef.bar["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Barn Island")

#   3. Bunny
Bun.Abun=cleanedabundance[cleanedabundance$Site=="Bunny",]  
Bun.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Bun.Abun, family=poisson())
coef.bun<-coef(Bun.Abun.model)
plot(Total~SolarDay, data=Bun.Abun, xlim=x.scale)
curve(exp(coef.bun["(Intercept)"]+coef.bun["SolarDay"]*x+coef.bun["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Bunny")

#   4. Chapman
Cha.Abun=cleanedabundance[cleanedabundance$Site=="Chapman",] 
Cha.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Cha.Abun, family=poisson())
coef.cha<-coef(Cha.Abun.model)
plot(Total~SolarDay, data=Cha.Abun, xlim=x.scale)
curve(exp(coef.cha["(Intercept)"]+coef.cha["SolarDay"]*x+coef.cha["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Chapman")

#   5. Forsythe
For.Abun=cleanedabundance[cleanedabundance$Site=="Forsythe",] 
For.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=For.Abun, family=poisson())
coef.for<-coef(For.Abun.model)
plot(Total~SolarDay, data=For.Abun, xlim=x.scale)
curve(exp(coef.for["(Intercept)"]+coef.for["SolarDay"]*x+coef.for["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Forsythe")

#   6. Hammo
Ham.Abun=cleanedabundance[cleanedabundance$Site=="Hammo",] 
Ham.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Ham.Abun, family=poisson())
coef.ham<-coef(Ham.Abun.model)
plot(Total~SolarDay, data=Ham.Abun, xlim=x.scale)
curve(exp(coef.ham["(Intercept)"]+coef.ham["SolarDay"]*x+coef.ham["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Hammo")

#   7. Rachel Carson
Rac.Abun=cleanedabundance[cleanedabundance$Site=="Rachel Carson",] 
Rac.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Rac.Abun, family=poisson())
coef.rac<-coef(Rac.Abun.model)
plot(Total~SolarDay, data=Rac.Abun, xlim=x.scale)
curve(exp(coef.rac["(Intercept)"]+coef.rac["SolarDay"]*x+coef.rac["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Rachel Carson")

#   8. Sachuest
Sac.Abun=cleanedabundance[cleanedabundance$Site=="Sachuest",] 
Sac.Abun.model<-glm(Total~SqSolarDay+SolarDay, data=Sac.Abun, family=poisson())
coef.sac<-coef(Sac.Abun.model)
plot(Total~SolarDay, data=Sac.Abun, xlim=x.scale)
curve(exp(coef.sac["(Intercept)"]+coef.sac["SolarDay"]*x+coef.sac["SqSolarDay"]*x^2), add = T, col="red")
title(line=-3, "Sachuest")

dev.off()

## TRIAL>>> Abundance~Taxa for Asparagus site... 
par(mfrow=c(1,2))

#Orthoptera
Asp.Abun=cleanedabundance[cleanedabundance$Site=="Asparagus",] 
Asp.Orth.model<-glm(Orthoptera~SqSolarDay+SolarDay, data=Asp.Abun, family=poisson())
coef.orth<-coef(Asp.Orth.model)
plot(Orthoptera~SolarDay, data=Asp.Abun, xlab="Solar Day", ylab="Orthoptera Abundance")
curve(exp(coef.orth["(Intercept)"]+coef.orth["SolarDay"]*x+coef.orth["SqSolarDay"]*x^2), add = T, col="red")

#Hemiptera
Asp.Abun=cleanedabundance[cleanedabundance$Site=="Asparagus",] 
Asp.Hemi.model<-glm(Hemiptera~SqSolarDay+SolarDay, data=Asp.Abun, family=poisson())
coef.hemi<-coef(Asp.Hemi.model)
plot(Hemiptera~SolarDay, data=Asp.Abun, xlab="Solar Day", ylab="Orthoptera Abundance")
curve(exp(coef.hemi["(Intercept)"]+coef.hemi["SolarDay"]*x+coef.hemi["SqSolarDay"]*x^2), add = T, col="red")

#       ...
