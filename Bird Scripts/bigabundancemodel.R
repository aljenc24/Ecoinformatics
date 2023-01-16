require(car)
require(multcomp)

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






