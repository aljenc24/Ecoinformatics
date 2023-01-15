require(car)
require(multcomp)

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
plot(Abundance~Taxa, data=stacked.abun)

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
par(mfrow=c(2,2))
plot(model.treat.taxa)
par(mfrow=c(1,1))

###   Abundance~Treatment*Site
model.treat.site<-glm(Abundance~0+Treatment*Site, data=stacked.abun, family=poisson())
summary(model.treat.site)
Anova(model.treat.site)
par(mfrow=c(2,2))
plot(model.treat.site)
par(mfrow=c(1,1))

###   Abundance~Treatment*SolarDay
model.treat.sol<-glm(Abundance~0+Treatment*SolarDay, data=stacked.abun, family=poisson())
summary(model.treat.sol)
Anova(model.treatsol)
par(mfrow=c(2,2))
plot(model.treat.sol)
par(mfrow=c(1,1))


#         BIOMASS

### What Kind of Effect does Exclosure have on Abundance >>>>>>>>>>>>>>>>>

stacked.biomass<-read.csv("./Cleaned Bird Data/cleanedbiomass_stacked.csv")
head(stacked.biomass)
stacked.biomass$Taxa=as.factor(stacked.biomass$Taxa)
model.orderbiom<-glm(Mass~0+Taxa, data=stacked.biomass, family=poisson())

plot(Mass~Taxa*Site, data=stacked.biomass)

