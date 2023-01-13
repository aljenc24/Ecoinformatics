require(car)
require(multcomp)

###   Abundance~Taxa
stacked.abun<-read.csv("./Cleaned Bird Data/cleanedabundance_stacked.csv")
head(stacked.abun)
stacked.abun$Taxa=as.factor(stacked.abun$Taxa)
model.orderabun<-glm(Abundance~0+Taxa, data=stacked.abun, family=poisson())
summary(model.orderabun)
Anova(model.orderabun)
order.test=glht(model.orderabun,linfct=mcp(Taxa="Tukey"))
cld(order.test)

###   Abundance~Taxa+Site
model.abunsite<-glm(Abundance~0+Taxa+Site, data=stacked.abun, family=poisson())
summary(model.abunsite)
Anova(model.abunsite)
abunsite.test=glht(model.abunsite,linfct=mcp(Taxa="Tukey"))
cld(abunsite.test)

###   Abundance~Taxa*Site
model.abunsiteint<-glm(Abundance~0+Taxa*Site, data=stacked.abun, family=poisson())
summary(model.abunsiteint)
Anova(model.abunsiteint)
# Significant interaction between Taxa and Site


###   Abundance~Taxa*Site+SolarDay+Treatment
model.abunsitesol<-glm(Abundance~0+Taxa*Site+Period+Treatment, data=stacked.abun, family=poisson())
summary(model.abunsitesol)
Anova(model.abunsitesol)



###   Abundance~Taxa*Site+SolarDay+Treatment
model.abunsitesol<-glm(Abundance~0+Taxa*Site+Period+Treatment, data=stacked.abun, family=poisson())
summary(model.abunsitesol)
Anova(model.abunsitesol)





