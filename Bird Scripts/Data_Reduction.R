source("./Bird Scripts/Correlation.R")

# Data Reduction

# 1. Principle Components Analysis --> gives us New Variables, same number of variables we put in, variables will be "orthogonal"
# what is the axis that explains most of the data... prioritize that one and move on to second best, third best...

#Biomass
cleaned.mass.ordered
mass.pc<- prcomp(cleaned.mass.ordered, center=T,scale=T)
summary(mass.pc)
# PC1 --> is there a lot of biomass, PC2--> terrestrial or aquatic?

#Abundance
abun.pc<- prcomp(cleanedabund.ordered, scale=T)
summary(abun.pc)
abun.pc
# PC1 --> 

#Bodysize
body.pc<-prcomp(cleaned.body.ordered, scale=T)
summary(body.pc)

# Factor Analysis --> Factor Analysis seems to be yielding smaller proportions of variance than our PC tests above
                        # Zach is looking into it... 
#Biomass
mass.fac<- factanal(cleaned.mass.ordered, factors=1)
summary(mass.fac)
mass.fac

#Abundance
abun.fac<- factanal(cleanedabund.ordered, 3)
abun.fac










