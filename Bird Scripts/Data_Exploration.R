#   1. ABUNDANCE EXPLORATION

read.csv("./Cleaned Bird Data/cleanedabundance.csv")

#   --> I want to plot the effect of treatment type on abundance --> Boxplot
#Experimenting
split_newabundance <- split(newabundance, newabundance$Treatment)
split_newabundance
control_samples <- split_newabundance[[1]]  # Contains only rows with "control" in the "group" column
treatment_samples <- split_newabundance[[2]]  # Contains only rows with "treatment1" in the "group" column

unique(newabundance$Treatment)
treat.effect <-unique(newabundance$Treatment)
control <-treat.effect[c(1)]
exclosure <-treat.effect[c(2)]

#Another Approach
abundance.control<-newabundance[newabundance$treatment=="control",]
abundance.treatment<-newabundance[newabundance$treatment=="exclosure",]
hist()

#   --> I want to explore the effect of site on abundance --> Boxplot


#   2. BIOMASS EXPLORATION

read.csv("./Cleaned Bird Data/cleanedbiomass.csv")

#   --> I want to examine the effect of Site on Total --> Boxplot

boxplot(Site ~ Total.mass, data = newbiomass)

#   --> I want to examine the abundances of genus at each site --> Boxplot

#   --> I want to examine the abundances of genus in controlled and enclosed environments --> boxplots


#   3. BODYSIZE EXPLORATION

read.csv("./Cleaned Bird Data/cleanedbodysize.csv")

#   --> I want to examine the effect of treatment type on abundance


