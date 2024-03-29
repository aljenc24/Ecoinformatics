#Merging Datasets
# --> Key Variable
require(car)
require(multcomp)
require(corrplot)
require(pracma)

source("./Bird Scripts/Atomization.R")
colnames(cleanedbiomass)[colnames(cleanedbiomass)=="EC"]<-"Treatment"

#To identify variation in column titles and change all columns to new title
wrongnames<- c("EC", "Treatment", "sbif", "sjedgflqigrdw")
colnames(cleanedbiomass)[colnames(cleanedbiomass)%in%wrongnames]<-"Treatment"
colnames(cleanedabundance)[colnames(cleanedabundance)=="Ex.Cont.Number"]<-"Number"

#removing unnecessary x column
cleanedabundance<-cleanedabundance[,!colnames(cleanedabundance)=="X"]
cleanedbiomass<-cleanedbiomass[,!colnames(cleanedbiomass)=="X"]
cleanedtrialbodysize<-cleanedtrialbodysize[,!colnames(cleanedtrialbodysize)=="X"]

#Standardizing Treatment type titles
all.treat.effect <-unique(c(cleanedabundance$Treatment, cleanedbiomass$Treatment, cleanedtrialbodysize$Treatment))
Exclosure<-all.treat.effect[c(2,3)]
Control<-all.treat.effect[c(1,4)] 
cleanedabundance$Treatment[cleanedabundance$Treatment%in%Exclosure]<-"Exclosure"
cleanedbiomass$Treatment[cleanedbiomass$Treatment%in%Exclosure]<-"Exclosure"
cleanedtrialbodysize$Treatment[cleanedtrialbodysize$Treatment%in%Exclosure]<-"Exclosure"

cleanedabundance$Treatment[cleanedabundance$Treatment%in%Control]<-"Control"
cleanedbiomass$Treatment[cleanedbiomass$Treatment%in%Control]<-"Control"
cleanedtrialbodysize$Treatment[cleanedtrialbodysize$Treatment%in%Control]<-"Control"

#fixing names using a forloop
oldabunnames<- c("Orthoptera","Hemiptera","Lepidoptera", "Diptera", "Hymenoptera", "Coleoptera", "Thysanoptera", "Amphipoda", "Snail", "Isopoda", "Spider", "Pseudoscorpionida")
newabunnames<-paste0("num.",oldabunnames)

for(i in 1:length(oldabunnames))
{
  colnames(cleanedabundance)[colnames(cleanedabundance)==oldabunnames[i]]<-newabunnames[i]
}

head(cleanedabundance)

#delete notes, exclude columns, Period 1
cleanedabundance<-cleanedabundance[,!colnames(cleanedabundance)=="Notes"]
cleanedbiomass<-cleanedbiomass[,!colnames(cleanedbiomass)=="Notes"]
cleanedbiomass<-cleanedbiomass[,!colnames(cleanedbiomass)=="EXCLUDE"]
cleanedabundance<- cleanedabundance[!cleanedabundance$Period==1,]
cleanedbiomass<-cleanedbiomass[!cleanedbiomass$Period==1,]

#try to play around with the merge function

merged.abun.bio<-merge(cleanedabundance, cleanedbiomass,
      by = c("Site", "Date", "Number", "Treatment"),
      all=FALSE,
      no.dups = FALSE,
      incomparables = NULL) 

# Delete Duplicate Columns

merged.abun.bio<-merged.abun.bio[,!colnames(merged.abun.bio)=="State.y"]
merged.abun.bio<-merged.abun.bio[,!colnames(merged.abun.bio)=="Year.y"]
merged.abun.bio<-merged.abun.bio[,!colnames(merged.abun.bio)=="SolarDay.y"]
merged.abun.bio<-merged.abun.bio[,!colnames(merged.abun.bio)=="Period.y"]

# AbunSite<-cleanedabundance$Site
# BioSite<- cleanedbiomass$Site
# merge(cleanedabundance, cleanedbiomass, # Data frames or objects to be coerced
#       by = intersect("Site", "Site"), # Columns used for merging
#       by.x = by, by.y = by) # Columns used for merging
#       # all = FALSE, # If TRUE, all.x = TRUE and all.y = TRUE
#       # all.x = all, all.y = all, # If TRUE, adds rows for each row in x (y) that not match a row in y (x).
#       # sort = TRUE, # Whether to sort the output by the 'by' columns
#       # suffixes = c(".x",".y"), # Suffixes for creating unique column names
#       # no.dups = TRUE, # Whether to avoid duplicated column names appending more suffixes or not
#       # incomparables = NULL) # How to deal with values that can not be matched


#Homework --> are abundance and biomass related? does more bugs mean more mass? glm (abundance=y, cubed root mass=x, poisson glm) --> correlation (spearman)

bio.abun.model<-glm(Total~nthroot(Total.mass, n=3), data=merged.abun.bio, family=poisson())
summary(bio.abun.model) # Significant Relationship

# Effect of mass on abundance for amphipoda, spiders, snails, and hemiptera

amphi.bio.abun.model<-glm(num.Amphipoda~nthroot(Amphi.mass,n=3), data=merged.abun.bio, family=poisson())
summary(amphi.bio.abun.model)
Anova(amphi.bio.abun.model)

hemi.bio.abun.model<-glm(num.Hemiptera~nthroot(Hemi.mass,n=3), data=merged.abun.bio, family=poisson())
summary(hemi.bio.abun.model)
Anova(hemi.bio.abun.model)

snail.bio.abun.model<-glm(num.Snail~nthroot(Snail.mass,n=3), data=merged.abun.bio, family=poisson())
summary(snail.bio.abun.model)
Anova(snail.bio.abun.model)

spider.bio.abun.model<-glm(num.Spider~nthroot(Spider.mass,n=3), data=merged.abun.bio, family=poisson())
summary(spider.bio.abun.model)
Anova(spider.bio.abun.model)

#  ~ Significant Relationships between these species abundance and mass

Orth.bio.abun.model<-glm(num.Orthoptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Orth.bio.abun.model) # Significant
Hemi.bio.abun.model<-glm(num.Hemiptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Hemi.bio.abun.model) # Significant
Lepi.bio.abun.model<-glm(num.Lepidoptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Lepi.bio.abun.model) # Insignificant effect
Dipt.bio.abun.model<-glm(num.Diptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Dipt.bio.abun.model) # significant and insignificant
Hyme.bio.abun.model<-glm(num.Hymenoptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Hyme.bio.abun.model) # significant and insignificant
Cole.bio.abun.model<-glm(num.Coleoptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Cole.bio.abun.model) # Significant
Thys.bio.abun.model<-glm(num.Thysanoptera~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Thys.bio.abun.model) # significant and insignificant
Amph.bio.abun.model<-glm(num.Amphipoda~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Amph.bio.abun.model) # significant
Snai.bio.abun.model<-glm(num.Snail~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Snai.bio.abun.model) # significant
Isop.bio.abun.model<-glm(num.Isopoda~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Isop.bio.abun.model) # significant and insignificant
Spid.bio.abun.model<-glm(num.Spider~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Spid.bio.abun.model) # significant
Pseu.bio.abun.model<-glm(num.Pseudoscorpionida~nthroot(Spider.mass,n=3)+nthroot(Snail.mass,n=3)+nthroot(Hemi.mass,n=3)+nthroot(Amphi.mass,n=3)+nthroot(Other.mass,n=3), data=merged.abun.bio, family=poisson())
summary(Pseu.bio.abun.model) # significant and insignificant
#make into function

Bio.Abun.Table<-cor(x=merged.abun.bio[,c("Amphi.mass","Hemi.mass","Spider.mass","Snail.mass","num.Orthoptera","num.Hemiptera","num.Lepidoptera", "num.Diptera", "num.Hymenoptera", "num.Coleoptera", "num.Thysanoptera", "num.Amphipoda", "num.Snail", "num.Isopoda", "num.Spider", "num.Pseudoscorpionida")], method="spearman")
write.csv(Bio.Abun.Table, file="./Cleaned Bird Data/BiomassAbundanceCorrTable.csv")
summary(Bio.Abun.Table)
corrplot(Bio.Abun.Table)
hist(Bio.Abun.Table[lower.tri(Bio.Abun.Table)])
#Mostly Positive Correlation 
    # Amphi mass & num --> 0.845
    # Hemi mass & num --> 0.909
    # Snail mass & num --> 0.955
    # Spider mass & num --> 0.673

##High correlation with top bug predator... spider

# PCA

ordered.merged.abun.bio<-apply(merged.abun.bio[,c("Amphi.mass","Hemi.mass","Spider.mass","Snail.mass","num.Orthoptera","num.Hemiptera","num.Lepidoptera", "num.Diptera", "num.Hymenoptera", "num.Coleoptera", "num.Thysanoptera", "num.Amphipoda", "num.Snail", "num.Isopoda", "num.Spider", "num.Pseudoscorpionida")], 2, rank)
head(ordered.merged.abun.bio)
mass.abun.pc<- prcomp(ordered.merged.abun.bio, center=T,scale=T)
summary(mass.abun.pc)
mass.abun.pc
#PC 1 --> when there are lots of bugs there are lots of everything, PC 2--> when there are a lot of aquatic invertebrates there are little to no terrestrial invertebrates

