# this Script was used to Clean, Merge, and Output data from Bird Data

# Questions
# --> how do I save scripts without duplicating them

# >>> ABUNDANCE CLEANING <<<

View(abundance)
# unique function to see whether there are error messeges in notes column
unique(abundance$Notes)
#labeling notes as allnotes                              
allnotes<-unique(abundance$Notes)
allnotes
#labeling Unique Notes as getridof                          
getridof<-allnotes[c(2,3)]
getridof
#graphically separating these two subgroups
abundance$Notes%in%getridof
#changed TRUE and FALSE to get the computer to recognize which data to remove >>
!abundance$Notes%in%getridof
#removing 2 inputs with comments >>
newabundance<-abundance[!abundance$Notes%in%getridof,]

#testing to see if 2 inputs with comments had been removed >>
nrow(abundance)
# number of rows was = 382
nrow(newabundance)
# number of rows is now = 380

write.csv(newabundance,"./Cleaned Bird Data/cleanedabundance.csv")

# >>> BIOMASS CLEANING <<<

#Getting rid of Negative Masses
View(biomass)
biomass$Amphi.mass[biomass$Amphi.mass<0]<-0
biomass$Spider.mass[biomass$Spider.mass<0]<-0
biomass$Hemi.mass[biomass$Hemi.mass<0]<-0
biomass$Snail.mass[biomass$Snail.mass<0]<-0
biomass$Other.mass[biomass$Other.mass<0]<-0

#Deleting Samples that should be Excluded
unique(biomass$Notes)
notes<-unique(biomass$Notes)
delete<-notes[c(2,3,4)]
!biomass$Notes%in%delete
newbiomass<-biomass[!biomass$Notes%in%delete,]
nrow(biomass)
nrow(newbiomass)

write.csv(newbiomass,"./Cleaned Bird Data/cleanedbiomass.csv")

# --> How do I check that the total mass column is accurate?

# >>> BODYSIZE CLEANING <<<
View(bodysize)
unique(bodysize$Notes)
#determine which samples to delete