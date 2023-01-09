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
source("./Bird Scripts/Data_Visualiser.R")
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

#Creating New Total Column and Overriding original, incorrect data column
newbiomass$Total.mass <-newbiomass$Amphi.mass+newbiomass$Spider.mass+newbiomass$Hemi.mass+newbiomass$Snail.mass+newbiomass$Other.mass


#Creating New Cleaned Biomass Data Set
write.csv(newbiomass,"./Cleaned Bird Data/cleanedbiomass.csv")

#Creating New Dataset to visually test if data has been corrected
hist(newbiomass$Total.mass,main="Biomass",ylab = "Frequency",breaks=50,xlab="Total Mass",xlim=c(0,3.5))

# >>> BODYSIZE CLEANING <<<
View(bodysize)
unique(bodysize$Notes)
bodysize.notes<-unique(bodysize$Notes)
bodydelete<-bodysize.notes[c(7)]
!bodysize$Notes%in%bodydelete
newbodysize<-bodysize[!bodysize$Notes%in%bodydelete,]
nrow(bodysize)
nrow(newbodysize)

#Correcting errors found by Zach
unique(bodysize$EXCLUDE)
bodysize.exclude<-unique(bodysize$Notes)
body.exclude.delete<-bodysize.exclude[c(2,3,4)]
!bodysize$EXCLUDE%in%body.exclude.delete
newbodysize<-bodysize[!bodysize$EXCLUDE%in%body.exclude.delete,]
nrow(bodysize)
nrow(newbodysize)

#>>> I want to correct the errors you outlines in the EXCLUDE column but 
#>it wont let me delete the rows you created using the unique function. 
#>It just comes up with no false values. What do I need to change or specify 
#>for the computer to understand my command

#Creating New Cleaned Body Size Data Set
write.csv(newbodysize,"./Cleaned Bird Data/cleanedbodysize.csv")
