#this Script was used to Clean, Merge, and Output data from Bird Data

# >>> ABUNDANCE CLEANING <<<
View(abundance)
# unique fuction to see whether there are error messeges in notes column
unique(abundance$Notes)
#labeling notes as allnotes                              
allnotes<-unique(abundance$Notes)
allnotes
#labeling Unique Notes as getridof                          
getridof<-allnotes[c(2,3)]
getridof
#graphically separating these two subgroups
abundance$Notes%in%getridof
#swaped TRUE and FALSE to get the computer to recognize which data to remove >>
!abundance$Notes%in%getridof
#removing 2 inputs with comments >>
newabundance<-abundance[!abundance$Notes%in%getridof,]

#testing to see if 2 inputs with comments had been removed >>
nrow(abundance)
# number of rows was = 382
nrow(newabundance)
# number of rows is now = 380

