#Merging Datasets
# --> Key Variable

source("./Bird Scripts/Atomization.R")
colnames(cleanedbiomass)[colnames(cleanedbiomass)=="EC"]<-"Treatment"

#To identify variation in column titles and change all columns to new title
# wrongnames<- c("EC", "Treatment", "sbif", "sjedgflqigrdw")
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

#fixing names usign a forloop
oldabunnames<- c("Orthoptera","Hemiptera","Lepidoptera", "Diptera", "Hymenoptera", "Coleoptera", "Thysanoptera", "Amphipoda", "Snail", "Isopoda", "Spider", "Pseudoscorpionida")
newabunnames<-paste0("num.",oldabunnames)

for(i in 1:length(oldabunnames))
{
  colnames(cleanedabundance)[colnames(cleanedabundance)==oldabunnames[i]]<-newabunnames[i]
}

head(cleanedabundance)

#try to play around with the merge function --> baller
# --> delete notes and excludes columns



