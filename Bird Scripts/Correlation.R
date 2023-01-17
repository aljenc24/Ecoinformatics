require(corrplot)

### CORRELATION FOR ABUNDANCE DATASET

plot(Diptera~Hemiptera, data=cleanedabundance)

#   Pearson Correlation --> Is there relation between the values of X1 and X2

#   OTHER KINDS OF CORRELATION --> 
#   Spearman Rank --> not if the two values of variables are correlated but whether or not their ranks are correlated (does not need to be normal or linear)
#     --> must be monotomic (single directionality, if it has positive and negative slope it is not monotomic)
plot(order(Diptera)~order(Hemiptera), data=cleanedabundance)
Taxa.Corr.Table<-cor(x=cleanedabundance[,c("Orthoptera","Hemiptera","Lepidoptera", "Diptera", "Hymenoptera", "Coleoptera", "Thysanoptera", "Amphipoda", "Snail", "Isopoda", "Spider", "Pseudoscorpionida")], method="spearman")
write.csv(Taxa.Corr.Table, file="./Cleaned Bird Data/TaxaCorrelationTable.csv")
hist(Taxa.Corr.Table[lower.tri(Taxa.Corr.Table)])
#Bottom up Trophic Cascade? Minimal negative correlation

corrplot(Taxa.Corr.Table)

cleanedabund.ordered<-apply(cleanedabundance[,c("Orthoptera","Hemiptera","Lepidoptera", "Diptera", "Hymenoptera", "Coleoptera", "Thysanoptera", "Amphipoda", "Snail", "Isopoda", "Spider", "Pseudoscorpionida")], 2, rank)

pairs(cleanedabund.ordered,pch=16,cex=0.2)

### CORRELATION FOR BIOMASS DATASET
cleanedbiomass<-read.csv("./Cleaned Bird Data/cleanedbiomass.csv")
plot(order(Amphi.mass)~order(Hemi.mass), data=cleanedbiomass)
Taxa.Mass.Corr.Table<-cor(x=cleanedbiomass[,c("Amphi.mass","Hemi.mass","Spider.mass","Snail.mass")], method="spearman")
write.csv(Taxa.Mass.Corr.Table, file="./Cleaned Bird Data/TaxaBiomassCorrelationTable.csv")
corrplot(Taxa.Mass.Corr.Table)
#ONLY Positive Correlation w/ Biggest correlation between amphi.mass and spider.mass
cleaned.mass.ordered<-apply(cleanedbiomass[,c("Amphi.mass","Hemi.mass","Spider.mass","Snail.mass")], 2, rank)
pairs(cleaned.mass.ordered,pch=16)

### CORRELATION FOR BODYSIZE DATASET
cleanedbodysize<-read.csv("./Cleaned Bird Data/_________.csv")
plot(order()~order(), data=)
Taxa.Size.Corr.Table<-cor(x=cleanedbodysize[,c()], method="spearman")
write.csv(Taxa.Mass.Corr.Table, file="./Cleaned Bird Data/TaxaBiomassCorrelationTable.csv")
corrplot(Taxa.Mass.Corr.Table)

