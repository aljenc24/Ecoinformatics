source("./Bird Scripts/Atomization.R")

# Function Writing

delete.columns<- function(dataset,conames,checknames=T)
{
  if(checknames)
  {
    if(sum(!conames%in%colnames(dataset))>0) 
    {warning("Not all column names are present in dataset")}
  }
  dataset<-dataset[,!colnames(dataset)%in%conames]
  return(dataset)
}

test.abun<-delete.columns(cleanedabundance, c("X", "Notes"))



