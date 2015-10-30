getmonitor <- function(id, directory, summarize = FALSE) {
id<-as.numeric(id)
filename<-sprintf("%s/%03d.csv",directory,id)
data<-read.csv(filename)
if (summarize==TRUE)
{print(summary(data))}
return(data)}


excludeSpecificCSV <- function(directory, id = 1:332) {
nobs<-numeric(0)
source("getmonitor.R")
for (id2 in id) {
data <-  getmonitor(id2,directory)
nobs <-c(nobs, nrow(na.omit(data)))}
df <- data.frame(id,nobs)
return(df)
}




