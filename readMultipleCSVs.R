getmonitor <- function(id, directory, summarize = FALSE) {
id<-as.numeric(id)
filename<-sprintf("%s/%03d.csv",directory,id)
data<-read.csv(filename)
if (summarize==TRUE)
{print(summary(data))}
return(data)}
