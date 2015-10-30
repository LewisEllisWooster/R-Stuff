#Following Functions parse HTML from homicides.txt 




cause<- function(cause=NULL) {

if(cause!='Asphyxiation' & cause!='Blunt Force' & cause!='Other' & cause!='Shooting' & cause!='Stabbing' & cause!='Unknown' 
stop('invalid cause')
homicides<-readLines("homicides.txt")
return(head(homicides))
}

##1st Count function
count<- function(cause=NULL) {
homicides<-readLines("homicides.txt")
if(is.null(cause))
{stop("error: no cause of death")}

if(cause!=grep('[Aa]sphyxiation',homicides,values=TRUE) & cause!='[Bb]lunt [Ff]orce' & cause!='[Oo]ther' & cause!='[Ss]hooting' & cause!='[Ss]tabbing' & cause!='[Uu]nknown') 
{stop('invalid cause')}

return(head(homicides))

}


count1<- function(cause=NULL) {

cause<-sprintf("%s",cause)

if(is.null(cause))
{stop("error: no cause of death")}

W<-grepl("[Aa]sphyxiation",cause)| grepl("[Bb]lunt [Ff]orce",cause)| grepl("[Oo]ther",cause)| grepl("[Ss]hooting",cause)|
grepl("[Ss]tabbing",cause)| grepl("[Un]known",cause)

if(W==FALSE)
{stop("invalid cause")}
homicides<-readLines("homicides.txt")
length(grep(sprintf("Cause: %s",cause), homicides,ignore.case=TRUE))
}






agecount <- function(age = NULL) {

if(is.null(age))
{stop("error: no age")}

homicides<-readLines("homicides.txt")
r<-regexpr(".[1-9]+ [Yy]ears [Oo]ld",homicides)
u<-regmatches(homicides,r)
k<-gsub(" years old|","",u)
q<-as.numeric(k)
df<-data.frame(q)
age.count<-subset(df,df[,1]==age)
agecoun<-nrow(age.count)
return(agecoun)
}




count2<- function(cause=NULL) {

cause<-sprintf("%s",cause)

if(is.null(cause))
{stop("error: no cause of death")}

W<-grepl("[Aa]sphyxiation",cause)| grepl("[Bb]lunt [Ff]orce",cause)| grepl("[Oo]ther",cause)| grepl("[Ss]hooting",cause)|
grepl("[Ss]tabbing",cause)| grepl("[Un]known",cause)

if(W==FALSE)
{stop("invalid cause")}
homicides<-readLines("homicides.txt")
length(grep(sprintf("Cause: %s",cause), homicides,ignore.case=TRUE))
}




