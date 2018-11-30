# Cargamos los datos y librerías
library(lubridate)
library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)
library(ggpubr)

datos<-read.csv('Datos_Spain.csv',header=TRUE, sep=",")
#summary(datos)

# Nulos por columna.
sapply(datos, function(x) sum(is.na(x)))
datos$Results.First.Posted[datos$Results.First.Posted == "null"] <- NA
datos$Gender[datos$Gender == "null"] <- NA
datos$Locations[datos$Locations == ""] <- NA
datos<- datos[!is.na(datos$Gender),]
datos <- datos[!is.na(datos$Locations),]


# Cambiamos el formato de la fecha

AssingDay <- function(array) {
  
  if(grepl(',', array,fixed = TRUE)==TRUE){
    
    return(array)
  }
  else{
    x = (strsplit(array,' '))
    
    return(paste(x[[1]][1] , '1,' , x[[1]][2]) )
  }
}

datos$Completion.Date = as.character(datos$Completion.Date)
datos$Completion.Date  = lapply(datos$Completion.Date , AssingDay)
datos$Completion.Date = unlist(datos$Completion.Date)
datos$Completion.Date  = as.Date(datos$Completion.Date, format = "%B %d,%Y")


datos$Primary.Completion.Date = as.character(datos$Primary.Completion.Date)
datos$Primary.Completion.Date  = lapply(datos$Primary.Completion.Date  , AssingDay)
datos$Primary.Completion.Date= unlist(datos$Primary.Completion.Date )
datos$Primary.Completion.Date = as.Date(datos$Primary.Completion.Date , format = "%B %d,%Y")

datos$Results.First.Posted = as.character(datos$Results.First.Posted)
datos$Results.First.Posted  = lapply(datos$Results.First.Posted  , AssingDay)
datos$Results.First.Posted= unlist(datos$Results.First.Posted )
datos$Results.First.Posted = as.Date(datos$Results.First.Posted , format = "%B %d,%Y")

# Creamos una nueva columna con la difencia de tiempo en la Primary Completion Date y los resultados publicados
datos$DaysInterval = datos$Results.First.Posted  - datos$Primary.Completion.Date
datos$DaysInterval<-as.numeric(datos$DaysInterval)


# Unificamos los nombres del campo Funded.Bys.
datos$Funded.Bys<-as.character(datos$Funded.Bys)
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Other|Industry', 'Industry|Other')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Other|NIH', 'NIH|Other')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='NIH|Industry', 'Industry|NIH')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Other|NIH|Industry'|datos$Funded.Bys=='Other|Industry|NIH','Industry|Other|NIH')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Industry|NIH|Other','Industry|Other|NIH')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Industry|NIH|Other'|datos$Funded.Bys=='NIH|Other|Industry' ,'Industry|Other|NIH')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='U.S. Fed|Industry'|datos$Funded.Bys=='Industry|U.S. Fed','Industry')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='U.S. Fed|Other'|datos$Funded.Bys== 'Other|U.S. Fed','Other')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Other|Industry|U.S. Fed','Industry|Other')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Other|NIH|U.S. Fed','NIH|Other')
datos$Funded.Bys<-replace(datos$Funded.Bys,datos$Funded.Bys=='Industry|Other|U.S. Fed|NIH','Industry|Other|NIH')


# Pasamos el campo a factor.
datos$Funded.Bys<-as.factor(datos$Funded.Bys)
summary(datos$Funded.Bys)


# Buscar el país y si son estudios multicéntricos.

GetLocations <- function(array) {
  
  if(grepl('|', array,fixed = TRUE)==TRUE){
    
    return("Multicentric")
  }
  else{
  x = (strsplit(array,','))
  
  #print(length(x))
  return(x[[1]][length(x[[1]])])
  }
}



# Buscamos la ciudad
GetCity <- function(array) {
  
  if(grepl('|', array,fixed = TRUE)==TRUE){
    
    return("Multicentric")
  }
  else{
    x = (strsplit(array,','))
    
    #print(length(x))
    return(x[[1]][length(x[[1]])-1])
  }
}

# Creamos las nuevas variable Country y City.
datos$Locations<-as.character(datos$Locations)
datos$Country = lapply(datos$Locations, GetLocations)
datos$City<-lapply(datos$Locations,GetCity)
# Convertimos a vector
datos$Country<-unlist(datos$Country)
datos$City<-unlist(datos$City)
datos$City<-as.factor(datos$City)

# Creamos la columna Multicentric y convertimos los datos a factor.
library(dplyr)
datos$Multicentric<-ifelse(datos$Country =='Multicentric', '1', '0')
datos$Multicentric<-as.factor(datos$Multicentric)
datos$Country<-as.factor(datos$Country)

# ELiminamos las columnas que no queremos.
cols.dont.want <- c("Status", "URL","NCT.Number","Title","Enrollment","Study.Type", "First.Posted", "Last.Update.Posted","Locations") 
datos <- datos[, ! names(datos) %in% cols.dont.want, drop = F]


# Guardamos los datos en un fichero csv.

write.csv(datos, file = "Datos_Spain_final.csv")

summary(datos)

summary(datos$City)
# Se cumple la fecha de publicación?
summary(datos$DaysInterval)
dias <- subset(datos, Study.Results == 'Has Results')
cumplen_fecha <- subset(dias, DaysInterval < 366)
summary(cumplen_fecha$DaysInterval)















