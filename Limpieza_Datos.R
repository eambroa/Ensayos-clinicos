# Cargamos los datos y librerías necesarias
library(lubridate)
library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)
library(ggpubr)

datos<-read.csv('Datos.csv',header=TRUE, sep=",")
# summary(datos)

# Nulos por columna.
sapply(datos, function(x) sum(is.na(x)))
datos$Results.First.Posted[datos$Results.First.Posted == "null"] <- NA
datos$Gender[datos$Gender == "null"] <- NA
datos$Locations[datos$Locations == ""] <- NA
datos<- datos[!is.na(datos$Gender),]
datos <- datos[!is.na(datos$Locations),]

# ELiminamos las columnas que no queremos.
cols.dont.want <- c("Status", "URL","NCT.Number","Title","Enrollment") 
datos <- datos[, ! names(datos) %in% cols.dont.want, drop = F]

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

#Nos quedamos con los datos que tiene más de 10 ocurrencias en cada nivel.
datos <-  datos %>%
  group_by(datos$Funded.Bys) %>%
  filter(n() >10) %>% 
  droplevels()
cols.dont.want <- c("datos$Funded.Bys") 
datos <- datos[, ! names(datos) %in% cols.dont.want, drop = F]


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


datos$Locations<-as.character(datos$Locations)
# Creamos la nueva variable Country.
datos$Country = lapply(datos$Locations, GetLocations)

# Convertimos a vector
datos$Country<-unlist(datos$Country)

# Creamos la columna Multicentric y convertimos los datos a factor.

datos$Multicentric<-ifelse(datos$Country =='Multicentric', '1', '0')
datos$Multicentric<-as.factor(datos$Multicentric)
datos$Country<-as.factor(datos$Country)

# Eliminamos la columna 'Locations' ya que ya no hace falta
cols.dont.want <- c("Locations") 
datos <- datos[, ! names(datos) %in% cols.dont.want, drop = F]

# Guardamos los datos en un fichero csv.
write.csv(datos, file = "Datos_finales.csv")

summary(datos)


# Gráficos

# Agrupamos por tipo de financiación.
datos_filter <- datos %>%
  group_by(Study.Results, Funded.Bys) %>%
  summarise(counts = n()) 


p <- ggplot(datos_filter, aes(x = Funded.Bys, y = counts)) +
  geom_bar(
    aes(color = Study.Results, fill = Study.Results),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) +coord_flip()

p+geom_text(
  aes(label = counts, group = Study.Results), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5)+labs(x = "Tipo de financiación",y='Número de estudios', 
                                 title='Publicación de resultados según el tipo de financiación')+
  theme(plot.title = element_text(hjust = 0.5))
  

# Agrupamos por tipo de fase.
datos_filter2 <- datos %>%
  group_by(Study.Results,Phases) %>%
  summarise(count=n()) 

head(datos_filter2,20)

# plot
a <- ggplot(datos_filter2, aes(x =Phases, y = count)) +
  geom_bar(
    aes(color = Study.Results, fill = Study.Results),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7)

a+geom_text(
  aes(label = count, group = Study.Results), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5)+labs(x = "Tipo de fase", y='Número de estudios', 
                                 title='Publicación de resultados según el tipo de fase')+
  theme(plot.title = element_text(hjust = 0.5))

# Agrupamos por género.
datos_filter3 <- datos %>%
  group_by(Study.Results, Gender) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)*100)
 

# plot
b <- ggplot(datos_filter3, aes(x =Gender, y = perc)) +
  geom_bar(
    aes(color = Study.Results, fill = Study.Results),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 
b
b+geom_text(
  aes(label = count, group = Study.Results), 
  position = position_dodge(0.8),
  vjust = -0.3, size = 3.5)+labs(x = "Género", y='Porcentaje (%)', 
                                 title='Publicación de resultados según género')+
  theme(plot.title = element_text(hjust = 0.5))

# Agrupamos por 'género'Multicentric'.
datos_filter4 <- datos %>%
  group_by(Study.Results, Multicentric) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)*100)


# plot
c <- ggplot(datos_filter4, aes(x =Multicentric, y = perc)) +
  geom_bar(
    aes(color = Study.Results, fill = Study.Results),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

c+labs(x = "Multicéntrico", y='Porcentaje (%)', 
                                 title='Publicación de resultados según si son Multicéntricos')+
  theme(plot.title = element_text(hjust = 0.5))



# Características de los ensayos clínicos.


# Agrupamos los estudios según tengan resultados o no.
results <- subset(datos, Study.Results == 'Has Results')
summary(results)
no_results <- subset(datos, Study.Results == 'No Results Available')

# Características de los estudios con resultados
datos_filter5 <- results %>%
  group_by(Study.Results, Multicentric) %>%
  summarise(count=n())

# plot
a <- ggplot(datos_filter5, aes(x =Multicentric, y = count)) +
  geom_bar(
    aes(color = Multicentric, fill = Multicentric),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

A<-a+labs(x = "Multicéntrico", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")

A

datos_filter6 <- results %>%
  group_by(Study.Results, Gender) %>%
  summarise(count=n())


b <- ggplot(datos_filter6, aes(x =Gender, y = count)) +
  geom_bar(
    aes(color = Gender, fill = Gender),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

B<-b+labs(x = "Género", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")

datos_filter7 <- results %>%
  group_by(Study.Results, Phases) %>%
  summarise(count=n())

c <- ggplot(datos_filter7, aes(x =Phases, y = count)) +
  geom_bar(
    aes(color = Phases, fill = Phases),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

C<-c+labs(x = "Fase", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")


datos_filter8 <- results %>%
  group_by(Study.Results, Funded.Bys) %>%
  summarise(count=n())

d <- ggplot(datos_filter8, aes(x =Funded.Bys, y = count)) +
  geom_bar(
    aes(color = Funded.Bys, fill = Funded.Bys),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 
D<-d +labs(x = "Tipo financiación", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")

D+ggtitle(label='Tipo de financiación de los ensayos clínicos con resultados')

grid.arrange(A, B,C,D, ncol=1)  




# Características de los estudios sin resultados
datos_filter9 <- no_results %>%
  group_by(Study.Results, Multicentric) %>%
  summarise(count=n())

# plot
a <- ggplot(datos_filter9, aes(x =Multicentric, y = count)) +
  geom_bar(
    aes(color = Multicentric, fill = Multicentric),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

A<-a+labs(x = "Multicéntrico", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")

datos_filter10 <- no_results %>%
  group_by(Study.Results, Gender) %>%
  summarise(count=n())


b <- ggplot(datos_filter10, aes(x =Gender, y = count)) +
  geom_bar(
    aes(color = Gender, fill = Gender),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

B<-b+labs(x = "Género", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")

datos_filter11 <- no_results %>%
  group_by(Study.Results, Phases) %>%
  summarise(count=n())

c <- ggplot(datos_filter11, aes(x =Phases, y = count)) +
  geom_bar(
    aes(color = Phases, fill = Phases),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 

C<-c+labs(x = "Fase", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")


datos_filter12 <- no_results %>%
  group_by(Study.Results, Funded.Bys) %>%
  summarise(count=n())

d <- ggplot(datos_filter12, aes(x =Funded.Bys, y = count)) +
  geom_bar(
    aes(color = Funded.Bys, fill = Funded.Bys),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7) 
D<-d +labs(x = "Tipo financiación", y='Número de estudios')+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")


ggarrange(A, B,C,D, ncol=1, nrow=4, common.legend = FALSE)



