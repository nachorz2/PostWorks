##Postwork
#Trayendo librerias necesarias en todo el script
library(devtools)
library(dplyr)
library(ggplot2)
library(plotly)
library(boot)
library(fbRanks)

##Este dato es para poner tu liga
#setwd("C:/Users/Tere/Documents/Nacho/CursopythonBEDU/files")
#PostWork Sesión 1#####

#Lugar de desstino 

SP11920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
download.file(url = SP11920, destfile = "SP1-2019-20.csv", mode = "wb")


#Leyendo CSV
datasp1<-lapply("SP1-2019-20.csv", read.csv)
data <- do.call(rbind, datasp1)


#Creando Data Frame con los datos que queremos únicamente
goles <- select(data, FTHG, FTAG)
goles
#Encontrando Frecuencias
frecuencia<- table(goles)
df<-data.frame(frecuencia)
#Encontrando total de datos
sum(df$Freq)
df

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
ld<-table(goles$FTHG)
ldf<-data.frame(ld)
ldf <- mutate(ldf, pm = Freq/sum(Freq))
ldf <- rename(ldf,  FTHG=Var1)
ldf
#Probabilidad marginal goles visitante 
vis<-table(goles$FTAG)
visdf<-data.frame(vis)
visdf<-mutate(visdf, pm = Freq/sum(Freq))
visdf <- rename(visdf,  Golvis = FTHG )
visdf
#Probabilidad conjunta
frecuencia<- table(goles)
df<-data.frame(frecuencia)
sum(df$Freq)
df <- mutate(df, probabilidad = Freq/sum(Freq))
df <- rename(df, Gloc = FTHG, Gvis = FTAG)
df


#PostWork Sesión 2#####

s11718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
s11819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
#Previamente descargado
#s11920 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"

#Descargando Archivos
download.file(url = s11718, destfile = "SP1-11718.csv", mode = "wb")
download.file(url = s11819, destfile = "SP1-11819.csv", mode = "wb")

#Leyendo  ccv
s1718 <- read.csv("SP1-11718.csv")
s1819 <-read.csv("SP1-11819.csv")
#temporada 19-20 es "data"
data
#Analizando BD's
str(s1718); str(s1819);str(data)
head(s1718); head(s1819); head(data)
view(s1718);view(s1819);view(data)
summary(s1718);summary(s1819);summary(data)
#Trayendo Datos Necesarios
s1718f<-select(s1718,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
s1718f <- mutate(s1718f, Date = as.Date(Date, "%d/%m/%y"))

s1819f<-select(s1819,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
s1819f <- mutate(s1819f, Date = as.Date(Date,"%d/%m/%Y"))
summary(s1819f)
head(s1819f)

s1920f<-select(data,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
s1920f<- mutate(s1920f, Date = as.Date(Date, "%d/%m/%Y"))
summary(s1920f)


head(s1920f)
str(s1920f)
#Uniendo Data Frames
s171819<-rbind(s1718f,s1819f,s1920f)
str(s171819)
#Cambiando Formato Fecha
#s171819<-mutate(s171819, Date = as.Date(Date, "%d/%m/%y"))
summary(s171819)

#Postwork Sesion 3####


#Creando Data Frame con los datos que queremos únicamente
goles <- select(s171819, FTHG, FTAG)
goles
#Encontrando Frecuencias
frecuencia<- table(goles)
df<-data.frame(frecuencia)
#Encontrando total de datos
sum(df$Freq)
df

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
ld<-table(goles$FTHG)
ldf<-data.frame(ld)
ldf <- mutate(ldf, pm = Freq/sum(Freq))
ldf <- rename(ldf,  FTHG=Var1)
ldf
#Probabilidad marginal goles visitante 
vis<-table(goles$FTAG)
visdf<-data.frame(vis)
visdf<-mutate(visdf, pm = Freq/sum(Freq))
visdf <- rename(visdf,  Golvis = Var1 )
visdf
#Probabilidad conjunta
frecuencia<- table(goles)
df<-data.frame(frecuencia)
sum(df$Freq)
df <- mutate(df, probabilidad = Freq/sum(Freq))
df <- rename(df, Gloc = FTHG, Gvis = FTAG)
df


#Gráfico BARRAS Probabilidad de que un Visitante Anote x Goles
ggplotly(ggplot(data=visdf, aes(x=Golvis, y=pm, fill=pm)) + 
           geom_bar(stat="identity", position="dodge")+
           ylab("Probabilidad")+
           xlab("# Goles")+
           ggtitle("Probabilidad de que un visitante anote"))
#Gráfico BARRAS Probabilidad de que un Local Anote x Goles  
ggplotly(ggplot(data=ldf, aes(x=reorder(FTHG,-pm), y=pm, fill=pm)) + 
           geom_bar(stat="identity", position="dodge")+
           ylab("Probabilidad")+
           xlab("# Goles")+
           ggtitle("Probabilidad de que un Local anote"))
#Heat MAP Probabilidad MArginal de que ambos equipos Anoten x Goles
ggplotly(ggplot(df, aes(x = Gloc, y = Gvis, fill = probabilidad)) + geom_tile()+
           ggtitle("HeatMap Goles")+
           ylab("Goles Visitante")+
           xlab("Goles Local"))

#Para verificar que la probabilidad es correcta la suma
#De probabilidades es igual a uno
sum(visdf$pm)
sum(df$probabilidad)
sum(ldf$pm)

#Postwork Sesion 4#####
df
ldf
visdf
(mutate(df, probvis = visdf$pm))

(merge(x = df, y = visdf,all = TRUE))
?merge

cocientes <- (merge(df, visdf, by.x = "Gvis", by.y = "Golvis", all = TRUE))
cocientes <- (select(cocientes, Gvis, Gloc, Freq.x, probabilidad,pm ))
cocientes <- rename(cocientes,conjunta =  probabilidad ,  margvis= pm)
cocientes <- merge(cocientes, ldf, by.x = "Gloc", by.y = "FTHG")
cocientes <- select (cocientes, Gloc, Gvis, Freq.x, conjunta, margvis, pm)
cocientes <- rename(cocientes, margloc = pm)
cocientes <- (mutate(cocientes, cociente = conjunta/(margvis*margloc)))
cocientes
filter(cocientes, cociente > 0.9 & cociente < 1.2)
mean(cocientes$cociente)
sd(cocientes$cociente)


library(boot)

nboot <- 1000
stat.boot <- numeric(nboot)
for (i in 1:nboot) {
  dat.boot <- sample(cocientes$cociente, replace=TRUE)
  stat.boot[i] <- mean(dat.boot)
}
hist(stat.boot)

mean(stat.boot)


pnorm(1.01, mean = mean(stat.boot),sd = sd(stat.boot)) -pnorm(0.99, mean = mean(stat.boot),sd = sd(stat.boot))


#Postwork sesion 5####

#1
SmallData <- select(s171819, Date, HomeTeam, AwayTeam, FTHG, FTAG)
SmallData <- rename(SmallData, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)
str(SmallData)
write.csv(SmallData, "soccer.csv", row.names = FALSE)

#2Con la función create.fbRanks.dataframes del paquete fbRanks 
#importe el archivo soccer.csv a R y al mismo tiempo asignelo a una 
#variable llamada listasoccer. Se creará una lista con los elementos 
#scores y teams que son data frames listos para la función rank.teams. 
#Asigna estos data frames a variables llamadas anotaciones y equipos.
?create.fbRanks.dataframes
listasoccer <- create.fbRanks.dataframes("soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

#3 Con ayuda de la función unique crea un vector de fechas (fecha) 
#que no se repitan y que correspondan a las fechas en las que 
#se jugaron partidos. Crea una variable llamada n que contenga 
#el número de fechas diferentes. Posteriormente, con la función rank.teams y 
#usando como argumentos los data frames anotaciones y equipos, 
#crea un ranking de equipos usando únicamente datos desde 
#la fecha inicial y hasta la penúltima fecha en la que se jugaron partidos, 
#estas fechas las deberá especificar en max.date y min.date. 
#Guarda los resultados con el nombre ranking.

?unique
fecha <- unique(anotaciones$date)
n <- length(fecha)
?rank.teams()
ranking <- rank.teams(scores = anotaciones, teams = equipos, max.date = fecha[n-1], min.date = fecha[1])

#4Finalmente estima las probabilidades de los eventos, el equipo 
#de casa gana, el equipo visitante gana o el resultado es un empate 
#para los partidos que se jugaron en la última fecha del vector de fechas fecha.
#Esto lo puedes hacer con ayuda de la función predict y usando como argumentos
#ranking y fecha[n] que deberá especificar en date.

?predict()            
## result <- predict(ranking, max.date = fecha[n], min.date = fecha[1] )
###??????
#Postwork sesion 6####
dir()
library(lubridate)
soccer <- SmallData
#Agregando suma de goles 
soccern<-  mutate(soccer, sumagoles = home.score + away.score,mes = month(date), año = year(date), dia = 1)
#Separando fecha para obtener solo mes y año
soccern<- mutate(soccern, mes= as.character(mes), año = as.character(año), dia = as.character(dia))
#Añadiendo columna nueva de fecha
soccern <- mutate(soccern, fecha = paste(año,mes,dia, sep = "/"))
#Agrupando por fecha y obteniendo promedio
promedio<- soccern %>%  group_by(fecha)%>%
  summarise(mean = mean(sumagoles))
#Dando formto de fecha a columna 
promedio<-mutate(promedio, fecha = as.Date(fecha, "%Y/%m/%d"))
#Ordenando por fecha 
promedio <- arrange(promedio, fecha)
#Creando Time Series del promedio
time<- ts(promedio$mean)
#Ploteando
plot(time)