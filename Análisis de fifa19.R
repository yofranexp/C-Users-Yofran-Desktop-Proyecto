library(tidyverse) 
library(ggplot2)
library(magrittr)
library(maps)
library(plotly)
library(stringr)
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(parallel)
library(doParallel)
library(DT) # no
library(tidytext) # no
library(gridExtra) # no 
library(factoextra) # no
library(stringr)


# Se hará un analisis exploratorio

fifa19 <- read.csv(file.choose(), header = T, sep = ",")

# Veamos la estructura de la data
glimpse(fifa19)

# Podemos observar que existen valores que contiene caracteres pocos comunes
# por lo tanto se realizará un limpieza de algunas variables,
# y algunas transformaciones para iniciar el análisis exploratorio

datos <- fifa19
# variables tales como;
# wage, height, weight, value

# value

datos$Value <- str_replace(datos$Value,"â,¬", "")
datos$Value

# ahora vamos con la "m"

valores_sin_m <- datos %>%
  filter(Value == str_count(Value, "M"))  # Codigo  necesario para detectar las M


datos_work <- datos %>%
  filter(Value != str_count(Value, "M"))

View(datos_work)
# eliminar los m

datos_work$Value <- str_sub(datos_work$Value,1,nchar(datos_work$Value)-1)
datos_work$Value

# Transformar

datos_work$Value <- as.numeric(datos_work$Value)
datos_work$Value # recordar multiplicar por un millon

# wage

datos_work$Wage <- str_replace(datos_work$Wage,"â,¬", "")
datos_work$Wage

# los valores que contienen K
glimpse(datos_work)


datos_work$Wage <- str_sub(datos_work$Wage,1,nchar(datos_work$Wage)-1)
datos_work$Wage

# transformar los valores
datos_work$Wage <- as.numeric(datos_work$Wage)
datos_work$Wage
# recordar multiplicar por mil


# valores duplicados
sum(duplicated(datos_work)) # No existen valores duplicados


# weight

datos_work$Weight <- str_replace(datos_work$Weight,"lbs", "")
datos_work$Weigh

# Transformacion
datos_work$Weigh <- as.numeric(datos_work$Weigh)
datos_work$Weigh

#height
# la medida de la altura está en pies
datos_work$Height <- str_replace_all(datos_work$Height,"'", ".")
datos_work$Height
datos_work$Height <- as.numeric(datos_work$Height)
datos_work$Height

# recordar transformarlo a metros

# estructura de la data
glimpse(datos_work)

# variables que no se utilizaran

datos_fifa <- datos_work %>%  
  select(-ID, -Body.Type, -Real.Face, -Joined, -Loaned.From, -Release.Clause, -Photo, -Flag, -Special, -Work.Rate)

# Crear las ligas segun a los clubes que pertenezcan los jugadores

bundesliga <- c(
  "1. FC Nürnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern München",
  "Borussia Dortmund", "Borussia Mönchengladbach", "Eintracht Frankfurt",
  "FC Augsburg", "FC Schalke 04", "Fortuna Düsseldorf", "Hannover 96",
  "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
  "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
)
premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
  
)
laliga <- c(
  "Athletic Club de Bilbao", "Atlético Madrid", "CD Leganés",
  "Deportivo Alavés", "FC Barcelona", "Getafe CF", "Girona FC", 
  "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
  "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
)

seriea <- c(
  "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
  "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
  "Torino","Udinese"
  
)

superlig <- c(
  "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol Basaksehir FK","BB Erzurumspor","Besiktas JK",
  "Bursaspor","Çaykur Rizespor","Fenerbahçe SK", "Galatasaray SK","Göztepe SK","Kasimpasa SK",
  "Kayserispor","Atiker Konyaspor","MKE Ankaragücü", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
)

bundesliga <- c(
  "1. FC Nürnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern München",
  "Borussia Dortmund", "Borussia Mönchengladbach", "Eintracht Frankfurt",
  "FC Augsburg", "FC Schalke 04", "Fortuna Düsseldorf", "Hannover 96",
  "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
  "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
)

premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
  
)

laliga <- c(
  "Athletic Club de Bilbao", "Atlético Madrid", "CD Leganés",
  "Deportivo Alavés", "FC Barcelona", "Getafe CF", "Girona FC", 
  "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
  "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
)

seriea <- c(
  "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
  "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
  "Torino","Udinese"
  
)

superlig <- c(
  "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol Basaksehir FK","BB Erzurumspor","Besiktas JK",
  "Bursaspor","Çaykur Rizespor","Fenerbahçe SK", "Galatasaray SK","Göztepe SK","Kasimpasa SK",
  "Kayserispor","Atiker Konyaspor","MKE Ankaragücü", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
)

ligue1 <- c(
  "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Étienne", "Dijon FCO", "En Avant de Guingamp",
  "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "Nîmes Olympique", 
  "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
  "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
)

eredivisie <- c(
  "ADO Den Haag","Ajax", "AZ Alkmaar", "De Graafschap","Excelsior","FC Emmen","FC Groningen",
  "FC Utrecht", "Feyenoord","Fortuna Sittard", "Heracles Almelo","NAC Breda",
  "PEC Zwolle", "PSV","SC Heerenveen","Vitesse","VVV-Venlo","Willem II"
)

liganos <- c(
  "Os Belenenses", "Boavista FC", "CD Feirense", "CD Tondela", "CD Aves", "FC Porto",
  "CD Nacional", "GD Chaves", "Clube Sport Marítimo", "Moreirense FC", "Portimonense SC", "Rio Ave FC",
  "Santa Clara", "SC Braga", "SL Benfica", "Sporting CP", "Vitória Guimarães", "Vitória de Setúbal"
)

datos_fifa <- datos_fifa %>%
  mutate(Ligas = case_when(
    Club %in% bundesliga ~ "Bundesliga",
    Club %in% premierLeague ~ "Premier League",
    Club %in% laliga ~ "La Liga",
    Club %in% seriea ~ "Serie A",
    Club %in% superlig ~ "Süper Lig",
    Club %in% ligue1 ~ "Ligue 1",
    Club %in% liganos ~ "Liga Nos",
    Club %in% eredivisie ~ "Eredivisie"
    ),
    país = case_when(
      Ligas == "Bundesliga" ~ "Germany",
      Ligas == "Premier League" ~ "UK",
      Ligas == "La Liga" ~ "Spain",
      Ligas == "Serie A" ~ "Italy",
      Ligas == "Süper Lig" ~ "Turkey",
      Ligas == "Ligue 1" ~ "France",
      Ligas == "Liga Nos" ~ "Portugal", 
      Ligas == "Eredivisie" ~ "Netherlands")) %>%
  filter(!is.na(Ligas)) %>%
  mutate_if(is.factor, as.character())

# transformar los datos wage y value y height

datos_fifa <- datos_fifa %>%
  mutate(Value = Value*1000000,
         Wage = Wage*1000,
         Height = round(Height/3.2808,2))

# ligas, paises de las ligas y pierna natural
# trabajar como factores


datos_fifa$país <- as.factor(datos_fifa$país)
datos_fifa$país
datos_fifa$Ligas <- as.factor(datos_fifa$Ligas)
datos_fifa$Ligas

datos_fifa$Preferred.Foot <- as.factor(datos_fifa$Preferred.Foot)
datos_fifa$Preferred.Foot

# veamos la cantidad de preferencia de pie, con respeto a las liga
levels(datos_fifa$Preferred.Foot)


datos_fifa <- datos_fifa %>% 
  filter(Preferred.Foot != "") # limpieza de data


# agregar un titulo
tabla_foot <- datos_fifa %>%
  group_by(Ligas, Preferred.Foot) %>%
  count()

ggplot(datos_fifa, aes(Preferred.Foot, fill = país))+
  geom_bar()+
  facet_grid(~país)+
  labs(x= "Pierna Natural", y = "Cantidad",
       title = "Cantidad de Jugadores de Pierna Natural con Respecto
       al País")

# analizar el grafico




# Nivel de overall segun la liga

datos_fifa %>%
  group_by(Ligas) %>%
  summarize(Promedio_overall = mean(Overall)) %>%
  ggplot(aes(reorder(Ligas, Promedio_overall), Promedio_overall, fill = Promedio_overall))+
  geom_col(show.legend = F)+
  coord_flip()+
  theme_minimal()+
  scale_fill_gradient(low = "#42b3f5", high = "#4272f5")+
  labs(x = "Overall Promedio", y = "Ligas de Fútbol",
       title = "Overall Promedio de las Liga del Mundo (2019)") # Hacer analisis
# Valor de las ligas

datos_fifa %>%
  group_by(Ligas) %>%
  summarize(Promedio_Value = sum(Value)) %>%
  ggplot(aes(reorder(Ligas, Promedio_Value), Promedio_Value, fill = Promedio_Value))+
  geom_col(show.legend = F)+
  coord_flip()+
  theme_minimal()+
  scale_fill_gradient(low = "#42b3f5", high = "#4272f5")+
  labs(x = "Value", y = "Ligas de Fútbol(Miles de millones)",
       title = "Value de las Liga del Mundo (2019)")+
  scale_y_continuous(labels = c("0 ???", "45 ???", "90 ???", "135 ???"))
# analizar 


# graficos de densidad
ggplot(datos_fifa, aes(Overall, fill = Ligas))+
  geom_density(show.legend = F)+
  theme_minimal()+
  facet_grid(Ligas~.)+
  scale_fill_manual(values = c(rep("#42ddf5", 8)))+
  labs(title = "Gráficos de Densidad las Mejores Ligas del Mundo (2019)")
 # Analizar los graficos de densidad de las ligas 


# Boxplot
ggplot(datos_fifa, aes(Ligas, Overall, fill = Ligas))+
  geom_boxplot(show.legend = F)+
  theme_minimal()+
  scale_fill_manual(values = c("#42ddf5", "#42ddf5","#42ddf5","#42ddf5",
                               "#42ddf5","#42ddf5", "#42ddf5", "#42ddf5"))+
  labs(title = "Box-plot Overall")

# relacion del salario entre Overall
options(scipen=9999) # para trabajar con enteros
ggplot(datos_fifa, aes(Overall, Wage, color = Ligas))+
  geom_point()+
  theme_minimal()+
  facet_grid(~Ligas)+
  labs(title = "Gráfico de Dipersión (Overall vs Wage)") # Hacer el analisis correspondiete

# Tabla que ayudara con la interpretación

datos_fifa %>%
  group_by(Ligas) %>%
  summarize(Correlación = cor(Overall, Wage))


# Comparemos dos ligas 

ligas <- datos_fifa %>%
  filter(Ligas %in% c("Premier League", "La Liga")) %>%
  select(Ligas,Crossing:SlidingTackle) %>%
  group_by(Ligas) %>%
  summarise(Crossing = mean(Crossing),
            Finishing = mean(Finishing),
            HeadingAccuracy = mean(HeadingAccuracy),  # variables mas relevantes
            ShortPassing = mean(ShortPassing),
            Volleys = mean(Volleys),
            Dribbling= mean(Dribbling),
            Curve = mean(Curve),
            Agility = mean(Agility),
            Aggression = mean(Aggression),
            BallControl = mean(BallControl)) %>%
  gather(Skill, Exp, Crossing:`BallControl`, -Ligas)
ligas

head(ligas)

vs <- datos_work%>%
  filter(Club %in% c("Tottenham Hotspur","Liverpool"))%>%
  select(Club,Crossing:SlidingTackle) %>%
  group_by(Club) %>%
  summarise(Crossing = mean(Crossing),
            Finishing = mean(Finishing),
            HeadingAccuracy = mean(HeadingAccuracy),  # variables mas relevantes
            ShortPassing = mean(ShortPassing),
            Volleys = mean(Volleys),
            Dribbling= mean(Dribbling),
            Curve = mean(Curve),
            Agility = mean(Agility),
            Aggression = mean(Aggression),
            Stamina = mean(Stamina),
            StadingTackle = mean(StadingTackle)
            BallControl = mean(BallControl)) %>%
  gather(Skill, Exp, Crossing:`BallControl`, -Club)


ggplot(vs, aes(Skill, Exp, fill = Club))+
  geom_col(position = "fill")+
  coord_flip()+
  scale_fill_manual(values = c("#c92a0a", "#270ac9"))+
  theme_minimal()+
  geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
  theme(legend.position = "top", axis.text.x=element_blank())+
  labs(title = "La Liga VS Premier Ligue", 
       caption = "FIFA 19",
       fill = NULL,x = NULL, y = NULL)


  
  
  

  
options(repr.plot.width = 15, repr.plot.height = 8)

ggplot(ligas, aes(Skill, Exp, fill = Ligas))+
  geom_col(position = "fill")+
  coord_flip()+
  scale_fill_manual(values = c("#c92a0a", "#270ac9"))+
  theme_minimal()+
  geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
  theme(legend.position = "top", axis.text.x=element_blank())+
  labs(title = "La Liga VS Premier Ligue", 
       caption = "FIFA 19",
       fill = NULL,x = NULL, y = NULL)

# comparemos dos jugadores neymar vs K. De Bruyne, con mas variables.

jugadores <- datos_fifa %>%
  filter(Name %in% c("Neymar Jr","K. De Bruyne")) %>%
  select(Name,Crossing:SlidingTackle) %>%
  rename_all(funs(gsub("[[:punct:]]", " ", .))) %>%
  gather(Skill, Exp, Crossing:`SlidingTackle`, -Name)

jugadores
# Gráfica para comparar los jugadores
ggplot(jugadores, aes(Skill, Exp, fill = Name))+
  geom_col(position = "fill")+
  coord_flip()+
  scale_fill_manual(values = c("#c92a0a", "#270ac9"))+
  theme_minimal()+
  geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
  theme(legend.position = "top", axis.text.x=element_blank())+
  labs(title = "Neymar Jr VS K. De Bruyne", 
       caption = "FIFA 19",
       fill = NULL,x = NULL, y = NULL)

 
# Modelo para pronosticar el salario


# primero daremos posiciones a los jugadores, para identificarlos de mejor manera

# 
delanteros <- c("ST", "LW", "RW", "LF", "RF", "RS","LS", "CF")
medio_campista <- c("CM","RCM","LCM", "CDM","RDM","LDM", "CAM", "LAM", "RAM", "RM", "LM")

datos_fifa <- datos_fifa %>%
  mutate(posiciones = as.factor(if_else(Position %in% delanteros,"Delantero",
                                        if_else(Position %in% medio_campista, "Medio Campista",
                                                if_else(Position == "GK", "Portero", "Defender")))))


datos_fifa <- datos_fifa %>%
  select(Name,Age, Overall, Potential, Value, Wage, Preferred.Foot,
         Height, Weight, Crossing:SlidingTackle,Ligas,país,posiciones, International.Reputation)

datos_fifa$International.Reputation<- as.factor(datos_fifa$International.Reputation)
datos_fifa$Weight <- as.numeric(datos_fifa$Weight)
glimpse(datos_fifa)

datos_fifa <- datos_fifa %>%
  select(-Weigh)

# modelo solo con los delanteros
atacantes <- datos_fifa %>%
  filter(posiciones == "Delantero")


# semilla
set.seed(143)

# Data de entrenamiento
particion <- sample(1:nrow(atacantes), size = 0.7*nrow(atacantes), replace = F)
fifa_train <- atacantes[particion,]
fifa_test <- atacantes[-particion,]


ggplot(atacantes,
       aes(log(Wage)))+
  geom_histogram()



modelo_step <- step(
    object = lm(log(Wage) ~.,
                fifa_train[,c(2:6,8:38)]), 
   direction = "backward",
   scope = list(upper= ~., lower = ~1),
   trace = F
   )

modelo_step
summary(modelo_step)

# Analisis conrrespondiente para los residuos del modelo

residuals <- fifa_train %>%
  transmute(Name = fifa_train$Name,
            Wage = Wage,
            residuals = residuals(modelo_step))

shapiro.test(residuals(modelo_step))


ggplot(residuals, aes(residuals))+
  geom_histogram()



predict <- fifa_train %>%
  transmute(Name =fifa_train$Name,
            Wage = Wage,
            valor_p = exp(predict(modelo_step, fifa_train)))

fifa_train %>%
  transmute(Name = fifa_train$Name,
            Wage = Wage,
            residuals = residuals(modelo_step)) %>%
  summarize(rmse = mean(residuals**2))


   
fifa_test %>%
  transmute(Name = fifa_test$Name,
            Wage = Wage,
            valor_p = exp(predict(modelo_step, fifa_test)))
  
  

datos_fifa %>%
  transmute(Name = datos_fifa$Name,
            Wage = Wage,
            valor_p = exp(predict(modelo_step, datos_fifa)))
  