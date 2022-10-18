###para iramuteq
install.packages("ca", repos="http://R-Forge.R-project.org")
install.packages("ca")
install.packages("rgl")
install.packages("proxy")
install.packages("ape")
install.packages("gee")
install.packages("textometry")
install.packages("wordcloud")
install.packages("irlba")
install.packages("RGraph")
R.Version()
###################################################
### leo el csv con los vínculos bajados de GDELT ene21
datDiciembre21 <- read.csv("SaludInvestiga2022/DatosMediosDiciembre2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
head(daDiciembre21)
View(datDiciembre21)
nrow(datDiciembre21)
# elimino las notas que no son relativas al tema
#contagios, coronavirus, covid-19, vacunas, sputnik-v, dosis, vuelos, contagios, contratos, vizzotti, gines, pfizer, moderna, malbran, sars-cov-2, salud, envenen, alberto, carrio, variante, cepa, virus, 
library(dplyr)
datDiciembre21Limpia <- datDiciembre21 %>% 
                    filter(grepl("contagi\\w+|coronavirus|covi\\w+|vacun\\w+|
                                  dosi\\w+|sput\\w+|vuel\\w+|contratos|vizzotti|
                                  gines|pfizer|avion|moderna|malbran|sars\\w+|salud|
                                  \\w+venen\\w+|alberto|carrio|variante\\w|cepa\\w|
                                  virus|astra\\w+|sinopharm|mutac\\w+",Link, ignore.case = TRUE))

nrow(datDiciembre21Limpia)
365 - 268
## instalo el rvest
install.packages("rvest")
library(rvest)
library(stringr)
# funcion para concatenar strings
'%&%' <- function(x, y)paste0(x,y)
#bajo los artículos
for (renglon in 1:nrow(datDiciembre21Limpia)) 
  {
  urldatos <- datDiciembre21Limpia[renglon, "Link"]
  urldatos <- as.character(urldatos)
  arthtml <- try({read_html(urldatos)})
  nodos <-try({html_nodes(arthtml, "p")})
  texto <- try({html_text(nodos)})
  try({write(texto,"SaludInvestiga2022/dic21/nota" %&% renglon %&% ".txt")})
}
#rainette método de reinert para R
install.packages("rainette")
library(rainette)
library(quanteda)
install.packages("tm")
library(tm)
txt <- "SaludInvestiga2022/dic21/salida"
#cargamos el corpus fijarse si es utf8 o ansi o que
corpusInsc <- SimpleCorpus(DirSource(txt, encoding = "latin1"), control = list(language = "es"))
#remuevo numeros
corpusInsc <- tm_map(corpusInsc, removeNumbers)
corpusInsc <- tm_map(corpusInsc, removeWords, c("www.pagina12", "<", 
                                                ">", "|", 
                                                "www.pagina.com.ar",
                                                "valorás",
                                                "mayoria",
                                                "cotidiano",
                                                "bombardeo",
                                                "@",
                                                "república",
                                                "mirada",
                                                "términos",
                                                "allá",
                                                "medios",
                                                "rigurosa",
                                                "condiciones",
                                                "gran","nº"))
writeLines(as.character(corpusInsc), 
           con="SaludInvestiga2022/dic21/salida/dic21SIL1.txt")

corpusInsc[[1]]$content
#arma segmentos de 40 palabras
corpus <- split_segments(corpusInsc, segment_size = 40)
#preprocesa quitando signos de puntuacion, palabras no usadas, etc
tok <- tokens(corpus, remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("spanish"))
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 10)
res <- rainette(dtm, k = 6, min_segment_size = 15)
rainette_explor(res, dtm, corpus)
rainette_plot(
  res, dtm, k = 6,
  n_terms = 20,
  free_scales = FALSE,
  measure = "chi2",
  show_negative = FALSE,
  text_size = 12
)
## Groups
cutree_rainette(res, k = 6)
####################################################################
datEnero21 <- read.csv("SaludInvestiga2022/DatosMediosEnero2021.csv", 
                           header = TRUE, sep = ",",stringsAsFactors = TRUE)
datFebrero21 <- read.csv("SaludInvestiga2022/DatosMediosFebrero2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datMarzo21 <- read.csv("SaludInvestiga2022/DatosMediosMarzo2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datAbril21 <- read.csv("SaludInvestiga2022/DatosMediosAbril2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datMayo21 <- read.csv("SaludInvestiga2022/DatosMediosMayo2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datJunio21 <- read.csv("SaludInvestiga2022/DatosMediosJunio2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datJulio21 <- read.csv("SaludInvestiga2022/DatosMediosJulio2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datAgosto21 <- read.csv("SaludInvestiga2022/DatosMediosAgosto2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datSeptiembre21 <- read.csv("SaludInvestiga2022/DatosMediosSeptiembre2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datOctubre21 <- read.csv("SaludInvestiga2022/DatosMediosOctubre2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datNoviembre21 <- read.csv("SaludInvestiga2022/DatosMediosNoviembre2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
datDiciembre21 <- read.csv("SaludInvestiga2022/DatosMediosDiciembre2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)

Mes <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", 
         "Ago", "Sep", "Oct", "Nov", "Dic")
Narticulos <- c(nrow(datEnero21), nrow(datFebrero21), nrow(datMarzo21), 
                nrow(datAbril21), nrow(datMayo21), nrow(datJunio21),
                nrow(datJulio21), nrow(datAgosto21), nrow(datSeptiembre21),
                nrow(datOctubre21), nrow(datNoviembre21), nrow(datDiciembre21))

nMes <- data.frame(Mes, Narticulos)

View(nMes)
library(ggplot2)
ggplot(nMes, aes(x=factor(Mes, levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", 
                                          "Ago", "Sep", "Oct", "Nov", "Dic")), 
                 y=Narticulos)) + geom_col() + labs(x = "Mes", y = "Cantidad")
#########################################################
install.packages("tidyverse")
#### para graficar analisis de sentimiento #############
#Enero
library(dplyr)
library(stringi)
vectTrans <- strsplit(as.character(datEnero21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datEnero21$diario <- df$V3
datEnero21 <- arrange(datEnero21,datEnero21$diario)
#febrero
vectTrans <- strsplit(as.character(datFebrero21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datFebrero21$diario <- df$V3
datFebrero21 <- arrange(datFebrero21,datFebrero21$diario)
#marzo
vectTrans <- strsplit(as.character(datMarzo21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datMarzo21$diario <- df$V3
datMarzo21 <- arrange(datMarzo21,datMarzo21$diario)
#abril
vectTrans <- strsplit(as.character(datAbril21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datAbril21$diario <- df$V3
datAbril21 <- arrange(datAbril21,datAbril21$diario)
#Mayo
vectTrans <- strsplit(as.character(datMayo21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datMayo21$diario <- df$V3
datMayo21 <- arrange(datMayo21,datMayo21$diario)
#junio
vectTrans <- strsplit(as.character(datJunio21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datJunio21$diario <- df$V3
datJunio21 <- arrange(datJunio21,datJunio21$diario)
#julio
vectTrans <- strsplit(as.character(datJulio21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datJulio21$diario <- df$V3
datJulio21 <- arrange(datJulio21,datJulio21$diario)
#agosto
vectTrans <- strsplit(as.character(datAgosto21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datAgosto21$diario <- df$V3
datAgosto21 <- arrange(datAgosto21,datAgosto21$diario)
#septiembre
vectTrans <- strsplit(as.character(datSeptiembre21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datSeptiembre21$diario <- df$V3
datSeptiembre21 <- arrange(datSeptiembre21,datSeptiembre21$diario)
#octubre
vectTrans <- strsplit(as.character(datOctubre21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datOctubre21$diario <- df$V3
datOctubre21 <- arrange(datOctubre21,datOctubre21$diario)
#noviembre
vectTrans <- strsplit(as.character(datNoviembre21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datNoviembre21$diario <- df$V3
datNoviembre21 <- arrange(datNoviembre21,datNoviembre21$diario)
#diciembre
vectTrans <- strsplit(as.character(datDiciembre21$Link), "[/]")
m1 <- stri_list2matrix(vectTrans, byrow=TRUE)
df <- as.data.frame(m1)
datDiciembre21$diario <- df$V3
datDiciembre21 <- arrange(datDiciembre21,datDiciembre21$diario)
#grafico
ggplot(datEnero21, aes(x=diario, 
                       y=V2Tone)) + geom_col()
View(datEnero21$diario)
install.packages("sqldf")
library(sqldf)
datEnero21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datEnero21 GROUP BY diario")
View(datEnero21Prom)
ggplot(datEnero21Prom, aes(x=diario,y=Tone)) + geom_col()

datFebrero21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datFebrero21 GROUP BY diario")
ggplot(datFebrero21Prom, aes(x=diario,y=Tone)) + geom_col()

datMarzo21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datMarzo21 GROUP BY diario")
ggplot(datMarzo21Prom, aes(x=diario,y=Tone)) + geom_col()

datAbril21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datAbril21 GROUP BY diario")
ggplot(datAbril21Prom, aes(x=diario,y=Tone)) + geom_col()

datMayo21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datMayo21 GROUP BY diario")
ggplot(datMayo21Prom, aes(x=diario,y=Tone)) + geom_col()

datJunio21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datJunio21 GROUP BY diario")
ggplot(datJunio21Prom, aes(x=diario,y=Tone)) + geom_col()

datJulio21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datJulio21 GROUP BY diario")
ggplot(datJulio21Prom, aes(x=diario,y=Tone)) + geom_col()

datAgosto21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datAgosto21 GROUP BY diario")
ggplot(datAgosto21Prom, aes(x=diario,y=Tone)) + geom_col()

datSeptiembre21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datSeptiembre21 GROUP BY diario")
ggplot(datSeptiembre21Prom, aes(x=diario,y=Tone)) + geom_col()

datOctubre21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datOctubre21 GROUP BY diario")
ggplot(datOctubre21Prom, aes(x=diario,y=Tone)) + geom_col()

datNoviembre21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datNoviembre21 GROUP BY diario")
ggplot(datNoviembre21Prom, aes(x=diario,y=Tone)) + geom_col()

datDiciembre21Prom <- sqldf("SELECT diario, AVG(V2Tone) AS Tone FROM datDiciembre21 GROUP BY diario")
ggplot(datDiciembre21Prom, aes(x=diario,y=Tone)) + geom_col()



