###para iramuteq
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
datEnero21 <- read.csv("SaludInvestiga2022/DatosMediosEnero2021.csv", 
                       header = TRUE, sep = ",",stringsAsFactors = TRUE)
head(datEnero21)
View(datEnero21)
nrow(datEnero21)
# elimino las notas que no son relativas al tema
#contagios, coronavirus, covid-19, vacunas, sputnik-v, dosis, vuelos, contagios, contratos, vizzotti, gines, pfizer, moderna, malbran, sars-cov-2, salud, envenen, alberto, carrio, variante, cepa, virus, 
library(dplyr)
datEnero21Limpia <- datEnero21 %>% 
                    filter(grepl("contagi\\w+|coronavirus|covi\\w+|vacun\\w+|
                                  dosi\\w+|sput\\w+|vuel\\w+|contratos|vizzotti|
                                  gines|pfizer|avion|moderna|malbran|sars\\w+|salud|
                                  \\w+venen\\w+|alberto|carrio|variante\\w|cepa\\w|
                                  virus|astra\\w+|sinopharm|mutac\\w+",Link, ignore.case = TRUE))

nrow(datEnero21Limpia)
365 - 268
## instalo el rvest
install.packages("rvest")
library(rvest)
library(stringr)
# funcion para concatenar strings
'%&%' <- function(x, y)paste0(x,y)

for (renglon in 1:nrow(datEnero21Limpia)) 
  {
  urldatos <- datEnero21Limpia[renglon, "Link"]
  urldatos <- as.character(urldatos)
  arthtml <- try({read_html(urldatos)})
  nodos <-try({html_nodes(arthtml, "p")})
  texto <- try({html_text(nodos)})
  try({write(texto,"SaludInvestiga2022/nota" %&% renglon %&% ".txt")})
}


t <- read.table("SaludInvestiga2022/ene22/nota1.txt", header=FALSE)
#codifico para R
files <- list.files(path="SaludInvestiga2022/ene22", pattern="*.txt", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
  t <- read.table(x, header=FALSE) # load file
  # apply function
  counter <<- counter + 1
  out <- "*nota" %&% counter
  # write to file
  write.table(out, "SaludInvestiga2022/ene22/salida", sep=" ", quote=FALSE, row.names=FALSE, col.names=FALSE)
})
#write(line,file="Text.txt",append=TRUE)