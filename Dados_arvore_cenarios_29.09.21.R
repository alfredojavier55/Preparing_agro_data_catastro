# --------------------------------------------------
# Dados árvore cenario
# --------------------------------------------------
library(epinemo);library(dplyr); library(tidyr); library(lubridate); library(scales)
#1.1.1 Cadastro 2017 ----
# setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
cad17 <- read.csv(file="cad2017.csv", colClasses = "character")

#1.1.2 Numero de predios ----
length(unique(paste(cad17$identificacion.propietario, cad17$nombre.sitio))) #105542
length(unique(cad17$identificacion.propietario)) #103340

#1.1.3 Numero de animais no cadastro ----
sum(as.numeric(cad17$cantidad)) #1204824

# 1.2.1 Cadastro 2018 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2018")

cad18 <- read.csv(file="cad2018.csv", colClasses = "character")
#1.2.2 Numero de predios ----
length(unique(paste(cad18$identificacion.propietario, cad18$nombre.sitio))) #126168
length(unique(cad18$identificacion.propietario)) #123713
#1.2.3 Numero de animais no cadastro ----
sum(as.numeric(cad18$cantidad)) #1311898

# 1.3.1 Cadastro 2019 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2019/antigos/")
cad19 <- read.csv(file="cad2019.csv", colClasses = "character")
#1.3.2 Numero de predios ----
length(unique(paste(cad19$identificacion.propietario, cad19$nombre.sitio))) #105083

#1.3.3 Numero de animais no cadastro ----
sum(as.numeric(cad19$cantidad), na.rm = TRUE) #1504601


# 1.3.1 Cadastro 2020 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2020")
cad20 <- read.csv(file="cad2020.csv", colClasses = "character")
colnames(cad20) <- tolower(iconv(colnames(cad20),  from = 'UTF-8', to = 'ASCII//TRANSLIT'))

#1.3.2 Numero de predios ----
length(unique(paste(cad20$identificacion.propietario, cad20$nombre.sitio))) #104778
length(unique(cad20$identificacion.propietario)) #103188

# Puedo eliminar la cantidad inactivos? 

cad20 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.activos), na.rm = TRUE))

cad20 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.inactivos), na.rm = TRUE))

cad20$cantidad <- cad20$cantidad.activos
cad20$cantidad.inactivos <- NULL
cad20$cantidad.activos <- NULL
colnames(cad20)

#1.3.3 Numero de animais no cadastro ----
sum(as.numeric(cad20$cantidad), na.rm = TRUE) #1107488


# 1.3.1 Cadastro 2021 ----
setwd("~/Backup/USP/Projeto fapesp/Dados/Cadastro/2021")
cad21 <- read.csv(file="cad2021.csv", colClasses = "character")
colnames(cad21) <- tolower(iconv(colnames(cad21),  from = 'UTF-8', to = 'ASCII//TRANSLIT'))

#1.3.2 Numero de predios ----
length(unique(paste(cad21$identificacion.propietario, cad21$nombre.sitio))) #104778
length(unique(cad21$identificacion.propietario)) #103188

# Puedo eliminar la cantidad inactivos? 
cad21 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.activos), na.rm = TRUE))

cad21 %>%
  group_by(tipo.operacion)%>%
  summarise(animales=sum(as.numeric(cantidad.inactivos), na.rm = TRUE))

cad21$cantidad <- cad21$cantidad.activos
cad21$cantidad.inactivos <- NULL
cad21$cantidad.activos <- NULL
colnames(cad21)

#1.3.3 Numero de animais no cadastro ----
sum(as.numeric(cad21$cantidad), na.rm = TRUE) #1107488

# Catastro por anos
cad20$X <- cad20$x
cad20$x <- NULL

cad21$X <- cad21$x
cad21$x <- NULL

# Agregar ano
cad17$ano <- "2017"
cad18$ano <- "2018"
cad19$ano <- "2019"
cad20$ano <- "2020"
cad21$ano <- "2021"

cad <- rbind(cad17,cad18,cad19, cad20, cad21)
cad <- rbind(cad19, cad20, cad21)

# Número de animales
cad %>%
  group_by(tipo.operacion)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))

cad %>%
  group_by(provincia, ano)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))

table(cad$producto)

cad %>%
  group_by(ano)%>%
  filter(producto == "Cerda madre")%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad), na.rm = TRUE))

#número de establecimientos
library(tidyr)

cad %>%
  group_by(tipo.operacion)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))%>%
  spread(key = "ano", value = "establecimientos")

cad %>%
  group_by(provincia, ano)%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))

table(cad$producto)

# Analisis Indocumentados
cad %>%
  group_by(tipo.operacion, ano)%>%
  filter(identificacion.propietario == 1768105720002)%>%
  # filter(tipo.operacion != "Faenador")%>%
  # filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))%>%
  spread(key = "ano", value = "establecimientos")

cad %>%
  group_by(tipo.operacion, ano)%>%
  filter(identificacion.propietario == 1768105720002)%>%
  # filter(tipo.operacion != "Faenador")%>%
  # filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(animales=sum(as.numeric(cantidad)))%>%
  spread(key = "ano", value = "animales")

#Eliminación indocumentados
cad <- cad[cad$identificacion.propietario != 1768105720002, ]

#Eliminar os nombre_sitio eliminar  #
cad <- cad[cad$nombre.sitio != "ELIMINAR", ]

# Establecimientos que tienen cerdas madres
cad %>%
  group_by(ano)%>%
  filter(producto == "Cerda madre")%>%
  filter(tipo.operacion != "Faenador")%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(establecimientos=length(unique(paste(identificacion.propietario, nombre.sitio))))

# Obteniendo datos para satscan
library(tidyverse)

vigi2019 <- cad %>%
  group_by(provincia, canton, parroquia) %>%
  filter(tipo.operacion != "Faenador")%>%
  filter(ano == 2019)%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(cantidad=length(unique(paste(identificacion.propietario, nombre.sitio))))

vigi2020 <- cad %>%
  group_by(provincia, canton, parroquia) %>%
  filter(tipo.operacion != "Faenador")%>%
  filter(ano == 2020)%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(cantidad=length(unique(paste(identificacion.propietario, nombre.sitio))))

vigi2021 <- cad %>%
  group_by(provincia, canton, parroquia) %>%
  filter(tipo.operacion != "Faenador")%>%
  filter(ano == 2021)%>%
  filter(tipo.operacion != "Feria de comercialización animal")%>%
  summarise(cantidad=length(unique(paste(identificacion.propietario, nombre.sitio))))

sum(vigi2019$cantidad) #104884
sum(vigi2020$cantidad) #104570
sum(vigi2021$cantidad) #144352

colnames(vigi) <- c("provincia", "canton", "parroquia", "cantidad")
sum(vigi2019$cantidad, na.rm = TRUE) #7023

vigi <- vigi2019
vigi <- vigi2020
vigi <- vigi2021

# Usando o mapa para relocalizar en provincia cantón y parroquia
source("~/Dropbox/3.UNL/Tesis/Darwin/Codigo/passthemap_guia.R") #colocar en la misma carpeta

c19 <- data.frame(ec3@data$DPA_PARROQ,ec3@data$cantidad, "2019-01-01")
colnames(c19) <- c("DPA_PARROQ","cantidad","fecha")

c20 <- data.frame(ec3@data$DPA_PARROQ,ec3@data$cantidad, "2020-01-01")
colnames(c20) <- c("DPA_PARROQ","cantidad","fecha")

c21 <- data.frame(ec3@data$DPA_PARROQ,ec3@data$cantidad, "2021-01-01")
colnames(c21) <- c("DPA_PARROQ","cantidad","fecha")

catastro <- rbind(c19,c20,c21)

# Número de parroquias sin catastro 
catastro %>%
  group_by(fecha) %>%
  summarise(sum(is.na(cantidad)))
#103
#89
#92

# Número de parroquias 
catastro %>%
  group_by(fecha) %>%
  summarise(sum(cantidad, na.rm = TRUE))

# Guardando catastro para satscan
setwd("~/Dropbox/3.UNL/Tesis/Darwin/Codigo/")
write.csv(catastro,file="pop_p.csv") #population_premises



# --------------------------------------------------
# 2 Vigilancia  Notificaçoes v2----
# --------------------------------------------------
# --------------------------------------------------
# Gerando únicamente os casos v3
# --------------------------------------------------
library(dplyr)
setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados")

#1 importando - modificando colnames de csv VEO por Rep_csv ----
# Estos arquivos tem os colnames certos
vge0 <- read.csv("Rep_General_EventosSan0.csv", colClasses = "character",encoding = "UTF-8")
vr0 <- read.csv("Rep_ResultadosGEV0.csv", colClasses = "character",encoding = "UTF-8")
vc0 <- read.csv("Rep_CierreGEV0.csv", colClasses = "character",encoding = "UTF-8")

#Estos arquivos tem os colnames errados, mas a informacao de cedula certa (baixados desde o sizse 11.12.2019)
vge <- read.csv("VEO1_G.csv", colClasses = "character", encoding = "UTF-8")
vr <- read.csv("VEO1_R.csv", colClasses = "character", encoding = "UTF-8")
vc <- read.csv("VEO1_C.csv", colClasses = "character",encoding = "UTF-8")

colnames(vge) <- colnames(vge0)
colnames(vr) <- colnames(vr0)
colnames(vc) <- colnames(vc0)

rm(vge0, vr0, vc0)

#2 acoplando bancos ----
#Banco Reporte geral de eventos
vge$canton <- vge$cantón
vge$cantón <- NULL

#Banco Resultados de eventos
vr$cant_muestras <- as.numeric(vr$cant_muestras)
vr$positivos <- as.numeric(vr$positivos)
vr$negativos <- as.numeric(vr$negativos)
vr$reactivos <- as.numeric(vr$reactivos)
vr$indeterminados <- as.numeric(vr$indeterminados)
vr$canton <- vr$cantón
vr$cantón <- NULL

# Banco Cierre de eventos
vc$existentes <- as.numeric(vc$existentes)
vc$enfermos <- as.numeric(vc$enfermos)
vc$muertos <- as.numeric(vc$muertos)
vc$sacrificad <- as.numeric(vc$sacrificad)
vc$canton <- vc$cantón
vc$cantón <- NULL

#3 Linkage dos reportes ----
v0 <- full_join(vc, vr)
v1 <- full_join(v0,vge)

# Filtro para notificaciones y casos
# para casos, activar filtros de diagnostico

v2 <- v1 %>%
  # filter(especie == "PORCINOS")%>%
  filter(detalle_diagnóstico == "Peste porcina clásica")%>% #filtering to see the general surveillance
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>% #filtering to see the general surveillance
  group_by(orden, provincia, canton, parroquia, cedula, propietario, semana, zona, 
           coord_x, coord_y, predio,t_explotación, notificador, 
           f_1er_enfermo, f_notificación, f_1era_visita, síndrome_presuntivo,
           patología, especie, edad, f_elaboración, f_ingreso, f_cierre_orden,
           responsable, vacuno, focal, dosis_focal, perifocal, dosis_perifocal, 
           especie_f, colecta, prueba_solicitada, detalle_diagnóstico)%>%
  summarise(existente=sum(existentes), enfermo=sum(enfermos), mortos=sum(muertos), 
            sacrifi=sum(sacrificad), afetados=sum(muertos,sacrificad), 
            pos=sum(positivos), total_muestras=sum(cant_muestras), 
            indeterm=sum(indeterminados),reactivo=sum(reactivos))

length(unique(v2$orden))
# 1652 eventos hasta 11/2020
# 1882 incluido 10/2021

#Usei código para importar diferente devo voltar a pasta com arquivos
setwd("~/Dropbox/0.USP/2 Projeto graduação/Projeto/Caraterização")

#4 base de vigilancia antes do size (base excel, arquivo pessoal do projeto ppc) ----
as <- read.csv("bd-2014-presizse.csv", colClasses = "character",
               encoding = "UTF-8")

as$existente <- as.numeric(as$existente)
as$mortos <- as.numeric(as$mortos)
as$sacrifi <- as.numeric(as$sacrifi)
as$afetados <- as.numeric(as$afetados)
as$enfermo <- as.numeric(as$enfermo)
as$pos <- as.numeric(as$pos)
as$total_muestras <- as.numeric(as$total_muestras)
as$indeterm <- as.numeric(as$indeterm)
as$reactivo <- as.numeric(as$reactivo)
as$prueba_solicitada <- as.character(NA)
as$detalle_diagnóstico <-as.character(NA)

as <- as[,c(1:31,41,42,32:40)]
all_equal(v2,as)

colnames(as)
colnames(v2)
v2 <- data.frame(v2)

v2 <- rbind(v2,as)


v2$ano <- year(dmy(v2$f_1er_enfermo))
v2$month <- month(dmy(v2$f_1er_enfermo))

# 1757 eventos 
# 1074

# I am deleting a case that is duplicated
v2 <- v2[v2$orden != "1",] #0700860679 loja paltas casamba
# Deleting 4 bovine registries
v2 <- v2[v2$orden != "2382",] 
v2 <- v2[v2$orden != "1096",] 
v2 <- v2[v2$orden != "4114",] 
v2 <- v2[v2$orden != "6270",] 
v2 <- v2[v2$orden != "-9",] #industrial PED don diego
# v2 <- v2[v2$orden != "-90",] #EVENTO CAMAL deleted because of no origin
# v2 <- v2[v2$orden != "980",] #EVENTO CAMAL ECOLOGICO DEL COLTA

length(unique(v2$orden))
#1758 com atualizacao 10/10/2021 todas as patologias

table(unique(v2$orden) %in% unique(v4$orden))

# 5 -- Vigilancia geral suinos ----
# numero de notificaciones
v2 %>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)))


tiff(filename = "Fig. Notificações_vig_geral_14-19.tiff",
     width=14, height=7, units="cm", res=600,
     compression = "lzw", pointsize = 12)

v2 %>%
  group_by(month)%>%
  filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_geral=length(unique(orden)),
            diagnosticados=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(month,notifi_geral))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Vigilancia geral PSC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

dev.off()

#6 Using the case-control db ----
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/")
# write.csv(v2, file="base-16.9.21.csv")
v2 <- read.csv(file = "base-16.9.21.csv", colClasses = "character")
v2$total_muestras <- as.numeric(v2$total_muestras)

#7 -- Vigilancia especifica ----
# Numero de casos e notificacoes ----
v2 %>%
  group_by(ano)%>%
  # filter(ano == "2020")%>%
  summarise(notifi_PPC=length(unique(orden)),
            surtos_PPC=sum(pos >= 1, na.rm = TRUE),
            Per=surtos_PPC/notifi_PPC)


# total 0.2695
v2 %>%
  group_by(ano)%>%
  filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  summarise(notifi=length(unique(orden)),
            surtos=sum(pos >= 1, na.rm = TRUE))

# 2.1Todas as notificações ----
# ano   notifi surtos
# 1 2014      67     30
# 2 2015     587    185
# 3 2016     458    139
# 4 2017     674    164
# 5 2018     533    108
# 6 2019     580    137
# 7 2020     295     54

# Atualizaçao 29 sept 2021
table(is.na(v2$total_muestras))

v2 %>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)), 
            notifi_pos=sum(pos >= 1, na.rm = TRUE),
            amostras=sum(as.numeric(total_muestras), na.rm = TRUE))

v2 %>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  group_by(ano, amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=sum(pos >= 1, na.rm = TRUE),
            amostras=sum(as.numeric(total_muestras)))

v2 %>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  group_by(amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=sum(pos >= 1, na.rm = TRUE),
            amostras=sum(total_muestras))
497+982
length(unique(v2$orden))

v2 %>%
  group_by(ano)%>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  summarise(notifi=length(unique(orden)),
            surtos=sum(pos >= 1, na.rm = TRUE),
            amostras=sum(total_muestras, na.rm = TRUE),
            positivos=sum(pos, na.rm = TRUE),
            prevalencia=positivos/amostras)

table(v2$síndrome_presuntivo[v2$ano > 2016])

# 
#    ano   notifi surtos
# 1 2014      35     24
# 2 2015     219     82
# 3 2016     127     32
# 4 2017     160     39
# 5 2018     178     66
# 6 2019     159     40
# 7 2020      54     7

# From case-control
#  ano  case control
# 2014   79      76
# 2015   82     247
# 2016   30     112
# 2017   39   +  134
# 2018   60     +151
# 2019   35+     132
# 2020   13      64

# % vigilancia clinica ----
# Notificacoes de ppc que tiveram diagnostico
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  group_by(ano,amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

#     ano amostrado notifi surtos amostras
# 1  2015 0              1      0        0
# 2  2015 1            218     82     1572
# 3  2016 1            127     30      844
# 4  2017 1            160     39     1000
# 5  2018 1            178     60     1212
# 6  2019 1            160     35      961
1+35+1+218+127+160+178+160+178+160

v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  group_by(ano, amostrado)%>%
            summarise(surtos=length(unique(subset(orden, pos>=1))),
                      amostras=sum(total_muestras, na.rm = TRUE))

library(tidyverse)
v2 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico != "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"si","no"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(amostrado, detalle_diagnóstico)%>%
  summarise(notifi=length(unique(orden)), 
            brotes=length(unique(subset(orden, pos>=1))),
            negativos=notifi-brotes,
            amostras=sum(total_muestras, na.rm = TRUE),
            total=brotes+negativos)

length(unique(v2$orden[v2$ano >2014 & v2$ano <2020],))
# 932+486

# Numero de eventos por ano seriam as amostras
v2 %>%
  group_by(ano)%>%
  summarise(eventos=length(unique(orden)))


v2 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico != "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"si","no"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            brotes=length(unique(subset(orden, pos>=1))),
            negativos=notifi-brotes,
            amostras=sum(total_muestras, na.rm = TRUE),
            total=brotes+negativos)

#Porcentagem de amostragem
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  group_by(ano,amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(as.numeric(total_muestras), na.rm = TRUE))

15
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"amostrado","nao_amostrado"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(ano, amostrado)%>%
  summarise(notifi=length(unique(orden)),
            caso=sum(as.numeric(caso)))%>%
  spread(key="amostrado", value="notifi")

length(unique(v2$orden))
916+338

table(v2$caso, v2$ano)

# % de amostragem nas notificacoes (desestimacao de casos)
1  2017   116   /175  0.6628
2  2018    92   /214  0.4299
3  2019   105   /168  0.625

116+175



116+175=291
92+214=306
105+168=273


c <-v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  filter(amostrado == 0)%>%
  group_by( amostrado, patología)%>%
  summarise(notifi=length(unique(orden)))


# Plotting vigi geral, especifica e surto
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/Code case-control/")
v4 <- read.csv(file = "base-16.9.21.csv", colClasses = "character")

v4$f_1er_enfermo <- ymd(v4$f_1er_enfermo)
# Changing to floor date week
v4$week <- floor_date(v2$f_1er_enfermo, "week")

# Procurando o tipo de notificacao
# Vigilancia geral, especifica psc e surto

table(v4$caso)

v2 <- v2[unique(v2$orden),]

v2 <- mutate(v2, type_of = ifelse(v2$orden %in% v4$orden[v4$caso == "0"],"2",
                                  ifelse(v2$orden %in% v4$orden[v4$caso == "1"], "3", "1")))
class(v2$type_of)
table(v2$type_of)

v2$f_1er_enfermo <- dmy(v2$f_1er_enfermo)
# Changing to floor date week
v2$month <- floor_date(v2$f_1er_enfermo, "month")


v2 <- v2[unique(v2$orden),]
v2 <- data.frame(v2)

ggplot(v2, aes(month, as.numeric(type_of),fill=type_of))+
  geom_col()


library(lubridate)
# Day of notification
v2$f_1er_enfermo <- ymd(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$month <- floor_date(v2$f_1er_enfermo, "month")
v2$Month <- month(v2$month)

# Analisis de vigilancia especifico para PSC Target surveillance
# Numero de casos e notificacoes
v2 %>%
  group_by(ano)%>%
  # filter(ano == "2020")%>%
  summarise(notifi_PPC=length(unique(orden)),
            surtos_PPC=sum(pos >= 1, na.rm = TRUE),
            Per=surtos_PPC/notifi_PPC)

# total 0.2695

#< Fig.2 Distribution of events ----
# Eventos discriminated by case and control, cat
setwd("~/Dropbox/0.USP/11.Teses/")

library(scales)

tiff(filename = "Fig. Notificações_vig_dirigida_14-19.tiff",
     width=14, height=7, units="cm", res=600,
     compression = "lzw", pointsize = 12)

v2 %>%
  group_by(month)%>%
  filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_PPC=n())%>%
  ggplot()+
  geom_col(aes(month,notifi_PPC))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificações PSC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))

dev.off()

tiff(filename = "Fig. Casos_vig_dirigida_14-19.tiff",
     width=14, height=7, units="cm", res=600,
     compression = "lzw", pointsize = 12)


v2 %>%
  group_by(month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_PPC=n(),
            surtos_PPC=sum(pos >= 1, na.rm = TRUE))%>%
  ggplot()+
  geom_col(aes(month,surtos_PPC))+
  scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Casos confirmados PSC",
       x="Meses")+
  theme_minimal()+
  theme(text = element_text(size = 14))

dev.off()

v2 %>%  
  group_by(month, Type=fcaso) %>%
  summarise(Number_of_events=n()) %>%
  ggplot() +
  geom_col(aes(month, Number_of_events))+
  facet_grid(rows = vars(Type), scales = "free_y")+
  labs(y="Number of events", 
       x="Date of start of the event (month)",
       fill="Herd type:")+
  theme_light()+
  theme(text = element_text(size = 14),
        axis.text.y =element_text(size=12),
        axis.text.x =element_text(size=12),
        legend.position = "top",
        legend.text = element_text(size=10),
        legend.key.size = unit(4, "mm"))+
  scale_fill_viridis_d(direction=-1)

dev.off()


v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  group_by(ano)%>%
  summarise(notifi=length(unique(orden)))

length((unique(v2$orden)))

v2 %>% 
  group_by(fnotificador) %>%
  #filter( ano == "2019")%>%
    summarise(notifi=n(), 
              surto = sum(as.numeric(caso), na.rm = TRUE), 
    prob_virar_surto =surto/notifi,
  porcentagen_de_notif= notifi/sum(174,1756,889),
  pro_pond = prob_virar_surto*porcentagen_de_notif,
  probab_pond =pro_pond/0.262,
  amostras=sum(total_muestras))

# Number of notifications by type of notificator
v2 %>%
  group_by(fnotificador)%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  summarise(notifi=n(), 
            surto = sum(as.numeric(caso), na.rm = TRUE)) 

# 1 1Owner          343    +75
# 2 2Agrocalidad     29     +5
# 3 3Sensor         179    +54

# Proprietario:
round(343/(343+179),2)
0.66
# Sensor:
round(1-(343/(343+179)),2)
0.34

v2 %>%
  group_by(ano, fnotificador)%>%
  filter(fnotificador != "2Agrocalidad")%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  summarise(notifi=n(), 
            surto = sum(as.numeric(caso), na.rm = TRUE)) 

# Percentages of notification
# 2017 0.686
109/(109+50)
# 2018 0.668
135/(135+67)
# 2019 0.615
99/(99+62)

# Vigilancia ativa
v2 %>%
  group_by(ano, fnotificador)%>%
  filter(fnotificador == "2Agrocalidad")%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  summarise(notifi=n(), 
            surto = sum(as.numeric(caso), na.rm = TRUE)) 

# Meses 
library(tidyverse)
mes <- v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"amostrado","nao_amostrado"))%>%
  filter(ano == 2020)%>%
  # filter(ano >2016)%>%
  group_by(ano, month, amostrado)%>%
  summarise(notifi=length(unique(orden)))%>%
  spread(key="amostrado", value="notifi")



# Graphics of the casos confirmados 

v2$f_1er_enfermo <- ymd(v2$f_1er_enfermo)
# Changing to floor date week
v2$month <- floor_date(v2$f_1er_enfermo, "month")


v2 %>%
  group_by(month)%>%
  # filter(ano <2020)%>%
  # filter(ano >2016)%>%
  summarise(notifi_PPC=n(),
            diagnosticados=sum(pos >= 1, na.rm = TRUE))%>%
  
  ggplot(v2, aes(month,numeric(type_of),fill=type_of))+
  geom_col()

ggplot(v4, aes(month,caso,fill=caso))+
  geom_col()



scale_y_continuous(breaks= pretty_breaks())+
  labs(y="Notificações PSC",
       x="Meses")+
  theme_minimal() +
  theme(text = element_text(size = 14))



 # -Proporcao da vigilancia por regiao ----
# Vou agregar a regiao para conseguir o poercentage por regiao
v2$pro <- tolower(iconv(v2$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(v2$pro)
v2$regiao <- v2$pro

v2$regiao <- gsub("azuay", "serra", v2$regiao)
v2$regiao <- gsub("bolivar", "serra", v2$regiao)
v2$regiao <- gsub("canar", "serra", v2$regiao)
v2$regiao <- gsub("carchi", "serra", v2$regiao)
v2$regiao <- gsub("chimborazo", "serra", v2$regiao)
v2$regiao <- gsub("cotopaxi", "serra", v2$regiao)
v2$regiao <- gsub("el oro", "litoral", v2$regiao)
v2$regiao <- gsub("santa elena", "litoral", v2$regiao)
v2$regiao <- gsub("guayas", "litoral", v2$regiao)
v2$regiao <- gsub("imbabura", "serra", v2$regiao)
v2$regiao <- gsub("loja", "serra", v2$regiao)
v2$regiao <- gsub("esmeraldas", "litoral", v2$regiao)
v2$regiao <- gsub("los rios", "litoral", v2$regiao)
v2$regiao <- gsub("manabi", "litoral", v2$regiao)
v2$regiao <- gsub("morona santiago", "amazonia", v2$regiao)
v2$regiao <- gsub("napo", "amazonia", v2$regiao)
v2$regiao <- gsub("orellana", "amazonia", v2$regiao)
v2$regiao <- gsub("pastaza", "amazonia", v2$regiao)
v2$regiao <- gsub("pichincha", "serra", v2$regiao)
v2$regiao <- gsub("santo domingo de los tsachilas", "litoral", v2$regiao)
v2$regiao <- gsub("sucumbios", "amazonia", v2$regiao)
v2$regiao <- gsub("tungurahua", "serra", v2$regiao)
v2$regiao <- gsub("zamora chinchipe", "amazonia", v2$regiao)
table(v2$regiao)


v2 %>%
  group_by(ano, regiao)%>%
  filter(ano == 2017)%>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  summarise(notifi=length(unique(orden)),
              porcentagem_vigi=(notifi/n()))
68+101+122



v2 %>%
  group_by(notificador,ano)%>%
  summarise(nu=n())%>%
  spread(key="ano", value="nu")
  

#mudando nomes no banco de vigilancia para fazer match com outros bancos
colnames(v2)
names(v2)[5] <- "identificador_operador"
names(v2)[6] <- "nombre_operador"
names(v2)[11] <- "nombre_sitio"

v3 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)")



# Eventos que tem cedulas erroneas

#2849
v2$identificador_operador <- gsub("1714523530", "1713551511", v2$identificador_operador)
#2874 
v2$identificador_operador <- gsub("1101119888", "1500219124", v2$identificador_operador)
#2987
v2$identificador_operador <- gsub("0702017419", "0702017419001", v2$identificador_operador)
#3064
v2$identificador_operador <- gsub("1001586470","1001586740", v2$identificador_operador)
#3197
v2$identificador_operador <- gsub("0105991963","0105491963", v2$identificador_operador)
#3279
v2$identificador_operador <- gsub("1101832473","1101184255", v2$identificador_operador)
#3327
v2$identificador_operador <- gsub("0917093569","0917093569001", v2$identificador_operador)
#3644
v2$identificador_operador <- gsub("1802888849","1792610095001", v2$identificador_operador)
#3737
v2$identificador_operador <- gsub("1307365466","1307385466", v2$identificador_operador)
#4091
v2$identificador_operador <- gsub("0909922223","0909729923", v2$identificador_operador)
#4098
v2$identificador_operador <- gsub("1705679395","1717558876001", v2$identificador_operador)
#2044
v2$identificador_operador <- gsub("0500991427","0502592231", v2$identificador_operador)

v2$identificador_operador <- gsub("17084411515", "1708411515", v2$identificador_operador) #DOMINGO HERNAN GARCIA VILLAGOMEZ

# Metodo hamming ele procura o numero de substitu;'oes de carater necesarias para tornar a em b
v2$identificador_operador <- gsub("0605410962", "0605210962", v2$identificador_operador) #SANDY DAQUILEMA
v2$identificador_operador <- gsub("0302742641", "0302742671", v2$identificador_operador) #CHIMBAINA AGUALEMA MARIA CARMEN
v2$identificador_operador <- gsub("1802900426", "1802200426", v2$identificador_operador) #BERMEO AGUIRRE MIGUEL LUCIANO
# Caso especial> Ayul vimos jose manuel correto SIZSE incorrecto no GUIA, vou modificar ao contrario para resgatar informações do guia, porem o numero correto é 0601581374, e nao 0681581374
v2$identificador_operador <- gsub("0601581374", "0681581374", v2$identificador_operador) #Jose manuel ayul
v2$identificador_operador <- gsub("0200018480", "0200018489", v2$identificador_operador) # HUGO RODRIGO  GAVILANEZ CHAVEZ 
v2$identificador_operador <- gsub("1400233992", "1400233993", v2$identificador_operador) # LUIS ALFREDO CASTILLO 
v2$identificador_operador <- gsub("1900216176", "1900216126", v2$identificador_operador) # VICTOR MANUEL SOTO PEÑAFIEL
v2$identificador_operador <- gsub("1306928691", "1306928696", v2$identificador_operador) # GRANJA WILNTON GEOVANNI
v2$identificador_operador <- gsub("1712194169", "1712194164", v2$identificador_operador) # MARIA ROSARIO CUMBAJIN TUPIZA
v2$identificador_operador <- gsub("1702422771", "1702422773", v2$identificador_operador) # ANGEL AMBLE CANDO GORDILLO


# Organizando se e caso ou nao
# casos com 0 positivos tem que ser 0 nao NA
v2 <- v2 %>% mutate(caso = ifelse(pos >=1,1,0))

v2 %>%
  group_by(ano)%>%
  summarise(notifi=n(), surto = sum(caso, na.rm = TRUE))


# Gerando arquivo com casos
v3 <- v2 %>%
  #filter(ano == "2018")%>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  filter(caso == "1")

v3 %>% group_by(ano)%>%
  summarise(n())

#2.1 numero de casos ----
# ano   `n()`

# 1 2014     24
# 2 2015     82
# 3 2016     32
# 4 2017     39
# 5 2018     66
# 6 2019     29

# 2.1 número de casos  
length(unique(v3$orden))  
sum(v3$caso)

length(unique(v2$identificador_operador))




#11/12/2019 uso o arquivo total 966 registros 650 notificacoes e 316 casos
#10/09/2020 uso o arquivo total 943 registros 650 notificacoes e 316 casos
table(v2$caso)
length(unique(v2$orden))
duplicated(v2$orden)
# Gerar os casos primeiro para filtrar os casos producidos por vigilancia, movimentação e fiscalização.
v4 <- v2 <- v2[v2$ano == 2018,]

v4 <- v2
v2 <- v2

# 2.3 notificações que geram casos (todas as patologias incluindo PPC)
v4 %>% 
  mutate(notificador2 = ifelse(notificador == "OTROS", "SENSORES", notificador))%>%
  group_by(notificador2) %>%
  #filter( ano == "2019")%>%
  summarise(notifi=n(), surto = sum(caso, na.rm = TRUE), 
            prob_virar_surto =surto/notifi,
            porcentagen_de_notif= notifi/sum(174,1756,889),
            pro_pond = prob_virar_surto*porcentagen_de_notif,
            probab_pond =pro_pond/0.262,
            amostras=sum(total_muestras))

30+185+139+164+108+113
61+426+252

#todos os anos
# Notificador2   notifi surto prob_virar_surto porc_notif pro_pond probab_pond
# 1 AGROCALIDAD     174    61            0.351     0.0617   0.0216      0.0826
# 2 PROPIETARIO    1756   426            0.243     0.623    0.151       0.577 
# 3 SENSORES        889   252            0.283     0.315    0.0894      0.341 

# Probabilidade de virar surto, ponderada pelo número de casos historicos.

#numero de notificações e casos
v2 %>% 
  mutate(notificador2 = ifelse(notificador == "OTROS", "SENSORES", notificador))%>%
  group_by(ano) %>%
  summarise(notifi=n(), surto = sum(caso, na.rm = TRUE)) 



# Sensibilidade ----
1 − ( 1 − CSeU )n.

1-(1-1.98)^70


# PROPORÇAO DA VIGILANCIA ----
# Location of the project ----
setwd("~/Dropbox/0.USP/7.Publicações/Risk factors and space-time analysis associated with presentation of Classical Swine Fever in Ecuador/")
# write.csv(v2, file="base-16.9.21.csv")
v2 <- read.csv(file = "base-16.9.21.csv", colClasses = "character")

v2 <- data.frame(v2)
str(v2)
# Mutating information ----
# Modifying dates ----
# Day of notification
v2$f_1er_enfermo <- ymd(v2$f_1er_enfermo)
# Changing to floor date week
v2$week <- floor_date(v2$f_1er_enfermo, "week")
# Best visualizations by month
v2$month <- floor_date(v2$f_1er_enfermo, "month")
v2$Month <- month(v2$month)

v2%>%
filter(ano=="2017")%>%
    group_by(ano,fcaso,fregiao)%>%
  summarise(n())

table(v2$fcaso, v2$fregiao, v2$ano)


table(v2$fcaso, v2$fdes_lav, v2$fregiao, v2$ano)



, ,  = 3Highlands,  = 2017

        NO SI
case     0 17
control  4 56
(17+56)/(17+4+56)

tabela <- xtabs(~ v2$fdes_lav[v2$ano == "2017"] + v2$fcaso[v2$ano== "2017"])
addmargins(tabela)
round(prop.table(tabela, 1),2)





# Porcentagen amostras procesadas para notificacao PPC ----
# Amostragem em casos
v2 %>%
  group_by(ano)%>%
  filter(caso =="1")%>%
  # filter(ano !='2014')%>%
  summarize(casos_PSC=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            por_amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)

#   ano          casos   pop amostras positivas %amostragem
# 2 2015            82  2082      446       264     0.214 
# 3 2016            30  1275      267       125     0.209 
# 4 2017            39  1072      547       149     0.510 
# 5 2018            60  1959      763       215     0.389 
# 6 2019            35  1019      414       132     0.406 
# 7 2020            13   347      149        46     0.429 

# Numero de animais examinados 2014-2020
# 149,402,763
v2 %>%
  group_by(ano)%>%
  filter(caso =="1")%>%
  # filter(ano !='2014')%>%
  summarize(casos_PSC=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            por_amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)

a<- v2 %>%
  group_by(ano)%>%
  filter(caso =="1")%>%
  # filter(ano !='2014')%>%
  summarize(casos_PSC=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            por_amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)
summary(a$amostras)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 149.0   247.5   414.0   402.0   496.5   763.0

# Numero de amostras procesadas por caso ----
amos <- v2 %>%
  group_by(ano,orden)%>%
  filter(caso =="1")%>%
  summarize(casos=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)

summary(amos$amostras)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   4.000   8.325   9.000 102.000 

summary(amos$amostragem)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1341  0.4330  0.4321  0.5804  2.4000
summary(amos$por_positividade)

summary(amos$prev)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0007559 0.0802393 0.2152510 0.2603661 0.4000000 2.0000000 

#numero de amostras procesadas por regiao
v2 %>%
  group_by(ano,regiao)%>%
  filter(caso =="1")%>%
  filter(ano == "2017")%>%
  summarize(casos=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)





33# Amostragem em controles
v2 %>%
  group_by(ano)%>%
  filter(caso =="0")%>%
  # filter(ano !='2014')%>%
  summarize(casos_PSC=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            por_amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)

#    ano     controles   pop amostras positivas amostragem
# 2 2015           247 28742     1246         0     0.0434
# 3 2016           112  3752      722         0     0.192 
# 4 2017           134 13482      800         0     0.0593
# 5 2018           151 33773      825         0     0.0244
# 6 2019           132 10157      700         0     0.0689
# 7 2020            64  1700      286         0     0.168 

amos <- v2 %>%
  group_by(ano,orden)%>%
  filter(caso =="0")%>%
  summarize(controles=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            amostragem=amostras/pop)

summary(amos$amostragem)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.08466 0.26087 0.35131 0.50000 3.00000      10 


# Amostragem geral
v2 %>%
  group_by(ano)%>%
  # filter(ano !='2014')%>%
  summarize(casos_PSC=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            por_amostragem=amostras/pop,
            por_positividade=positivas/amostras,
            prev=positivas/pop)

hist(v2$populacao[v2$caso=='1' & v2$ano =='2017'])
ano   notificacoes   pop amostras positivas amostragem
2 2015           329 30824     1692       264     0.0549
3 2016           142  5027      989       125     0.197 
4 2017           173 14554     1347       149     0.0926
5 2018           211 35732     1588       215     0.0444
6 2019           167 11176     1114       132     0.0997
7 2020            77  2047      435        46     0.213 
amos <- v2 %>%
  group_by(ano,orden)%>%
  filter(ano !='2014')%>%
  summarize(notificacoes=n(),
            pop=sum(populacao),
            amostras=sum(as.numeric(total_muestras)),
            positivas=sum(as.numeric(pos)),
            amostragem=amostras/pop)

summary(amos$amostragem)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  0.0000  0.1200  0.3333  0.3904  0.5000  3.0000







# 3 Vacinação ----
# Home folder ----
# Transfering higher cadastral number since 2016-2019 population ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/2017/")
v17 <- read.csv("vac2017m.csv", colClasses = "character")
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/2018/")
v18 <- read.csv("vac2018m.csv", colClasses = "character")
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/2019/")
v19 <- read.csv("vac2019m.csv", colClasses = "character")
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/2020/")
v20 <- read.csv("vac2020m.csv", colClasses = "character")

# delete cuvs anulados from 19 and 20
v19 <- v19[v19$Estado != "anulado",]
v20 <- v20[v20$Estado != "anulado",]

#delete colum estado
v19$Estado <- NULL
v20$Estado <- NULL

# rbind of all CUV
vac <- rbind(v17,v18,v19)

#number of animals
vac$Número.Productos.Vacunados <- as.numeric(vac$Número.Productos.Vacunados)

#adding year
vac$year <- year(dmy(vac$Fecha.Vacunacion))

vac %>%
  group_by(year)%>%
  summarise(vac=sum(Número.Productos.Vacunados, na.rm = TRUE))

vac %>%
  group_by(m, year)%>%
  summarise(vac=sum(Número.Productos.Vacunados, na.rm = TRUE))%>%
  spread(key = "m", value="vac")

colnames(vac)
#  Agregando provincia----
vac$pro <- tolower(iconv(vac$Provincia.Sitio, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(vac$pro)
vac$regiao <- vac$pro

vac$regiao <- gsub("azuay", "serra", vac$regiao)
vac$regiao <- gsub("bolivar", "serra", vac$regiao)
vac$regiao <- gsub("canar", "serra", vac$regiao)
vac$regiao <- gsub("carchi", "serra", vac$regiao)
vac$regiao <- gsub("chimborazo", "serra", vac$regiao)
vac$regiao <- gsub("cotopaxi", "serra", vac$regiao)
vac$regiao <- gsub("el oro", "litoral", vac$regiao)
vac$regiao <- gsub("santa elena", "litoral", vac$regiao)
vac$regiao <- gsub("guayas", "litoral", vac$regiao)
vac$regiao <- gsub("imbabura", "serra", vac$regiao)
vac$regiao <- gsub("loja", "serra", vac$regiao)
vac$regiao <- gsub("esmeraldas", "litoral", vac$regiao)
vac$regiao <- gsub("los rios", "litoral", vac$regiao)
vac$regiao <- gsub("manabi", "litoral", vac$regiao)
vac$regiao <- gsub("morona santiago", "amazonia", vac$regiao)
vac$regiao <- gsub("napo", "amazonia", vac$regiao)
vac$regiao <- gsub("orellana", "amazonia", vac$regiao)
vac$regiao <- gsub("pastaza", "amazonia", vac$regiao)
vac$regiao <- gsub("pichincha", "serra", vac$regiao)
vac$regiao <- gsub("santo domingo de los tsachilas", "litoral", vac$regiao)
vac$regiao <- gsub("sucumbios", "amazonia", vac$regiao)
vac$regiao <- gsub("tungurahua", "serra", vac$regiao)
vac$regiao <- gsub("zamora chinchipe", "amazonia", vac$regiao)
table(vac$regiao)

colnames(vac)

vac %>%
  filter(year=="2019")%>%
  summarise(vacunados=length(unique(
    paste(Identificación.Propietario,Nombre.Sitio))))

86014

# Proporcao de vacinacao
vac %>%
  group_by(regiao,year)%>%
  filter(year=="2017")%>%
  summarise(vacunados=length(unique(
    paste(Identificación.Propietario,Nombre.Sitio))),
            porcentagem=vacunados/86014)

# Numero de animais vaccinados
vac %>%
  # group_by(regiao,year)%>%
  filter(year=="2019")%>%
  summarise(sum(Número.Productos.Vacunados))

# Proporcao por categoria de risco
vac %>%
  group_by(regiao,year)%>%
  filter(year=="2017")%>%
  summarise(nvac=sum(Número.Productos.Vacunados),
            por=nvac/1811077)
   regiao    year `sum(Número.Productos.Vacunados)`
1 amazonia  2017                             60637  0.03
2 litoral   2017                           1102811  0.609
3 serra     2017                            647629  0.358


# 4 Fiscalizacção vacinação ----
library(tidyverse)
library(lubridate)
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/vacinacao/fiscalizacion/codigo_sitio")
fv1 <- readxl::read_excel("fiscalizacion2016.xlsx")
fv2 <- readxl::read_excel("fiscalizacion2017.xlsx")
fv3 <- readxl::read_excel("fiscalizacion2018.xlsx")
fv4 <- readxl::read_excel("fiscalizacion2019.xlsx")

fv <- rbind(fv2,fv3,fv4)
fv <-fv %>% mutate(ano=substring(fecha_fiscalizacion, 1,4))


#8626 fiscalizaçoes
length(unique(fv$codigo_sitio)) #6213 predios fiscalizados

fv %>% 
  group_by(ano)%>%
  summarise(n=n(),
            predios_fiscalizados = length(unique(codigo_sitio)),
            pos = sum(estado_fiscalizacion == "positivo"),
            neg = sum(estado_fiscalizacion == "negativo"))

#   ano       n predios_fiscalizados   pos   neg
# 2 2016   2316                 2125  2183   133
# 3 2017   9516                 7374  8646   870
# 4 2018   8584                 6184  7676   908
# 5 2019  10120                 7136  8660  1460

fv %>% 
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion))
  )%>%
  filter(ano=="2019")%>%
  summarise(fiscalizaçoes=n(),
            predios_fiscalizados = length(unique(codigo_sitio)),
            pos = sum(estado_fiscalizacion == "positivo"),
            neg = sum(estado_fiscalizacion == "negativo"))



fv %>% 
  group_by(ano,estado_fiscalizacion) %>%
  summarise(fisc=n(), n_predios=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=n_predios)

# ano   negativo positivo
# 2 2016       127     2013
# 3 2017       719     6824
# 4 2018       721     5645
# 5 2019      1141     6236

# Replacing every year manually
fv %>%
  group_by(ano,estado_fiscalizacion) %>%
  filter(ano=="2019")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano=="2019"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)

#    ano   negativo positivo
# 1 2017         7       38
# 2 2018         8       38
# 3 2019         8       37

fv %>%
  group_by(ano,
           estado_fiscalizacion, 
           month=month(ymd(fecha_fiscalizacion))
           ) %>%
  filter(ano == "2017")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2017"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)

#    ano   month negativo positivo
# 1  2017      1       NA        3
# 2  2017      2       NA        2
# 3  2017      3       NA        6
# 4  2017      4       NA        5
# 5  2017      5       NA        5
# 6  2017      6        3        5
# 7  2017      7        1        9
# 8  2017      8        2        3
# 9  2017      9        1        1
# 10 2017     10        1        6
# 11 2017     11       NA        4
# 12 2017     12       NA        3


# Identificando os casos positivos a PPC
# ano   negativo  positivo
# 2017        0          10
# 2018        6          20
# 2019        1           7

fv %>%
  group_by(ano,estado_fiscalizacion, 
           month=month(ymd(fecha_fiscalizacion))
  ) %>%
  filter(ano == "2017")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2017" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)


fv %>%
  group_by(ano,estado_fiscalizacion, 
           # month=month(ymd(fecha_fiscalizacion))
  ) %>%
  filter(ano == "2018")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2018" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)


fv %>%
  group_by(ano,estado_fiscalizacion, 
           # month=month(ymd(fecha_fiscalizacion))
  ) %>%
  filter(ano == "2019")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2019" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)


# Fiscalizacion olhar o numero de visitas que tecnicos fazem
fv %>% 
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion)),
           provincia,usuario_responsable)%>%
  # filter(ano=="2017")%>%
  summarise(n=n()/20,
            predios_fiscalizados = length(unique(codigo_sitio))/20,
            pos = sum(estado_fiscalizacion == "positivo"),
            neg = sum(estado_fiscalizacion == "negativo"))%>%
  arrange(desc(predios_fiscalizados))

# Ruben dario mar 419 fiscalizaciones ano 2019



# Codigo para olhar se as fiscalizacoes tem relacao com as notificacoes
# Crio um codigo que filtre os predios pelo nome da cedula, e ano, fazendo
# relacao com a data da fiscalizacao e a data da notificacao, se eles estiverem
# 30 dias antes o depois da notificacao entendemos que tem relacao.
# incluimos tambem um filtro para saber se gerou um caso de PSC ou nao
v2017 <- fv %>%
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion)),
           identificacion_propietario,
           estado_fiscalizacion, 
  ) %>%
  filter(ano == "2018")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2018" & v2$caso == "1"])%>%
  summarise(fisc=length(unique(codigo_sitio)))%>%
  spread(key=estado_fiscalizacion, value=fisc)

v2017$fecha_fisc <- fv$fecha_fiscalizacion[match(v2017$identificacion_propietario,
                                                 fv$identificacion_propietario)]

v2017$fecha_fisc <- ymd(v2017$fecha_fisc)
v2017$fecha_noti <- v2$f_notificación[match(v2017$identificacion_propietario,
                                            v2$identificador_operador)]
v2017$fecha_noti <- dmy(v2017$fecha_noti)
v2017$dias_not2fisc <- v2017$fecha_fisc - v2017$fecha_noti 

v17 <-v2017
v18 <-v2017
v19 <-v2017


# Conferir com o arquivo v19 os dias de diferenca -/+ 30



# 4 Movimentação ----
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/Codigo_sitio")
mov18 <- readxl::read_excel("movilizacion2019.xlsx")
length(unique(mov18$numero_certificado))# 2017:321482 ; 2018:401189 2019:489337 ;movimentações
sum(as.numeric(mov18$cantidad)) #2017:3005958; 2018:3472296; 2019:3941454

mov18 <- data.frame(mov18)
banco <- createUniqueIds(mov18,
                         from = "codigo_sitio_origen",
                         to= "codigo_sitio_destino")



library(epinemo); library(tidyverse)

# 3 Predios que movimentaram ----
length(unique(banco$correspondence$network.id)) #2017:66787 ; 2018:82253; 2019:101547

#5 Fiscalização de movimentacao
# Ficalización movimentação
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/fiscalizacion/codigo_sitio")
fm <- read.csv("fiscalizacao2016-2019.csv", colClasses = "character")

# setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/movimentacao/fiscalización 09.2016-12.2018")
# fm18 <- read.csv(file = "fiscalmov2016-2018.csv", colClasses = "character")
# fm18 <- fm18 %>%
#   mutate(ano = substring(fecha.fiscalizacion, 1, 4))

#fm18 <-fm18[fm18$ano == "2018",]

#fm18$sitio <- paste(fm18$identificacion.propietario, fm18$sitio.origen)
#200075 fiscalizacoes de movimentação
table(fm$tipo_usuario)

# numero de fiscalizacoes
fm %>%
  group_by(ano) %>%
  summarise(n())

# ano    `n()`
# 1 2016    1602
# 2 2017  146771
# 3 2018  202176
# 4 2019  247315

fm %>%
  group_by(ano, tipo_usuario) %>%
  summarise(fiscalizadores=length(unique(identificacion_responsable_fiscalizacion)))


fm %>%
  select(ano, codigo_sitio_origen, tipo_usuario)%>%
  group_by(ano) %>%
  summarise(predios=length(unique(codigo_sitio_origen)))

#   ano   predios
# 1 2016      370
# 2 2017    32605
# 3 2018    40992
# 4 2019    49717

fm %>% group_by(ano, resultado, tipo_usuario) %>%
  filter(ano != "2016")%>%
  #filter(tipo_usuario == "usuario externo")%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key = tipo_usuario, value=fisc)

# 1 2017  negativo   3611
# 2 2017  positivo  31611
# 3 2018  negativo   4947
# 4 2018  positivo  39543
# 5 2019  negativo   4756
# 6 2019  positivo  48581

31611/(31611+3611)

table(fm$accion_correctiva)


# Fiscalizacoes cada mes
# Para calcular sensibilidad na movimentaçao 7.04.22----

library(lubridate)

# Anuais
fm %>%
  select(ano, resultado,
         codigo_sitio_origen, 
         tipo_usuario, 
         fecha_fiscalizacion,
         accion_correctiva)%>%
  group_by(ano, tipo_usuario) %>%
  filter(ano == 2017)%>%
  # filter(resultado=="negativo")%>%
  filter(accion_correctiva=="inactivar emision de certificado")%>%
  summarise(predios=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=predios)


fm %>%
  select(ano, resultado,
         codigo_sitio_origen, 
         tipo_usuario, 
         fecha_fiscalizacion,
         accion_correctiva)%>%
  group_by(ano, month(ymd(fecha_fiscalizacion)), tipo_usuario) %>%
  filter(ano == 2017)%>%
  filter(accion_correctiva=="inactivar emision de certificado")%>%
  summarise(predios=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=predios)


fm %>%
  select(ano, codigo_sitio_origen, tipo_usuario, fecha_fiscalizacion)%>%
  group_by(ano, month(ymd(fecha_fiscalizacion))) %>%
  filter(ano == 2017)%>%
  summarise(predios=length(unique(codigo_sitio_origen)))


# Fiscalizações por interno ou externo predios
library(tidyr)
fm %>% group_by(ano, tipo_usuario, resultado) %>%
  filter(ano != "2016")%>%
  filter(tipo_usuario == "usuario externo")%>%
  # filter(tipo_usuario == "usuario interno")%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=resultado, value=fisc)

# ano   tipo_usuario       negativo positivo
# 1 2017  usuario externo     1656  + 25353
# 2 2018  usuario externo     2620    32383
# 3 2019  usuario externo     2111    41666 

# ano   tipo_usuario       negativo positivo
# 1 2017  usuario interno     2259     9741
# 2 2018  usuario interno     2609    10955
# 3 2019  usuario interno     2946    10284

# 2017
# externo 
(1656 + 25353)/((2259 +  9741)+(1656 + 25353))
#0.6923 de fiscalizacoes efetuadas pelos externos

#internos
(2259+  9741)/((2259+  9741)+(1656+ 25353))

(1656)/(1656+ 25353)
# 0.0613 Externo ficalizacao negativa

#interno 
2259/(2259+  9741)
#0.188 Interno fiscalizacao negativa





# Numero de fiscalizações inactivação de usuario
# amostra de vigilancia por movimentacao
#mudar ano e usuario para reporte
fm %>%
  group_by(ano,
           month(ymd(fecha_fiscalizacion)),
           tipo_usuario, resultado, accion_correctiva) %>%
  filter(ano == "2019")%>%
  filter(accion_correctiva == "inactivar emision de certificado")%>%
  filter(tipo_usuario == "usuario externo")%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=resultado, value=fisc)






# Fiscalização de movimentação que termina em notificação
fm %>% select(identificacion_propietario,ano, codigo_sitio_origen, tipo_usuario, resultado, 
              accion_correctiva)%>%
  group_by(ano,tipo_usuario, resultado) %>%
  filter(resultado == "negativo") %>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(identificacion_propietario %in% v2$identificador_operador)%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=fisc)

# ano   resultado                `externo` `interno`
# 1 2016  negativo                 NA             1
# 2 2017  negativo                 33             7
# 3 2018  negativo                 30            15
# 4 2019  negativo                 33             8


month(ymd(fecha_fiscalizacion))

# Fiscalização de movimentação que termina em notificação
fm %>% select(identificacion_propietario,
              ano, 
              codigo_sitio_origen, 
              tipo_usuario, 
              resultado, 
              accion_correctiva,
              fecha_fiscalizacion)%>%
  group_by(ano,
           month=month(ymd(fecha_fiscalizacion)),
           tipo_usuario 
  ) %>%
  filter(resultado == "negativo") %>%
  filter(ano == 2017)%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(identificacion_propietario %in% v2$identificador_operador[v2$ano == "2017"])%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=fisc)





# Fiscalização que terminou em notificação e fez um caso positivo
fm %>% select(identificacion_propietario,ano, codigo_sitio_origen, tipo_usuario, resultado, 
              accion_correctiva)%>%
  group_by(ano,tipo_usuario, resultado) %>%
  filter(resultado == "negativo") %>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(accion_correctiva != "anular certificado")%>%
  filter(identificacion_propietario %in% v3$identificador_operador)%>%
  summarise(fisc=length(unique(codigo_sitio_origen)))%>%
  spread(key=tipo_usuario, value=fisc)

# ano   resultado `usuario externo` `usuario interno`
# 1 2017  negativo            5                NA
# 2 2018  negativo            4                 1
# 3 2019  negativo            3                 3

# Para identificar as fiscalizações que terminaram como caso positivo
table(fm$identificacion_propietario %in% v3$identificador_operador)
# 0 zero


# ppc em 2 CSMI usuario interno

#quantas fiscalizações se encontran nos eventos sanitarios
table(fm18$identificacion.propietario %in% v2$cedula)
# FALSE  TRUE 
# 199388  687

fm18 %>% group_by(tipo.usuario, resultado, accion.correctiva) %>%
  #filter(resultado == "negativo")%>%
  filter(identificacion.propietario %in% v2$cedula) %>%
  summarise(fisc=n())%>%
  spread(key=resultado, value=fisc)

fm18 %>% group_by(tipo.usuario, resultado, accion.correctiva) %>%
  filter(identificacion.propietario %in% v2$cedula) %>%
  summarise(fisc=n())%>%
  spread(key=resultado, value=fisc)






table(fis18$resultado)
#9066 negativo

table(fis18$accion.correctiva)
# Anular certificado 3981
# Inactivar emision 5085




#olhando o cadastro quantos codigos de sitio existem
# Banco Cadastro 2019-sept ----
setwd("~/Dropbox/0.USP/1 Projeto/Conferir-dados")
c <- readxl::read_excel("catastroProdCom2019-09-18.xlsx")
length(unique(c$codigo.sitio)) #207735


# Adding region
# creating the regions columns
c$pro <- tolower(iconv(c$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(c$pro)
c$regiao <- c$pro

c$regiao <- gsub("azuay", "serra", c$regiao)
c$regiao <- gsub("bolivar", "serra", c$regiao)
c$regiao <- gsub("canar", "serra", c$regiao)
c$regiao <- gsub("carchi", "serra", c$regiao)
c$regiao <- gsub("chimborazo", "serra", c$regiao)
c$regiao <- gsub("cotopaxi", "serra", c$regiao)
c$regiao <- gsub("el oro", "litoral", c$regiao)
c$regiao <- gsub("santa elena", "litoral", c$regiao)
c$regiao <- gsub("guayas", "litoral", c$regiao)
c$regiao <- gsub("imbabura", "serra", c$regiao)
c$regiao <- gsub("loja", "serra", c$regiao)
c$regiao <- gsub("esmeraldas", "litoral", c$regiao)
c$regiao <- gsub("los rios", "litoral", c$regiao)
c$regiao <- gsub("manabi", "litoral", c$regiao)
c$regiao <- gsub("morona santiago", "amazonia", c$regiao)
c$regiao <- gsub("napo", "amazonia", c$regiao)
c$regiao <- gsub("orellana", "amazonia", c$regiao)
c$regiao <- gsub("pastaza", "amazonia", c$regiao)
c$regiao <- gsub("pichincha", "serra", c$regiao)
c$regiao <- gsub("santo domingo de los tsachilas", "litoral", c$regiao)
c$regiao <- gsub("sucumbios", "amazonia", c$regiao)
c$regiao <- gsub("tungurahua", "serra", c$regiao)
c$regiao <- gsub("zamora chinchipe", "amazonia", c$regiao)

c$regiao <- gsub("amazonia", "3amazonic", c$regiao)
c$regiao <- gsub("litoral", "3coastal", c$regiao)
c$regiao <- gsub("serra", "1highlands", c$regiao)
table(c$regiao)
table(c$regiao)
c$
setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2017")
cad17 <- read.csv(file="cad2017.csv", colClasses = "character")
colnames(cad17)

cad17$pro <- tolower(iconv(cad17$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad17$pro)
cad17$pro <- tolower(iconv(cad17$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad17$pro)
cad17$regiao <- cad17$pro

cad17$regiao <- gsub("azuay", "serra", cad17$regiao)
cad17$regiao <- gsub("bolivar", "serra", cad17$regiao)
cad17$regiao <- gsub("canar", "serra", cad17$regiao)
cad17$regiao <- gsub("carchi", "serra", cad17$regiao)
cad17$regiao <- gsub("chimborazo", "serra", cad17$regiao)
cad17$regiao <- gsub("cotopaxi", "serra", cad17$regiao)
cad17$regiao <- gsub("el oro", "litoral", cad17$regiao)
cad17$regiao <- gsub("santa elena", "litoral", cad17$regiao)
cad17$regiao <- gsub("guayas", "litoral", cad17$regiao)
cad17$regiao <- gsub("imbabura", "serra", cad17$regiao)
cad17$regiao <- gsub("loja", "serra", cad17$regiao)
cad17$regiao <- gsub("esmeraldas", "litoral", cad17$regiao)
cad17$regiao <- gsub("los rios", "litoral", cad17$regiao)
cad17$regiao <- gsub("manabi", "litoral", cad17$regiao)
cad17$regiao <- gsub("morona santiago", "amazonia", cad17$regiao)
cad17$regiao <- gsub("napo", "amazonia", cad17$regiao)
cad17$regiao <- gsub("orellana", "amazonia", cad17$regiao)
cad17$regiao <- gsub("pastaza", "amazonia", cad17$regiao)
cad17$regiao <- gsub("pichincha", "serra", cad17$regiao)
cad17$regiao <- gsub("santo domingo de los tsachilas", "litoral", cad17$regiao)
cad17$regiao <- gsub("sucumbios", "amazonia", cad17$regiao)
cad17$regiao <- gsub("tungurahua", "serra", cad17$regiao)
cad17$regiao <- gsub("zamora chinchipe", "amazonia", cad17$regiao)

cad17$regiao <- gsub("amazonia", "3amazonic", cad17$regiao)
cad17$regiao <- gsub("litoral", "3coastal", cad17$regiao)
cad17$regiao <- gsub("serra", "1highlands", cad17$regiao)
table(cad17$regiao)
table(cad17$regiao)

setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2018")
cad18 <- read.csv(file="cad2018.csv", colClasses = "character")
length(unique(paste(cad18$identificacion.propietario, cad18$nombre.sitio))) #126168
sum(as.numeric(cad18$cantidad)) #2354109

cad18$pro <- tolower(iconv(cad18$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad18$pro)
cad18$regiao <- cad18$pro

cad18$regiao <- gsub("azuay", "serra", cad18$regiao)
cad18$regiao <- gsub("bolivar", "serra", cad18$regiao)
cad18$regiao <- gsub("canar", "serra", cad18$regiao)
cad18$regiao <- gsub("carchi", "serra", cad18$regiao)
cad18$regiao <- gsub("chimborazo", "serra", cad18$regiao)
cad18$regiao <- gsub("cotopaxi", "serra", cad18$regiao)
cad18$regiao <- gsub("el oro", "litoral", cad18$regiao)
cad18$regiao <- gsub("santa elena", "litoral", cad18$regiao)
cad18$regiao <- gsub("guayas", "litoral", cad18$regiao)
cad18$regiao <- gsub("imbabura", "serra", cad18$regiao)
cad18$regiao <- gsub("loja", "serra", cad18$regiao)
cad18$regiao <- gsub("esmeraldas", "litoral", cad18$regiao)
cad18$regiao <- gsub("los rios", "litoral", cad18$regiao)
cad18$regiao <- gsub("manabi", "litoral", cad18$regiao)
cad18$regiao <- gsub("morona santiago", "amazonia", cad18$regiao)
cad18$regiao <- gsub("napo", "amazonia", cad18$regiao)
cad18$regiao <- gsub("orellana", "amazonia", cad18$regiao)
cad18$regiao <- gsub("pastaza", "amazonia", cad18$regiao)
cad18$regiao <- gsub("pichincha", "serra", cad18$regiao)
cad18$regiao <- gsub("santo domingo de los tsachilas", "litoral", cad18$regiao)
cad18$regiao <- gsub("sucumbios", "amazonia", cad18$regiao)
cad18$regiao <- gsub("tungurahua", "serra", cad18$regiao)
cad18$regiao <- gsub("zamora chinchipe", "amazonia", cad18$regiao)

cad18$regiao <- gsub("amazonia", "3amazonic", cad18$regiao)
cad18$regiao <- gsub("litoral", "3coastal", cad18$regiao)
cad18$regiao <- gsub("serra", "1highlands", cad18$regiao)
table(cad18$regiao)
table(cad18$regiao)


setwd("/media/alfredo/Backup/USP/Projeto fapesp/Dados/Cadastro/2019/antigos/")
cad19 <- read.csv(file="cad2019.csv", colClasses = "character")

cad19$pro <- tolower(iconv(cad19$provincia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
table(cad19$pro)
cad19$regiao <- cad19$pro

cad19$regiao <- gsub("azuay", "serra", cad19$regiao)
cad19$regiao <- gsub("bolivar", "serra", cad19$regiao)
cad19$regiao <- gsub("canar", "serra", cad19$regiao)
cad19$regiao <- gsub("carchi", "serra", cad19$regiao)
cad19$regiao <- gsub("chimborazo", "serra", cad19$regiao)
cad19$regiao <- gsub("cotopaxi", "serra", cad19$regiao)
cad19$regiao <- gsub("el oro", "litoral", cad19$regiao)
cad19$regiao <- gsub("santa elena", "litoral", cad19$regiao)
cad19$regiao <- gsub("guayas", "litoral", cad19$regiao)
cad19$regiao <- gsub("imbabura", "serra", cad19$regiao)
cad19$regiao <- gsub("loja", "serra", cad19$regiao)
cad19$regiao <- gsub("esmeraldas", "litoral", cad19$regiao)
cad19$regiao <- gsub("los rios", "litoral", cad19$regiao)
cad19$regiao <- gsub("manabi", "litoral", cad19$regiao)
cad19$regiao <- gsub("morona santiago", "amazonia", cad19$regiao)
cad19$regiao <- gsub("napo", "amazonia", cad19$regiao)
cad19$regiao <- gsub("orellana", "amazonia", cad19$regiao)
cad19$regiao <- gsub("pastaza", "amazonia", cad19$regiao)
cad19$regiao <- gsub("pichincha", "serra", cad19$regiao)
cad19$regiao <- gsub("santo domingo de los tsachilas", "litoral", cad19$regiao)
cad19$regiao <- gsub("sucumbios", "amazonia", cad19$regiao)
cad19$regiao <- gsub("tungurahua", "serra", cad19$regiao)
cad19$regiao <- gsub("zamora chinchipe", "amazonia", cad19$regiao)

cad19$regiao <- gsub("amazonia", "3amazonic", cad19$regiao)
cad19$regiao <- gsub("litoral", "3coastal", cad19$regiao)
cad19$regiao <- gsub("serra", "1highlands", cad19$regiao)
table(cad19$regiao)

table(cad19$identificacion.propietario %in% c$identificacion.propietario)

# Relação entre propriedades e proprietarios
length(unique(c$codigo.sitio))-length(unique(c$identificacion.propietario))
207735/202302
1.026 mais propriedades que predios

#cuantos proprietarios se encontraram registrados em cualquer ano              
proprietarios <- length(unique(c(c$identificacion.propietario,
                cad17$identificacion.propietario,
                cad18$identificacion.propietario,
                cad19$identificacion.propietario)))

# 258997 propietarios. Asumo que cada proprietario tem 1.026 propriedades
# propriedades <- 258997*1.026
# 264176.9

propriedades/407475
propriedades/342421
0.648 considerando as propriedades do censo 2000

#vou fazer comparacoes com o maior número de propriedades ja cadastradas
#Maior numero de sitios do cadastro histórico ----
cadastro_total <-length(unique(c(c$codigo.sitio))) #2018
length(unique(cad17$identificacion.propietario))

a <- length(unique(c$identificacion.propietario))*1.026
207735
207561.9

# 2017
b <- length(unique(cad17$identificacion.propietario))
b <- length(unique(cad17$nombre.sitio))
b <- length(unique(paste(cad17$identificacion.propietario, cad17$nombre.sitio)))
cad17$codigo.sitio <- paste(cad17$identificacion.propietario, cad17$nombre.sitio)

length(unique(cad17$codigo.sitio))
cad17 %>%
  group_by(regiao)%>%
  summarize(n=length(unique(codigo.sitio)),
            n/105542)

#   regiao         n `n/105542`
# 1 1highlands 82539     0.782 
# 2 3amazonic   5081     0.0481
# 3 3coastal   17922     0.170 

# 2018
d <- length(unique(cad18$identificacion.propietario))
123713
# 2019
e <- length(unique(cad19$identificacion.propietario))
103296

cad18$codigo.sitio <- paste(cad18$identificacion.propietario, cad18$nombre.sitio)

length(unique(cad18$codigo.sitio))

cad18 %>%
  group_by(regiao)%>%
  summarize(n=length(unique(codigo.sitio)),
            n/126168)

# regiao         n `n/126168`
# 1 1highlands 97394     0.772 
  # 2 3amazonic   6665     0.0528
# 3 3coastal   22114     0.175 


cad19$codigo.sitio <- paste(cad19$identificacion.propietario, cad19$nombre.sitio)
length(unique(cad19$codigo.sitio))

cad19 %>%
  group_by(regiao)%>%
  summarize(n=length(unique(codigo.sitio)),
            n/105083)

# regiao         n `n/105083`
# 1 1highlands 78588     0.748 
# 2 3amazonic   5524     0.0526
# 3 3coastal   20973     0.200 



cadastro_anual <- c(b,d,e)
proporcao_cadastro <- cadastro_anual/a 

mean(proporcao_cadastro)
sd(proporcao_cadastro)



# comparacção existencias animais ----
sum(as.numeric(cad17$cantidad))
1204824
sum(as.numeric(cad18$cantidad))
1311989
sum(as.numeric(cad19$Cantidad.activos), na.rm = TRUE) + sum(as.numeric(cad19$Cantidad.inactivos), na.rm = TRUE)

# Cadastro (2017,2018,2019)
ESPAC (2017,2018,2019)
1115473
1283338
1162685

Agrocalidad (2017,2018,2019) Considering mataderos and ferias
1204824
1311989
1504601

withouth mataderos e ferias
1097469
1207607
1111668

Agrocalidad/ESPAC (2017,2018,2019)
1204824/1115473  8.01%
1311989/1283338  2.23%
1504601/1162685  29.41%

# Withouth mataderos
1-1097469/1115473  -1.6%
1-1207607/1283338  -5.9%
1-1111668/1162685  -4.4%


length(unique(paste(cad19$identificacion.propietario, 
                    cad19$nombre.sitio)))




# Comparar com codigo

# Notificacoes de ppc que tiveram diagnostico
v2 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(ano,amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

#     ano amostrado notifi surtos amostras
# 1  2015 0              1      0        0
# 2  2015 1            218     82     1572
# 3  2016 1            127     30      844
# 4  2017 1            160     39     1000
# 5  2018 1            178     60     1212
# 6  2019 1            160     35      961

v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(ano, amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

library(tidyverse)
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico != "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"si","no"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(amostrado, detalle_diagnóstico)%>%
  summarise(notifi=length(unique(orden)), 
            brotes=length(unique(subset(orden, pos>=1))),
            negativos=notifi-brotes,
            amostras=sum(total_muestras, na.rm = TRUE),
            total=brotes+negativos)

length(unique(v2$orden[v2$ano >2014 & v2$ano <2020],))
932+486

v2 %>%
  filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  filter(detalle_diagnóstico != "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"si","no"))%>%
  filter(ano <2020)%>%
  filter(ano >2014)%>%
  group_by(amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            brotes=length(unique(subset(orden, pos>=1))),
            negativos=notifi-brotes,
            amostras=sum(total_muestras, na.rm = TRUE),
            total=brotes+negativos)

#Porcentagem de amostragem
v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(amostrado)%>%
  summarise(notifi=length(unique(orden)), 
            surtos=length(unique(subset(orden, pos>=1))),
            amostras=sum(total_muestras, na.rm = TRUE))

v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  group_by(ano, amostrado)%>%
  summarise(notifi=length(unique(orden)))%>%
  spread(key="amostrado", value="notifi")

# % de amostragem nas notificacoes (desestimacao de casos)
1  2017   116   /175  0.6628
2  2018    92   /214  0.4299
3  2019   105   /168  0.625

# 2017:
122/(175+116) #notificaciones 
c <-v2 %>%
  # filter(prueba_solicitada != "Peste Porcina Clásica (Ac.)") %>%
  # filter(detalle_diagnóstico == "Peste porcina clásica")%>%
  mutate(total_muestras = ifelse(is.na(total_muestras),0,total_muestras))%>%
  mutate(amostrado=ifelse(total_muestras >=1,"1","0"))%>%
  filter(ano <2020)%>%
  filter(ano >2016)%>%
  filter(amostrado == 0)%>%
  group_by( amostrado, patología)%>%
  summarise(notifi=length(unique(orden)))




# Riscos ajustados
library(RSurveillance)
# Vigilancia ativa
# Lavagen
round(adj.risk(c(1,17.61), c(0.167,0.833)),2)
# 0.07 1.19

adj.risk(c(1,18), c(0.167,0.833))
# [1] 0.06596 1.18726

#vaccination coverage
sum(0.815, 0.185)
round(adj.risk(c(1,1.92), c(0.815, 0.185)),4)
# 0.8546 1.6407

#age
sum(0.815, 0.185)
round(adj.risk(c(1,1.57,1.45), c(0.131, 0.816, 0.053)),3)
# [1] 0.672 1.054 0.974

1	0,131
1,57	0,816
1,45	0,053



