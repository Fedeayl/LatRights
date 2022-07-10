library(ggplot2)
library(dplyr)
library(patchwork)

Data <- rio::import(here::here("Data", "PELA", "PELA-Fusionada.sav"))

#### Funciones auxiliares ####
remove4 <- function(x) {ifelse(x<0|x>4,NA,x)} #Función reemplazar NA
remove5 <- function(x) {ifelse(x<0|x>5,NA,x)} #Función reemplazar NA
remove7 <- function(x) {ifelse(x<0|x>7,NA,x)} #Función reemplazar NA
remove10<- function(x) {ifelse(x<0|x>10,NA,x)} #Función reemplazar NA

estandar4 <- function(x) {(x-1)/(4-1)} #Función estandarización 1-4
estandar5 <- function(x) {(x-1)/(5-1)} #Función estandarización 1-5
estandar7 <- function(x) {(x-1)/(7-1)} #Función estandarización 1-7
estandar10<- function(x) {(x-1)/(10-1)} #Función estandarización 1-10



#### Creación del índice ####

# Reemplazar NAs y estandarizar
is.na(Data$RE1b) <- 0
Data$VAL1 <- estandar10(remove10(Data$VAL1))
Data$VAL1d<- estandar5(remove5(Data$VAL1d))
Data$VAL2 <- estandar5(remove5(Data$VAL2))
Data$VAL2d<- estandar10(remove10(Data$VAL2d))
Data$RE2  <- estandar10(remove10(Data$RE2))
Data$RE1b <- estandar5(remove5(Data$RE1b))



# Combinación de las dos variables
Data$VAL1hom <- ifelse(is.na(Data$VAL1)==TRUE, Data$VAL1d, Data$VAL1)
Data$VAL2hom <- ifelse(is.na(Data$VAL2)==TRUE, Data$VAL2d, Data$VAL2)
Data$REhom <- ifelse(is.na(Data$RE2)==TRUE, Data$RE1b, Data$RE2)

Data <- Data[!is.na(Data$VAL1hom) & !is.na(Data$VAL2hom),]

# Índice Conservador
Data$Ind_Con <- rowSums(Data[,c("VAL1hom", "VAL2hom")], na.rm = T) / 2

# Variable económica
Data$EM1 <- estandar10(remove10(Data$EM1))
Data$EM1c <- estandar5(remove5(Data$EM1c))
Data$EM1hom <- ifelse(is.na(Data$EM1c)==TRUE, Data$EM1, Data$EM1c)



#### Gráficos Argentina ####

Argentina <- Data[Data$PAIS == 01 & Data$año %in% c(1998, 2004, 2008),]

Argentina_Res <- Argentina[Argentina$PARTIDO_P %in% c(1,12) & Argentina$año == 1998 |
                           Argentina$PARTIDO_P %in% c(1,12) & Argentina$año == 2004 |
                           Argentina$PARTIDO_P %in% c(2, 7) & Argentina$año == 2008, ]


ArC <- ggplot(Argentina_Res, aes(x=Ind_Con, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Conservadurismo", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()

ArE <- ggplot(Argentina_Res, aes(x=EM1hom, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Estado-Mercado", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()


#### Gráficos Brasil ####

Brasil <- Data[Data$PAIS == 18 & Data$año %in% c(2005, 2010),]

Brasil_Res <- Brasil[Brasil$PARTIDO_P %in% c(492,495,496,497,507,508) & Brasil$año == 2005 |
                     Brasil$PARTIDO_P %in% c(492,495,496,498,500,502) & Brasil$año == 2010, ]



BrC <- ggplot(Brasil_Res, aes(x=Ind_Con, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice conservadurismo", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()

BrE <- ggplot(Brasil_Res, aes(x=EM1hom, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Estado-Mercado", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()

#### Gráficos Bolivia ####


Bolivia <- Data[Data$PAIS == 02 & Data$año %in% c(2003, 2006, 2010),]

Bolivia_Res <- Bolivia[Bolivia$PARTIDO_P %in% c(31,35) & Bolivia$año == 2003 |
                       Bolivia$PARTIDO_P %in% c(31,38) & Bolivia$año == 2006 |
                       Bolivia$PARTIDO_P %in% c(40)    & Bolivia$año == 2010, ]



BoC <- ggplot(Bolivia_Res, aes(x=Ind_Con, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Conservadurismo", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()


BoE <- ggplot(Bolivia_Res, aes(x=EM1hom, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Estado-Mercado", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()



#### Gráficos Chile ####
Chile <- Data[Data$PAIS == 03 & Data$año %in% c(2002, 2006, 2010),]

Chile_Res <- Chile[Chile$PARTIDO_P %in% c(53,54,55) & Chile$año == 2002 |
                           Chile$PARTIDO_P %in% c(53,54,55) & Chile$año == 2006 |
                           Chile$PARTIDO_P %in% c(54,55)    & Chile$año == 2010, ]



ChC <- ggplot(Chile_Res, aes(x=Ind_Con, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Conservadurismo", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()


ChE <- ggplot(Chile_Res, aes(x=EM1hom, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Estado-Mercado", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()


#### Gráficos El Salvador ####


Salvador <- Data[Data$PAIS == 07 & Data$año %in% c(2006, 2009, 2012),]

Salvador_Res <- Salvador[Salvador$PARTIDO_P %in% c(135, 137, 138) & Salvador$año == 2006 |
                         Salvador$PARTIDO_P %in% c(135, 137, 138) & Salvador$año == 2009 |
                         Salvador$PARTIDO_P %in% c(135, 137, 142) & Salvador$año == 2012,]


ESC <- ggplot(Salvador_Res, aes(x=Ind_Con, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Conservadurismo", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()


ESE <- ggplot(Salvador_Res, aes(x=EM1hom, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Estado-Mercado", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()



#### Gráficos Honduras ####


Honduras <- Data[Data$PAIS == 09 & Data$año %in% c(2002, 2006, 2010),]

Honduras_Res <- Honduras[Honduras$PARTIDO_P %in% c(194) & Honduras$año == 2002 |
                         Honduras$PARTIDO_P %in% c(194) & Honduras$año == 2006 |
                         Honduras$PARTIDO_P %in% c(194) & Honduras$año == 2010,]


Hn1 <- ggplot(Honduras_Res, aes(x=Ind_Con, fill=as.factor(año)))+
        geom_density(alpha=0.7)+
        xlim(0,1) +
        labs(x = "", y = "",
             title = "Indice Conservadurismo", 
             subtitle = "",
             caption = "Elaboración en base a datos de PELA",
             fill= "Año") +
        theme_minimal()


