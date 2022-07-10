library(dplyr)
Data <- rio::import(here::here("Data", "PELA", "PELA-Fusionada.sav"))


#### Funciones auxiliares ####
remove4 <- function(x) {ifelse(x<0|x>4,NA,x)} #Función reemplazar NA
remove5 <- function(x) {ifelse(x<0|x>5,NA,x)} #Función reemplazar NA
remove7 <- function(x) {ifelse(x<0|x>7,NA,x)} #Función reemplazar NA
remove10<- function(x) {ifelse(x<0|x>10,NA,x)} #Función reemplazar NA

recode5 <- function(x) {dplyr::recode(x,`1` = 1, `2` = 1, `3` = 2, `4` = 2, 
                        `5` = 3, `6` = 3, `7` = 4, `8` = 4, `9` = 5, `10` = 5)}



#### Selección ####

Base <- Data[Data$PAIS %in% c(1,2,3,6,7,9,13,16) & Data$año < 2014, ]


#### Años de giro ####

Giros <- cbind(c("Argentina", "Bolivia", "Chile", "CostaRica", 
                 "Ecuador", "El Salvador", "Honduras", "Mexico", 
                 "Nicaragua","Paraguay", "Uruguay", "Brasil"), 
               c(1,2,3,5,6,7,9,10,11,13,16,18),
               c(2003, 2005, 2006, 2014, 2006, 
                 2009, 2005,2018, 2006, 2006, 2008, 2002))

Base <- Base %>% mutate(giro = case_when( 
                PAIS == 1 & año >= 2003 ~ 1,
                PAIS == 2 & año >= 2005 ~ 1,
                PAIS == 3 & año >= 2006 ~ 1,
                PAIS == 5 & año >= 2014 ~ 1,
                PAIS == 6 & año >= 2006 ~ 1,
                PAIS == 7 & año >= 2009 ~ 1,
                PAIS == 9 & año >= 2005 ~ 1,
                PAIS == 13& año >= 2006 ~ 1,
                PAIS == 16& año >= 2008 ~ 1,
                TRUE ~ 0))


#### Variables ####

Base$EM1 <- recode5(Base$EM1)
Base$EM1 <- remove5(Base$EM1)
Base$EM1c <- remove5(Base$EM1c)
Base$EOM <- ifelse(is.na(Base$EM1)==TRUE, Base$EM1c, Base$EM1)


Base$ID1 <- remove10(Base$ID1)
Ideologias <- doBy::summary_by(Base, ID1~PARTIDO_P+PAIS+año + giro, FUN=mean, na.rm=TRUE)
EcoM <- doBy::summary_by(Base, EOM~PARTIDO_P+PAIS+año+ giro, FUN=mean, na.rm=TRUE)
EcoV <- doBy::summary_by(Base, EOM~PARTIDO_P+PAIS+año+ giro, FUN=var, na.rm=TRUE)

BaseR <- Ideologias %>% full_join(., EcoM) %>% full_join(., EcoV)

BaseR$Derecha <- ifelse(BaseR$ID1.mean >=5.5, yes = 1, no=0)


#### Variance Test ####

Ideo_partido <- cbind.data.frame(BaseR$PARTIDO_P, BaseR$año, BaseR$Derecha)
names(Ideo_partido) <- c("PARTIDO_P", "año", "Derecha")


BaseS <- dplyr::left_join(Base, Ideo_partido, by=c("año", "PARTIDO_P"))


result <- var.test(log(Base[BaseS$giro == 1 & BaseS$Derecha == 1 ,]$EOM), 
                   log(Base[BaseS$giro == 0 & BaseS$Derecha == 1, ]$EOM),
                   alternative = "greater")



# Salida de tabla en latex
stargazer_htest = function (data, ...) {
        summary = data.frame(`Test statistic` = data$statistic,
                             DF = data$parameter,
                             `p value` = data$p.value,
                             `Alternative hypothesis` = data$alternative,
                             check.names = FALSE)
        stargazer::stargazer(summary, flip = TRUE, summary = FALSE,
                  notes = paste(data$method, data$data.name, sep = ': '), ...)
}

stargazer_htest(result)

#### BOXPLOT ####

library(ggplot2)
library(ggthemes)

BaseR$giro <- ifelse(BaseR$giro==1, "Luego", "Antes")
BaseG <- BaseR[BaseR$Derecha==1 & !is.na(BaseR$EOM.var)==T,]

BECO <- ggplot(BaseG, aes(x = as.factor(giro), y =log(EOM.var) )) + 
        stat_boxplot(geom = "errorbar", # Error bars
                     width = 0.1) +
        xlab("Giro a la izquierda")+
        ylab("log(Varianza)")+# Bars width
        geom_boxplot() + 
        theme_calc() +
        theme(text=element_text(size=12, family="Times New Roman"))


jpeg(filename = "Figures/DimEconómica.jpg", 
     width = 1700, height = 1200, res = 300)
BECO
dev.off()


#### Medias ####
mean(BaseG[BaseG$giro=="Antes",]$EOM.mean, na.rm = T)
mean(BaseG[BaseG$giro=="Luego",]$EOM.mean, na.rm = T)


