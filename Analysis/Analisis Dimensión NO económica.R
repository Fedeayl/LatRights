
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

Base$VAL2 <- recode5(Base$VAL2)
Base$VAL2 <- remove5(Base$VAL2)
Base$VAL2d <- remove5(Base$VAL2d)
Base$Aborto <- ifelse(is.na(Base$VAL2)==TRUE, Base$VAL2d, Base$VAL2)

Base$ID1 <- remove10(Base$ID1)

Base$FFAA1 <- recode5(Base$FFAA1)
Base$FFAA1 <- remove5(Base$FFAA1)
Base$FFAA1c <- remove5(Base$FFAA1c)
Base$Ejercito <- ifelse(is.na(Base$FFAA1)==TRUE, Base$FFAA1c, Base$FFAA1)


#### BOXPLOT ####

Ideologias <- doBy::summary_by(Base, ID1~PARTIDO_P+PAIS+año + giro, FUN=mean, na.rm=TRUE)
AbortoM <- doBy::summary_by(Base, Aborto~PARTIDO_P+PAIS+año+ giro, FUN=mean, na.rm=TRUE)
AbortoV <- doBy::summary_by(Base, Aborto~PARTIDO_P+PAIS+año+ giro, FUN=var, na.rm=TRUE)

EjercitoM <- doBy::summary_by(Base, Ejercito~PARTIDO_P+PAIS+año+ giro, FUN=mean, na.rm=TRUE)
EjercitoV <- doBy::summary_by(Base, Ejercito~PARTIDO_P+PAIS+año+ giro, FUN=var, na.rm=TRUE)

BaseR <- Ideologias %>% full_join(., AbortoM) %>% full_join(., AbortoV) %>%
        full_join(., EjercitoM) %>% full_join(., EjercitoV)


BaseR$Derecha <- ifelse(BaseR$ID1.mean >=5.5, yes = 1, no=0)


#### Variance Test ####

Ideo_partido <- cbind.data.frame(BaseR$PARTIDO_P, BaseR$año, BaseR$Derecha)
names(Ideo_partido) <- c("PARTIDO_P", "año", "Derecha")


BaseS <- dplyr::left_join(Base, Ideo_partido, by=c("año", "PARTIDO_P"))



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


result1 <- var.test(log(BaseS[BaseS$giro == 1 & BaseS$Derecha == 1 ,]$Aborto), 
                    log(BaseS[BaseS$giro == 0 & BaseS$Derecha == 1, ]$Aborto),
                    alternative = "less")

car::leveneTest(y = Aborto~factor(giro), BaseS[BaseS$Derecha == 1 ,])


result2 <- var.test(log(BaseS[BaseS$giro == 1 & BaseS$Derecha == 1 ,]$Ejercito), 
                    log(BaseS[BaseS$giro == 0 & BaseS$Derecha == 1, ]$Ejercito),
                    alternative = "less")

car::leveneTest(y = Ejercito~factor(giro), BaseS[BaseS$Derecha == 1 ,])

stargazer_htest(result1)
stargazer_htest(result2)


mean(BaseS[BaseS$Derecha==1 &BaseS$giro==0,]$Aborto, na.rm=T)
mean(BaseS[BaseS$Derecha==1 &BaseS$giro==1,]$Aborto, na.rm=T)

#### BOXPLOT ####

library(ggplot2)
library(patchwork)
library(ggthemes)

BaseR$giro <- ifelse(BaseR$giro==1, "Luego", "Antes")
BaseG <- BaseR[BaseR$Derecha==1 & !is.na(BaseR$Aborto.var)==T,]
BaseG2<- BaseR[BaseR$Derecha==1 & !is.na(BaseR$Ejercito.var)==T,]


ABOR <- ggplot(BaseG, aes(x = as.factor(giro), y =log(Aborto.var) )) + 
        stat_boxplot(geom = "errorbar", # Error bars
                     width = 0.1) +
        labs(title="Opinión sobre el aborto")+
        xlab("Giro a la izquierda")+
        ylab("log(Varianza)")+# Bars width
        geom_boxplot() + 
        theme_calc() +
        theme(text=element_text(size=10, family="Times New Roman"))


EJER <- ggplot(BaseG2, aes(x = as.factor(giro), y =log(Ejercito.var) )) + 
        stat_boxplot(geom = "errorbar", # Error bars
                     width = 0.1) +
        labs(title = "Valoración de las FFAA")+
        xlab("Giro a la izquierda")+
        ylab("log(Varianza)")+# Bars width
        geom_boxplot() + 
        theme_calc() +
        theme(text=element_text(size=10, family="Times New Roman"))




jpeg(filename = "Figures/DimNOEco.jpg", 
     width = 2200, height = 1200, res = 300)
ABOR+EJER
dev.off()


