Data <- rio::import(here::here("Data", "PELA", "PELA-Fusionada.sav"))

Base <- Data[Data$PAIS %in% c(1,2,3,6,7,9,13,16) & Data$año < 2014, ]



#### Creación de variables ####

Giros <- cbind(c("Argentina", "Bolivia", "Chile", "CostaRica", 
                 "Ecuador", "El Salvador", "Honduras", "Mexico", 
                 "Nicaragua","Paraguay", "Uruguay", "Brasil"), 
               c(1,2,3,5,6,7,9,10,11,13,16,18),
               c(2003, 2005, 2006, 2014, 2006, 
                 2009, 2005,2018, 2006, 2006, 2008, 2002))



Ideologias <- doBy::summary_by(Base, ID1~PARTIDO_P+PAIS+año, FUN=mean, na.rm=TRUE)


Base <- dplyr::left_join(Base, Ideologias, by=c("año", "PARTIDO_P", "PAIS"))

Base$Derecha <- ifelse(Base$ID1.mean >=5.5, yes = 1, no=0)

Base <- Base %>% dplyr::mutate(giro = dplyr::case_when( 
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



#### Clasificación de Issues ####

Vec <-  1:62

Position <- c(3, 5, 6, 7, 12, 13, 16, 18, 23, 26, 27,
              31, 36, 37, 39, 40, 47, 51, 54, 62)

Valence <- dplyr::setdiff(Vec, Position)

Economic <- c(1, 3, 5, 6, 18, 23, 26, 27, 31, 37,
              38, 48, 50, 51, 54, 62)

NoEconomic <-  dplyr::setdiff(Vec, Economic)


Base$PositionIssue <- ifelse(Base$PRO2011 %in% Position, 1, 0 )
Base$ValenceIssue  <- ifelse(Base$PRO2011 %in% Valence, 1, 0 )
Base$EconomicIssue <- ifelse(Base$PRO2011 %in% Economic, 1, 0 )
Base$NoEconomIssue <- ifelse(Base$PRO2011 %in% NoEconomic, 1, 0 )


#### Cuenta ####

Pos <- doBy::summary_by(Base[Base$Derecha == 1,], PositionIssue + ValenceIssue~giro, FUN=sum)[c(1,2),]
Pos$total <- c(Pos$PositionIssue.sum[1]+Pos$ValenceIssue.sum[1],
               Pos$PositionIssue.sum[2]+Pos$ValenceIssue.sum[2])


Eco <- doBy::summary_by(Base[Base$Derecha == 1,], EconomicIssue + NoEconomIssue~giro, FUN=sum)[c(1,2),]
Eco$total <- c(Eco$EconomicIssue.sum[1] + Eco$NoEconomIssue.sum[1],
               Eco$EconomicIssue.sum[2] + Eco$NoEconomIssue.sum[2])



