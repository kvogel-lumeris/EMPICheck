if (!requireNamespace('htmlwidgets') || packageVersion('htmlwidgets') <= '0.3.2')
  devtools::install_github('ramnathv/htmlwidgets')
# install DT
if (!require("DT")) devtools::install_github("rstudio/DT")


library(RODBC)
library(data.table)
library(sqldf)
library(tidyr)
library(dplyr)
library(knitr)
library(pander)
library(tcltk)
library(ggplot2)

##get common functions
wd <-getwd()
setwd('..')
source('./SharedDataScripts/CommonFunctions.R')
setwd(wd)
remove(wd)


##iPaaS
IEnv <-"Stage"
IDNS <- "IPAAS"
IClient <- "CHS"
IClientDB <-IDNS
Entity <- 55


##ODS
OEnv <-"UAT 01"
ODNS <- "ODS"
OClient <- "CHS"
OClienntODS <-paste(OClient, ODNS, sep="")
OClientDB <-paste(OClient, ODNS, sep="")
Client <- 9


OServerAddress <-fnGetServerAddress(OEnv,ODNS)

sQuery <- strwrap(paste("SELECT M.SYS_MBR_SK
                        ,M.MBR_ID
                        ,M.EMPI
                        ,UPPER(LTRIM(RTRIM(M.MBR_LAST_NM))) AS MBR_LAST_NM
                        ,UPPER(LTRIM(RTRIM(M.MBR_FIRST_NM))) AS MBR_FIRST_NM
                        ,UPPER(LTRIM(RTRIM(M.MBR_GENDER_CD))) AS MBR_GENDER_CD 
                        ,M.MBR_DOB
                        ,LTRIM(RTRIM(MA.ZIP_CD)) AS ZIP_CD
                        FROM MBR M WITH (NOLOCK)
                        JOIN MBR_ADDR MA WITH (NOLOCK)
                        ON M.SYS_MBR_SK = MA.SYS_MBR_SK
                        AND M.HOME_ADDR_TP_CD_SK = MA.ADDR_TP_CD_SK
                        WHERE M.REC_DEL_IND <> 'Y'
                        AND MA.REC_DEL_IND <> 'Y'",sep = ""), width=10000, simplify=TRUE)

MEMBERS <- fnQueryDatabaseGeneric(OServerAddress, OClientDB, sQuery)
MEMBERS$MBR_DOB <- as.Date(MEMBERS$MBR_DOB)


IServerAddress <-fnGetServerAddress(IEnv, IDNS)

#Get iPaaS EMPIs
sQuery <- strwrap(paste("SELECT A.ENTITY_PATIENT_ID AS SYS_MBR_SK
                        ,A.MBR_ID, B.PATIENT_EMPI_ID AS EMPI
                        ,UPPER(LTRIM(RTRIM(A.PATIENT_LAST_NAME))) AS MBR_LAST_NM
                        ,UPPER(LTRIM(RTRIM(A.PATIENT_FIRST_NAME))) AS MBR_FIRST_NM
                        ,UPPER(LTRIM(RTRIM(A.PATIENT_GENDER))) AS MBR_GENDER_CD
                        ,A.PATIENT_DOB AS MBR_DOB
                        ,LTRIM(RTRIM(A.PATIENT_ZIP)) AS ZIP_CD
                        FROM IPAAS.PATIENT_MASTER A
                        JOIN IPAAS.PATIENT_EMPI_LINK B ON A.PATIENT_ID = B.PATIENT_ID
                        JOIN IPAAS.IFDMG001MT C ON B.PATIENT_EMPI_ID = C.EMPI_ID
                        WHERE A.ENTITY_ID = ",Client,"
                        AND C.ENTITY_ID = ",Entity,"
                        ",sep = ""), width=10000, simplify=TRUE)

IPAAS <-fnQueryDatabaseGeneric(IServerAddress, IClientDB, sQuery)
IPAAS$MBR_DOB <- as.Date(IPAAS$MBR_DOB)



#Get MBR
sQuery <-strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, A.MBR_ID, A.EMPI
                       FROM MBR A
                       JOIN PAT B ON A.SYS_MBR_SK = B.SYS_MBR_SK",sep = ""), width=10000, simplify=TRUE)

MBR <- fnQueryDatabaseGeneric(OServerAddress, OClientDB, sQuery)
MBR <- mutate(MBR, TYPE = "MBR")



#Get MBR_MNTH_DTL
sQuery <-strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, A.MBR_ID, A.EMPI
                        FROM MBR_MNTH_DTL A
                        JOIN PAT B ON A.SYS_MBR_SK = B.SYS_MBR_SK",sep = ""), width=10000, simplify=TRUE)

MBR_MNTH_DTL <- fnQueryDatabaseGeneric(OServerAddress, OClientDB, sQuery)
MBR_MNTH_DTL <- mutate(MBR_MNTH_DTL, TYPE = "MBR_MNTH_DTL")


#Get PAT
sQuery <- strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, B.MBR_ID, A.EMPI
                       FROM PAT A 
                       JOIN MBR B ON A.SYS_MBR_SK = B.SYS_MBR_SK",sep = ""), width=10000, simplify=TRUE)

PAT <- fnQueryDatabaseGeneric(OServerAddress, OClientDB, sQuery)
PAT <- mutate(PAT, TYPE = "PAT")


#Get MBR_DISPLAY_DTL
sQuery <-strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, A.MBR_ID, A.EMPI
                       FROM MBR_DISPLAY_DTL A",sep = ""), width=10000, simplify=TRUE)

MBR_DISPLAY_DTL <- fnQueryDatabaseGeneric(OServerAddress, OClientDB, sQuery)
MBR_DISPLAY_DTL <- mutate(MBR_DISPLAY_DTL, TYPE = "MBR_DISPLAY_DTL")


ODS <- bind_rows(MBR, MBR_MNTH_DTL, MBR_DISPLAY_DTL, PAT)
ODS <- ODS %>%
  inner_join(MEMBERS, by =c("MBR_ID", "SYS_MBR_SK", "EMPI"))


test <- IPAAS %>%
  left_join(ODS, by = "MBR_ID") %>%
  filter(EMPI.y != EMPI.x) %>%
  group_by(TYPE) %>%
  summarise(Mbrs = n_distinct(EMPI.x)
  ) %>%
  arrange(TYPE)

test2 <- IPAAS %>%
  inner_join(ODS, by = c("MBR_ID", "EMPI")) %>%
  group_by(TYPE) %>%
  summarise(Mbrs = n_distinct(EMPI)
  ) %>%
  arrange(TYPE)


test5 <- ODS %>%
  inner_join(MEMBERS, by ="MBR_ID")

test6 <- ODS %>%
  semi_join(MEMBERS, by ="SYS_MBR_SK")

test7 <- ODS %>%
  inner_join(MEMBERS, by =c("MBR_ID", "SYS_MBR_SK", "EMPI"))


test4 <- IPAAS %>%
  left_join(MEMBERS, by = c("MBR_LAST_NM", "MBR_FIRST_NM", "MBR_GENDER_CD", "MBR_DOB", "ZIP_CD")) %>%
  filter(EMPI.y != EMPI.x) %>%
  summarise(Mbrs = n_distinct(EMPI.x)
  ) 

test5 <- IPAAS %>%
  left_join(MEMBERS, by = c("MBR_LAST_NM", "MBR_FIRST_NM", "MBR_GENDER_CD", "MBR_DOB", "ZIP_CD")) %>%
  filter(SYS_MBR_SK.y != SYS_MBR_SK.x) %>%
  summarise(Mbrs = n_distinct(EMPI.x)
  ) 




