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


OClient <- CLIENTS %>%
  filter(Abbrev == input$clientInput) %>%
  select(Abbrev) %>%
  unique()

OClientDB <-paste(OClient, "ODS", sep="")

ClientID <- CLIENTS %>%
  filter(Abbrev == input$clientInput) %>%
  select(ClientID) %>%
  unique()

ODSAddress <- SERVERS %>%
  filter(Type == "ODS") %>%
  filter(Name == input$odsenvInput) %>%
  select(Path) %>%
  unique() 

IPAASAddress <<- SERVERS %>%
  filter(Type == "IPAAS") %>%
  filter(Name == input$ipaasenvInput) %>%
  select(Path) %>%
  unique() 

EntityID <- ENTITIES() %>%
  filter(ENTITY_NM == input$entityInput) %>%
  select(ENTITY_ID) %>%
  unique() 



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

MEMBERS <- fnQueryDatabaseGeneric(ODSAddress, OClientDB, sQuery)
MEMBERS$MBR_DOB <- as.Date(MEMBERS$MBR_DOB)



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
                        WHERE A.ENTITY_ID = ",ClientID,"
                        AND C.ENTITY_ID = ",EntityID,"
                        ",sep = ""), width=10000, simplify=TRUE)

IPAAS <-fnQueryDatabaseGeneric(IPAASAddress, "IPAAS", sQuery)
IPAAS$MBR_DOB <- as.Date(IPAAS$MBR_DOB)



#Get MBR
sQuery <-strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, A.MBR_ID, A.EMPI
                       FROM MBR A
                       JOIN PAT B ON A.SYS_MBR_SK = B.SYS_MBR_SK",sep = ""), width=10000, simplify=TRUE)

MBR <- fnQueryDatabaseGeneric(ODSAddress, OClientDB, sQuery)
MBR <- mutate(MBR, TYPE = "MBR")


#Get MBR_MNTH_DTL
sQuery <-strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, A.MBR_ID, A.EMPI
                       FROM MBR_MNTH_DTL A
                       JOIN PAT B ON A.SYS_MBR_SK = B.SYS_MBR_SK",sep = ""), width=10000, simplify=TRUE)

MBR_MNTH_DTL <- fnQueryDatabaseGeneric(ODSAddress, OClientDB, sQuery)
MBR_MNTH_DTL <- mutate(MBR_MNTH_DTL, TYPE = "MBR_MNTH_DTL")


#Get PAT
sQuery <- strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, B.MBR_ID, A.EMPI
                        FROM PAT A 
                        JOIN MBR B ON A.SYS_MBR_SK = B.SYS_MBR_SK",sep = ""), width=10000, simplify=TRUE)

PAT <- fnQueryDatabaseGeneric(ODSAddress, OClientDB, sQuery)
PAT <- mutate(PAT, TYPE = "PAT")


#Get MBR_DISPLAY_DTL
sQuery <-strwrap(paste("SELECT DISTINCT A.SYS_MBR_SK, A.MBR_ID, A.EMPI
                       FROM MBR_DISPLAY_DTL A",sep = ""), width=10000, simplify=TRUE)

MBR_DISPLAY_DTL <- fnQueryDatabaseGeneric(ODSAddress, OClientDB, sQuery)
MBR_DISPLAY_DTL <- mutate(MBR_DISPLAY_DTL, TYPE = "MBR_DISPLAY_DTL")


ODS <- bind_rows(MBR, MBR_MNTH_DTL, MBR_DISPLAY_DTL, PAT)
ODS <- ODS %>%
  inner_join(MEMBERS, by =c("MBR_ID", "SYS_MBR_SK", "EMPI"))

rm(MBR)
rm(MBR_MNTH_DTL)
rm(PAT)
rm(MBR_DISPLAY_DTL)
rm(MEMBERS)