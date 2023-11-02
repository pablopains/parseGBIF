#' @title Get the last name of the main collector
#' @name collectors_get_name
#'
#' @description Get the last name of the main collector in recordedBy field
#'
#' @param x recordedBy field
#'
#' @details Returns the last name
#'
#' @return
#' last name of the main collector
#'
#' @author Pablo Hendrigo Alves de Melo,
#'         Nadia Bystriakova &
#'         Alexandre Monro
#'
#' @seealso \code{\link[parseGBIF]{collectors_prepare_dictionary}}, \code{\link[parseGBIF]{collectors_update_dictionary}}
#'
#' @examples
#' \donttest{
#' help(collectors_get_name)
#'
#' collectors_get_name('Melo, P.H.A & Monro, A.')
#'
#' collectors_get_name('Monro, A. & Melo, P.H.A')
#' }
#'
#' @import stringr
#' @export
collectors_get_name <- function(x)
{

  # require(stringr)

   x = gsub("[?]","",x) # teste pablo 10-02-2020

   x = gsub("[.]"," ",x) # teste pablo 10-02-2020

   if (length(grep("\\|",x))>0)
   {
      x = strsplit(x,"\\|")[[1]][1]
   }

   x = gsub("[á|à|â|ã|ä]","a",x)
   x = gsub("[Á|À|Â|Ã|Ä]","A",x)

   x = gsub("[ó|ò|ô|õ|ö]","o",x)
   x = gsub("[Ó|Ò|Ô|Õ|Ö]","O",x)

   x = gsub("[í|ì|î|ï]","i",x)
   x = gsub("[Í|Ì|Î|Ï]","I",x)

   x = gsub("[ú|ù|û|ü]","u",x)
   x = gsub("[Ú|Ù|Û|Ü]","U",x)

   x = gsub("[é|è|ê|ë]","e",x)
   x = gsub("[É|È|Ê|Ë]","E",x)

   x = gsub("ñ","n",x)
   x = gsub("Ñ","N",x)

   x = gsub("ç","c",x)
   x = gsub("Ç","C",x)

   x = gsub("\\(|\\)"," ",x) # teste pablo 10-02-2020
   x = gsub("\\[|\\]"," ",x) # teste pablo 10-02-2020
   x = gsub("[\"]"," ",x) # teste pablo 10-02-2020

   if (length(grep("&",x))>0)
   {
      x = strsplit(x,"&")[[1]][1]
   }

   if (length(grep(";",x))>0)
   {
      x_t <- strsplit(x,";")[[1]][1]

      # para capturar padrão iniciado por ;
      if (nchar(x_t)==0)
      {
         x_t <- strsplit(x,";")[[1]][2]
         if (is.na(x_t )) { x_t <- ""}
      }

      if (nchar(x_t)>0)
      {
         x <- x_t
      } else
      {
         x <- ''
      }

   }

   vl = grep(",| ",x)

   if (length(vl)>0) {

      xx = strsplit(x,",")[[1]][1]

      xx = strsplit(xx," ")[[1]]

      xx = xx[xx!=""]

      # # hibrido no genero
      # if(substr(xx[1],1,1)=="×")
      # {
      #   sobren <- xx[1]
      #
      #   if(length(xx)>1)
      #   {
      #     sobren <- paste0(xx[1], ' ', xx[2])
      #   }
      #   return(sobren)
      # }

      if (max(nchar(xx))>2) {

        vll = which(nchar(xx)==max(nchar(xx)))
         if (length(vll)>1) {
            vll = vll[length(vll)]
         }
         sobren = xx[vll]

        # teste sobrenome
        # sobren = xx[length(vll)]

      } else {
         sb = strsplit(x,",")[[1]]
         sb = str_trim(sb)
         nsb = nchar(sb)
         sbvl = which(nsb==max(nsb))
         if (length(sbvl)>1) {
            sbvl = sbvl[length(sbvl)]
         }
         sobren = sb[sbvl]
      }
   } else {
      xx = strsplit(x," ")[[1]]

      # aqui pega a palavra com maior comprimento
      sobren = xx[length(xx)]

   }
   sobren = str_trim(sobren)
   sobren = gsub("?","", sobren)
   sobren = paste(sobren,sep="-")
   sobren = toupper(sobren)
   if (length(sobren)>0){
      x = strsplit(sobren,"\\|")[[1]]
      sobren = x[1]
      return(sobren)
   } else {
      return(NA)
   }
}

# library(stringr)
#
# collectors_get_name(x='×Cryptbergia rubra')

# INCLUIR / NA SEPARAÇÃO DOS COLETOR PRINCIPAL DE ET ALI

# SUBSTITUIR
# HALINA ANDRYŁOJĆ
# ANIOŁ J.
# ARHANGEL'SKIY
# ARHIPOVIČA
# BARTOŇOVÁ,V.
# DVOŘÁK,FRANTIŠEK & GRÜLL,F.
# DVOŘÁK,FRANTIŠEK, SEDLÁČEK,J. & TOMÁNEK,J.
# DVOŘÁK,JOSEF
# DVOŘÁKOVÁ,H.
# DVOŘÁKOVÁ,M.
# DVOŘÁČKOVÁ,K.

# GRAŻYNA
# GRA�YNA
# GRŰLL F.
# HANÁČEK,C.
# KATARZYNA GŁÓWCZYŃSKA
# BARBARA GŁOWIŃSKA
# M. IVANAUSKIENĖ
# JAŃCZAK MARIA
# JAŻDŻEWSKA NATALIA
# JEDLIČKA
# KAČIČNIK M.
# KOZIOŁ,E. & CHARYTONOWICZ,H.
# KOZŁOWSKA O.
# KOŚCIAN M.
# MALHOBĀ
# MALNAČS
# # JADWIGA DZI�GIELEWSKA
# # EKWI�SKA
# ELŻBIETA NOCUŃ

# MIEßLER,O.
# [MISSING; EX HERB. F. KÆSER]
# NÁBĚLEK,FRANTIŠEK
# NÁBĚLEK,VOJTĚCH

# EDVARD J. HAVNØ
# BĀRTULE
# BŁASZCZYK
# BŁASZCZYK
# BŁAWAT
# BŁOCKI
# BŁOŃSKI
# DOLNÍČKOVÁ,IVA
# BEATA DOMAGAŁA

# PANČIĆ
# PLUHAŘ,V.
# PODPĚRA,J. & ŠIRJAEV,G.
# I. RESMERITĂ
# RIMEICĀNE
# STEŠEVIĆ,DANIJELA
# VAĎURA,M.
# VLČKOVÁ,PAVLA
# WOŹNIAK A.
# WSZAŁEK-ROŻEK KATARZYNA, AFRANOWICZ-CIEŚLAK RENATA, BARTŁOMIEJ HAJEK, MARKOWSKI RYSZARD, CHOJNACKI WŁODZIMIERZ
# ZDVOŘÁK,P. & DUCHÁČEK,M.
# HUMBERTO DE ´BARREIRO
# EUGENIUSZ ĆWIKLIŃSKI
# ČÁP,J. & ČÁPOVÁ,J.
# ŻUCHOWSKI PIOTR
# FILIP ŻYŁOWSKI
# ZIĘTEK A.


# COLLECTOR(S): WILLIAM J. HESS, GEROULD S. WILHELM
# COLLECTORS: A & M COLLOTZI 410
# COLLECTORS: A BEETLE B02318

# FA'ASUAGA
# FALP'EV
# D'AGOSTINO
# D'AGRICULTURE
# D'ALBERTIS
# D'ALLEIZETTE
# D´ALEIZETTE
# GAL'PERINA
# KONDRAT'EVA
# F. LEONT'EV
# LEONT'EV F.
# LEONT'EVA
# AFANDI MA'ROEF
# M'BOUNGOU, R; BURGT, XM VAN DER; MPANDZOU, A; DOUHI, L; NKONDI, F; MERKLINGER, F
# MEL'GUNOV
# MUL'TANOVSKAYA
# NIKOL'SKAYA
# O'BYRNE
# N. PAL'CHEVSKIY
# E.V. PIS'MARKINA;D.S. LABUTIN;M.V. PUZYR'KINA
# G. RYL'SKIY
# RYL'SKIY
# I. SHEREMET'EVA;A. SCHERBAKOV
# SIN'KOVA M. A.
# SOLOV'EVA O.S
# STREL'NIKOVA T.O.

# TIRAR - E PEGAR O ULTIMO
# FAMBART-TINEL
# FÉLIX-DA-SILVA, M.M.
# FERREIRA-SILVA

# OU PERNULTIMO ?
# JUAN S. POSADA-MONTOYA
# SOUKUP-CARMONA, --
# PARECE MAIS PRECISO



# GRUPO, EQUIPE TEAM, GROUP
# GRUPO DE TRABAJO SPB MIG


# NAO PEGAR COM MAIS DE UM PONTO NO NOME
# EWANGO C.E.N. & GAKIMA J.B.

# GADELHA NETO, PC; BARBOSA, MR; LIMA, IB; SILVA, CS
# JR. FILHO, NETO, SOBRINHO,
# FERREIRA-JUNIOR
# F. SILVA FILHO; S. PUGUES
# FILHO NETO, SJ
# FILHO, A. .C.
# FILHO NETO	FILHO NETO, SJ
# SE SOBRAR SO 1 PALARA FILHO, DEIXAR
# FILHO, W.E.
# FILHO, W.H.; COELHO, L.A. & TEIXEIRA, R.N.C.
# FILHO-ALCOFORADO, FG
# SADDI F.A. MATTOS FILHO
# SADDIR A. MATTOS FILHO


# EVITAR ENTRE ( )
# AUDIBERT HIPPOLYTE (CONSERVATOIRE BOTANIQUE NATIONAL MÉDITERRANÉEN DE PORQUEROLLES)

# DEGEN R
# DEGEN, A. VON
# DEGEN,A. DE
# DEGEN,A. VON

# FERNANDO N. BIURRUN
# FERNANDO O. ZULOAGA

# FRANCISCO  SOARES PINHEIRO
# FRANCISCO A. VIVAR C.;H. SARANGO;BALCÁZAR
# FRANCISCO ALVARADO; JOSE GONZÁLEZ ; ULISES CHAVARRÍA

# COLLECTOR(S): ALAN R. BATTEN, STEPHEN M. MURPHY, JANICE C. DAWE
# COLLECTOR(S): ALBERT W. JOHNSON, LESLIE A. VIERECK, HERBERT R. MELCHIOR
# COLLECTOR(S): ALF ERLING PORSILD, AUGUST JOHANN JULIUS BREITUNG
# COLLECTOR(S): ALF SCHMITT
# COLLECTOR(S): ALLAIRE K. DIAMOND, ALAN R. BATTEN
# COLLECTOR(S): ALLISON W. CUSICK
# COLLECTOR(S): AMY LARSEN

# COLLECTORS: A BRYANT 100
# COLLECTORS: A BRYANT 177
# COLLECTORS: A BRYANT 18
# COLLECTORS: A BRYANT 184
# COLLECTORS: A SIMMONDS
# COLLECTORS: A. PINKARD
# COLLECTORS: A. ROSTAD
# COLLECTORS: A. SCHNEIDER
# COLLECTORS: ALF LUNDEGREN
# COLLECTORS: ANDREW L. BRYANT
# COLLECTORS: AVEN NELSON
# COLLECTORS: B BLAZIER 11
# COLLECTORS: B KLINGER

# CHRISTOPHER A. BENDER

# collectors_get_name(x='BETHANY LESTER| CHRISTINA LUND, SARAH HUNKINS')
#
# collectors_get_name(x='aaaaaaa bbbbbbb| CHRISTINA LUND, SARAH HUNKINS')
#
# collectors_get_name(x='CASTRO-SANTOS A DE')

# CATHERINE A. GEURDS

# X	LESTER	BETHANY LESTER; FRANK FARRUGGIA
# X	LESTER	BETHANY LESTER; FRANK FARRUGGIA, JEN RIDDELL
# X	LESTER	BETHANY LESTER; KITTY LUND
# X	LESTER	BETHANY LESTER; LES LANDRUM
# X	LEWIS	BETHANY LEWIS


# CANDIDO ET ALLI
# L. CAPELARI JR.

