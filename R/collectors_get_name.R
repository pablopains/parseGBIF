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


   x = toupper(x)

   x = gsub("COLLECTOR(S): ","",x)
   x = gsub("COLLECTORS: ","",x)

   x = gsub("COLLABORATION; ","",x)

   x = gsub("PROJETO FLORA CRISTALINO; ","",x)
   x = gsub("PROJETO FLORA CRISTALINO;","",x)


   x = gsub( "ET AL.; ","",x)
   x = gsub( "ET AL.;","",x)

   # x = gsub("[á|à|â|ã|ä]","a",x)
   x = gsub("[Á|À|Â|Ã|Ä]","A",x)

   # x = gsub("[ó|ò|ô|õ|ö]","o",x)
   x = gsub("[Ó|Ò|Ô|Õ|Ö]","O",x)

   # x = gsub("[í|ì|î|ï]","i",x)
   x = gsub("[Í|Ì|Î|Ï]","I",x)

   # x = gsub("[ú|ù|û|ü]","u",x)
   x = gsub("[Ú|Ù|Û|Ü]","U",x)

   # x = gsub("[é|è|ê|ë]","e",x)
   x = gsub("[É|È|Ê|Ë]","E",x)

   # x = gsub("ñ","n",x)
   x = gsub("Ñ","N",x)

   # x = gsub("ç","c",x)
   x = gsub("Ç","C",x)

   x = gsub("\\(|\\)"," ",x) # teste pablo 10-02-2020
   x = gsub("\\[|\\]"," ",x) # teste pablo 10-02-2020
   x = gsub("[\"]"," ",x) # teste pablo 10-02-2020

   # 16-11-2023
   {

     x = gsub("Ć","C",x)
     x = gsub("¡","I",x)
     x = gsub("Ś","S",x)
     x = gsub("¢","O",x)
     x = gsub("Ł","L",x)
     x = gsub("'","",x)
     x = gsub("´","",x)
     x = gsub("Č","C",x)
     x = gsub("Ň","N",x)
     x = gsub("Ń","N",x)
     x = gsub("Ř","r",x)
     x = gsub("Š","S",x)
     x = gsub("Ü","U",x)
     x = gsub("Ż","Z",x)
     x = gsub("Ű","U",x)
     x = gsub("Ė","E",x)
     x = gsub("Ě","E",x)
     x = gsub("Ø","O",x)
     x = gsub("Ă","A",x)
     x = gsub("Ę","E",x)
     x = gsub("`","",x)
     x = gsub("’","",x)

     x = gsub("  "," ",x)

   }

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


