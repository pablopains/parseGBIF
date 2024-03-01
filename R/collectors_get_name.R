#' @title Get the last name of the main collector
#' @name collectors_get_name
#'
#' @description Get the last name of the main collector in recordedBy field
#'
#' @param x recordedBy field
#' @param surname_selection_type Allows you to select two types of results
#' for the main collector's last name: large_string - word with the
#' largest number of characters or last_name - literally the last name
#' of the main collector, with more than two characters.
#' @param maximum_characters_in_name Maximum characters in name
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
collectors_get_name <- function(x,
                                surname_selection_type = 'largest_string', #'last_name' OR largest_string
                                max_words_name = 6,
                                maximum_characters_in_name = 4
                                # ,bionomia = FALSE
)
{

  # maximum_characters_in_name <- 3

  no_name = c('ET AL',
              'AL',
              'JR',
              'ET',
              'TEAM',
              'JUNIOR',
              'FILHO',
              'NETO',
              'SOBRINHO',
              'RESEARCH',
              'BY',
              'IN',
              'FROM',
              'DE',
              'STAFF',
              'EXPED',
              'EXP',
              'DEPARTMENT',
              'OF',
              # 'AGRICULTURE',
              'CENTER',
              'COLLECTION',
              'UNIVERSITY',
              'COLLEGE',
              'EX',
              'HERB',
              'SCHOOL',
              'TECH',
              'DEPT',
              'II',
              'III',
              'AND',
              'COL',
              'COLL',
              'WITH',
              'DEN',
              'VAN',
              'CLUB',
              'BOTANICAL',
              'GARDEN',
              'GARDENS',
              'SOCIETY',
              'HERBARIUM',
              'SECTION',
              'FIELD',
              'TRIP',
              'CLASS',
              'BOTANY',
              'EXPEDITION',
              'TRANSECT',
              'COLLECTOR',
              'ILLEGIBLE',
              'LIEJA',
              'ALLI',
              'ETAL',
              'PROF',

              # [DATA NOT CAPTURED]

              'HUEFS' # incluir colecões
              )


  check_date_num <- function(x_t,
                             no_name)
  {
    x_r <- FALSE

    x_t_1 <- lubridate::ymd(x_t, locale="en_US.UTF-8")
    if(!is.na(x_t_1))
    {
      x_r <- lubridate::is.Date(x_t_1)
    }

    if(is.na(x_t_1) & !x_r)
    {
      x_t_1 <- as.numeric(x_t)
      if(!is.na(x_t_1))
      {
        x_r <- is.numeric(x_t_1)
      }

    }

    if(!x_r)
    {
      x_t_1 <- str_replace_all(x_t, "[^A-Z]", "")
      x_r <- is.na(x_t_1) | x_t_1 ==''
    }

    if(!x_r)
    {
      x_r <- x_t %in% no_name
    }

    # if(!x_r)
    # {
    #   x_r <- nchar(x_t)==1
    # }


    return(x_r)
  }

  {
  # "family": "Cooley",
  # "given": "George R. Wood",
  # "suffix": null,
  # "particle": null,
  # "dropping_particle": null,
  # "nick": null,
  # "appellation": null,
  # "title": null
  # require(stringr)
  # require(jsonlite)

  # bionomia_result <- ''
  #
  # if (bionomia == TRUE)
  # {
  # # x <- 'Monro, A. & Melo, P.H.A'
  #   x <- 'PEDRO ACEVEDO-RODRÍGUEZ;SCOTT A. MORI;CAROL A. GRACIE;BOBBI ANGELL;P. ANDERSON;ERIC J. GOUDA;A. R. A. GÖRTS-VAN RIJN'
  #
  #   x <- ''
  #
  # x_bionomia <- x
  #
  # if(length(grep("&",x))>0)
  # {
  #   x_bionomia <- gsub("&","+",x_bionomia)
  # }else if(length(grep("\\|",x))>0)
  # {
  #   x_bionomia <- gsub("\\|","+",x_bionomia)
  # }else if(length(grep(";",x))>0)
  # {
  #   x_bionomia <- gsub(";","+",x_bionomia)
  # }
  #
  # # ' AND '
  #
  #
  # base_url <- "https://api.bionomia.net/parse.json?names=%s"
  # bionomia_url <- sprintf(base_url,  list(page = x_bionomia))
  # bionomia_url <- gsub(' ', "%20", bionomia_url)
  # bionomia_result <- jsonlite::read_json(bionomia_url)
  #
  # # bionomia_result[[1]][['parsed']][[1]]
  #
  #
  #
  # # https://api.bionomia.net/reconcile?queries=%7B%22q0%22%3A%7B%22query%22%3A%22ABBE+LB%22%2C%22type%22%3A%22http%3A%2F%2Fschema.org%2FPerson%22%7D%7D
  #
  # }
}

  {

  x = gsub("[?]","",x) # teste pablo 10-02-2020

  x = gsub("[.]"," ",x) # teste pablo 10-02-2020

  if (length(grep("\\|",x))>0)
  {
    x = strsplit(x,"\\|")[[1]][1]
  }

  x = toupper(x)

  x = gsub("COLLECTOR\\(S\\)\\: ","",x)
  x = gsub("COLLECTOR\\(S\\)\\:","",x)


  x = gsub("COLLECTORS\\: ","",x)
  x = gsub("COLLECTORS\\:","",x)

  x = gsub("COLLABORATION; ","",x)
  x = gsub("COLLABORATION;","",x)


  x_s <- str_locate(x,'\\(')[2]
  x_e <- str_locate(x,'\\)')[2]

  if(!is.na(x_s) & !is.na(x_e))
  {
    if (x_s>0 & x_e>0)
    {
      x <- str_sub(x, 1, x_s-1)
      if(substr(x, nchar(x),nchar(x))==' ')
      {
        x <- substr(x, 1,nchar(x)-1)
      }
    }

  }

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
  x = gsub("[Ĕ|É|È|Ê|Ë]","E",x)

  # x = gsub("ñ","n",x)
  x = gsub("Ñ","N",x)

  # x = gsub("ç","c",x)
  x = gsub("Ç","C",x)

  x = gsub("\\(|\\)"," ",x) # teste pablo 10-02-2020
  x = gsub("\\[|\\]"," ",x) # teste pablo 10-02-2020
  x = gsub("[\"]"," ",x) # teste pablo 10-02-2020

  # 16-11-2023

    x = gsub("Ē","E",x)
    x = gsub("Ņ","N",x)

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

    x = gsub("Ń","N",x)
    x = gsub("Ź","Z",x)

    x = gsub("Ť","T",x)
    x = gsub("Ľ","L",x)
    x = gsub("Ā","A",x)
    x = gsub("Ů","U",x)
    x = gsub("Ź","Z",x)
    x = gsub("Ź","Z",x)

    x = gsub("Å","A",x)

    x = gsub("ß","SS",x)

    x = gsub("Ž","Z",x)

    x = gsub("Ļ","L",x)

    x = gsub("Ð","D",x)

    x = gsub("Æ","E",x)

    x = gsub("Þ","B",x)

    x = gsub("Ý","Y",x)

    x = gsub("Ī","I",x)

    x = gsub("Ū","U",x)

    x = gsub("Ţ","T",x)




    # LIEJA	Y. GUILLEVIC [LIEJA 17529]

    # <TD><TD>





    # ?
    # x = gsub("‡","C",x)

    x = gsub("`","",x)

    x = gsub("  "," ",x)

  }

  if(is.na(x))
  {return('UNKNOWN-COLLECTOR')}

  if(x=='')
  {return('UNKNOWN-COLLECTOR')}

  if (length(grep("&",x))>0)
  {
    x = strsplit(x,"&")[[1]][1]
  }

  if (length(grep(" AND ",x, fixed = T))>0)
  {
    x = strsplit(x," AND ")[[1]][1]
  }

  if (length(grep(" WITH ",x, fixed = T))>0)
  {
    x = strsplit(x," WITH ")[[1]][1]
  }

  if (length(grep(" ET ",x, fixed = T))>0)
  {
    x = strsplit(x," ET ")[[1]][1]
  }

  if (length(grep(" TO ",x, fixed = T))>0)
  {
    x = strsplit(x," TO ")[[1]][1]
  }

  if (length(grep(" IN ",x, fixed = T))>0)
  {
    x = strsplit(x," IN ")[[1]][1]
  }


  if (length(grep(" , ",x, fixed = T))>0)
  {
    x = strsplit(x," , ")[[1]][1]
  }


  # ' Y '
  # '/ '

  # if (length(grep("  ",x, fixed = T))>0)
  # {
  #   x = strsplit(x,"  ")[[1]][1]
  # }


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

  # if(length(vl)==1)
  # {
  #
  # }


  if(length(vl)>0)
  {

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

    if (max(nchar(xx))>2)
    {
      if(surname_selection_type == 'largest_string')
      {

        vll = which(nchar(xx)==max(nchar(xx)))

        if (length(vll)>1)
        {
          vll = vll[length(vll)]
        }

        sobren = xx[vll]
      }else
      {
        sobren = ''

        ind_name <- xx != no_name &
          (nchar(xx)>1) &
          grepl('A|E|I|O|U|Y',xx) &
          str_count(xx, '-')<2

        if(sum(ind_name)>0)
        {
          for(i2 in length(xx[ind_name==TRUE]):1)
          {
            if(i2>max_words_name){next}
            if(nchar(xx[ind_name==TRUE][i2])>=maximum_characters_in_name &
               !check_date_num(xx[ind_name==TRUE][i2],
                               no_name=no_name)) #&(!lubridate::is.Date(as.Date(xx[i2]))))
            {
              sobren = xx[ind_name==TRUE][i2]
              break
            }
          }
        }

      }

      # teste sobrenome
      # sobren = xx[length(vll)]

    }else{
      sb = strsplit(x,",")[[1]]
      sb = str_trim(sb)
      nsb = nchar(sb)
      sbvl = which(nsb==max(nsb))
      if (length(sbvl)>1) {
        sbvl = sbvl[length(sbvl)]
      }
      sobren = sb[sbvl]
    }

  }else{
    xx = strsplit(x," ")[[1]]

    if(surname_selection_type == 'largest_string')
    {
      # aqui pega a palavra com maior comprimento
      sobren = xx[length(xx)]
    }else
    {

      sobren = ''

      ind_name <- xx != no_name &
        (nchar(xx)>1) &
        grepl('A|E|I|O|U|Y',xx) &
        str_count(xx, '-')<2

      if(sum(ind_name)>0)
      {
        for(i2 in length(xx[ind_name==TRUE]):1)
        {
          if(i2>max_words_name){next}
          if(nchar(xx[ind_name==TRUE][i2])>=maximum_characters_in_name &
             !check_date_num(xx[ind_name==TRUE][i2],
                             no_name=no_name)) #&(!lubridate::is.Date(as.Date(xx[i2]))))
          {
            sobren = xx[ind_name==TRUE][i2]
            break
          }
        }
      }


    }
  }

  sobren = str_trim(sobren)
  sobren = gsub("?","", sobren)
  sobren = paste(sobren,sep="-")
  sobren = toupper(sobren)

  if(length(sobren)>0 & !is.na(sobren) & nchar(sobren)>=maximum_characters_in_name)
  {
    x = strsplit(sobren,"\\|")[[1]]
    sobren = x[1]

    if(substr(sobren, 1,1)=='-')
    {
      # sobren <- substr(sobren, 2,nchar(sobren)-1)
      sobren <- substr(sobren, 2,nchar(sobren))
    }

    if(substr(sobren, nchar(sobren),nchar(sobren))=='-')
    {
      sobren <- substr(sobren, 1,nchar(sobren)-1)
    }

    if(grepl('-', sobren) &
       str_count(sobren, '-')==1)
    {

      x_t = strsplit(sobren,"-")[[1]]

      ind_name <- ! x_t %in% c('JUNIOR','JR','FILHO','NETO','SOBRINHO')

      ind_name <- ind_name & !nchar(x_t)==1

      if(sum(ind_name)==1)
      {
        sobren <- x_t[ind_name==T]
      }

      if(sum(ind_name)==0)
      {
        sobren <- ''
      }


    }


    return(sobren)

    # return(list(main_collector_surname = sobren,
    #             bionomia = bionomia_result))
  }else{
    return(NA)
  }
}


