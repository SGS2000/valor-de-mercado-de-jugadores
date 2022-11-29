#Librerías
library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(stringr)
library(downloader)

######################################
############Transfermarkt#############
######################################

  #Definir función
extraer_datos = function(nro_pagina) {
  direccion = paste("https://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop?land_id=0&ausrichtung=alle&spielerposition_id=alle&altersklasse=alle&jahrgang=0&kontinent_id=0&plus=1&page=",
                    nro_pagina,
                sep="") #Buscar URL
  pagina = URLencode(as.character(direccion)) #Codificar la página como URL
  download(pagina, "pagina.html", mode = "wb") #Descargar la página como archivo HTML
  dir <- read_html("pagina.html") #Leer el código de la página
  
  #Datos en la tabla principal
  #Nombres
  nombre = dir %>% 
    html_nodes("td.hauptlink a") %>% 
    html_text()%>%
    .[seq(1, length(.), 2)]%>%
    as.data.frame(.)%>%
    set_names(.,"nombre")
  
  #Valor (en millones de euros)
  valor = dir %>% 
    html_nodes(".rechts.hauptlink a") %>% 
    html_text() %>%
    gsub("€", "",.) %>%
    gsub("m", "",.) %>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"valor")
  
  #Club
  paises_a_borrar = dir %>% #Algunos jugadores tienen más de dos nacionalidades
    html_nodes("td.zentriert img.flaggenrahmen") %>% 
    html_attr("title") %>%
    unique() %>%
    as.data.frame()%>%
    set_names(.,"paises_a_borrar")
  
  club = dir %>% 
    html_nodes("td.zentriert img") %>% 
    html_attr("alt") %>%
    as.data.frame(.)%>%
    set_names(.,"club")
  
  club = filter(club, !club %in% paises_a_borrar$paises_a_borrar)
  remove(paises_a_borrar)
  
  #Posicion
  posicion = dir %>% 
    html_nodes(".inline-table tr td") %>% 
    html_text()%>%
    .[seq(3, length(.), 3)] %>%
    as.data.frame()
  
  colnames(posicion) = "posicion"
  posicion$posicion = as.factor(posicion$posicion)
  
  levels(posicion$posicion) = list("Centro delantero" = "Centre-Forward",
                                   "Extremo izquierdo" = "Left Winger",
                                   "Extremo derecho" = "Right Winger",
                                   "Volante ofensivo" = "Attacking Midfield",
                                   "Mediocentro" = "Central Midfield",
                                   "Volante defensivo" = "Defensive Midfield",
                                   "Defensa central" = "Centre-Back",
                                   "Lateral izquierdo" = "Left-Back", 
                                   "Lateral derecho" = "Right-Back", 
                                   "Segundo delantero"= "Second Striker",
                                   "Arquero" = "Goalkeeper",
                                   "Volante izquierdo" = "Left Midfield",
                                   "Volante derecho" = "Right Midfield"
  )
  
  
  #Edad
  edad = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text()%>%
    .[seq(2, length(.), 13)] %>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"edad")
  
  #Cantidad de partidos
  partidos = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(5, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"partidos")
  
  #Goles
  goles = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(6, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"goles")
  
  #Goles en contra
  goles_en_contra = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(7, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"goles_en_contra")
  
  #Asistencias
  asistencias = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(8, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"asistencias")
  
  #Tarjetas amarillas
  amarillas = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(9, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"amarillas")
  
  #Segundas amarillas
  segundas_amarillas = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(10, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"segundas_amarillas")
  
  #Tarjetas rojas
  rojas = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(11, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"rojas")
  
  #Ingresos al campo de juego
  ingresos = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(12, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"ingresos")
  
  #Salidas del campo de juego
  salidas = dir %>% 
    html_nodes("td.zentriert") %>% 
    html_text() %>%
    .[seq(13, length(.), 13)]%>%
    as.numeric(.)%>%
    as.data.frame(.)%>%
    set_names(.,"salidas")
  
  #URLs de los jugadores
  dirjugadores = dir %>% 
    html_nodes("td.hauptlink a") %>% 
    html_attr('href') %>%
    .[seq(1, length(.), 2)]
  
  #Datos en la página de cada jugador
  for (j in 1:length(dirjugadores)) {
    
    direccion2 = paste("https://www.transfermarkt.com",dirjugadores[j],sep="") #Buscar URL
    pagina2 = URLencode(as.character(direccion2)) #Codificar la página como URL
    download(pagina2, "pagina2.html", mode = "wb") #Descargar la página como archivo HTML
    dir2 <- read_html("pagina2.html") #Leer el código de la página
    
    etiquetas = dir2 %>% #Etiquetas en la tabla a la izquierda
      html_nodes(".info-table__content.info-table__content--regular") %>% 
      html_text()
    
    #Selección
    seleccion = dir2 %>% 
      html_nodes(".data-header__content img") %>% 
      html_attr("title") %>% 
      .[2]%>%
      as.data.frame(.)%>%
      set_names(.,"seleccion")
    
    #Pais del club
    pais_del_club = dir2 %>% 
      html_nodes(".data-header__content img") %>% 
      html_attr("title") %>% 
      .[1]%>%
      as.data.frame(.)%>%
      set_names(.,"pais_del_club")
    
    pais_del_club$pais_del_club=str_replace_all(pais_del_club$pais_del_club,"England","Inglaterra") #Traduccion
    pais_del_club$pais_del_club=str_replace_all(pais_del_club$pais_del_club,"France","Francia") #Traduccion
    pais_del_club$pais_del_club=str_replace_all(pais_del_club$pais_del_club,"Spain","España") #Traduccion
    pais_del_club$pais_del_club=str_replace_all(pais_del_club$pais_del_club,"Germany","Alemania") #Traduccion
    pais_del_club$pais_del_club=str_replace_all(pais_del_club$pais_del_club,"Italy","Italia") #Traduccion
    pais_del_club$pais_del_club=str_replace_all(pais_del_club$pais_del_club,"Netherlands","Países Bajos") #Traduccion
    
    #Altura
    altura = dir2 %>% 
      html_nodes(".info-table__content.info-table__content--bold") %>% 
      html_text() %>%
      .[which(etiquetas=="Height:")] %>%
      gsub(",", "\\.",.) %>%
      gsub("m", "",.) %>%
      str_trim()%>%
      as.numeric(.)%>%
      as.data.frame(.)%>%
      set_names(.,"altura")
    
    #Pierna hábil
    pierna = dir2 %>% 
      html_nodes(".info-table__content.info-table__content--bold") %>% 
      html_text() %>%
      .[which(etiquetas=="Foot:")]%>%
      as.data.frame(.)%>%
      set_names(.,"pierna")
    
    pierna$pierna = as.factor(pierna$pierna) #Traduccion
    levels(pierna$pierna) = list("izquierda" ="left",
                                 "derecha" = "right",
                                 "ambas"="both"
    )
    
    #Representante
    if("Player agent:" %in% etiquetas){
      representante = dir2 %>% 
        html_nodes(".info-table__content.info-table__content--bold") %>% 
        html_text() %>%
        .[which(etiquetas=="Player agent:")]%>%
        str_trim()
    } else representante = "Desconocido"
    
    representante = representante%>%
      as.data.frame(.)%>%
      set_names(.,"representante")
    
    representante$representante=str_replace_all(representante$representante,"Relatives","Parientes") #Traduccion
    representante$representante=str_replace_all(representante$representante,"no agent","Sin representante")
    representante$representante=str_replace_all(representante$representante,"Agent is known - Player under 18","Desconocido")
    representante$representante=str_replace_all(representante$representante,"not clarified","Desconocido")
    
    #Perfil de Instagram
    insta = dir2 %>%
      html_nodes(".socialmedia-icons a") %>%
      html_attr("href")%>%
      as.data.frame(.) %>%
      set_names(.,"perfil_insta") %>%
      filter(grepl(pattern = "instagram",perfil_insta))

    if (!nrow(insta) == 1) {
      insta = "Sin cuenta" %>%
        as.data.frame(insta) %>%
        set_names(.,"perfil_insta")
    }
    
    #Construcción del dataset
    nuevos2 = cbind(seleccion,pais_del_club,altura,pierna,representante,insta)

    if(j>1){
      datos_jugador = rbind(datos_jugador,nuevos2)
    } else{datos_jugador = nuevos2}
  }
  
  #Construir dataset
  nuevos=cbind(nombre,valor,edad,club,posicion,partidos,goles,goles_en_contra,asistencias,
               amarillas,segundas_amarillas,rojas,ingresos,salidas,datos_jugador)
  
  #Devolver resultados
  return(nuevos)
}

  #Recolección de datos
limite = 8 #Cada página muestra 25 jugadores, se extraen los primeros 200

nuevos = NULL
for (i in 1:limite) {
  
  while(is.null(nuevos)){
    try({
    nuevos = extraer_datos(i)
    })}
  
  if(i>1){
    datos = rbind(datos,nuevos)
  } else{datos = nuevos}
  
  nuevos = NULL
}

######################################
############Salary Sport#############
######################################
#Correr después de tener los datos de Transfermarkt

  #Obtener URLS
nombres = datos$nombre %>%
  tolower() %>%
  gsub(" ", "-",.)

direcciones = ""
for (i in 1:nrow(datos)) {
  direcciones[i] = paste("https://salarysport.com/football/player/",nombres[i],"/",sep="")
}

  #Obtener datos de salarios (libras)
for (i in 1:nrow(datos)) {
  sueldos = NULL
  try({
  pagina = URLencode(as.character(direcciones[i]))
  download(pagina, "salary.html", mode = "wb")
  dir <- read_html("salary.html")

  sueldos = dir %>%
    html_nodes("table td") %>%
    html_text() %>%
    .[3] %>%
    gsub("£", "",.) %>%
    gsub(",", "",.) %>%
    str_trim()%>%
    as.numeric(.)%>%
    as.data.frame(.) %>%
    set_names(.,"sueldo")
  })
   
  if(is.null(sueldos)){
    sueldos = NA #Nota: Algunos jugadores no figuran con el mismo nombre en ambas páginas, revisar manualmente
  }

  if(i>1){
    sueldos2021 = rbind(sueldos2021,sueldos)
  } else{sueldos2021 = sueldos}
}

  #Añadir al conjunto anterior
datos2 = cbind(datos, as.data.frame(sueldos2021))