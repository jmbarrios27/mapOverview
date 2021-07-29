# elininar null
data <- atmprueba_lleno
data <- data[complete.cases(data),]

# creating a new spatialpoints dataframe el numero 2 y 3 representa la posicion
# en el DF
data.SP <-SpatialPointsDataFrame(data[,c(14,15)], data[,-c(14,15)])


# colores
data <- data %>% 
  mutate(color = case_when(str_detect(volumen, "Muy Bajo") ~ "red",
                           str_detect(volumen, "Bajo") ~ "darkred",
                           str_detect(volumen, "Regular") ~ "orange",
                           str_detect(volumen, "Bueno") ~ "darkgreen",
                           str_detect(volumen, "Alto") ~ "green",
                           TRUE ~ "a default"))

# usando dataframe con todos los valores y colores
final_data <- as.data.frame(data)

final_data <- final_data %>% 
  mutate(codigo_adquirente = case_when(str_detect(adquirente, "BANCO NACIONAL DE PANAMA") ~ 1,
                                       str_detect(adquirente, "CANAL BANK") ~ 2,
                                       str_detect(adquirente, "COOPEDUC") ~ 3,
                                       str_detect(adquirente, "BANCO DELTA") ~ 4,
                                       str_detect(adquirente, "BANISTMO") ~ 5,
                                       str_detect(adquirente, "BANCO GENERAL") ~ 6,
                                       str_detect(adquirente, "COOESAN") ~ 7,
                                       str_detect(adquirente, "CACECHI") ~ 8,
                                       str_detect(adquirente, "SCOTIABANK") ~ 9,
                                       str_detect(adquirente, "MULTIBANK") ~ 10,
                                       
                                       str_detect(adquirente, "TOWER BANK") ~ 11,
                                       str_detect(adquirente, "BANCO FICOHSA") ~ 12,
                                       str_detect(adquirente, "BANISI") ~ 13,
                                       str_detect(adquirente, "BANESCO") ~ 14,
                                       str_detect(adquirente, "BANCO ALIADO") ~ 15,
                                       str_detect(adquirente, "CAPITAL BANK") ~ 16,
                                       str_detect(adquirente, "BCT BANK") ~ 17,
                                       str_detect(adquirente, "MMG BANK") ~ 18,
                                       str_detect(adquirente, "CACSA") ~ 19,
                                       str_detect(adquirente, "ST GEORGES BANK") ~ 20,
                                       
                                       str_detect(adquirente, "DAVIVIENDA") ~ 21,
                                       str_detect(adquirente, "CAJA DE AHORROS") ~ 22,
                                       str_detect(adquirente, "COOPEVE") ~ 23,
                                       str_detect(adquirente, "EDIOACC") ~ 24,
                                       str_detect(adquirente, "COEDUCO") ~ 25,
                                       str_detect(adquirente, "BAC CREDOMATIC") ~ 26,
                                       str_detect(adquirente, "METROBANK") ~ 27,
                                       str_detect(adquirente, "BICSA") ~ 28,
                                       str_detect(adquirente, "BANCO LAFISE") ~ 29,
                                       str_detect(adquirente, "BIBANK") ~ 30,
                                       
                                       str_detect(adquirente, "BANCO MERCANTIL") ~ 31,
                                       str_detect(adquirente, "UNIBANK") ~ 32,
                                       str_detect(adquirente, "CREDICORP BANK") ~ 33
  ))

# Creando grupos
groups = as.character(unique(final_data$adquirente))

mapa_colores = leaflet(final_data) %>% addTiles(group = "OpenStreetMap")#>%
# Filtrando por grupos.
for(g in groups) 
{
  d = final_data[final_data$adquirente == g, ]
  #mapa con pinchetas noramles
  icons_for <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = d$color
  )
  mapa_colores = mapa_colores %>% addAwesomeMarkers(data = d, lat = d$latitud, lng =d$longitud, icon=icons_for,group = as.character(g) ,
                                                    popup = paste("<b>","Adquirente:" ,"</b>",d$adquirente,"<br>",
                                                                  "<b>","Provincia:","</b>", d$provincia, "<br>",
                                                                  "<b>","Distrito:","</b>", d$distrito,"<br>",
                                                                  "<b>","Corregimiento","</b>", d$corregimiento, "<br>",
                                                                  "<b>","ID TERMINAL:","</b>", d$terminales, "<br>",
                                                                  "<b>","Ubicación:","</b>", d$location, "<br>",
                                                                  "<b>","Promedio de transacciones:","</b>", d$Promedio, "<br>",
                                                                  "<b>","Volumen transacciones:","</b>", d$volumen,"<br>",
                                                                  "<b>","Latitud:","</b>", d$latitud,"<br>",
                                                                  "<b>","Longitud;","</b>",d$longitud),
                                                    clusterOptions = markerClusterOptions())
}



# mapa
mapa_funcional <-mapa_colores %>% addLayersControl(overlayGroups = groups)%>%
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")

