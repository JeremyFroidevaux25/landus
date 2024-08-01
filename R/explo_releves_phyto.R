dat_phyto = readRDS("_data/cbna_phyto_chamonix.rds")

head(dat_phyto)
unique(dat_phyto$date_releve_deb) # 28 journees observations
length(unique(dat_phyto$id_releve)) # 246 releves

unique(grep("Prairie", dat_phyto$comm_milieu, value = TRUE))

library(dplyr)
dat = dat_phyto %>% select(x_l93, y_l93, alti_calc, lib_exposition, pente, abondance, rec_total_pl, rec_herba_pl, rec_arbo_pl, rec_arbu_pl, rec_sarb_pl, rec_crypto_pl, haut_herba, haut_arbo, haut_arbu, haut_sarb, lib_syntaxon, nom_reconnu, id_releve, plage_pente, comm_milieu)


str(dat)


sort(unique(dat$nom_reconnu)) # 844 espèces

# # liste ligneux
# - Abies alba
# - Acer campestre
# - Acer pseudoplatanus
# - Actaea spicata (baies)
# - Alnus sp.
# - Amelanchier ovalis
# - Arctostaphylos uva-ursi
# - Aria edulis (Alisier blanc)
# - Betula pendula 
# - Betula pubescens
# - Calluna vulgaris
# - Chamaemespilus alpina (sorbier nain)
# - Corylus avellana (noisettier)
# - Crataegus monogyna (aubépine)
# - Empetrum nigrum (camarine)
# - Fagus sylvatica
# - Frangula alnus (Bourdaine)
# - Fraxinus excelsior 
# - Genista tinctoria
# - Ilex aquifolium (houx)
# - Juniperus communis (et subsp nain)
# - Kalmia procumbens (Azalée des Alpes / Loiseleurie couchée)
# - Larix decidua
# - Lonicera sp. (chevrefeuille)
# - Picea abies
# - Populus tremula
# - Prunus sp.
# - Quercus petraea
# - Rhododendron ferrugineum
# - Ribes rubrum (groseillier)
# - Rubus sp.
# - Salix sp.
# - Sambucus racemosa (sureau noir)
# - Sorbus aucuparia
# - Thymus sp.
# - Vaccinium myrtillus
# - Vaccinium uliginosum
# - Vaccinium vitis-idaea
# 
# 
# 
# - Fougères?
# - Mousses?
  
### visu points
library(leaflet)
mmap = leaflet () %>% 
  setView(lng = 6.85, lat = 45.91, zoom = 11) 

mmap %>% addTiles()

mmap %>% addProviderTiles(providers$OpenTopoMap) 

mmap %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$OpenTopoMap, 
                          options = providerTileOptions(opacity = 0.35)) %>%
  addCircleMarkers(sites)

library(sp)
sites = unique(dat[,c("x_l93", "y_l93", "id_releve", "comm_milieu")])
coordinates(sites) <- ~ x_l93 + y_l93
proj4string(sites) <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
library(sf)
sites_wgs84 <- spTransform(sites, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")



dV = dat[grep("Vaccinium", dat$nom_reconnu),]
sites.vacci = unique(dV[which(dV$abondance %in% c("3", "4")), 'id_releve'])
dR = dat[grep("Rhodo", dat$nom_reconnu),]
sites.rhodo  = unique(dR[which(dR$abondance %in% c("3", "4")), 'id_releve'])
dJ = dat[grep("Vaccinium", dat$nom_reconnu),]
sites.junip  = unique(dJ[which(dJ$abondance %in% c("3", "4")), 'id_releve'])

sites_wgs84[which(sites_wgs84$id_releve%in%sites.vacci), "type"] = "Vaccinium"
sites_wgs84[which(sites_wgs84$id_releve%in%sites.rhodo), "type"] = "Rhododendron"
sites_wgs84[which(sites_wgs84$id_releve%in%sites.junip), "type"] = "Juniperus"


mmap %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$OpenTopoMap, 
                   options = providerTileOptions(opacity = 0.35)) %>%
  addMarkers(coordinates(sites_wgs84)[,1], coordinates(sites_wgs84)[,2], label = sites_wgs84$comm_milieu)

mmap %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$OpenTopoMap, 
                   options = providerTileOptions(opacity = 0.35)) %>%
  addMarkers(coordinates(sites_wgs84)[which(!is.na(sites_wgs84$type)),1], coordinates(sites_wgs84)[which(!is.na(sites_wgs84$type)),2] ,label = sites_wgs84$type[which(!is.na(sites_wgs84$type))])

mmap %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$OpenTopoMap, 
                   options = providerTileOptions(opacity = 0.35)) %>%
  addCircleMarkers(coordinates(sites_wgs84)[which(sites_wgs84$type=="Rhododendron"),1], coordinates(sites_wgs84)[which(sites_wgs84$type=="Rhododendron"),2])

mmap %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$OpenTopoMap, 
                   options = providerTileOptions(opacity = 0.35)) %>%
  addCircleMarkers(coordinates(sites_wgs84)[which(sites_wgs84$type=="Vaccinium"),1], coordinates(sites_wgs84)[which(sites_wgs84$type=="Vaccinium"),2])

mmap %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$OpenTopoMap, 
                   options = providerTileOptions(opacity = 0.35)) %>%
  addCircleMarkers(coordinates(sites_wgs84)[which(sites_wgs84$type=="Juniperus"),1], coordinates(sites_wgs84)[which(sites_wgs84$type=="Juniperus"),2])


### Niches

sites_plus = merge(sites_wgs84, unique(dat[,c("id_releve", "pente", "lib_exposition", "alti_calc", "plage_pente")]),  by = "id_releve")

sites_plus

library(ggplot2)

ggplot(sites_plus@data, aes(x = reorder(type, pente), y = pente)) +
  geom_boxplot() 

ggplot(sites_plus@data, aes(x = reorder(type, alti_calc), y = alti_calc)) +
  geom_boxplot() 

### analyse multivariee?
sites_plus@data$expoNS = sites_plus@data$expoEO = factor(sites_plus@data$lib_exposition)
levels(sites_plus@data$expoNS) = c("0", "0" , "0.25", "-0.25", "1", "0.75", "0.5", "0.5", "0", "0", "-1", "-0.75", "-0.75", "-0.5", "-0.5")
sites_plus@data$expoNS = as.numeric(as.character(sites_plus@data$expoNS))
levels(sites_plus@data$expoEO) = c("0", "1" , "0.75", "0.75", "0", "-0.25", "0.5", "-0.5", "-1", "0", "0", "0.25", "-0.25", "0.5", "-0.5")
sites_plus@data$expoEO = as.numeric(as.character(sites_plus@data$expoEO))

sites_plus@data$pente_cl = factor(sites_plus@data$plage_pente)
levels(sites_plus@data$pente_cl) = c("NA", "1", "75", "3", "35", "55", "15")
sites_plus@data$pente_cl = as.numeric(as.character(sites_plus@data$pente_cl))
sites_plus@data$pente[which(is.na(sites_plus@data$pente))] = sites_plus@data$pente_cl[which(is.na(sites_plus@data$pente))] 

summary(sites_plus@data$pente)

sites_plus_alldata = na.omit(sites_plus@data[c("pente", "alti_calc", "expoNS", "expoEO", "type")])

library(ade4)
pca = dudi.pca(sites_plus_alldata[c("pente", "alti_calc", "expoNS", "expoEO")], scannf=F, nf=2)

s.label(2*pca$co)
s.class(0.5*pca$li, fac = factor(sites_plus_alldata$type), add.plot = TRUE )

        