library(tidyverse)
require(sf)
require(mapview)
library(extrafont)

load("maps_sf.RData")

dists_sf_south = dists_sf %>%
  filter(STATE.NAME %in% c("Maharashtra","Karnataka","Tamil Nadu"))

dists_sf_rem = dists_sf %>%
  filter(STATE.NAME %in% c("Ladakh","Himachal Pradesh","Uttarakhand",
                           "Kerala"))

dists_sf_nilgiris = dists_sf %>%
  filter(DISTRICT.NAME %in% c("The Nilgiris"))

pre_filters = st_read("indiama-editedSQ/indiama-editedSQ.shp", type=3)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
pre_filters = pre_filters %>% st_transform(crs_string)

mapviewOptions(fgb = FALSE)
map_pre_filters = mapView(pre_filters, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(pre_filters,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_dists_rem = mapView(dists_sf_rem, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(dists_sf_rem,c("DISTRICT.NAME"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#110000")

map_pre_comb = map_pre_filters + map_dists_rem
mapshot(map_pre_comb, "pre_filters_dists.html")

map_pre_comb = leafem::addMouseCoordinates(map_pre_comb)


wg = st_read("wg_boundary/wg_boundary.shp")
wg = wg %>% dplyr::select(geometry)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
wg = wg %>% st_transform(crs_string)

ng1000 = st_read("Nilgiri_Below1000/Nilgiri_Below1000.shp")
ng1000 = ng1000 %>% dplyr::select(fid,geometry)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ng1000 = ng1000 %>% st_transform(crs_string)
ng1000 = ng1000 %>% filter(fid == 198) %>% select(-fid)
ng1000 = nngeo::st_remove_holes(ng1000)

split_line = st_sfc(st_linestring(matrix(c(76.537, 11.507, 76.493, 11.584), 
                                  ncol = 2, byrow = TRUE)), crs = crs_string)
ng1000_split = ng1000 %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:2)

ng1000_masinagudi = ng1000_split %>% filter(fid == 1)
st_write(ng1000_masinagudi, dsn = "requested_filter_polygons/Tamil Nadu/masinagudi/masinagudi.shp")

ng1000_gudalur = ng1000_split %>% filter(fid == 2)
st_write(ng1000_gudalur, dsn = "requested_filter_polygons/Tamil Nadu/gudalur/gudalur.shp")


ng1600 = st_read("Nilgiri_above1600/Nilgiri_above1600.shp")
ng1600 = ng1600 %>% dplyr::select(fid,geometry)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ng1600 = ng1600 %>% st_transform(crs_string)
ng1600 = ng1600 %>% filter(fid == 35)
st_write(ng1600, dsn = "requested_filter_polygons/Tamil Nadu/nilgiri_plateau/nilgiri_plateau.shp")


mapviewOptions(fgb = FALSE)
map_wg = mapView(wg, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(wg,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_wg = leafem::addMouseCoordinates(map_wg)

map_ng_masinagudi = mapView(ng1000_masinagudi, zcol = NULL, map.types = c("Esri.WorldImagery"),
                            layer.name = NULL, 
                            popup = leafpop::popupTable(ng1000_masinagudi,c("fid","geometry"), 
                                                        feature.id=FALSE, 
                                                        row.numbers=FALSE), 
                            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ng_masinagudi = leafem::addMouseCoordinates(map_ng_masinagudi)

map_ng_gudalur = mapView(ng1000_gudalur, zcol = NULL, map.types = c("Esri.WorldImagery"),
                         layer.name = NULL, 
                         popup = leafpop::popupTable(ng1000_gudalur,c("fid","geometry"), 
                                                     feature.id=FALSE, 
                                                     row.numbers=FALSE), 
                         alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ng_gudalur = leafem::addMouseCoordinates(map_ng_gudalur)


map_ng_plateau = mapView(ng1600, zcol = NULL, map.types = c("Esri.WorldImagery"),
                         layer.name = NULL, 
                         popup = leafpop::popupTable(ng1600,c("fid","geometry"), 
                                                     feature.id=FALSE, 
                                                     row.numbers=FALSE), 
                         alpha.regions = 0, lwd = 5, legend = NULL, color = "#666666")

map_dists_south = mapView(dists_sf_south, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(dists_sf_south,c("DISTRICT.NAME"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#110000")

map_wg_comb = map_wg + map_dists_south
mapshot(map_wg_comb, "wg_dists.html")

mapshot(map_ng_masinagudi, "ng_masinagudi.html")
mapshot(map_ng_gudalur, "ng_gudalur.html")
mapshot(map_ng_plateau, "ng_plateau.html")




## intersection polygons

########### Uttarakhand

pre_filters_ul_1 = pre_filters %>% filter(AREA_1 == "India--Uttarakhand--Below1000m")
pre_filters_ul_1 = st_make_valid(pre_filters_ul_1)
pre_filters_ul_2 = pre_filters %>% filter(AREA_1 == "India--Uttarakhand--Above1000m")
pre_filters_ul_2 = st_make_valid(pre_filters_ul_2)

# Nainital (1-2)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Nainital"))
name = "nainital"
f1 = st_intersection(pre_filters_ul_1,dist)
f2 = st_intersection(pre_filters_ul_2,dist)

dir.create(paste("requested_filter_polygons/Uttarakhand/",name,"Below1000",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Uttarakhand/",name,"Below1000","/",name,"Below1000.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Uttarakhand/",name,"Below1000.html",sep=""))

dir.create(paste("requested_filter_polygons/Uttarakhand/",name,"Above1000",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Uttarakhand/",name,"Above1000","/",name,"Above1000.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Uttarakhand/",name,"Above1000.html",sep=""))


# Pauri Garhwal (1-2)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Pauri Garhwal"))
name = "paurigarhwal"
f1 = st_intersection(pre_filters_ul_1,dist)
f2 = st_intersection(pre_filters_ul_2,dist)

dir.create(paste("requested_filter_polygons/Uttarakhand/",name,"Below1000",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Uttarakhand/",name,"Below1000","/",name,"Below1000.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Uttarakhand/",name,"Below1000.html",sep=""))

dir.create(paste("requested_filter_polygons/Uttarakhand/",name,"Above1000",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Uttarakhand/",name,"Above1000","/",name,"Above1000.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Uttarakhand/",name,"Above1000.html",sep=""))


# Dehradun (1-2)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Dehradun"))
name = "dehradun"
f1 = st_intersection(pre_filters_ul_1,dist)
f2 = st_intersection(pre_filters_ul_2,dist)

dir.create(paste("requested_filter_polygons/Uttarakhand/",name,"Below1000",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Uttarakhand/",name,"Below1000","/",name,"Below1000.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Uttarakhand/",name,"Below1000.html",sep=""))

dir.create(paste("requested_filter_polygons/Uttarakhand/",name,"Above1000",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Uttarakhand/",name,"Above1000","/",name,"Above1000.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Uttarakhand/",name,"Above1000.html",sep=""))






# Tamil Nadu

# kanniyakumari

tn_kk = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kanniyakumari"))
tn_kk_ghats = st_intersection(tn_kk,wg)
st_write(tn_kk_ghats, dsn = "requested_filter_polygons/Tamil Nadu/kanniyakumari_ghats/kanniyakumari_ghats.shp")

map_tn_kk_ghats = mapView(tn_kk_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_kk_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_kk_ghats = leafem::addMouseCoordinates(map_tn_kk_ghats)
mapshot(map_tn_kk_ghats, "requested_filter_polygons/Tamil Nadu/kanniyakumari_ghats.html")

# tirunelveli

tn_ti = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Tirunelveli"))
tn_ti_ghats = st_intersection(tn_ti,wg)
st_write(tn_ti_ghats, dsn = "requested_filter_polygons/Tamil Nadu/tirunelveli_ghats/tirunelveli_ghats.shp")

map_tn_ti_ghats = mapView(tn_ti_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_ti_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_ti_ghats = leafem::addMouseCoordinates(map_tn_ti_ghats)
mapshot(map_tn_ti_ghats, "requested_filter_polygons/Tamil Nadu/tirunelveli_ghats.html")

# tenkasi

tn_te = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Tenkasi"))
tn_te_ghats = st_intersection(tn_te,wg)
st_write(tn_te_ghats, dsn = "requested_filter_polygons/Tamil Nadu/tenkasi_ghats/tenkasi_ghats.shp")

map_tn_te_ghats = mapView(tn_te_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_te_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_te_ghats = leafem::addMouseCoordinates(map_tn_te_ghats)
mapshot(map_tn_te_ghats, "requested_filter_polygons/Tamil Nadu/tenkasi_ghats.html")

# coimbatore

tn_co = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Coimbatore"))
tn_co_ghats = st_intersection(tn_co,wg)

split_line = st_sfc(st_linestring(matrix(c(76.827, 10.508, 77.059, 10.416), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
co_split = tn_co_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:4)

tn_co_ghats = co_split %>% filter(fid %in% c(2,3))
st_write(tn_co_ghats, dsn = "requested_filter_polygons/Tamil Nadu/coimbatore_ghats/coimbatore_ghats.shp")

tn_co_ghats_valparai = co_split %>% filter(fid == 4)
st_write(tn_co_ghats_valparai, dsn = "requested_filter_polygons/Tamil Nadu/coimbatore_ghats_valparai/coimbatore_ghats_valparai.shp")


map_tn_co_ghats = mapView(tn_co_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_co_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_co_ghats = leafem::addMouseCoordinates(map_tn_co_ghats)
mapshot(map_tn_co_ghats, "requested_filter_polygons/Tamil Nadu/coimbatore_ghats.html")

map_tn_co_ghats_valparai = mapView(tn_co_ghats_valparai, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_co_ghats_valparai,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_co_ghats_valparai = leafem::addMouseCoordinates(map_tn_co_ghats_valparai)
mapshot(map_tn_co_ghats_valparai, "requested_filter_polygons/Tamil Nadu/coimbatore_ghats_valparai.html")

# dharmapuri

tn_dh = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Dharmapuri"))
tn_dh_ghats = st_intersection(tn_dh,wg)
st_write(tn_dh_ghats, dsn = "requested_filter_polygons/Tamil Nadu/dharmapuri_ghats/dharmapuri_ghats.shp")

map_tn_dh_ghats = mapView(tn_dh_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_dh_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_dh_ghats = leafem::addMouseCoordinates(map_tn_dh_ghats)
mapshot(map_tn_dh_ghats, "requested_filter_polygons/Tamil Nadu/dharmapuri_ghats.html")

# dindigul

tn_di = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Dindigul"))
tn_di_ghats = st_intersection(tn_di,wg)
st_write(tn_di_ghats, dsn = "requested_filter_polygons/Tamil Nadu/dindigul_ghats/dindigul_ghats.shp")

map_tn_di_ghats = mapView(tn_di_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_di_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_di_ghats = leafem::addMouseCoordinates(map_tn_di_ghats)
mapshot(map_tn_di_ghats, "requested_filter_polygons/Tamil Nadu/dindigul_ghats.html")


# erode

tn_er = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Erode"))
tn_er_ghats = st_intersection(tn_er,wg)
st_write(tn_er_ghats, dsn = "requested_filter_polygons/Tamil Nadu/erode_ghats/erode_ghats.shp")

map_tn_er_ghats = mapView(tn_er_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_er_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_er_ghats = leafem::addMouseCoordinates(map_tn_er_ghats)
mapshot(map_tn_er_ghats, "requested_filter_polygons/Tamil Nadu/erode_ghats.html")


# krishnagiri

tn_ks = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Krishnagiri"))
tn_ks_ghats = st_intersection(tn_ks,wg)
st_write(tn_ks_ghats, dsn = "requested_filter_polygons/Tamil Nadu/krishnagiri_ghats/krishnagiri_ghats.shp")

map_tn_ks_ghats = mapView(tn_ks_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_ks_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_ks_ghats = leafem::addMouseCoordinates(map_tn_ks_ghats)
mapshot(map_tn_ks_ghats, "requested_filter_polygons/Tamil Nadu/krishnagiri_ghats.html")


# madurai

tn_ma = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Madurai"))
tn_ma_ghats = st_intersection(tn_ma,wg)
st_write(tn_ma_ghats, dsn = "requested_filter_polygons/Tamil Nadu/madurai_ghats/madurai_ghats.shp")

map_tn_ma_ghats = mapView(tn_ma_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_ma_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_ma_ghats = leafem::addMouseCoordinates(map_tn_ma_ghats)
mapshot(map_tn_ma_ghats, "requested_filter_polygons/Tamil Nadu/madurai_ghats.html")


# salem

tn_sa = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Salem"))
tn_sa_ghats = st_intersection(tn_sa,wg)
st_write(tn_sa_ghats, dsn = "requested_filter_polygons/Tamil Nadu/salem_ghats/salem_ghats.shp")

map_tn_sa_ghats = mapView(tn_sa_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_sa_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_sa_ghats = leafem::addMouseCoordinates(map_tn_sa_ghats)
mapshot(map_tn_sa_ghats, "requested_filter_polygons/Tamil Nadu/salem_ghats.html")


# theni

tn_th = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Theni"))
tn_th_ghats = st_intersection(tn_th,wg)
st_write(tn_th_ghats, dsn = "requested_filter_polygons/Tamil Nadu/theni_ghats/theni_ghats.shp")

map_tn_th_ghats = mapView(tn_th_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_th_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_th_ghats = leafem::addMouseCoordinates(map_tn_th_ghats)
mapshot(map_tn_th_ghats, "requested_filter_polygons/Tamil Nadu/theni_ghats.html")



# tiruppur

tn_tp = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Tiruppur"))
tn_tp_ghats = st_intersection(tn_tp,wg)

split_line = st_sfc(st_linestring(matrix(c(77.057, 10.492, 77.331, 10.418), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
tp_split = tn_tp_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:4)

tn_tp_ghats = tp_split %>% filter(fid %in% c(1,2,3))
st_write(tn_tp_ghats, dsn = "requested_filter_polygons/Tamil Nadu/tiruppur_ghats/tiruppur_ghats.shp")

tn_tp_ghats_anamalai = tp_split %>% filter(fid == 4)
st_write(tn_tp_ghats_anamalai, dsn = "requested_filter_polygons/Tamil Nadu/tiruppur_ghats_anamalai/tiruppur_ghats_anamalai.shp")


map_tn_tp_ghats = mapView(tn_tp_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_tp_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_tp_ghats = leafem::addMouseCoordinates(map_tn_tp_ghats)
mapshot(map_tn_tp_ghats, "requested_filter_polygons/Tamil Nadu/tiruppur_ghats.html")

map_tn_tp_ghats_anamalai = mapView(tn_tp_ghats_anamalai, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                   layer.name = NULL, 
                                   popup = leafpop::popupTable(tn_tp_ghats_anamalai,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                               feature.id=FALSE, 
                                                               row.numbers=FALSE), 
                                   alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_tp_ghats_anamalai = leafem::addMouseCoordinates(map_tn_tp_ghats_anamalai)
mapshot(map_tn_tp_ghats_anamalai, "requested_filter_polygons/Tamil Nadu/tiruppur_ghats_anamalai.html")

# madurai

tn_ma = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Madurai"))
tn_ma_ghats = st_intersection(tn_ma,wg)
st_write(tn_ma_ghats, dsn = "requested_filter_polygons/Tamil Nadu/madurai_ghats/madurai_ghats.shp")

map_tn_ma_ghats = mapView(tn_ma_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_ma_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_ma_ghats = leafem::addMouseCoordinates(map_tn_ma_ghats)
mapshot(map_tn_ma_ghats, "requested_filter_polygons/Tamil Nadu/madurai_ghats.html")

# virudhunagar

tn_vr = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Virudhunagar"))
tn_vr_ghats = st_intersection(tn_vr,wg)
st_write(tn_vr_ghats, dsn = "requested_filter_polygons/Tamil Nadu/virudhunagar_ghats/virudhunagar_ghats.shp")

map_tn_vr_ghats = mapView(tn_vr_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_vr_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_vr_ghats = leafem::addMouseCoordinates(map_tn_vr_ghats)
mapshot(map_tn_vr_ghats, "requested_filter_polygons/Tamil Nadu/virudhunagar_ghats.html")

# sirumalai, etc.

tn_a1 = st_read("Sirumalai.kml", type=3)
tn_a2 = st_read("Alagar Hills.kml", type=3)
tn_a3 = st_read("Karanthamalai.kml", type=3)

tn_sirumalai = rbind(tn_a1,tn_a2,tn_a3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
tn_sirumalai = tn_sirumalai %>% st_transform(crs_string)

st_write(tn_sirumalai, dsn = "requested_filter_polygons/Tamil Nadu/sirumalai/sirumalai.shp")

map_tn_sirumalai = mapView(tn_sirumalai, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(tn_sirumalai,c("Name","geometry"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_sirumalai = leafem::addMouseCoordinates(map_tn_sirumalai)
mapshot(map_tn_sirumalai, "requested_filter_polygons/Tamil Nadu/sirumalai.html")


# kolli hills

tn_kolli_hills = st_read("Kolli Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
tn_kolli_hills = tn_kolli_hills %>% st_transform(crs_string)

st_write(tn_kolli_hills, dsn = "requested_filter_polygons/Tamil Nadu/kolli_hills/kolli_hills.shp")

map_tn_kolli_hills = mapView(tn_kolli_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                           layer.name = NULL, 
                           popup = leafpop::popupTable(tn_kolli_hills,c("Name","geometry"), 
                                                       feature.id=FALSE, 
                                                       row.numbers=FALSE), 
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_kolli_hills = leafem::addMouseCoordinates(map_tn_kolli_hills)
mapshot(map_tn_kolli_hills, "requested_filter_polygons/Tamil Nadu/kolli_hills.html")


# kalvarayan hills

tn_kalvarayan_hills = st_read("Kalvarayan Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
tn_kalvarayan_hills = tn_kalvarayan_hills %>% st_transform(crs_string)

st_write(tn_kalvarayan_hills, dsn = "requested_filter_polygons/Tamil Nadu/kalvarayan_hills/kalvarayan_hills.shp")

map_tn_kalvarayan_hills = mapView(tn_kalvarayan_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                             layer.name = NULL, 
                             popup = leafpop::popupTable(tn_kalvarayan_hills,c("Name","geometry"), 
                                                         feature.id=FALSE, 
                                                         row.numbers=FALSE), 
                             alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_kalvarayan_hills = leafem::addMouseCoordinates(map_tn_kalvarayan_hills)
mapshot(map_tn_kalvarayan_hills, "requested_filter_polygons/Tamil Nadu/kalvarayan_hills.html")


# jawadhu hills

tn_jawadhu_hills = st_read("Jawadhu Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
tn_jawadhu_hills = tn_jawadhu_hills %>% st_transform(crs_string)

st_write(tn_jawadhu_hills, dsn = "requested_filter_polygons/Tamil Nadu/jawadhu_hills/jawadhu_hills.shp")

map_tn_jawadhu_hills = mapView(tn_jawadhu_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                  layer.name = NULL, 
                                  popup = leafpop::popupTable(tn_jawadhu_hills,c("Name","geometry"), 
                                                              feature.id=FALSE, 
                                                              row.numbers=FALSE), 
                                  alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_tn_jawadhu_hills = leafem::addMouseCoordinates(map_tn_jawadhu_hills)
mapshot(map_tn_jawadhu_hills, "requested_filter_polygons/Tamil Nadu/jawadhu_hills.html")




########################## West Bengal
# sundarbans

wb_sundarbans = st_read("Sundarbans.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
wb_sundarbans = wb_sundarbans %>% st_transform(crs_string)

st_write(wb_sundarbans, dsn = "requested_filter_polygons/West Bengal/sundarbans/sundarbans.shp")

map_wb_sundarbans = mapView(wb_sundarbans, zcol = NULL, map.types = c("Esri.WorldImagery"),
                               layer.name = NULL, 
                               popup = leafpop::popupTable(wb_sundarbans,c("Name","geometry"), 
                                                           feature.id=FALSE, 
                                                           row.numbers=FALSE), 
                               alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_wb_sundarbans = leafem::addMouseCoordinates(map_wb_sundarbans)
mapshot(map_wb_sundarbans, "requested_filter_polygons/West Bengal/sundarbans.html")




########################## Assam
# kamrup rainforests

as_kamrup_forests = st_read("Assam-Meghalaya lowlands.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
as_kamrup_forests = as_kamrup_forests %>% st_transform(crs_string)

as = states_sf %>%
  filter(STATE.NAME %in% c("Assam"))
as_kamrup_forests = st_intersection(as,as_kamrup_forests)

st_write(as_kamrup_forests, dsn = "requested_filter_polygons/Assam/kamrup_forests/kamrup_forests.shp")

map_as_kamrup_forests = mapView(as_kamrup_forests, zcol = NULL, map.types = c("Esri.WorldImagery"),
                            layer.name = NULL, 
                            popup = leafpop::popupTable(as_kamrup_forests,c("Name","STATE.GEOM"), 
                                                        feature.id=FALSE, 
                                                        row.numbers=FALSE), 
                            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_as_kamrup_forests = leafem::addMouseCoordinates(map_as_kamrup_forests)
mapshot(map_as_kamrup_forests, "requested_filter_polygons/Assam/kamrup_forests.html")


# nameri forests

as_nameri_forests = st_read("Sonai Rupai-Nameri.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
as_nameri_forests = as_nameri_forests %>% st_transform(crs_string)

as = states_sf %>%
  filter(STATE.NAME %in% c("Assam"))
as_nameri_forests = st_intersection(as,as_nameri_forests)

st_write(as_nameri_forests, dsn = "requested_filter_polygons/Assam/nameri_forests/nameri_forests.shp")

map_as_nameri_forests = mapView(as_nameri_forests, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                layer.name = NULL, 
                                popup = leafpop::popupTable(as_nameri_forests,c("Name","STATE.GEOM"), 
                                                            feature.id=FALSE, 
                                                            row.numbers=FALSE), 
                                alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_as_nameri_forests = leafem::addMouseCoordinates(map_as_nameri_forests)
mapshot(map_as_nameri_forests, "requested_filter_polygons/Assam/nameri_forests.html")


########################## Arunachal Pradesh
# west kameng low

ar_west_kameng_low = st_read("West Kameng low.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ar_west_kameng_low = ar_west_kameng_low %>% st_transform(crs_string)

wk = dists_sf %>%
  filter(DISTRICT.NAME %in% c("West Kameng"))
ar_west_kameng_low = st_intersection(wk,ar_west_kameng_low)

st_write(ar_west_kameng_low, dsn = "requested_filter_polygons/Arunachal Pradesh/west_kameng_low/west_kameng_low.shp")

map_ar_west_kameng_low = mapView(ar_west_kameng_low, zcol = NULL, map.types = c("Esri.WorldImagery"),
                            layer.name = NULL, 
                            popup = leafpop::popupTable(ar_west_kameng_low,c("Name","DISTRICT.GEOM"), 
                                                        feature.id=FALSE, 
                                                        row.numbers=FALSE), 
                            alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ar_west_kameng_low = leafem::addMouseCoordinates(map_ar_west_kameng_low)
mapshot(map_ar_west_kameng_low, "requested_filter_polygons/Arunachal Pradesh/west_kameng_low.html")


# west kameng high

ar_west_kameng_high = st_read("West Kameng high.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ar_west_kameng_high = ar_west_kameng_high %>% st_transform(crs_string)

wk = dists_sf %>%
  filter(DISTRICT.NAME %in% c("West Kameng"))
ar_west_kameng_high = st_intersection(wk,ar_west_kameng_high)

st_write(ar_west_kameng_high, dsn = "requested_filter_polygons/Arunachal Pradesh/west_kameng_high/west_kameng_high.shp")

map_ar_west_kameng_high = mapView(ar_west_kameng_high, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                 layer.name = NULL, 
                                 popup = leafpop::popupTable(ar_west_kameng_high,c("Name","DISTRICT.GEOM"), 
                                                             feature.id=FALSE, 
                                                             row.numbers=FALSE), 
                                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ar_west_kameng_high = leafem::addMouseCoordinates(map_ar_west_kameng_high)
mapshot(map_ar_west_kameng_high, "requested_filter_polygons/Arunachal Pradesh/west_kameng_high.html")


# lower dibang valley low

ar_lower_dibang_valley_low = st_read("Lower Dibang Valley low.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ar_lower_dibang_valley_low = ar_lower_dibang_valley_low %>% st_transform(crs_string)

ldv = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Lower Dibang Valley"))
ar_lower_dibang_valley_low = st_intersection(ldv,ar_lower_dibang_valley_low)

st_write(ar_lower_dibang_valley_low, dsn = "requested_filter_polygons/Arunachal Pradesh/lower_dibang_valley_low/lower_dibang_valley_low.shp")

map_ar_lower_dibang_valley_low = mapView(ar_lower_dibang_valley_low, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                 layer.name = NULL, 
                                 popup = leafpop::popupTable(ar_lower_dibang_valley_low,c("Name","DISTRICT.GEOM"), 
                                                             feature.id=FALSE, 
                                                             row.numbers=FALSE), 
                                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ar_lower_dibang_valley_low = leafem::addMouseCoordinates(map_ar_lower_dibang_valley_low)
mapshot(map_ar_lower_dibang_valley_low, "requested_filter_polygons/Arunachal Pradesh/lower_dibang_valley_low.html")


############### Andhra Pradesh
# northern eastern ghats

ap_northern_eg = st_read("Andhra northern EG.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ap_northern_eg = ap_northern_eg %>% st_transform(crs_string)

ap = states_sf %>%
  filter(STATE.NAME %in% c("Andhra Pradesh"))
ap_northern_eg = st_intersection(ap,ap_northern_eg)

st_write(ap_northern_eg, dsn = "requested_filter_polygons/Andhra Pradesh/northern_eg/northern_eg.shp")

map_ap_northern_eg = mapView(ap_northern_eg, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                layer.name = NULL, 
                                popup = leafpop::popupTable(ap_northern_eg,c("Name","STATE.GEOM"), 
                                                            feature.id=FALSE, 
                                                            row.numbers=FALSE), 
                                alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ap_northern_eg = leafem::addMouseCoordinates(map_ap_northern_eg)
mapshot(map_ap_northern_eg, "requested_filter_polygons/Andhra Pradesh/northern_eg.html")


##########################Gujarat
# coast

gj_coast = st_read("Gujarat Coast.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
gj_coast = gj_coast %>% st_transform(crs_string)

st_write(gj_coast, dsn = "requested_filter_polygons/Gujarat/coast/coast.shp")

map_gj_coast = mapView(gj_coast, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                layer.name = NULL, 
                                popup = leafpop::popupTable(gj_coast,c("Name","geometry"), 
                                                            feature.id=FALSE, 
                                                            row.numbers=FALSE), 
                                alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_gj_coast = leafem::addMouseCoordinates(map_gj_coast)
mapshot(map_gj_coast, "requested_filter_polygons/Gujarat/coast.html")







############### Karnataka
# nagarhole-bandipur

ka_nagarhole_bandipur = st_read("Nagarhole-Bandipur landscape.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
ka_nagarhole_bandipur = ka_nagarhole_bandipur %>% st_transform(crs_string)

ka = states_sf %>%
  filter(STATE.NAME %in% c("Karnataka"))
ka_nagarhole_bandipur = st_intersection(ka,ka_nagarhole_bandipur)

st_write(ka_nagarhole_bandipur, dsn = "requested_filter_polygons/karnataka/nagarhole_bandipur/nagarhole_bandipur.shp")

map_ka_nagarhole_bandipur = mapView(ka_nagarhole_bandipur, zcol = NULL, map.types = c("Esri.WorldImagery"),
                             layer.name = NULL, 
                             popup = leafpop::popupTable(ka_nagarhole_bandipur,c("Name","STATE.GEOM"), 
                                                         feature.id=FALSE, 
                                                         row.numbers=FALSE), 
                             alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_nagarhole_bandipur = leafem::addMouseCoordinates(map_ka_nagarhole_bandipur)
mapshot(map_ka_nagarhole_bandipur, "requested_filter_polygons/Karnataka/nagarhole_bandipur.html")



# ramanagara

ka_rm = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Ramanagara"))
ka_rm_ghats = st_intersection(ka_rm,wg)
st_write(ka_rm_ghats, dsn = "requested_filter_polygons/Karnataka/ramanagara_ghats/ramanagara_ghats.shp")

map_ka_rm_ghats = mapView(ka_rm_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_rm_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_rm_ghats = leafem::addMouseCoordinates(map_ka_rm_ghats)
mapshot(map_ka_rm_ghats, "requested_filter_polygons/Karnataka/ramanagara_ghats.html")



# chamarajanagara

ka_cj = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Chamarajanagara"))
ka_cj_ghats = st_intersection(ka_cj,wg) %>% dplyr::select(-STATE.NAME)
ka_cj_ghats = st_difference(ka_cj_ghats,ka_nagarhole_bandipur)
ka_cj_ghats = st_cast(ka_cj_ghats, "POLYGON") %>% mutate(fid = 1:5) %>% filter(fid == 1)
st_write(ka_cj_ghats, dsn = "requested_filter_polygons/Karnataka/chamarajanagara_ghats/chamarajanagara_ghats.shp")

map_ka_cj_ghats = mapView(ka_cj_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_cj_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_cj_ghats = leafem::addMouseCoordinates(map_ka_cj_ghats)
mapshot(map_ka_cj_ghats, "requested_filter_polygons/Karnataka/chamarajanagara_ghats.html")


# dakshina kannada

ka_dk = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Dakshina Kannada"))
ka_dk_ghats = st_intersection(ka_dk,wg)
ka_dk_ghats = st_cast(ka_dk_ghats, "POLYGON") %>% mutate(fid = 1:2) %>% filter(fid == 2)
st_write(ka_dk_ghats, dsn = "requested_filter_polygons/Karnataka/dakshina_kannada_ghats/dakshina_kannada_ghats.shp")

map_ka_dk_ghats = mapView(ka_dk_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_dk_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_dk_ghats = leafem::addMouseCoordinates(map_ka_dk_ghats)
mapshot(map_ka_dk_ghats, "requested_filter_polygons/Karnataka/dakshina_kannada_ghats.html")


# hassan

ka_hs = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Hassan"))
ka_hs_ghats = st_intersection(ka_hs,wg)
ka_hs_ghats = st_cast(ka_hs_ghats, "POLYGON") %>% mutate(fid = 1:5) %>% filter(fid == 1)
st_write(ka_hs_ghats, dsn = "requested_filter_polygons/Karnataka/hassan_ghats/hassan_ghats.shp")

map_ka_hs_ghats = mapView(ka_hs_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_hs_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_hs_ghats = leafem::addMouseCoordinates(map_ka_hs_ghats)
mapshot(map_ka_hs_ghats, "requested_filter_polygons/Karnataka/hassan_ghats.html")


# udupi

ka_ud = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Udupi"))
ka_ud_ghats = st_intersection(ka_ud,wg)
st_write(ka_ud_ghats, dsn = "requested_filter_polygons/Karnataka/udupi_ghats/udupi_ghats.shp")

map_ka_ud_ghats = mapView(ka_ud_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_ud_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_ud_ghats = leafem::addMouseCoordinates(map_ka_ud_ghats)
mapshot(map_ka_ud_ghats, "requested_filter_polygons/Karnataka/udupi_ghats.html")


# belagavi

ka_bg = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Belagavi"))
ka_bg_ghats = st_intersection(ka_bg,wg)
ka_bg_ghats = st_cast(ka_bg_ghats, "POLYGON") %>% mutate(fid = 1:4) %>% filter(fid == 4)
st_write(ka_bg_ghats, dsn = "requested_filter_polygons/Karnataka/belagavi_ghats/belagavi_ghats.shp")

map_ka_bg_ghats = mapView(ka_bg_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_bg_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_bg_ghats = leafem::addMouseCoordinates(map_ka_bg_ghats)
mapshot(map_ka_bg_ghats, "requested_filter_polygons/Karnataka/belagavi_ghats.html")



# shivamogga

ka_sh = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Shivamogga"))
ka_sh_ghats = st_intersection(ka_sh,wg)
st_write(ka_sh_ghats, dsn = "requested_filter_polygons/Karnataka/shivamogga_ghats/shivamogga_ghats.shp")

map_ka_sh_ghats = mapView(ka_sh_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_sh_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_sh_ghats = leafem::addMouseCoordinates(map_ka_sh_ghats)
mapshot(map_ka_sh_ghats, "requested_filter_polygons/Karnataka/shivamogga_ghats.html")


# uttara_kannada

ka_uk = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Uttara Kannada"))
ka_uk_ghats = st_intersection(ka_uk,wg)
st_write(ka_uk_ghats, dsn = "requested_filter_polygons/Karnataka/uttara_kannada_ghats/uttara_kannada_ghats.shp")

map_ka_uk_ghats = mapView(ka_uk_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_uk_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_uk_ghats = leafem::addMouseCoordinates(map_ka_uk_ghats)
mapshot(map_ka_uk_ghats, "requested_filter_polygons/Karnataka/uttara_kannada_ghats.html")



# chikkamagaluru

ka_ck = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Chikkamagaluru"))
ka_ck_ghats = st_intersection(ka_ck,wg)
ka_ck_ghats = st_cast(ka_ck_ghats, "POLYGON") %>% mutate(fid = 1:2) %>% filter(fid == 2)
st_write(ka_ck_ghats, dsn = "requested_filter_polygons/Karnataka/chikkamagaluru_ghats/chikkamagaluru_ghats.shp")

map_ka_ck_ghats = mapView(ka_ck_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(ka_ck_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_ka_ck_ghats = leafem::addMouseCoordinates(map_ka_ck_ghats)
mapshot(map_ka_ck_ghats, "requested_filter_polygons/Karnataka/chikkamagaluru_ghats.html")



######################## Maharashtra
# kolhapur

mh_ko = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kolhapur"))
mh_ko_ghats = st_intersection(mh_ko,wg)

split_line = st_sfc(st_linestring(matrix(c(74.268, 16.076, 73.996, 17.032), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
ko_split = mh_ko_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:11)

mh_ko_ghats = ko_split %>% filter(fid %in% c(6))
st_write(mh_ko_ghats, dsn = "requested_filter_polygons/Maharashtra/kolhapur_ghats/kolhapur_ghats.shp")

map_mh_ko_ghats = mapView(mh_ko_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(mh_ko_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_ko_ghats = leafem::addMouseCoordinates(map_mh_ko_ghats)
mapshot(map_mh_ko_ghats, "requested_filter_polygons/Maharashtra/kolhapur_ghats.html")



# sangli

mh_sn = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Sangli"))
mh_sn_ghats = st_intersection(mh_sn,wg)
mh_sn_ghats = st_cast(mh_sn_ghats, "POLYGON") %>% mutate(fid = 1:9) %>% filter(fid == 7)
st_write(mh_sn_ghats, dsn = "requested_filter_polygons/Maharashtra/sangli_ghats/sangli_ghats.shp")

map_mh_sn_ghats = mapView(mh_sn_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(mh_sn_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_sn_ghats = leafem::addMouseCoordinates(map_mh_sn_ghats)
mapshot(map_mh_sn_ghats, "requested_filter_polygons/Maharashtra/sangli_ghats.html")




# satara

mh_st = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Satara"))
mh_st_ghats = st_intersection(mh_st,wg)

split_line = st_sfc(st_linestring(matrix(c(73.946, 17.108, 74.031, 18.715), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
st_split = mh_st_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:5)

mh_st_ghats = st_split %>% filter(fid %in% c(1))


split_line = st_sfc(st_linestring(matrix(c(73.965, 17.845, 73.870, 18.135), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
st_split = mh_st_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:2)

mh_st_ghats = st_split %>% filter(fid %in% c(1))


st_write(mh_st_ghats, dsn = "requested_filter_polygons/Maharashtra/satara_ghats/satara_ghats.shp")

map_mh_st_ghats = mapView(mh_st_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(mh_st_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_st_ghats = leafem::addMouseCoordinates(map_mh_st_ghats)
mapshot(map_mh_st_ghats, "requested_filter_polygons/Maharashtra/satara_ghats.html")



# pune

mh_pu = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Pune"))
mh_pu_ghats = st_intersection(mh_pu,wg)

split_line = st_sfc(st_linestring(matrix(c(73.898, 18.246, 73.718, 18.594), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
pu_split = mh_pu_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:3)

mh_pu_ghats = pu_split %>% filter(fid %in% c(1))


split_line = st_sfc(st_linestring(matrix(c(73.779, 18.901, 73.927, 19.416), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
pu_split = mh_pu_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:3)

mh_pu_ghats = pu_split %>% filter(fid %in% c(1))


st_write(mh_pu_ghats, dsn = "requested_filter_polygons/Maharashtra/pune_ghats/pune_ghats.shp")

map_mh_pu_ghats = mapView(mh_pu_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(mh_pu_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_pu_ghats = leafem::addMouseCoordinates(map_mh_pu_ghats)
mapshot(map_mh_pu_ghats, "requested_filter_polygons/Maharashtra/pune_ghats.html")



# ahmednagar

mh_ah = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Ahmadnagar"))
mh_ah_ghats = st_intersection(mh_ah,wg)

split_line = st_sfc(st_linestring(matrix(c(73.912, 19.347, 73.930, 19.703), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
ah_split = mh_ah_ghats %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:3)

mh_ah_ghats = ah_split %>% filter(fid %in% c(2))
st_write(mh_ah_ghats, dsn = "requested_filter_polygons/Maharashtra/ahmednagar_ghats/ahmednagar_ghats.shp")

map_mh_ah_ghats = mapView(mh_ah_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(mh_ah_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_ah_ghats = leafem::addMouseCoordinates(map_mh_ah_ghats)
mapshot(map_mh_ah_ghats, "requested_filter_polygons/Maharashtra/ahmednagar_ghats.html")



# nashik

mh_ns = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Nashik"))
mh_ns_ghats = st_intersection(mh_ns,wg)
mh_ns_ghats = st_cast(mh_ns_ghats, "POLYGON") %>% mutate(fid = 1:2) %>% filter(fid == 2)
st_write(mh_ns_ghats, dsn = "requested_filter_polygons/Maharashtra/nashik_ghats/nashik_ghats.shp")

map_mh_ns_ghats = mapView(mh_ns_ghats, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(mh_ns_ghats,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_ns_ghats = leafem::addMouseCoordinates(map_mh_ns_ghats)
mapshot(map_mh_ns_ghats, "requested_filter_polygons/Maharashtra/nashik_ghats.html")


# nandurbar_hills

mh_nandurbar_hills = st_read("Central India Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
mh_nandurbar_hills = mh_nandurbar_hills %>% st_transform(crs_string)

nb = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Nandurbar"))
mh_nandurbar_hills = st_intersection(nb,mh_nandurbar_hills)

st_write(mh_nandurbar_hills, dsn = "requested_filter_polygons/Maharashtra/nandurbar_hills/nandurbar_hills.shp")

map_mh_nandurbar_hills = mapView(mh_nandurbar_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                         layer.name = NULL, 
                                         popup = leafpop::popupTable(mh_nandurbar_hills,c("Name","DISTRICT.GEOM"), 
                                                                     feature.id=FALSE, 
                                                                     row.numbers=FALSE), 
                                         alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_nandurbar_hills = leafem::addMouseCoordinates(map_mh_nandurbar_hills)
mapshot(map_mh_nandurbar_hills, "requested_filter_polygons/Maharashtra/nandurbar_hills.html")


# dhule_hills

mh_dhule_hills = st_read("Central India Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
mh_dhule_hills = mh_dhule_hills %>% st_transform(crs_string)

dh = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Dhule"))
mh_dhule_hills = st_intersection(dh,mh_dhule_hills)

st_write(mh_dhule_hills, dsn = "requested_filter_polygons/Maharashtra/dhule_hills/dhule_hills.shp")

map_mh_dhule_hills = mapView(mh_dhule_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                                 layer.name = NULL, 
                                 popup = leafpop::popupTable(mh_dhule_hills,c("Name","DISTRICT.GEOM"), 
                                                             feature.id=FALSE, 
                                                             row.numbers=FALSE), 
                                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_dhule_hills = leafem::addMouseCoordinates(map_mh_dhule_hills)
mapshot(map_mh_dhule_hills, "requested_filter_polygons/Maharashtra/dhule_hills.html")



# jalgaon_hills

mh_jalgaon_hills = st_read("Central India Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
mh_jalgaon_hills = mh_jalgaon_hills %>% st_transform(crs_string)

jg = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Jalgaon"))
mh_jalgaon_hills = st_intersection(jg,mh_jalgaon_hills)

st_write(mh_jalgaon_hills, dsn = "requested_filter_polygons/Maharashtra/jalgaon_hills/jalgaon_hills.shp")

map_mh_jalgaon_hills = mapView(mh_jalgaon_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                             layer.name = NULL, 
                             popup = leafpop::popupTable(mh_jalgaon_hills,c("Name","DISTRICT.GEOM"), 
                                                         feature.id=FALSE, 
                                                         row.numbers=FALSE), 
                             alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_jalgaon_hills = leafem::addMouseCoordinates(map_mh_jalgaon_hills)
mapshot(map_mh_jalgaon_hills, "requested_filter_polygons/Maharashtra/jalgaon_hills.html")



# buldhana_hills

mh_buldhana_hills = st_read("Central India Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
mh_buldhana_hills = mh_buldhana_hills %>% st_transform(crs_string)

bu = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Buldana"))
mh_buldhana_hills = st_intersection(bu,mh_buldhana_hills)

st_write(mh_buldhana_hills, dsn = "requested_filter_polygons/Maharashtra/buldhana_hills/buldhana_hills.shp")

map_mh_buldhana_hills = mapView(mh_buldhana_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                             layer.name = NULL, 
                             popup = leafpop::popupTable(mh_buldhana_hills,c("Name","DISTRICT.GEOM"), 
                                                         feature.id=FALSE, 
                                                         row.numbers=FALSE), 
                             alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_buldhana_hills = leafem::addMouseCoordinates(map_mh_buldhana_hills)
mapshot(map_mh_buldhana_hills, "requested_filter_polygons/Maharashtra/buldhana_hills.html")



# amravati_hills

mh_amravati_hills = st_read("Central India Hills.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
mh_amravati_hills = mh_amravati_hills %>% st_transform(crs_string)

am = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Amravati"))
mh_amravati_hills = st_intersection(am,mh_amravati_hills)

st_write(mh_amravati_hills, dsn = "requested_filter_polygons/Maharashtra/amravati_hills/amravati_hills.shp")

map_mh_amravati_hills = mapView(mh_amravati_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                             layer.name = NULL, 
                             popup = leafpop::popupTable(mh_amravati_hills,c("Name","DISTRICT.GEOM"), 
                                                         feature.id=FALSE, 
                                                         row.numbers=FALSE), 
                             alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_amravati_hills = leafem::addMouseCoordinates(map_mh_amravati_hills)
mapshot(map_mh_amravati_hills, "requested_filter_polygons/Maharashtra/amravati_hills.html")


# nagpur_hills

mh_nagpur_hills = st_read("Nagpur Hills North.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
mh_nagpur_hills = mh_nagpur_hills %>% st_transform(crs_string)

ng = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Nagpur"))
mh_nagpur_hills = st_intersection(ng,mh_nagpur_hills)

st_write(mh_nagpur_hills, dsn = "requested_filter_polygons/Maharashtra/nagpur_hills/nagpur_hills.shp")

map_mh_nagpur_hills = mapView(mh_nagpur_hills, zcol = NULL, map.types = c("Esri.WorldImagery"),
                             layer.name = NULL, 
                             popup = leafpop::popupTable(mh_nagpur_hills,c("Name","DISTRICT.GEOM"), 
                                                         feature.id=FALSE, 
                                                         row.numbers=FALSE), 
                             alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_mh_nagpur_hills = leafem::addMouseCoordinates(map_mh_nagpur_hills)
mapshot(map_mh_nagpur_hills, "requested_filter_polygons/Maharashtra/nagpur_hills.html")



################## ladakh

# leh_mountains

la_le = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Leh"))

split_line = st_sfc(st_linestring(matrix(c(76.921, 35.942, 76.284, 34.602), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
le_split = la_le %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:2)

la_le = le_split %>% filter(fid %in% c(1))


st_write(la_le, dsn = "requested_filter_polygons/Ladakh/leh_mountains/leh_mountains.shp")

map_la_le = mapView(la_le, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(la_le,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_la_le = leafem::addMouseCoordinates(map_la_le)
mapshot(map_la_le, "requested_filter_polygons/Ladakh/leh_mountains.html")


########### Himachal Pradesh

# lahaul

hp_lahaul = st_read("Lahaul.kml", type=3) %>% dplyr::select(-Description)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
hp_lahaul = hp_lahaul %>% st_transform(crs_string)

ls = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Lahul and Spiti"))
hp_lahaul = st_intersection(ls,hp_lahaul)

st_write(hp_lahaul, dsn = "requested_filter_polygons/Himachal Pradesh/lahaul/lahaul.shp")

map_hp_lahaul = mapView(hp_lahaul, zcol = NULL, map.types = c("Esri.WorldImagery"),
                              layer.name = NULL, 
                              popup = leafpop::popupTable(hp_lahaul,c("Name","DISTRICT.GEOM"), 
                                                          feature.id=FALSE, 
                                                          row.numbers=FALSE), 
                              alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_hp_lahaul = leafem::addMouseCoordinates(map_hp_lahaul)
mapshot(map_hp_lahaul, "requested_filter_polygons/Himachal Pradesh/lahaul.html")


# kangra_high

hp_ka_high = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kangra"))

split_line = st_sfc(st_linestring(matrix(c(76.154, 32.25, 76.693, 32.047), 
                                         ncol = 2, byrow = TRUE)), crs = crs_string)
ka_split = hp_ka_high %>%
  lwgeom::st_split(split_line) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fid = 1:2)

hp_ka_high = ka_split %>% filter(fid %in% c(1))


st_write(hp_ka_high, dsn = "requested_filter_polygons/Himachal Pradesh/kangra_high/kangra_high.shp")

map_hp_ka_high = mapView(hp_ka_high, zcol = NULL, map.types = c("Esri.WorldImagery"),
                    layer.name = NULL, 
                    popup = leafpop::popupTable(hp_ka_high,c("DISTRICT.NAME","DISTRICT.GEOM"), 
                                                feature.id=FALSE, 
                                                row.numbers=FALSE), 
                    alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_hp_ka_high = leafem::addMouseCoordinates(map_hp_ka_high)
mapshot(map_hp_ka_high, "requested_filter_polygons/Himachal Pradesh/kangra_high.html")



########### Kerala

pre_filters_kl_1 = pre_filters %>% filter(AREA_1 == "India--Kerala--0to40m")
pre_filters_kl_1 = st_make_valid(pre_filters_kl_1)
pre_filters_kl_2 = pre_filters %>% filter(AREA_1 == "India--Kerala--40-200m")
pre_filters_kl_2 = st_make_valid(pre_filters_kl_2)
pre_filters_kl_3 = pre_filters %>% filter(AREA_1 == "India--Kerala--200-600m")
pre_filters_kl_3 = st_make_valid(pre_filters_kl_3)
pre_filters_kl_4 = pre_filters %>% filter(AREA_1 == "India--Kerala--600-1200m")
pre_filters_kl_4 = st_make_valid(pre_filters_kl_4)
pre_filters_kl_5n = pre_filters %>% filter(AREA_1 == "India--Kerala--1200m-plus--North")
pre_filters_kl_5n = st_make_valid(pre_filters_kl_5n)
pre_filters_kl_5s = pre_filters %>% filter(AREA_1 == "India--Kerala--1200m-plus--South")
pre_filters_kl_5s = st_make_valid(pre_filters_kl_5s)
pre_filters_kl_6 = pre_filters %>% filter(AREA_1 == "India--Kerala--Wayanad-MinusHighlands")
pre_filters_kl_6 = st_make_valid(pre_filters_kl_6)



# Thiruvananthapuram (all 5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Thiruvananthapuram"))
name = "thiruvananthapuram"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5s,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))



# Kollam (all 5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kollam"))
name = "kollam"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5s,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))


# Alapuzha (done)

# Pathanamthitta (2-5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Pathanamthitta"))
name = "pathanamthitta"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5s,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))

# Kottayam (2-4)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kottayam"))
name = "kottayam"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

# Idukki (2-5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Idukki"))
name = "idukki"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f2 = st_union(f2,f1) %>% select(c(1:4,9))
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5s,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))

# Ernakulam (1-5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Ernakulam"))
name = "ernakulam"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5s,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))

# Thrissur (1-5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Thrissur"))
name = "thrissur"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

# Palakkad (1-5) (south and north)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Palakkad"))
name = "palakkad"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5n = st_intersection(pre_filters_kl_5n,dist)
f5s = st_intersection(pre_filters_kl_5s,dist)


dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus_north",sep=""))
st_write(f5n, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus_north","/",name,"1200plus_north.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5n = mapView(f5n, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5n,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5n = leafem::addMouseCoordinates(map_f5n)
mapshot(map_f5n, paste("requested_filter_polygons/Kerala/",name,"1200plus_north.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus_south",sep=""))
st_write(f5s, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus_south","/",name,"1200plus_south.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5s = mapView(f5s, zcol = NULL, map.types = c("Esri.WorldImagery"),
                  layer.name = NULL, 
                  popup = leafpop::popupTable(f5s,c("geometry"), 
                                              feature.id=FALSE, 
                                              row.numbers=FALSE), 
                  alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5s = leafem::addMouseCoordinates(map_f5s)
mapshot(map_f5s, paste("requested_filter_polygons/Kerala/",name,"1200plus_south.html",sep=""))


# Malappuram (1-5)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Malappuram"))
name = "malappuram"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5n,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))


# Kozhikode (1-5)


dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kozhikode"))
name = "kozhikode"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5n,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))


# Wayanad (5) (all else)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Wayanad"))
name = "wayanad"
f5 = st_intersection(pre_filters_kl_5n,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"1200plus",sep=""))
st_write(f5, dsn = paste("requested_filter_polygons/Kerala/",name,"1200plus","/",name,"1200plus.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f5 = mapView(f5, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f5,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f5 = leafem::addMouseCoordinates(map_f5)
mapshot(map_f5, paste("requested_filter_polygons/Kerala/",name,"1200plus.html",sep=""))

# Kannur (1-4)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kannur"))
name = "kannur"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)
f5 = st_intersection(pre_filters_kl_5n,dist)
f4 = st_union(f4,f5) %>% select(c(1:4,9))

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))

# Kasaragod (1-4)

dist = dists_sf %>%
  filter(DISTRICT.NAME %in% c("Kasaragod"))
name = "kasaragod"
f1 = st_intersection(pre_filters_kl_1,dist)
f2 = st_intersection(pre_filters_kl_2,dist)
f3 = st_intersection(pre_filters_kl_3,dist)
f4 = st_intersection(pre_filters_kl_4,dist)

dir.create(paste("requested_filter_polygons/Kerala/",name,"0-40",sep=""))
st_write(f1, dsn = paste("requested_filter_polygons/Kerala/",name,"0-40","/",name,"0-40.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f1 = mapView(f1, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f1,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f1 = leafem::addMouseCoordinates(map_f1)
mapshot(map_f1, paste("requested_filter_polygons/Kerala/",name,"0-40.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"40-200",sep=""))
st_write(f2, dsn = paste("requested_filter_polygons/Kerala/",name,"40-200","/",name,"40-200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f2 = mapView(f2, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f2,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f2 = leafem::addMouseCoordinates(map_f2)
mapshot(map_f2, paste("requested_filter_polygons/Kerala/",name,"40-200.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"200-600",sep=""))
st_write(f3, dsn = paste("requested_filter_polygons/Kerala/",name,"200-600","/",name,"200-600.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f3 = mapView(f3, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f3,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f3 = leafem::addMouseCoordinates(map_f3)
mapshot(map_f3, paste("requested_filter_polygons/Kerala/",name,"200-600.html",sep=""))

dir.create(paste("requested_filter_polygons/Kerala/",name,"600-1200",sep=""))
st_write(f4, dsn = paste("requested_filter_polygons/Kerala/",name,"600-1200","/",name,"600-1200.shp",sep=""))
mapviewOptions(fgb = FALSE)
map_f4 = mapView(f4, zcol = NULL, map.types = c("Esri.WorldImagery"),
                 layer.name = NULL, 
                 popup = leafpop::popupTable(f4,c("geometry"), 
                                             feature.id=FALSE, 
                                             row.numbers=FALSE), 
                 alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")
map_f4 = leafem::addMouseCoordinates(map_f4)
mapshot(map_f4, paste("requested_filter_polygons/Kerala/",name,"600-1200.html",sep=""))




plot(st_geometry(f5), main = "Original Polygon", col = "lightblue", lwd = 2)
plot(st_geometry(split_line), main = "Intersection Polygon", col = "lightblue", lwd = 2)
plot(st_geometry(split_line), add = TRUE, col = "red", lty = 1, lwd = 2)

