library(tools)
library(stringr)

# read files and combine

# Andhra Pradesh

fil = st_read("requested_filter_polygons/Andhra Pradesh/northern_eg/northern_eg.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Andhra Pradesh--Northern Eastern Ghats","Andhra Pradesh","multiple")
fil.comb = fil

# Arunachal Pradesh

fil = st_read("requested_filter_polygons/Arunachal Pradesh/lower_dibang_valley_low/lower_dibang_valley_low.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Arunachal Pradesh--Dibang Valley Lowlands","Arunachal Pradesh","Lower Dibang Valley")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Arunachal Pradesh/west_kameng_high/west_kameng_high.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Arunachal Pradesh--West Kameng Highlands","Arunachal Pradesh","West Kameng")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Arunachal Pradesh/west_kameng_low/west_kameng_low.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Arunachal Pradesh--West Kameng Lowlands","Arunachal Pradesh","West Kameng")
fil.comb = rbind(fil.comb,fil)


# Assam

fil = st_read("requested_filter_polygons/Assam/kamrup_forests/kamrup_forests.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Assam--Garbhanga Forest Landscape","Assam","multiple")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Assam/nameri_forests/nameri_forests.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Assam--Nameri Forest Landscape","Assam","Sonitpur")
fil.comb = rbind(fil.comb,fil)


# Gujarat

fil = st_read("requested_filter_polygons/Gujarat/coast/coast.shp", type=3)
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Gujarat--Coast","Gujarat","multiple")
fil.comb = rbind(fil.comb,fil)


# Himachal Pradesh

fil = st_read("requested_filter_polygons/Himachal Pradesh/kangra_high/kangra_high.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Himachal Pradesh--Kangra Highlands","Himachal Pradesh","Kangra")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Himachal Pradesh/lahaul/lahaul.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Himachal Pradesh--Lahaul","Himachal Pradesh","Lahaul")
fil.comb = rbind(fil.comb,fil)


# Karnataka

fil = st_read("requested_filter_polygons/Karnataka/belagavi_ghats/belagavi_ghats.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Belagavi Ghats","Karnataka","Belagavi")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/chamarajanagara_ghats/chamarajanagara_ghats.shp", type=3) 
fil = fil[,c(-1,-2,-3)]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Chamarajanagara Ghats","Karnataka","Chamarajanagara")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/chikkamagaluru_ghats/chikkamagaluru_ghats.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Chikkamagaluru Ghats","Karnataka","Chikkamagaluru")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/dakshina_kannada_ghats/dakshina_kannada_ghats.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Dakshina Kannada Ghats","Karnataka","Dakshina Kannada")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/hassan_ghats/hassan_ghats.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Hassan Ghats","Karnataka","Hassan")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/nagarhole_bandipur/nagarhole_bandipur.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Nagarhole Forest Landscape","Karnataka","multiple")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/ramanagara_ghats/ramanagara_ghats.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Ramanagara Ghats","Karnataka","Ramanagara")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/shivamogga_ghats/shivamogga_ghats.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Shivamogga Ghats","Karnataka","Shivamogga")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/udupi_ghats/udupi_ghats.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Udupi Ghats","Karnataka","Udupi")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Karnataka/uttara_kannada_ghats/uttara_kannada_ghats.shp", type=3) 
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Karnataka--Uttara Kannada Ghats","Karnataka","Uttara Kannada")
fil.comb = rbind(fil.comb,fil)



# ######## Kerala
# 
# directory_path = "./requested_filter_polygons/Kerala"
# 
# # List directory names within the specified directory
# directory_names = list.dirs(directory_path, full.names = FALSE)
# directory_names = directory_names[directory_names != ""]
# 
# for (i in directory_names)
# {
#   split_string <- str_split(i, "(?=[^A-Za-z])", n = 2)
#   result = split_string[[1]][1]
#   fil = st_read(paste("requested_filter_polygons/Kerala/",i,"/",i,".shp",sep=""), type=3)
#   fil = fil[,-1]
#   names(fil)[1:3] = c("Name","State","District")
#   st_geometry(fil) = "Geometry"
#   fil[1:3] = c(paste("Kerala--",toTitleCase(i),sep=""),"Kerala",toTitleCase(result))
#   fil.comb = rbind(fil.comb,fil)
# }


###### Ladakh

fil = st_read("requested_filter_polygons/Ladakh/leh_mountains/leh_mountains.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Ladakh--Leh Mountains","Ladakh","Leh")
fil.comb = rbind(fil.comb,fil)


##### Maharashtra

directory_path = "./requested_filter_polygons/Maharashtra"

# List directory names within the specified directory
directory_names = list.dirs(directory_path, full.names = FALSE)
directory_names = directory_names[directory_names != ""]

for (i in directory_names)
{
  split_string <- str_split(i, "_")
  result <- unlist(split_string)
  fil = st_read(paste("requested_filter_polygons/Maharashtra/",i,"/",i,".shp",sep=""), type=3) 
  if (ncol(fil) == 5)
  {
    fil = fil[,-1]
  }
  names(fil)[1:3] = c("Name","State","District")
  st_geometry(fil) = "Geometry"
  fil[1:3] = c(paste("Maharashtra--",toTitleCase(result[1])," ",toTitleCase(result[2]),sep=""),"Maharashtra",toTitleCase(result[1]))
  fil.comb = rbind(fil.comb,fil)
}

##### Uttarakhand

fil = st_read("requested_filter_polygons/Uttarakhand/nainitalBelow1000/nainitalBelow1000.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Uttarakhand--NainitalBelow1000","Uttarakhand","Nainital")
fil.comb = rbind(fil.comb,fil)

# fil = st_read("requested_filter_polygons/Uttarakhand/nainitalAbove1000/nainitalAbove1000.shp", type=3) 
# fil = fil[,-1]
# names(fil)[1:3] = c("Name","State","District")
# st_geometry(fil) = "Geometry"
# fil[1:3] = c("Uttarakhand--NainitalAbove1000","Uttarakhand","Nainital")
# fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Uttarakhand/dehradunBelow1000/dehradunBelow1000.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Uttarakhand--DehradunBelow1000","Uttarakhand","Dehradun")
fil.comb = rbind(fil.comb,fil)

# fil = st_read("requested_filter_polygons/Uttarakhand/dehradunAbove1000/dehradunAbove1000.shp", type=3) 
# fil = fil[,-1]
# names(fil)[1:3] = c("Name","State","District")
# st_geometry(fil) = "Geometry"
# fil[1:3] = c("Uttarakhand--DehradunAbove1000","Uttarakhand","Dehradun")
# fil.comb = rbind(fil.comb,fil)


##### Tamil Nadu

directory_path = "./requested_filter_polygons/Tamil Nadu"

# List directory names within the specified directory
directory_names = list.dirs(directory_path, full.names = FALSE)
directory_names = directory_names[directory_names != ""]

toexclude = c("coimbatore_ghats_valparai","gudalur","masinagudi","sirumalai",
              "tiruppur_ghats_anamalai","jawadhu_hills","kalvarayan_hills",
              "kolli_hills","nilgiri_plateau","coimbatore_ghats","tiruppur_ghats")
directory_names = directory_names[!directory_names %in% toexclude]


fil = st_read("requested_filter_polygons/Tamil Nadu/coimbatore_ghats_valparai/coimbatore_ghats_valparai.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Valparai","Tamil Nadu","Coimbatore")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/gudalur/gudalur.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Gudalur","Tamil Nadu","The Nilgiris")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/masinagudi/masinagudi.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Masinagudi","Tamil Nadu","The Nilgiris")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/sirumalai/sirumalai.shp", type=3) 
fil = st_sf(st_combine(fil))
fil = fil %>% mutate(a = NA, b = NA, c = NA) %>% relocate(a,b,c)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Sirumalai Hills","Tamil Nadu","multiple")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/tiruppur_ghats_anamalai/tiruppur_ghats_anamalai.shp", type=3) 
fil = fil[,-1]
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Tiruppur Ghats--Anamalai","Tamil Nadu","Tiruppur")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/jawadhu_hills/jawadhu_hills.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Jawadhu Hills","Tamil Nadu","multiple")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/kalvarayan_hills/kalvarayan_hills.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Kalvarayan Hills","Tamil Nadu","multiple")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/kolli_hills/kolli_hills.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Kolli Hills","Tamil Nadu","multiple")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/nilgiri_plateau/nilgiri_plateau.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Nilgiri Plateau","Tamil Nadu","The Nilgiris")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/coimbatore_ghats/coimbatore_ghats.shp", type=3) 
fil = st_sf(st_combine(fil))
fil = fil %>% mutate(a = NA, b = NA, c = NA) %>% relocate(a,b,c)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Coimbatore Ghats","Tamil Nadu","Coimbatore")
fil.comb = rbind(fil.comb,fil)

fil = st_read("requested_filter_polygons/Tamil Nadu/tiruppur_ghats/tiruppur_ghats.shp", type=3) 
fil = st_sf(st_combine(fil))
fil = fil %>% mutate(a = NA, b = NA, c = NA) %>% relocate(a,b,c)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("Tamil Nadu--Tiruppur Ghats","Tamil Nadu","Tiruppur")
fil.comb = rbind(fil.comb,fil)


for (i in directory_names)
{
  split_string <- str_split(i, "_")
  result <- unlist(split_string)
  fil = st_read(paste("requested_filter_polygons/Tamil Nadu/",i,"/",i,".shp",sep=""), type=3) 
  if (ncol(fil) == 5)
  {
    fil = fil[,-1]
  }
  names(fil)[1:3] = c("Name","State","District")
  st_geometry(fil) = "Geometry"
  fil[1:3] = c(paste("Tamil Nadu--",toTitleCase(result[1])," ",toTitleCase(result[2]),sep=""),"Tamil Nadu",toTitleCase(result[1]))
  fil.comb = rbind(fil.comb,fil)
}


###### West Bengal

fil = st_read("requested_filter_polygons/West Bengal/sundarbans/sundarbans.shp", type=3) 
fil = fil %>% mutate(a = NA, b = NA) %>% relocate(a,b)
names(fil)[1:3] = c("Name","State","District")
st_geometry(fil) = "Geometry"
fil[1:3] = c("West Bengal--Sundarbans","West Bengal","multiple")
fil.comb = rbind(fil.comb,fil)


for_sheet = st_drop_geometry(fil.comb)
write.csv(for_sheet,"requested_filter_polygons/proposed_polygon_filters.csv",row.names=F)

st_write(fil.comb, dsn = "requested_filter_polygons/0_proposed_polygon_filters/0_proposed_polygon_filters.shp")

map_proposed = mapView(fil.comb, zcol = NULL, map.types = c("Esri.WorldImagery"),
                          layer.name = NULL, 
                          popup = leafpop::popupTable(fil.comb,c("Name","State","District"), 
                                                      feature.id=FALSE, 
                                                      row.numbers=FALSE), 
                          alpha.regions = 0, lwd = 5, legend = NULL, color = "#660000")

map_proposed = leafem::addMouseCoordinates(map_proposed)
mapshot(map_proposed, "requested_filter_polygons/proposed_polygon_filters.html")
