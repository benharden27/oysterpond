data_path <- "~/data/oyster_pond/OP_all_data_2005_2017updated_Mar 19 2018.xlsx"

df <- readxl::read_excel(data_path, skip = 1, n_max = 1)
data_names <- stringr::str_to_lower(names(df))

df <- readxl::read_excel(data_path, skip = 3)
names(df) <- data_names

names(df)[3] <- "height_ransom"
names(df)[4] <- "air_temp"
names(df)[10] <- "do_mgl"
names(df)[11] <- "do_pc"

df <- df[ ,1:26]

stringr::str_replace_all(df$site," ",".")

df$site <- stringr::str_replace(df$site,"([A-Z]) ([0-9])","\\1\\2")
df$site <- stringr::str_replace(df$site,"([0-9]) ([0-9])","\\1_\\2")
df$site <- stringr::str_replace(df$site,"([0-9]) (top)","\\1_\\2")
df$site <- stringr::str_replace(df$site,"([0-9]) (bottom)","\\1_\\2")
df$site <- stringr::str_replace(df$site,"([0-9]) ([a-z])","\\1\\2")

df$site <- stringr::str_to_lower(df$site)
df$site[stringr::str_which(df$site,"ransom.*dock.*b")] <- "ransom_bottom"
df$site[stringr::str_which(df$site,"ransom.*dock.*top|ransom.*dock$")] <- "ransom_top"
df$site[stringr::str_which(df$site,"treetop.*dock.*top|treetop.*dock$|treeptop.*dock$|treeptop.*dock.*top")] <- "treetop_top"
df$site[stringr::str_which(df$site,"treetop.*dock$")] <- "treetop_top"
df$site[stringr::str_which(df$site,"treetop.*dock.*b|treeptop.*dock.*b")] <- "treetop_bottom"
df$site[stringr::str_which(df$site,"spohr.*dock.*b")] <- "spohr_bottom"
df$site[stringr::str_which(df$site,"spohr.*dock.*top|spohr.*dock$")] <- "spohr_top"
df$site[stringr::str_which(df$site,"weir")] <- "weir"
df$site[stringr::str_which(df$site,"wastepoint|waste.*point")] <- "waste_point"
df$site <- stringr::str_replace(df$site," ","_")


df <- dplyr::mutate(df, depth = 0)
df$depth[stringr::str_which(df$site,"1m")] <- 1
df$depth[stringr::str_which(df$site,"2m")] <- 2
df$depth[stringr::str_which(df$site,"3m")] <- 3
df$depth[stringr::str_which(df$site,"4m")] <- 4
df$depth[stringr::str_which(df$site,"5m")] <- 5
df$depth[stringr::str_which(df$site,"op3_bottom")] <- 6
df$depth[stringr::str_which(df$site,"op2_bottom")] <- 4
df$depth[stringr::str_which(df$site,"op1_bottom")] <- 4

df$site <- stringr::str_remove_all(df$site,"_.*")


for(i in 1:nrow(df)) {
  if(is.na(df$date[i])) {
    df$date[i] <- df$date[i-1]
  }
}


spohr_loc <- c(41.542555, -70.640697)
ransom_loc <- c(41.546279, -70.641934)
treetop_loc <- c(41.547044, -70.640752)
weir_loc <- c(41.537310, -70.639976)
mosquito_loc <- c(41.546086, -70.641877)
op1_loc <- c(41.545567, -70.640186)
op2_loc <- c(41.541687, -70.638548)
op3_loc <- c(41.539257, -70.637410)
lagoon_loc <- c(41.536447, -70.639932)

lat <- rep(NA,nrow(df))
lat[stringr::str_which(df$site,"op1")] <- op1_loc[1]
lat[stringr::str_which(df$site,"op2")] <- op2_loc[1]
lat[stringr::str_which(df$site,"op3")] <- op3_loc[1]
lat[stringr::str_which(df$site,"treetop")] <- treetop_loc[1]
lat[stringr::str_which(df$site,"weir")] <- weir_loc[1]
lat[stringr::str_which(df$site,"mosquito")] <- mosquito_loc[1]
lat[stringr::str_which(df$site,"ransom")] <- ransom_loc[1]
lat[stringr::str_which(df$site,"spohr")] <- spohr_loc[1]
lat[stringr::str_which(df$site,"lagoon")] <- lagoon_loc[1]

lon <- rep(NA,nrow(df))
lon[stringr::str_which(df$site,"op1")] <- op1_loc[2]
lon[stringr::str_which(df$site,"op2")] <- op2_loc[2]
lon[stringr::str_which(df$site,"op3")] <- op3_loc[2]
lon[stringr::str_which(df$site,"treetop")] <- treetop_loc[2]
lon[stringr::str_which(df$site,"weir")] <- weir_loc[2]
lon[stringr::str_which(df$site,"mosquito")] <- mosquito_loc[2]
lon[stringr::str_which(df$site,"ransom")] <- ransom_loc[2]
lon[stringr::str_which(df$site,"spohr")] <- spohr_loc[2]
lon[stringr::str_which(df$site,"lagoon")] <- lagoon_loc[2]


df$salinity <- as.numeric(df$salinity)
df <- dplyr::mutate(df,lon = lon, lat = lat)

op_dat <- df


# saveRDS(op_dat,"oysterpond/data/op_dat.rds")

usethis::use_data(op_dat, overwrite = TRUE)

