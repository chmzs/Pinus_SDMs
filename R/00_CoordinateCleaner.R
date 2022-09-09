# https://docs.ropensci.org/CoordinateCleaner/articles/Cleaning_GBIF_data_with_CoordinateCleaner.html
out_df <- readr::read_csv("datas/Occs/Pinus_armandii_gbif_origin.csv")

library(countrycode)
library(CoordinateCleaner)  # 主要Occs数据清洗
library(dplyr)
library(ggplot2)


# select columns of interest
dat <- out_df %>%
  dplyr::select(species, longitude, latitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)

# remove records without coordinates
dat <- dat%>%
  filter(!is.na(longitude))%>%
  filter(!is.na(latitude))

#plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = longitude, y = latitude),
             colour = "darkred", size = 0.5)+
  theme_bw()


# 使用clean_coordinates包装函数
#convert country code from ISO2c to ISO3c
# 手动处理将”China’ 替换为 'CN'
dat$countryCode <- countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

#flag problems:标记无问题的
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat, 
                           lon = "longitude", 
                           lat = "latitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries")) # most test are on by default

summary(flags)
plot(flags, lon = "longitude", lat = "latitude")

# 我们将排除标记的记录，但通常建议进一步探索它们。
#Exclude problematic records
dat_cl <- dat[flags$.summary,]

#The flagged records
dat_fl <- dat[!flags$.summary,]


# 时间异常监测
flags <- cf_age(x = dat_cl,
                lon = "longitude",
                lat = "latitude",
                taxon = "species", 
                min_age = "year", 
                max_age = "year", 
                value = "flagged")

dat_cl[!flags, "year"]
dat_cl <- dat_cl[flags, ]

# 使用 GBIF 元数据提高数据质量
#Remove records with low coordinate precision
hist(dat_cl$coordinateUncertaintyInMeters / 1000, breaks = 20)


