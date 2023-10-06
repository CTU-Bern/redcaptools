token <- read.table("O:/Projects/RC_TOKENS/redcaptools_basic.txt")$V1
url <- "https://redcap.ctu.unibe.ch/api/"
importdemo_dict <- redcap_export_meta(token, url)$meta
save(importdemo_dict, file = "data/importdemo_dict.RData")

importdemo_data <- read.csv("data-raw/importdemo_data.csv", na.strings = "", strip.white = TRUE, fileEncoding = "UTF-8-BOM")
save(importdemo_data, file = "data/importdemo_data.RData")
