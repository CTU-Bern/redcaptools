token <- read.table("O:/Projects/RC_TOKENS/redcaptools_basic.txt")$V1
url <- "https://redcap.ctu.unibe.ch/api/"
meta <- redcap_export_meta(token, url)$meta
save(meta, file = "data/meta.RData")

importdemo <- read.csv("data/importdemo.csv", na.strings = "", strip.white = TRUE, fileEncoding = "UTF-8-BOM")
save(importdemo, file = "data/importdemo.RData")



