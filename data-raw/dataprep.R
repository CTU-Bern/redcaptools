meta <- redcap_export_meta(token, url)$meta
meta <- meta[-c(33:38),]
save(meta, file = "data/meta.RData")

importdemo <- read.csv("data/importdemo.csv", na.strings = "", strip.white = TRUE, fileEncoding = "UTF-8-BOM")
save(importdemo, file = "data/importdemo.RData")



