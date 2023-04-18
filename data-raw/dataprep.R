proj <- redcap_export_meta(token, url)
save(proj,file = "data/proj.RData")

meta <- proj$metadata
meta <- meta[-c(33:38),]
save(meta, file = "data/meta.RData")

importdemo <- read.csv("data-raw/importdemo.csv", na.strings = "", strip.white = TRUE, fileEncoding = "UTF-8-BOM")
save(importdemo, file = "data/importdemo.RData")



