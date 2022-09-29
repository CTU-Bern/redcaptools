

secret_scrambled <- "DYrfd-J02ZulqR2mBA9B16vOPm9RwQg_JHCDH9P1ymzT6vNM8fV8yUnEgwUY1Al-"
secret_scrambled2 <- "Zl6q-5wKZbmJzJNHc9Ma67AUDh23-_xbxdvCrGqCL7QHTQUsVgmTy0T2h8f5Fsjc"
get_token <- function() httr2::secret_decrypt(secret_scrambled, "REDCAPTOOLS_KEY")
get_token2 <- function() httr2::secret_decrypt(secret_scrambled2, "REDCAPTOOLS_KEY")

