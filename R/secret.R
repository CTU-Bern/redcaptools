


secret_scrambled <- "DYrfd-J02ZulqR2mBA9B16vOPm9RwQg_JHCDH9P1ymzT6vNM8fV8yUnEgwUY1Al-"
get_token <- function() httr2::secret_decrypt(secret_scrambled, "REDCAPTOOLS_KEY")

