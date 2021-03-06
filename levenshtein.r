library(dplyr)



# Set Language to German
Sys.setenv(LANG = "de_DE.UTF-8")


df <- readRDS("raw/unaided_brand_awareness.rds")


lex.08_Bank <- c("commerzbank", "deutsche bank","hypo vereinsbank", "hypovereinsbank","hypobank",
                 "targobank", "sparkasse", "kreissparkasse", "stadtsparkasse","v+r","v&r", "volksbank", "raiffeisenbank", "volks und raiffeisenbank",
                 "postbank", "post bank", "ing diba","ingdiba", "santander", "sparda-bank", "sparda",
                 "union invest", "union", "n26")
