library(dplyr)



# Set Language to German
Sys.setenv(LANG = "de_DE.UTF-8")

#Reading dataset
df <- readRDS("raw/unaided_brand_awareness.rds")

#Creating Dictionary with aliases with banks of interest
dict <- list("Deutsche Bank" = c("deutsche bank", "db"),
             "Commerzbank" = c("commerzbank"),
             "Postbank" = c("postbank"), 
             "Hypovereinsbank" = c("hypo vereinsbank", "hypo", "hypobank"),
             "Targobank" = c("targobank"),
             "Sparkasse" = c("sparkasse", "kreissparkasse", "stadtsparkasse"),
             "Volks- und Raiffeisenbank" = c( "v+r","v&r", "volksbank", "raiffeisenbank", "volks und raiffeisenbank"),
             "Ingdiba" = c("ing diba","ingdiba"),
             "Santander"= c("santander"),
             "Spardabank" = c("sparda-bank", "sparda"),
             "Union Invest" = c("union invest", "union"),
             "N26" = c("n26"))
