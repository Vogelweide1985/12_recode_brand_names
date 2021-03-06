require(dplyr)
require(tidyr)


# Set Language to German
Sys.setenv(LANG = "de_DE.UTF-8")



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



df <- readRDS("raw/unaided_brand_awareness.rds") #Reading dataset
df <- pivot_longer(df, starts_with("f06"), values_to = "answer") #Transform to Long-Format
df <- filter(df, answer != "") #Remove blank answers


#Preprocess answers
string_format <- function(x) {     
   x <- gsub("^\\s+|\\s+$", "", x) # remove trailing spaces
   x <- tolower(x)   # to lower
   x <- gsub("ä","ae",x) # remove german "Sonderlaute"
   x <- gsub("ü","ue",x) # remove german "Sonderlaute"
   x <- gsub("ö","oe",x) # remove german "Sonderlaute"
   x <- gsub("ß","ss",x) # remove german "Sonderlaute"
   x <- gsub("|","",x)   # remove special characters
   return(x)
}

df$answer <- string_format(df$answer) #Applying function

#Preprocess dict to vector
dict_long <- unlist(dict)


