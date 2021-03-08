require(dplyr)
require(tidyr)
require(purrr)

# Set Language to German
Sys.setenv(LANG = "de_DE.UTF-8")



#Creating Dictionary with aliases with banks of interest
dict <- list("Deutsche Bank" = c("deutsche bank", "db"),
             "Commerzbank" = c("commerzbank"),
             "Postbank" = c("postbank"), 
             "Hypovereinsbank" = c("hypo vereinsbank", "hypo", "hypobank"),
             "Targobank" = c("targobank", "targo"),
             "Sparkasse" = c("sparkasse", "kreissparkasse", "stadtsparkasse"),
             "Volks- und Raiffeisenbank" = c( "v+r", "volksbank", "raiffeisenbank", "volks und raiffeisenbank"),
             "Ingdiba" = c("ing diba", "ing", "ingdiba", "diba"),
             "Santander"= c("santander"),
             "Spardabank" = c("sparda-bank", "sparda"),
             "Union Invest" = c("union invest", "union"),
             "N26" = c("n26"),
             "No answer" = c("keine", "keine ahnung"))



df <- readRDS("raw/unaided_brand_awareness.rds") #Reading dataset
head(df, 50)#Lets have a look

#Preprocess answers
df <- pivot_longer(df, starts_with("f06"), values_to = "answer") #Transform to Long-Format
df <- filter(df, answer != "") #Remove blank answers

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


#Lets have a look on frequencies
unique_answers <- df %>%
   group_by(answer) %>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   mutate(frq = n / sum(n)*100) %>%
   mutate(frq_sum = cumsum(frq))

#PLOTS


#Level 0: Exactmatching
unique_answers$lvl_0 <- NA
for(i in 1: length(dict_long)) {
   unique_answers$lvl_0 <- ifelse(unique_answers$answer == dict_long[i], dict_long[i],
                                        unique_answers$lvl_0  )
}

#Level 1: Contains Word matching
unique_answers$lvl_1 <- NA
for(i in 1: length(dict_long)) {
   unique_answers$lvl_1 <- ifelse(grepl(dict_long[i], unique_answers$answer), dict_long[i],
                                        unique_answers$lvl_1  )
}

#Level 2.1 and 2.2: LDM and LDM with Cut 2

#Creating levenshtein distance matrix (ldm)
levenshtein <- function(answers, dict, cut) {
   
   #Creating levenshetein distance
   df <- adist(answers, dict, costs = c("ins"=1, "del"=1, "sub"=2))
   
   df <- as.data.frame(df) 
   colnames(df) <- dict  # user friendly view
   
   #Decision making based on distance and cut
   cols <- c("ldm", "ldm_cut", "min_distance", "col_index") # view result cols
   df[, cols] <- NA # Init cols
   for ( i in 1:nrow(df) ) { 
      
      df[i,"col_index"] <-  which.min(df[i,!(colnames(df) %in% cols )])
      df[i,"min_distance"] <-  min(df[i,!(colnames(df) %in% cols )])
      df[i,"ldm"] <- if(!is.na(df[i,"col_index"])) {colnames(df[df$col_index[i]]) } else {NA}
      df[i,"ldm_cut"] <- if(df[i,"min_distance"] <=cut) {df[i,"ldm"]} else {NA}
   }
   
   df$answers <- answers # original answers
   df <-df[,c("answers",cols[1:(length(cols)-1)], dict)] #selecting
   return(df)
}


ldm <- levenshtein(unique_answers$answer, dict_long, cut = 2)

#Binding to df
unique_answers$lvl_2_1 <- ldm$ldm
unique_answers$lvl_2_2<- ldm$ldm_cut

