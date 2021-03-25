require(dplyr)
require(tidyr)
require(purrr)
require(forcats)
require(caret) # only for confusion matrix
require(knitr) # for visualisiing purposes only, feel free to skip
require(kableExtra) # for visualisiing purposes only, feel free to skip
Sys.setenv(LANG = "de_DE.UTF-8") # Set Language to German

#Reading dataset
df <- readRDS("raw/unaided_brand_awareness.rds") 
colnames(df) <- c("ID", paste("response", 1:8, sep = "_"))

#First look
kableExtra::kable(head(df[df$response_1 != "keine",], 40), "html") %>% kable_minimal() 
length(unique(df$ID)) # N interviewe

remove("vctrs")
#Creating Dictionary with aliases with banks of interest
dict <- list("Deutsche_Bank" = c("deutsche bank", "db"),
             "Commerzbank" = c("commerzbank"),
             "Postbank" = c("postbank"), 
             "Hypovereinsbank" = c("hypo vereinsbank", "hypo", "hypobank"),
             "Targobank" = c("targobank", "targo"),
             "Sparkasse" = c("sparkasse", "kreissparkasse", "stadtsparkasse"),
             "Volks_und_Raiffeisenbank" = c( "v+r", "volksbank", "raiffeisenbank", "volks und raiffeisenbank"),
             "Ingdiba" = c("ing diba", "ing", "ingdiba", "diba"),
             "Santander"= c("santander"),
             "Spardabank" = c("sparda-bank", "sparda"),
             "Union_Invest" = c("union invest", "union"),
             "N26" = c("n26"),
             "No_answer" = c("keine", "keine ahnung"))




#Preprocess answers
df <- tidyr::pivot_longer(df, starts_with("response"), values_to = "answer") #Transform to Long-Format
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


#Lets have a look on frequencies
df_unique <- df %>%
   group_by(answer) %>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   mutate(frq = n / sum(n)*100) %>%
   mutate(frq_sum = cumsum(frq))


#Optional: Wordcloud
library(wordcloud)
library(RColorBrewer)
wordcloud::wordcloud(df_unique$answer, df_unique$n, max.words = 1000, colors=brewer.pal(8, "Dark2"),
                      random.order = F)



dict_long <- unlist(dict) #Preprocess dict to vector

#Level 0: Exactmatching
df_unique$lvl_0 <- NA
for(i in 1: length(dict_long)) {
   df_unique$lvl_0 <- ifelse(df_unique$answer == dict_long[i], dict_long[i],
                                        df_unique$lvl_0  )
}

#Level 1: Contains Word matching
df_unique$lvl_1 <- NA
for(i in 1: length(dict_long)) {
   df_unique$lvl_1 <- ifelse(grepl(dict_long[i], df_unique$answer), dict_long[i],
                                        df_unique$lvl_1  )
}

#Level 2.1 and 2.2: LDM and LDM with Cut 2, and Cut depending on string length

#Creating levenshtein distance matrix (ldm)
levenshtein <- function(answers, dict) {
   
   #Creating levenshetein distance
   df <- adist(answers, dict, costs = c("ins"=1, "del"=1, "sub"=2))
   
   df <- as.data.frame(df) 
   colnames(df) <- dict  # user friendly view
   
   #Decision making based on distance and cut
   cols <- c("min_distance", "col_index") # view result cols
   df[, cols] <- NA # Init cols
   for ( i in 1:nrow(df) ) { 
      
      df[i,"col_index"] <-  which.min(df[i,!(colnames(df) %in% cols )])
      df[i,"min_distance"] <-  min(df[i,!(colnames(df) %in% cols )])
      #df[i,"ldm"] <- if(!is.na(df[i,"col_index"])) {colnames(df[df$col_index[i]]) } else {NA}
   }
   df$ldm <- ifelse(!is.na(df["col_index"]), colnames(df)[df$col_index], NA )
   df$answer <- answers # original answers
   return(df)
}


ldm <- levenshtein(df_unique$answer, dict_long)

#Binding to df
df_unique <- left_join(df_unique, ldm)
df_unique$lvl_2_1 <- df_unique$ldm

# Setting specific distance cuts, so classification is stopped, when dist is to far
df_unique$lvl_2_2<- ifelse(df_unique$min_distance<= 2, df_unique$ldm, NA)
df_unique$lvl_2_3<- ifelse(   (df_unique$min_distance <= 2 & nchar(df_unique$answer) >3) | 
                              (df_unique$min_distance <= 4 & nchar(df_unique$answer) >=8) | 
                              (df_unique$min_distance <= 6 & nchar(df_unique$answer) >=12), df_unique$ldm, NA)


# Level 3: Bringing levenshtein and Matching together

df_unique$lvl_3 <- ifelse(!is.na(df_unique$lvl_1), df_unique$lvl_1, df_unique$lvl_2_3)


# Level 4: Manual Recoding to calculate confusion Matrix | not perfect!!!

correct_na <- c("dkb", "dbk", "spar", "debk", "dibs", "dinga", "inba", "tarox", "voksdbank", "bausparkasse",
                "audibank", "apo bank", "digabank", "ergo bank", "volkswagenbank", "deutsche kreditbank",
                "volkswagen bank", "bausparkasse sha", "spreewaldbank")

correct_commerz <- c("commerz", "coba", "comerz", "commerbacg", "comerzbach", "comerz finanz	")
correct_raiffeisenbank <- c("voba", "raiba", "v+r", "reiba", "volks", "r v", "r*v", "r/v", 	"raifeisen", "v+r bank", "raiffeinsen",
  "reiffeisen", "raifeissen", "raiifeisen	", "vb bank", "vr bamk", "vr bsnk", "r&v bank", "v&r bank",
  "volks- uns raiffeisen", "volks-undraiffeisen", "volks u. raiffeisen", "volks+raiffeisen bank",
  "volks- reifeisenbank", "	volks reifen bank", "r u v bank", "r v banken", "r&v banken", "v + r banken", 
  "v r banken", "voba/raiffeisen", "volks-raiffeinsenbanken", "volks-reifeisenbank", "volks und raiba",
  "volksreifeisen bank", "volks-raiffeisen", "r+v meine bank", "vb heilbronn", "volks-raiffeissen", 
  "vr bank flaeming")
correct_post <- c("post", "deutsche postbank")
correct_n26 <- c("no. 26", "bank 26", "n25 bank")
correct_sparda <- c("spadaka")
correct_ingdiba <- c("inkbiba")
correct_sparkasse <- c("haspa", "nospa", "kreissparkassse koeln", "landesspaarkasse", "stadtsparkass muenchen", "sparkasse boeblingen",
  "spk mittelthueringen	")
correct_db <- c("dt bank", "d. bank", "dt.bank")

#Recoding lvl_3 with manual recodes to lvl_4, the "reference"
df_unique$lvl_4 <-  df_unique$lvl_3 
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_na, NA, df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_commerz, "commerzbank", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_raiffeisenbank, "raiffeisenbank", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_post, "postbank", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_n26, "n26", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_sparda, "sparda", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_ingdiba, "ingdiba", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_sparkasse , "sparkasse", df_unique$lvl_4)
df_unique$lvl_4 <- ifelse(df_unique$answer %in% correct_db  , "db", df_unique$lvl_4)


#Recoding all Alisases to names and building factors for confusion matrix
#Could be purred -> lazy
df_unique$lvl_0_rec <- as_factor(df_unique$lvl_0) %>% fct_collapse(!!!dict, ) %>% fct_explicit_na()
df_unique$lvl_1_rec <- as_factor(df_unique$lvl_1) %>% fct_collapse(!!!dict)  %>% fct_explicit_na()
df_unique$lvl_2_1_rec <- as_factor(df_unique$lvl_2_1) %>% fct_collapse(!!!dict) %>% fct_explicit_na()
df_unique$lvl_2_2_rec <- as_factor(df_unique$lvl_2_2) %>% fct_collapse(!!!dict) %>% fct_explicit_na()
df_unique$lvl_2_3_rec <- as_factor(df_unique$lvl_2_3) %>% fct_collapse(!!!dict) %>% fct_explicit_na()
df_unique$lvl_3_rec <- as_factor(df_unique$lvl_3) %>% fct_collapse(!!!dict) %>% fct_explicit_na()
df_unique$lvl_4_rec <- as_factor(df_unique$lvl_4) %>% fct_collapse(!!!dict) %>% fct_explicit_na()


#Final Results -> Based on Unique ansers
caret::confusionMatrix(df_unique$lvl_0_rec, df_unique$lvl_4_rec )
caret::confusionMatrix(df_unique$lvl_1_rec, df_unique$lvl_4_rec )
caret::confusionMatrix(df_unique$lvl_2_1_rec, df_unique$lvl_4_rec )
caret::confusionMatrix(df_unique$lvl_2_2_rec, df_unique$lvl_4_rec )
caret::confusionMatrix(df_unique$lvl_2_3_rec, df_unique$lvl_4_rec )
caret::confusionMatrix(df_unique$lvl_3_rec, df_unique$lvl_4_rec )

#Final Results -> Based on answers x Frequency
#Again lazy copy-paste coding, sorry
df_unique_expanded <- df_unique[rep(row.names(df_unique), df_unique$n),] # Expand df
caret::confusionMatrix(df_unique_expanded$lvl_0_rec, df_unique_expanded$lvl_4_rec )
caret::confusionMatrix(df_unique_expanded$lvl_1_rec, df_unique_expanded$lvl_4_rec )
caret::confusionMatrix(df_unique_expanded$lvl_2_1_rec, df_unique_expanded$lvl_4_rec )
caret::confusionMatrix(df_unique_expanded$lvl_2_2_rec, df_unique_expanded$lvl_4_rec )
caret::confusionMatrix(df_unique_expanded$lvl_2_3_rec, df_unique_expanded$lvl_4_rec )
caret::confusionMatrix(df_unique_expanded$lvl_3_rec, df_unique_expanded$lvl_4_rec )


