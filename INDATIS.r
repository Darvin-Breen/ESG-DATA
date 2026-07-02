# ============================================================================
#ESG 
# ============================================================================

rm(list = ls())

# 1. SETUP ENVIRONMENT ------------------------------------------------------
library(tidyverse)
library(tidytext)
library(readr)
library(stopwords)
library(wordcloud)
library(openxlsx)
library(ggplot2)
library(patchwork)
library(igraph)
library(ggraph)

# ============================================================================
# 2. LOAD TEXT FILES WITH YEAR EXTRACTION -----------------------------------
# (from Script 1 and modified script)
# ============================================================================

folder_path <- "/Users/Neng/Desktop/ESG COMPLETE TXT"

if(!dir.exists(folder_path)) {
  stop("Folder not found: ", folder_path)
}

file_list <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)

if(length(file_list) == 0) {
  stop("No .txt files found in: ", folder_path)
}

texts <- map(file_list, ~ {
  tryCatch({
    paste(read_lines(.x, locale = locale(encoding = "UTF-8")), collapse = " ")
  }, error = function(e) {
    message("Error reading: ", .x)
    NA_character_
  })
}) %>% 
  set_names(tools::file_path_sans_ext(basename(file_list)))

texts <- discard(texts, is.na)

# Create corpus_df with year column
corpus_df <- tibble(
  doc_id = names(texts),
  text = map_chr(texts, as.character),
  year = str_extract(doc_id, "[0-9]{2}$")
) %>%
  mutate(year = ifelse(year %in% c("23", "24"), year, NA)) %>%
  filter(!is.na(year))

cat("Loaded", nrow(corpus_df), "documents\n")
cat("Years:", unique(corpus_df$year), "\n")


# ============================================================================
# 2.5 STANDARDIZE YEAR FORMAT TO 2 DIGITS
# ============================================================================

corpus_df <- corpus_df %>%
  mutate(
    doc_id = str_trim(doc_id),                     # Remove leading/trailing spaces
    doc_id = str_replace(doc_id, "2023", "23"),    # 4-digit to 2-digit
    doc_id = str_replace(doc_id, "2024", "24"),    # 4-digit to 2-digit
    year = str_extract(doc_id, "(23|24)$")         # Re-extract year (now clean)
  )

cat("Year format standardized. Unique years:", paste(unique(corpus_df$year), collapse=", "), "\n")

# ============================================================================
# 3. STOP WORDS (Italian + English + Company Names + Noise)
# ============================================================================

# 3.1 Italian stop words
italian_stopwords <- stopwords("it", source = "stopwords-iso")
italian_stop_words <- tibble(word = italian_stopwords, lexicon = "italian")

custom_italian_stop_words <- tibble(
  word = c("delle", "della", "del", "dei", "degli", "una", "loro", "sono","banca","moncler's","snam","unipolis","n.a",
           "alla", "alle", "allo", "agli", "questo", "questa", "questi","unicredit's","mps","enel's","terpump",
           "queste", "quello", "quella", "quelli", "quelle", "come", "con","diasorin's","hera's","www.mediobanca.com",
           "per", "tra", "fra", "sul", "sulla", "sui", "sugli", "sulle","recordati's","terna's","mediolanum's",
           "nella", "nelle", "negli", "siano", "essere", "aver", "avere","azimut's","generali's","recordati", 
           "dell'", "_esgcon", "i.e.", "www.issgovernance.com", "mediolanum","tenaris's","www.gruppotim.it",
           "saipem", "bper", "terna", "enel", "pirelli", "hera", "generali", "tim","www,prysmian.com",
           "campari","poste","italiane","stellantis","italgas", "prysmian", "intesa", "sanpaolo", "unicredit","prysmian's","fcs",
           "eni", "nexi", "fineco", "interpump", "finecobank", "unipolsai","fineco's","bank's","st's","investors.st.it",
           "amplifon", "bpm", "montepaschi", "unipol", "inwit", "moncler","eni's","brunello","cucinelli","www.st.com",
           "camparisti", "tenaris", "diasorin", "iveco", "leonardo","leonardo's", "mediobanca","inwit's","pirelli's",
           "pagg", "which", "bancobpm", "sondrio","saipem's","italiane's","ferrari","nexigroup", "unicreditgroup",
           "stmicroelectronics", "paschi", "telecom", "azimut", "siena", "erg","gruppo.bancobpm.it","bpm's","www,erg,eu",
           "pump","nb_", "nb", "pry", "gruppotim","p.p","bank's","bank","eni's","sanpaolo's","www.nexigroup.com","www.stellantis.com"),
  lexicon = "custom_italian"
)

# 3.2 English stop words
english_stop_words <- stop_words %>% filter(lexicon == "snowball")

custom_english_stop_words <- tibble(
  word = c("figure", "table", "appendix", "chapter", "section",
           "et", "al", "eg", "ie", "etc", "vs", "vol", "no",
           "would", "could", "should", "may", "might", "must",
           "using", "used", "use", "based", "according","bancobpm.it","gmps",
           "iaeg", "safs", "wmr", "cnfs", "cmb", "elig", "das", "slt", "fpt", "rkfo",
           "urc", "ncih", "odji", "ajm", "qcnb", "wdz", "rdoc", "odjin", "fehj", "ncihm",
           "ekh", "meha", "mwj", "bwj", "dji", "cwdw", "ntg", "brugg", "uop", "trak",
           "grpg", "cbsr", "elvs", "chrto", "gpsq","investors.st.com","banco","popolare","bmps",
           "han", "css", "mil", "ing", "tion", "ment", "mber", "emplo", "com", "pos","usd","www.terna.it",
           "roup", "olic", "ort", "erations", "ation", "ime", "eet", "par", "gar", "neg",
           "tled", "ves", "onal", "ons", "ﬁed", "iden", "repor", "opportuni", "ﬁnancial", "speciﬁc",
           "www.generali.com", "www.issgovernance.com", "www.gruppotim.it", "www.prysmian.com",
           "www.st.com", "www.nexigroup.com", "www.stellantis.com", "www.erg.eu","www.italglas.it","bps",
           "gruppo.bancobpm.it", "investors.st.it","yes",
           "pb_", "m_j", "i_j", "dl_hedc", "speci.c", "identi.ed", "certi.ed", "s.a",
           "s.r.l", "s.p.a", "u.m", "n.d", "n.s","n.v"),
  lexicon = "custom_english"
)

# 3.3 COMPANY NAMES AS STOP WORDS (to prevent brand names from appearing in clusters)
company_names <- c(
  # Automotive
  "renault", "ferrari", "bmw", "mercedes", "mercedesbenz", "benz", "iveco", "toyota", 
  "audi", "ford", "hyundai", "lamborghini", "piaggio", "porsche", "volkswagen", 
  "stellantis", "landrover", "cnh", "cuh",
  
  # Infrastructure/Construction
  "webuild", "anas", "saipem", "mundys", "ansaldo", "enav", "pizzarotti", "impresa",
  
  # Consulting
  "deloitte", "ey", "pwc", "bcg", "kpmg", "engineering", "capgemini", "accenture", "mckinsey",
  
  # Furniture/Design
  "scavolini", "veneta", "flos", "natuzzi", "bb", "italia",
  
  # Energy/Utilities
  "terna", "a2a", "enel", "eni", "acea", "edison", "snam", "iren", "hera", "gse", "erg", 
  "italgas", "sorgenia", "maire", "nadara", "kuwait", "petroleum", "esso", "saras", "nuovo", "pignone",
  
  # Luxury/Fashion
  "loreal", "luxottica", "brunello", "cucinelli", "otb", "prada", "versace", "armani", 
  "gucci", "bulgari", "moncler", "valentino", "ovs", "zegna", "ermenegildo", "tods", 
  "benetton", "kiko", "dolce", "gabbana", "lir", "geox", "diadora", "hm", "nike", 
  "bottega", "veneta", "safilo", "ferragamo", "marcolin", "adidas", "yoox", "netaporter", 
  "oniverse", "calzedonia", "furla", "cavalli", "loro", "piana", "zara", "maxmara", "max", "mara",
  
  # Banking/Finance
  "intesa", "sanpaolo", "unicredit", "poste", "italiane", "sace", "bper", "mediobanca", 
  "banco", "bpm", "generali", "credit", "agricole", "bnl", "bnpparibas", "paribas", 
  "credem", "azimut", "unipol", "monte", "paschi", "fineco", "allianz", "sella", 
  "illimity", "nexi", "popolare", "sondrio", "mediolanum", "borsa", "deutsche", "reale", 
  "mutua", "fsi", "exor", "cattolica", "quintet", "21invest",
  
  # Food/Beverage
  "ferrero", "barilla", "lavazza", "illy", "granlatte", "granarolo", "heineken", "mcdonalds", 
  "campari", "nestle", "neste", "amadori", "cameo", "cocacola", "cola", "bauli", "veronesi", 
  "nescafe", "kellogg", "cremonini", "benedetto", "parmalat", "conad", "coop", "lidl", 
  "ikea", "amazon", "eataly", "autogrill", "pg", "eurospin", "esselunga",
  
  # Industrial/Manufacturing
  "pirelli", "fincantieri", "siemens", "prysmian", "lego", "brembo", "abb", "amplifon", 
  "samsung", "tenaris", "stmicroelectronics", "apple", "ima", "ibm", "huawei", "ariston", 
  "delonghi", "cementir", "holding", "nintendo", "technogym", "danieli", "cir", "whirlpool", 
  "interpump",
  
  # Telecommunications/Media
  "leonardo", "tim", "openfiber", "sky", "vodafone", "fastweb", "inwit", "mediaset", "iliad", 
  "arnoldo", "mondadori", "wind", "tre", "gedi", "netflix", "rai", "dazn", "cairo", "disney",
  
  # Pharma
  "menarini", "angelini", "sanofi", "chiesi", "roche", "novartis", "abbvie", "bracco", 
  "diasorin", "gsk", "recordati", "janssen",
  
  # Transportation
  "ferrovie", "trenitalia", "ita", "airways", "italo", "ntv",
  
  # Common fragments to remove
  "spa", "srl", "holding", "group", "gruppo", "brand", "ltd", "inc", "bv", "nv"
)

# 3.4 ADDITIONAL NOISE WORDS (boilerplate, filing metadata, etc.)
noise_words <- c(
  # Filing metadata
  "annual", "report", "financial", "statement", "consolidated", "fiscal", "year",
  "february", "march", "april", "may", "june", "july", "august", "september", 
  "october", "november", "december", "january", "monday", "tuesday", "wednesday", 
  "thursday", "friday", "saturday", "sunday",
  
  # Document structure
  "page", "chapter", "section", "appendix", "figure", "table", "exhibit", "note",
  "see", "refer", "reference", "footnote", "disclaimer", "boilerplate",
  
  # Legal boilerplate
  "hereby", "therein", "thereof", "hereinafter", "aforesaid", "pursuant",
  "notwithstanding", "whereas", "henceforth", "thereto", "therewith", "wherein",
  
  # Generic action words (low signal)
  "provide", "includes", "including", "ensure", "maintain", "continue", "follow",
  "require", "requires", "permit", "allows", "enable", "facilitate",
  
  # Numbers
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "first", "second", "third", "fourth", "fifth"
)

# 3.5 Combine all stop words
custom_company_stops <- tibble(
  word = unique(tolower(company_names)),
  lexicon = "company_names"
)

custom_noise_stops <- tibble(
  word = noise_words,
  lexicon = "noise"
)

# Bind everything together
all_stop_words <- bind_rows(
  italian_stop_words,           # ISO Italian stop words
  custom_italian_stop_words,    # Custom Italian words
  english_stop_words,           # Snowball English stop words
  custom_english_stop_words,    # Custom English words
  custom_company_stops,         # Company names (NEW)
  custom_noise_stops            # Noise words (NEW)
)

# Optional: Check how many stop words we have
cat("Total stop words loaded:", nrow(all_stop_words), "\n")
cat("  - Italian:", nrow(italian_stop_words), "\n")
cat("  - Custom Italian:", nrow(custom_italian_stop_words), "\n")
cat("  - English (Snowball):", nrow(english_stop_words), "\n")
cat("  - Custom English:", nrow(custom_english_stop_words), "\n")
cat("  - Company names:", nrow(custom_company_stops), "\n")
cat("  - Noise words:", nrow(custom_noise_stops), "\n")

# ============================================================================
# END OF STOP WORDS SECTION
# ============================================================================

# ============================================================================
# 4. TEXT TOKENIZATION AND CLEANING -----------------------------------------
# (from Script 1)
# ============================================================================

tidy_text <- corpus_df %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "['’‘]")) %>%
  anti_join(all_stop_words, by = "word") %>%
  filter(!str_detect(word, "[0-9]")) %>%
  filter(nchar(word) > 2) %>%
  count(doc_id, word, sort = TRUE)   # without year here for original TF‑IDF

# For year‑separated work we create a separate version that keeps year
tidy_text_with_year <- corpus_df %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "['’‘]")) %>%
  anti_join(all_stop_words, by = "word") %>%
  filter(!str_detect(word, "[0-9]")) %>%
  filter(nchar(word) > 2) %>%
  count(doc_id, year, word, sort = TRUE)

cat("Tokenization complete\n")

# ============================================================================
# 5. ORIGINAL TF‑IDF (all documents, no year split) -------------------------
# (from Script 1)
# ============================================================================

tf_idf <- tidy_text %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf))

# Top 10 words per document
top_words <- tf_idf %>%
  group_by(doc_id) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

print(top_words)

# ============================================================================
# 6. WORD FREQUENCY, BIGRAMS, WORDCLOUD (from Script 1) ---------------------
# ============================================================================

overall_word_freq <- tidy_text %>%
  group_by(word) %>%
  summarize(total_freq = sum(n)) %>%
  arrange(desc(total_freq))

cat("ANALYSIS SUMMARY:\n")
cat("Total documents analyzed:", n_distinct(tidy_text$doc_id), "\n")
cat("Total unique words:", n_distinct(tidy_text$word), "\n")
cat("Total word occurrences:", sum(tidy_text$n), "\n")
cat("Average words per document:", round(sum(tidy_text$n) / n_distinct(tidy_text$doc_id), 0), "\n")

# By-year statistics
by_year_stats <- tidy_text_with_year %>%
  group_by(year) %>%
  summarise(
    docs = n_distinct(doc_id),
    unique_words = n_distinct(word),
    total_occurrences = sum(n),
    avg_words = round(sum(n) / n_distinct(doc_id), 0)
  )

print(by_year_stats)

# Top 10 words overall
top10_overall <- overall_word_freq %>%
  head(10) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Word = word, Frequency = total_freq)

print(top10_overall)



# Bigram analysis
bigram_analysis <- corpus_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% all_stop_words$word,
         !word2 %in% all_stop_words$word,
         !str_detect(word1, "[0-9']"),
         !str_detect(word2, "[0-9']"),
         nchar(word1) > 2,
         nchar(word2) > 2) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(doc_id, bigram, sort = TRUE)

bigram_tf_idf <- bigram_analysis %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))

top_bigrams <- bigram_tf_idf %>%
  group_by(doc_id) %>%
  slice_max(tf_idf, n = 8) %>%
  ungroup()



# ============================================================================
# 7. YEAR‑SEPARATED TF‑IDF AND WORD FREQUENCY CHANGE ------------------------
# (from modified script)
# ============================================================================

tf_idf_2023 <- tidy_text_with_year %>%
  filter(year == "23") %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf))

tf_idf_2024 <- tidy_text_with_year %>%
  filter(year == "24") %>%
  bind_tf_idf(word, doc_id, n) %>%
  arrange(desc(tf_idf))

cat("\n2023 TF-IDF documents:", n_distinct(tf_idf_2023$doc_id), "\n")
cat("2024 TF-IDF documents:", n_distinct(tf_idf_2024$doc_id), "\n")

word_change <- tidy_text_with_year %>%
  group_by(word, year) %>%
  summarise(freq = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = freq, values_fill = 0) %>%
  mutate(
    change = `24` - `23`,
    pct_change = ifelse(`23` > 0, (`24` - `23`) / `23` * 100, NA)
  ) %>%
  arrange(desc(abs(change)))

cat("\nTop 10 words that INCREASED in 2024:\n")
print(head(word_change %>% filter(change > 0) %>% select(word, change), 10))

cat("\nTop 10 words that DECREASED in 2024:\n")
print(head(word_change %>% filter(change < 0) %>% select(word, change), 10))

# ============================================================================
# 8. EXPORT RESULTS TO EXCEL (original + year‑separated) --------------------
# ============================================================================

excel_results <- list()
excel_results[["TF_IDF_Full"]] <- tf_idf %>% select(doc_id, word, n, tf, idf, tf_idf)
excel_results[["Top_Words_Per_Doc"]] <- top_words %>% select(doc_id, word, n, tf_idf)
excel_results[["Top_Bigrams_Per_Doc"]] <- top_bigrams %>% select(doc_id, bigram, n, tf_idf)
excel_results[["Overall_Word_Frequency"]] <- overall_word_freq
excel_results[["TF_IDF_2023"]] <- tf_idf_2023 %>% select(doc_id, word, n, tf, idf, tf_idf)
excel_results[["TF_IDF_2024"]] <- tf_idf_2024 %>% select(doc_id, word, n, tf, idf, tf_idf)
excel_results[["Word_Frequency_Change"]] <- word_change

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_filename <- paste0("Text_Analysis_Results_", timestamp, ".xlsx")
write.xlsx(excel_results, file = output_filename, creator = "NLP Analysis Pipeline")
cat("SUCCESS: All results exported to:", output_filename, "\n")

# ============================================================================
# 9. DOC2VEC + MOVEMENT VECTORS + CLUSTERING (from Script 2) ----------------
# ============================================================================

# Install/load required packages
if (!requireNamespace("doc2vec", quietly = TRUE)) install.packages("doc2vec")
if (!requireNamespace("umap", quietly = TRUE)) install.packages("umap")
if (!requireNamespace("proxy", quietly = TRUE)) install.packages("proxy")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")

library(doc2vec)
library(umap)
library(proxy)
library(factoextra)
library(cluster)
library(ggrepel)

cat("\nPreparing data for Doc2Vec...\n")

# Cleaning function for Doc2Vec
clean_for_doc2vec <- function(text) {
  if (!is.character(text)) return("")
  text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")
  if (is.na(text) || nchar(text) == 0) return("")
  text <- tolower(text)
  text <- gsub("[^[:alnum:][:space:]]", " ", text)
  text <- gsub("\\b\\d+\\b", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

corpus_clean <- corpus_df %>%
  mutate(
    text_clean = sapply(text, clean_for_doc2vec),
    text_chunks = map(text_clean, function(txt) {
      words <- unlist(strsplit(txt, "\\s+"))
      chunk_size <- 100
      n_chunks <- ceiling(length(words) / chunk_size)
      chunks <- character(n_chunks)
      for (i in 1:n_chunks) {
        start <- (i-1)*chunk_size + 1
        end <- min(i*chunk_size, length(words))
        chunks[i] <- paste(words[start:end], collapse = " ")
      }
      return(chunks)
    })
  )

cat("Documents cleaned and chunked.\n")

# Build chunk dataframe for doc2vec
# ============================================================================
# FIXED: DOC2VEC WITH CLEANED DOCUMENT IDs
# ============================================================================

# Clean document IDs (remove spaces)
corpus_clean <- corpus_clean %>%
  mutate(doc_id_clean = str_replace_all(doc_id, " ", "_"))

# Build chunk dataframe for doc2vec with cleaned IDs
doc2vec_df <- data.frame(doc_id = character(), text = character(), stringsAsFactors = FALSE)
for (i in 1:nrow(corpus_clean)) {
  doc_name <- corpus_clean$doc_id_clean[i]
  chunks <- corpus_clean$text_chunks[[i]]
  for (chunk in chunks) {
    doc2vec_df <- rbind(doc2vec_df, 
                        data.frame(doc_id = doc_name, 
                                   text = as.character(chunk),
                                   stringsAsFactors = FALSE))
  }
}

cat("Total chunks for Doc2Vec:", nrow(doc2vec_df), "\n")

# Train model
set.seed(123)
model_pvdm <- paragraph2vec(
  x = doc2vec_df,
  type = "PV-DM",
  dim = 100,
  iter = 15,
  min_count = 5,
  lr = 0.025,
  window = 10,
  hs = FALSE,
  negative = 5,
  sample = 0.001,
  threads = 4
)

# Extract vectors
doc_vectors <- as.matrix(model_pvdm, which = "docs")

# IMPORTANT: Map back to original doc_id for merging
vector_names <- rownames(doc_vectors)
original_names <- corpus_clean$doc_id[match(vector_names, corpus_clean$doc_id_clean)]
rownames(doc_vectors) <- original_names

# Remove any rows with NA
valid_rows <- !is.na(rowSums(doc_vectors))
doc_vectors <- doc_vectors[valid_rows, ]
cat("Valid document vectors:", nrow(doc_vectors), "out of", length(original_names), "\n")

# Similarity analysis
similarity_matrix <- proxy::simil(doc_vectors, method = "cosine")
sim_matrix <- as.matrix(similarity_matrix)
write.csv(sim_matrix, "document_similarity_matrix.csv")

# UMAP visualisation
set.seed(456)
umap_result <- umap(doc_vectors, n_components = 2, n_neighbors = 5)
viz_data <- data.frame(
  Document = rownames(doc_vectors),
  UMAP1 = umap_result$layout[, 1],
  UMAP2 = umap_result$layout[, 2]
)

viz_plot <- ggplot(viz_data, aes(x = UMAP1, y = UMAP2, label = Document)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_text_repel(size = 3, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "Document Embeddings Visualization (Doc2Vec)")
print(viz_plot)
ggsave("document_embeddings_plot.png", viz_plot, width = 12, height = 10, dpi = 300)

# Clustering on doc_vectors (static)
set.seed(789)
wss <- sapply(1:10, function(k) kmeans(doc_vectors, centers = k, nstart = 25)$tot.withinss)
n_clusters <- 4   # based on elbow
kmeans_result <- kmeans(doc_vectors, centers = n_clusters, nstart = 25)

viz_data$Cluster <- as.factor(kmeans_result$cluster)
cluster_plot <- ggplot(viz_data, aes(x = UMAP1, y = UMAP2, color = Cluster, label = Document)) +
  geom_point(size = 3) + geom_text_repel(size = 3) + theme_minimal()
ggsave("document_clusters_plot.png", cluster_plot, width = 12, height = 10, dpi = 300)

cluster_membership <- data.frame(Document = rownames(doc_vectors), Cluster = kmeans_result$cluster)
write.csv(cluster_membership, "cluster_membership.csv", row.names = FALSE)

# ============================================================================
# 10. MOVEMENT VECTORS CLUSTERING (from Script 2) --------------
# ============================================================================

vectors_df <- as.data.frame(doc_vectors) %>%
  rownames_to_column("doc_id") %>%
  separate(doc_id, into = c("company", "year"), sep = "_", remove = FALSE) %>%
  mutate(year = as.character(year))

unique_companies <- unique(vectors_df$company)
movement_list <- list()

for (comp in unique_companies) {
  vec_23 <- vectors_df %>% 
    filter(company == comp, year == "23") %>% 
    select(-company, -year, -doc_id) %>% 
    as.numeric()
  vec_24 <- vectors_df %>% 
    filter(company == comp, year == "24") %>% 
    select(-company, -year, -doc_id) %>% 
    as.numeric()
  
  if (length(vec_23) > 0 & length(vec_24) > 0 &&
      !any(is.na(c(vec_23, vec_24))) && !any(is.infinite(c(vec_23, vec_24)))) {
    movement_list[[comp]] <- vec_24 - vec_23
  }
}

movement_matrix <- do.call(rbind, movement_list)
rownames(movement_matrix) <- names(movement_list)
cat("Movement vectors computed for", nrow(movement_matrix), "companies\n")

# Elbow for movement clusters
wss_mov <- sapply(1:10, function(k) kmeans(movement_matrix, centers = k, nstart = 25)$tot.withinss)
elbow_df <- data.frame(k = 1:10, wss = wss_mov)
elbow_plot <- ggplot(elbow_df, aes(x = k, y = wss)) + geom_line() + geom_point() + theme_minimal()
print(elbow_plot)

k_clusters <- 6   # adjust based on elbow plot
clusters_mov <- kmeans(movement_matrix, centers = k_clusters, nstart = 25)
movement_df <- data.frame(company = rownames(movement_matrix), cluster = clusters_mov$cluster, stringsAsFactors = FALSE)

# UMAP of movement vectors
set.seed(789)
umap_mov <- umap(movement_matrix, n_components = 2, n_neighbors = 15)
viz_mov <- data.frame(
  company = rownames(movement_matrix),
  UMAP1 = umap_mov$layout[, 1],
  UMAP2 = umap_mov$layout[, 2],
  cluster = as.factor(movement_df$cluster)
)

cluster_mov_plot <- ggplot(viz_mov, aes(x = UMAP1, y = UMAP2, color = cluster, label = company)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(size = 2.5, max.overlaps = 20) +
  theme_minimal() +
  labs(title = "Company Clusters Based on Change in Sustainability Language (2024 - 2023)")
print(cluster_mov_plot)
ggsave("movement_clusters_umap.png", cluster_mov_plot, width = 14, height = 10, dpi = 300)

# ============================================================================
# 11. CLUSTER INTERPRETATION (What changed in each cluster)
# ============================================================================

# CORRECTED interpretation function (adds _23 and _24 suffix)
interpret_clusters_fixed <- function(movement_df, k_clusters, tf_idf_2023, tf_idf_2024) {
  
  cat("\n=== CLUSTER INTERPRETATION ===\n")
  
  for (cl in 1:k_clusters) {
    companies_in_cluster <- movement_df$company[movement_df$cluster == cl]
    
    # Create doc_id with year suffix
    companies_2023 <- paste0(companies_in_cluster, "_23")
    companies_2024 <- paste0(companies_in_cluster, "_24")
    
    # Top words in 2023 for this cluster
    words_2023 <- tf_idf_2023 %>%
      filter(doc_id %in% companies_2023) %>%
      group_by(word) %>%
      summarise(score = mean(tf_idf, na.rm = TRUE)) %>%
      arrange(desc(score)) %>%
      head(10)
    
    # Top words in 2024 for this cluster
    words_2024 <- tf_idf_2024 %>%
      filter(doc_id %in% companies_2024) %>%
      group_by(word) %>%
      summarise(score = mean(tf_idf, na.rm = TRUE)) %>%
      arrange(desc(score)) %>%
      head(10)
    
    cat("\n========================================\n")
    cat("CLUSTER", cl, "(", length(companies_in_cluster), "companies)\n")
    cat("Example companies:", paste(head(companies_in_cluster, 4), collapse = ", "))
    if(length(companies_in_cluster) > 4) cat(", ...")
    cat("\n----------------------------------------\n")
    
    if(nrow(words_2023) > 0) {
      cat("📌 2023 FOCUS:\n   ", paste(words_2023$word[1:min(6, nrow(words_2023))], collapse = ", "), "\n")
    } else {
      cat("📌 2023 FOCUS: (no matching documents)\n")
    }
    
    if(nrow(words_2024) > 0) {
      cat("\n📌 2024 FOCUS:\n   ", paste(words_2024$word[1:min(6, nrow(words_2024))], collapse = ", "), "\n")
    } else {
      cat("\n📌 2024 FOCUS: (no matching documents)\n")
    }
    
    # Calculate shifts
    all_words <- unique(c(words_2023$word, words_2024$word))
    if(length(all_words) > 0) {
      change_df <- data.frame(word = all_words) %>%
        left_join(words_2023, by = "word") %>%
        left_join(words_2024, by = "word") %>%
        mutate(
          score_2023 = coalesce(score.x, 0),
          score_2024 = coalesce(score.y, 0),
          change = score_2024 - score_2023
        ) %>%
        arrange(desc(abs(change)))
      
      increased <- head(change_df %>% filter(change > 0) %>% pull(word), 4)
      decreased <- head(change_df %>% filter(change < 0) %>% pull(word), 4)
      
      if(length(increased) > 0) {
        cat("\n📈 INCREASED MOST:", paste(increased, collapse = ", "), "\n")
      }
      if(length(decreased) > 0) {
        cat("📉 DECREASED MOST:", paste(decreased, collapse = ", "), "\n")
      }
    }
  }
}

# Run the interpretation
cat("\n\n========== RUNNING CLUSTER INTERPRETATION ==========\n")
interpret_clusters_fixed(movement_df, k_clusters, tf_idf_2023, tf_idf_2024)

# Save interpretation results to a text file
sink("cluster_interpretation_results.txt")
interpret_clusters_fixed(movement_df, k_clusters, tf_idf_2023, tf_idf_2024)
sink()

cat("\n✅ Interpretation saved to 'cluster_interpretation_results.txt'\n")

















# ============================================================================
# RQ1: COMMUNICATION vs ACCOUNTABILITY LEXICON ANALYSIS
# ============================================================================

# 1.1 Define lexicons
communication_lexicon <- c(
  "believe", "think", "aspire", "aim", "strive", "hope", "vision", "mission",
  "commit", "pledge", "promise", "initiative", "engage", "collaborate",
  "partner", "stakeholder", "dialogue", "trust", "reputation", "brand"
)

accountability_lexicon <- c(
  "target", "objective", "metric", "indicator", "performance", "progress",
  "achievement", "result", "outcome", "impact", "measure", "verify",
  "audit", "assurance", "compliance", "obligation", "responsibility",
  "governance", "oversight", "transparent", "disclose"
)

# 1.2 Create term frequency by year for each lexicon
lexicon_analysis <- tidy_text_with_year %>%
  mutate(
    category = case_when(
      word %in% communication_lexicon ~ "communication",
      word %in% accountability_lexicon ~ "accountability",
      TRUE ~ "other"
    )
  ) %>%
  filter(category != "other") %>%
  group_by(year, category) %>%
  summarise(total_freq = sum(n), .groups = "drop") %>%
  group_by(year) %>%
  mutate(proportion = total_freq / sum(total_freq))

# 1.3 Visualize shift
ggplot(lexicon_analysis, aes(x = year, y = proportion, fill = category)) +
  geom_col(position = "dodge") +
  labs(
    title = "Shift from Communication to Accountability Language (2023 vs 2024)",
    subtitle = "CSRD's Impact on Sustainability Reporting Discourse",
    y = "Proportion of Total Terms",
    x = "Year",
    fill = "Lexicon"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("accountability" = "#2E86AB", "communication" = "#A23B72"))

# 1.4 Statistical test (chi-square) - CORRECTED
# Create contingency table
lexicon_matrix <- lexicon_analysis %>%
  ungroup() %>%  # <-- KEY FIX: Remove grouping before pivoting
  select(year, category, total_freq) %>%
  pivot_wider(
    names_from = category,
    values_from = total_freq,
    values_fill = 0
  ) %>%
  as.data.frame() %>%
  column_to_rownames("year")

# View the matrix
cat("\nContingency Table:\n")
print(lexicon_matrix)

# Run chi-square test
chisq_test <- chisq.test(lexicon_matrix)

# Display results
cat("\n========== CHI-SQUARE TEST RESULTS ==========\n")
cat("Chi-square statistic:", round(chisq_test$statistic, 1), "\n")
cat("Degrees of freedom:", chisq_test$parameter, "\n")
cat("p-value:", ifelse(chisq_test$p.value < 0.001, "< 0.001", round(chisq_test$p.value, 4)), "\n")
cat("============================================\n")

# 1.5 Paired t-test (document-level)
# Calculate ratio per document
doc_lexicon_ratio <- tidy_text_with_year %>%
  mutate(
    category = case_when(
      word %in% communication_lexicon ~ "communication",
      word %in% accountability_lexicon ~ "accountability",
      TRUE ~ "other"
    )
  ) %>%
  filter(category != "other") %>%
  group_by(doc_id, year, category) %>%
  summarise(freq = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = freq, values_fill = 0) %>%
  mutate(
    total = communication + accountability,
    comm_ratio = communication / total,
    acc_ratio = accountability / total,
    acc_minus_comm = acc_ratio - comm_ratio
  )

# Extract company name and year from doc_id
doc_lexicon_ratio <- doc_lexicon_ratio %>%
  mutate(
    company = str_extract(doc_id, "^[^_]+"),
    year = as.character(year)
  )

# Pair 2023 and 2024 for each company
company_lexicon_shift <- doc_lexicon_ratio %>%
  select(company, year, acc_minus_comm) %>%
  pivot_wider(
    names_from = year,
    values_from = acc_minus_comm,
    names_prefix = "acc_minus_comm_"
  ) %>%
  mutate(
    shift = acc_minus_comm_24 - acc_minus_comm_23,
    shifted_up = shift > 0,
    shifted_down = shift < 0
  ) %>%
  arrange(desc(abs(shift)))

# Paired t-test
paired_test <- t.test(
  company_lexicon_shift$acc_minus_comm_23,
  company_lexicon_shift$acc_minus_comm_24,
  paired = TRUE
)

cat("\n========== PAIRED T-TEST RESULTS ==========\n")
cat("Mean 2023 ratio:", round(mean(company_lexicon_shift$acc_minus_comm_23, na.rm = TRUE), 4), "\n")
cat("Mean 2024 ratio:", round(mean(company_lexicon_shift$acc_minus_comm_24, na.rm = TRUE), 4), "\n")
cat("Mean shift:", round(mean(company_lexicon_shift$shift, na.rm = TRUE), 4), "\n")
cat("SD shift:", round(sd(company_lexicon_shift$shift, na.rm = TRUE), 4), "\n")
cat("t-statistic:", round(paired_test$statistic, 4), "\n")
cat("p-value:", round(paired_test$p.value, 4), "\n")
cat("95% CI:", round(paired_test$conf.int[1], 4), "-", round(paired_test$conf.int[2], 4), "\n")
cat("============================================\n")











# ============================================================================
# RQ2: SEMANTIC DIFFERENCE ANALYSIS (2023 vs 2024)
# ============================================================================

# 2.1 Average document vectors by year
year_centroids <- vectors_df %>%
  group_by(year) %>%
  summarise(across(starts_with("V"), mean, na.rm = TRUE)) %>%
  pivot_longer(-year, names_to = "dimension", values_to = "value") %>%
  pivot_wider(names_from = year, values_from = value)

# 2.2 Calculate semantic distance between year centroids
# Extract vectors as matrix
vec_2023 <- vectors_df %>%
  filter(year == "23") %>%
  select(starts_with("V")) %>%
  as.matrix()

vec_2024 <- vectors_df %>%
  filter(year == "24") %>%
  select(starts_with("V")) %>%
  as.matrix()

# 2.3 Cosine similarity between 2023 and 2024 centroids
centroid_23 <- colMeans(vec_2023)
centroid_24 <- colMeans(vec_2024)
cosine_sim <- sum(centroid_23 * centroid_24) / 
  (sqrt(sum(centroid_23^2)) * sqrt(sum(centroid_24^2)))
cat("\nCosine similarity between 2023 and 2024 centroids:", cosine_sim, "\n")
cat("Semantic distance (1 - similarity):", 1 - cosine_sim, "\n")

# 2.4 Within-year vs between-year similarity (to see if 2024 is more homogeneous)
# Within-year similarities
sim_23 <- mean(proxy::simil(vec_2023, method = "cosine")[lower.tri(matrix(NA, nrow(vec_2023), nrow(vec_2023)))])
sim_24 <- mean(proxy::simil(vec_2024, method = "cosine")[lower.tri(matrix(NA, nrow(vec_2024), nrow(vec_2024)))])

# Between-year similarity (random pairs)
set.seed(123)
n_pairs <- min(nrow(vec_2023), nrow(vec_2024)) * 10
idx_23 <- sample(1:nrow(vec_2023), n_pairs, replace = TRUE)
idx_24 <- sample(1:nrow(vec_2024), n_pairs, replace = TRUE)
between_sim <- mean(sapply(1:n_pairs, function(i) {
  cosine_similarity <- sum(vec_2023[idx_23[i],] * vec_2024[idx_24[i],]) /
    (sqrt(sum(vec_2023[idx_23[i],]^2)) * sqrt(sum(vec_2024[idx_24[i],]^2)))
  return(cosine_similarity)
}))

# 2.5 Visualize semantic convergence/difference
similarity_df <- data.frame(
  Comparison = c("Within 2023", "Within 2024", "Between Years"),
  Similarity = c(sim_23, sim_24, between_sim)
)

ggplot(similarity_df, aes(x = Comparison, y = Similarity, fill = Comparison)) +
  geom_col() +
  geom_text(aes(label = round(Similarity, 3)), vjust = -0.5) +
  labs(
    title = "Semantic Similarity: Within-Year vs Between-Year",
    subtitle = "Higher within-year similarity suggests reporting convergence",
    y = "Mean Cosine Similarity"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#2E86AB", "#A23B72", "#F18F01"))

# 2.6 Permutation test for significance
permute_test <- function(vec1, vec2, n_perm = 1000) {
  all_vecs <- rbind(vec1, vec2)
  group_labels <- c(rep("A", nrow(vec1)), rep("B", nrow(vec2)))
  
  observed_dist <- mean(proxy::simil(vec1, method = "cosine")[lower.tri(matrix(NA, nrow(vec1), nrow(vec1)))]) -
    mean(proxy::simil(vec2, method = "cosine")[lower.tri(matrix(NA, nrow(vec2), nrow(vec2)))])
  
  perm_dists <- replicate(n_perm, {
    perm_labels <- sample(group_labels)
    perm_vec1 <- all_vecs[perm_labels == "A", ]
    perm_vec2 <- all_vecs[perm_labels == "B", ]
    mean(proxy::simil(perm_vec1, method = "cosine")[lower.tri(matrix(NA, nrow(perm_vec1), nrow(perm_vec1)))]) -
      mean(proxy::simil(perm_vec2, method = "cosine")[lower.tri(matrix(NA, nrow(perm_vec2), nrow(perm_vec2)))])
  })
  
  p_value <- mean(abs(perm_dists) >= abs(observed_dist))
  return(list(observed = observed_dist, p_value = p_value))
}

perm_result <- permute_test(vec_2023, vec_2024)
cat("\nPermutation test for semantic shift:\n")
cat("Observed difference:", perm_result$observed, "\n")
cat("P-value:", perm_result$p_value, "\n")















# ============================================================================
# RQ3: TOPIC EMERGENCE, DISAPPEARANCE, AND GAINING RELEVANCE
# ============================================================================

# 3.1 Define topic lexicons (based on ESRS/CSRD themes)
topic_lexicons <- list(
  climate = c("climate", "emission", "carbon", "ghg", "temperature", "netzero", 
              "renewable", "energy", "decarbonization", "mitigation"),
  
  biodiversity = c("biodiversity", "ecosystem", "habitat", "species", "nature", 
                   "conservation", "pollution", "deforestation", "landuse"),
  
  social = c("employee", "worker", "labor", "diversity", "gender", "humanrights",
             "community", "stakeholder", "social", "inclusion", "health", "safety"),
  
  governance = c("governance", "board", "compensation", "oversight", "riskmanagement",
                 "internalcontrol", "audit", "transparency", "ethics", "anticorruption"),
  
  circular_economy = c("circular", "recycle", "waste", "resource", "efficiency",
                       "reuse", "material", "supplychain", "lifecycle", "ecodesign"),
  
  disclosure_compliance = c("esrs", "csrd", "efrag", "sasb", "gri", "issb", 
                            "taxonomy", "doble", "materiality", "doublemateriality",
                            "compliance", "disclosure", "reporting")
)

# 3.2 Calculate topic frequency by year
topic_analysis <- tidy_text_with_year %>%
  mutate(
    topic = case_when(
      word %in% topic_lexicons$climate ~ "climate",
      word %in% topic_lexicons$biodiversity ~ "biodiversity",
      word %in% topic_lexicons$social ~ "social",
      word %in% topic_lexicons$governance ~ "governance",
      word %in% topic_lexicons$circular_economy ~ "circular_economy",
      word %in% topic_lexicons$disclosure_compliance ~ "disclosure_compliance",
      TRUE ~ "other"
    )
  ) %>%
  filter(topic != "other") %>%
  group_by(year, topic) %>%
  summarise(freq = sum(n), .groups = "drop") %>%
  group_by(year) %>%
  mutate(proportion = freq / sum(freq))

# 3.3 Visualize topic shifts
ggplot(topic_analysis, aes(x = year, y = proportion, fill = topic)) +
  geom_col(position = "dodge") +
  labs(
    title = "Sustainability Topic Prevalence: 2023 vs 2024",
    subtitle = "How CSRD has reshaped reporting priorities",
    y = "Proportion of Total Terms",
    x = "Year",
    fill = "Topic"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")

# 3.4 Identify emerging topics (words that appear in 2024 but not 2023)
words_2023 <- tidy_text_with_year %>% filter(year == "23") %>% pull(word) %>% unique()
words_2024 <- tidy_text_with_year %>% filter(year == "24") %>% pull(word) %>% unique()

emerging_topics <- setdiff(words_2024, words_2023)
cat("\nNumber of emerging topics (new words in 2024):", length(emerging_topics), "\n")
cat("Sample emerging topics:", paste(head(emerging_topics, 20), collapse = ", "), "\n")

disappearing_topics <- setdiff(words_2023, words_2024)
cat("\nNumber of disappearing topics (words lost in 2024):", length(disappearing_topics), "\n")
cat("Sample disappearing topics:", paste(head(disappearing_topics, 20), collapse = ", "), "\n")

# 3.5 Top gaining and losing terms (already in your code but can be enhanced)
top_gainers <- word_change %>%
  filter(change > 0) %>%
  arrange(desc(change)) %>%
  head(20)

top_losers <- word_change %>%
  filter(change < 0) %>%
  arrange(change) %>%
  head(20)

cat("\n========== TOP GAINING TERMS (2024 vs 2023) ==========\n")
print(top_gainers %>% select(word, change, pct_change))

cat("\n========== TOP LOSING TERMS (2024 vs 2023) ==========\n")
print(top_losers %>% select(word, change, pct_change))

# 3.6 Focus on CSRD-related terms specifically
csrd_terms <- c("esrs", "csrd", "efrag", "doble", "doublemateriality", "materiality", 
                "iasb", "issb", "sasb", "gri", "disclosure", "compliance")

csrd_analysis <- word_change %>%
  filter(word %in% csrd_terms) %>%
  arrange(desc(abs(change)))

cat("\n========== CSRD-RELATED TERM CHANGES ==========\n")
print(csrd_analysis %>% select(word, `23`, `24`, change, pct_change))

# 3.7 Document-level topic diversity (breadth of reporting)
topic_diversity <- tidy_text_with_year %>%
  mutate(
    topic = case_when(
      word %in% topic_lexicons$climate ~ "climate",
      word %in% topic_lexicons$biodiversity ~ "biodiversity",
      word %in% topic_lexicons$social ~ "social",
      word %in% topic_lexicons$governance ~ "governance",
      word %in% topic_lexicons$circular_economy ~ "circular_economy",
      word %in% topic_lexicons$disclosure_compliance ~ "disclosure_compliance",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(topic)) %>%
  group_by(doc_id, year) %>%
  summarise(n_topics = n_distinct(topic), .groups = "drop") %>%
  group_by(year) %>%
  summarise(
    mean_topics = mean(n_topics),
    sd_topics = sd(n_topics),
    .groups = "drop"
  )

cat("\n========== TOPIC DIVERSITY BY YEAR ==========\n")
print(topic_diversity)

# 3.8 Export all results for reporting
rq3_results <- list(
  Topic_Analysis = topic_analysis,
  Emerging_Topics = data.frame(word = emerging_topics),
  Disappearing_Topics = data.frame(word = disappearing_topics),
  Top_Gainers = top_gainers,
  Top_Losers = top_losers,
  CSRD_Terms = csrd_analysis,
  Topic_Diversity = topic_diversity
)

write.xlsx(rq3_results, file = "RQ3_Topic_Analysis_Results.xlsx")











##############CHAPTER 4 ANALYSIS ############

cat("ANALYSIS SUMMARY:\n")
cat("Total documents analyzed:", n_distinct(tidy_text$doc_id), "\n")
cat("Total unique words:", n_distinct(tidy_text$word), "\n")
cat("Total word occurrences:", sum(tidy_text$n), "\n")
cat("Average words per document:", round(sum(tidy_text$n) / n_distinct(tidy_text$doc_id), 0), "\n")

# By-year statistics
by_year_stats <- tidy_text_with_year %>%
  group_by(year) %>%
  summarise(
    docs = n_distinct(doc_id),
    unique_words = n_distinct(word),
    total_occurrences = sum(n),
    avg_words = round(sum(n) / n_distinct(doc_id), 0)
  )

print(by_year_stats)

# Top 10 words overall
top10_overall <- overall_word_freq %>%
  head(10) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Word = word, Frequency = total_freq)

print(top10_overall)




# Get your actual numbers for lexicons
lexicon_counts <- tidy_text_with_year %>%
  mutate(
    category = case_when(
      word %in% communication_lexicon ~ "communication",
      word %in% accountability_lexicon ~ "accountability",
      TRUE ~ "other"
    )
  ) %>%
  filter(category != "other") %>%
  group_by(year, category) %>%
  summarise(total_freq = sum(n), .groups = "drop")

print(lexicon_counts)








# ============================================================================
# TABLE 4.6: COMPANY-LEVEL SHIFT DISTRIBUTION
# ============================================================================

# Count companies in each category
shift_counts <- data.frame(
  Category = c(
    "Toward Accountability (> 0.05)",
    "Minimal Change (|shift| ≤ 0.05)",
    "Toward Communication (< -0.05)"
  ),
  Count = c(
    sum(shift_summary$shift > 0.05, na.rm = TRUE),
    sum(abs(shift_summary$shift) <= 0.05, na.rm = TRUE),
    sum(shift_summary$shift < -0.05, na.rm = TRUE)
  )
)

# Calculate percentages
shift_counts <- shift_counts %>%
  mutate(
    Percentage = round(Count / sum(Count) * 100, 1),
    Display = paste0(Count, " (", Percentage, "%)")
  )

# Display the table
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("         TABLE 4.6: COMPANY-LEVEL SHIFT DISTRIBUTION\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
print(shift_counts %>% select(Category, Count, Percentage))
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("Total companies:", sum(shift_counts$Count), "\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")




# ============================================================================
# TABLE 4.7: TOP 10 COMPANIES MOVING TOWARD ACCOUNTABILITY
# ============================================================================

top10_accountability <- company_lexicon_shift %>%
  filter(shift > 0) %>%
  arrange(desc(shift)) %>%
  head(10) %>%
  mutate(
    acc_minus_comm_23 = round(acc_minus_comm_23, 3),
    acc_minus_comm_24 = round(acc_minus_comm_24, 3),
    shift = round(shift, 3)
  ) %>%
  select(
    Company = company,
    `2023 Ratio` = acc_minus_comm_23,
    `2024 Ratio` = acc_minus_comm_24,
    Shift = shift
  )

print(top10_accountability)

cat("TOP 10 COMPANIES SHIFTING TOWARD ACCOUNTABILITY (2023 -> 2024):\n")
print(head(company_lexicon_shift %>% 
             filter(shift > 0) %>% 
             select(company, acc_minus_comm_23, acc_minus_comm_24, shift) %>%
             arrange(desc(shift)), 10))



cat("\nTOP 10 COMPANIES SHIFTING TOWARD COMMUNICATION (2023 -> 2024):\n")
print(head(company_lexicon_shift %>% 
             filter(shift < 0) %>% 
             select(company, acc_minus_comm_23, acc_minus_comm_24, shift) %>%
             arrange(shift), 10))




# Cosine similarity between 2023 and 2024 centroids
cosine_sim <- sum(centroid_23 * centroid_24) / 
  (sqrt(sum(centroid_23^2)) * sqrt(sum(centroid_24^2)))
cat("\nCosine similarity between 2023 and 2024 centroids:", cosine_sim, "\n")
# Output: 0.9389399

# Within-year similarities
sim_23 <- mean(proxy::simil(vec_2023, method = "cosine")[lower.tri(matrix(NA, nrow(vec_2023), nrow(vec_2023)))])
sim_24 <- mean(proxy::simil(vec_2024, method = "cosine")[lower.tri(matrix(NA, nrow(vec_2024), nrow(vec_2024)))])
# Output: sim_23 ≈ 0.199, sim_24 ≈ 0.200

# Between-year similarity
between_sim <- mean(sapply(1:n_pairs, function(i) {
  cosine_similarity <- sum(vec_2023[idx_23[i],] * vec_2024[idx_24[i],]) /
    (sqrt(sum(vec_2023[idx_23[i],]^2)) * sqrt(sum(vec_2024[idx_24[i],]^2)))
  return(cosine_similarity)
}))
# Output: between_sim ≈ 0.191








# ============================================================================
# TABLE 4.11: TOPIC-RELATED WORD FREQUENCY CHANGES (2023 vs 2024)
# ============================================================================

# Define topic categories based on your word change data
topic_keywords <- list(
  Climate = c("climate", "emissions", "carbon", "ghg", "renewable", "netzero", "decarbonization"),
  Biodiversity = c("biodiversity", "nature", "ecosystem", "conservation", "habitat"),
  Social = c("employees", "diversity", "inclusion", "humanrights", "workers", "safety", "health"),
  Governance = c("governance", "board", "ethics", "compliance", "transparency", "oversight"),
  CircularEconomy = c("circular", "waste", "recycle", "resource", "efficiency", "sustainable"),
  Disclosure = c("disclose", "reporting", "esrs", "assurance", "audit", "taxonomy", "metrics")
)

# Calculate topic frequencies by year
topic_freq <- tidy_text_with_year %>%
  mutate(
    topic = case_when(
      word %in% topic_keywords$Climate ~ "Climate",
      word %in% topic_keywords$Biodiversity ~ "Biodiversity",
      word %in% topic_keywords$Social ~ "Social",
      word %in% topic_keywords$Governance ~ "Governance",
      word %in% topic_keywords$CircularEconomy ~ "Circular Economy",
      word %in% topic_keywords$Disclosure ~ "Disclosure/Compliance",
      TRUE ~ "Other"
    )
  ) %>%
  filter(topic != "Other") %>%
  group_by(year, topic) %>%
  summarise(freq = sum(n), .groups = "drop") %>%
  group_by(year) %>%
  mutate(proportion = freq / sum(freq) * 100)

# Pivot to wide format
topic_table <- topic_freq %>%
  select(year, topic, proportion) %>%
  pivot_wider(
    names_from = year,
    values_from = proportion,
    names_prefix = "year_"
  ) %>%
  mutate(
    Change = round(year_24 - year_23, 1),
    `2023` = round(year_23, 1),
    `2024` = round(year_24, 1)
  ) %>%
  select(Topic = topic, `2023`, `2024`, Change) %>%
  arrange(desc(abs(Change)))

print(topic_table)







# Create a bar chart showing topic changes
ggplot(topic_table, aes(x = reorder(Topic, Change), y = Change, fill = Change > 0)) +
  geom_col() +
  geom_text(aes(label = paste0(ifelse(Change > 0, "+", ""), round(Change, 1), "%")), 
            hjust = ifelse(topic_table$Change > 0, -0.2, 1.2), size = 4) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  coord_flip() +
  labs(
    title = "Topic Proportion Changes (2024 vs 2023)",
    x = "Topic",
    y = "Percentage Point Change",
    fill = "Direction"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "#2E86AB", "FALSE" = "#A23B72")) +
  theme(legend.position = "bottom")



# ============================================================================
# TABLE 4.13: TOP GAINING TERMS (2024 vs 2023)
# ============================================================================

top_gaining <- word_change %>%
  filter(`23` > 0 & `24` > `23`) %>%
  mutate(
    increase = `24` - `23`,
    pct_change = round(((`24` - `23`) / `23`) * 100, 1),
    Topic = case_when(
      word %in% c("esrs", "related", "impacts", "information", "risks", "risk",
                  "disclosure", "compliance", "assurance", "audit", "taxonomy",
                  "materiality", "standards", "reporting") ~ "Disclosure/Compliance",
      word %in% c("sustainability", "value", "opportunities", "performance",
                  "strategy", "goals", "targets") ~ "General",
      word %in% c("chain", "supplychain", "valuechain", "stakeholder",
                  "engagement", "dialogue") ~ "Social/Governance",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(desc(increase)) %>%
  head(10) %>%
  select(
    Word = word,
    Change = increase,
    `% Change` = pct_change,
    Topic
  )

print(top_gaining)




ggplot(top_gaining, aes(x = reorder(Word, Change), y = Change, fill = Topic)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Gaining Terms (2024 vs 2023)",
    x = "Word",
    y = "Absolute Frequency Change",
    fill = "Topic Category"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Disclosure/Compliance" = "#2E86AB",
    "General" = "#F18F01",
    "Social/Governance" = "#A23B72"
  ))



# ============================================================================
# TABLE 4.14: TOP LOSING TERMS (2024 vs 2023)
# ============================================================================

top_losing <- word_change %>%
  filter(`23` > 0 & `24` < `23`) %>%   # Words that decreased
  mutate(
    decrease = `24` - `23`,             # Negative change
    pct_change = round(((`24` - `23`) / `23`) * 100, 1),
    Topic = case_when(
      word %in% c("gri", "nfs", "sasb", "g4") ~ "Disclosure/Compliance",
      word %in% c("people", "women", "men", "employees", "workers", "diversity") ~ "Social",
      word %in% c("new", "topics", "innovation", "index", "capitale") ~ "General",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(decrease) %>%                  # Most negative first
  head(10) %>%
  select(
    Word = word,
    Change = decrease,
    `% Change` = pct_change,
    Topic
  )

print(top_losing)





# ============================================================================
# TABLE 4.15: CSRD-RELATED TERM CHANGES
# ============================================================================

csrd_terms <- c("esrs", "csrd", "doublemateriality", "materiality", 
                "disclosure", "compliance")

csrd_table <- word_change %>%
  filter(word %in% csrd_terms) %>%
  mutate(
    `2023` = `23`,
    `2024` = `24`,
    Change = `24` - `23`,
    `% Change` = ifelse(`23` == 0, "—", paste0(round(((`24` - `23`) / `23`) * 100, 1), "%"))
  ) %>%
  select(Term = word, `2023`, `2024`, Change, `% Change`) %>%
  arrange(desc(Change))

print(csrd_table)












# ============================================================================
# ALTERNATIVE: Calculate Similarity of Change Vectors
# ============================================================================

# 1. Calculate movement vectors for each company
movement_vectors <- list()

for (comp in unique(vectors_df$company)) {
  vec_23 <- vectors_df %>%
    filter(company == comp, year == "23") %>%
    select(starts_with("V")) %>%
    as.numeric()
  
  vec_24 <- vectors_df %>%
    filter(company == comp, year == "24") %>%
    select(starts_with("V")) %>%
    as.numeric()
  
  if (length(vec_23) > 0 && length(vec_24) > 0) {
    movement_vectors[[comp]] <- vec_24 - vec_23
  }
}

movement_matrix <- do.call(rbind, movement_vectors)

# 2. Calculate similarity of movement vectors
movement_sim <- proxy::simil(movement_matrix, method = "cosine")
avg_movement_sim <- mean(movement_sim[lower.tri(movement_sim)])

cat("Average similarity of movement vectors:", round(avg_movement_sim, 4), "\n")

