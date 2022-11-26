# Start-up ----
# pkgs 
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))

# params
param.k_select = 10

# Make Obsidian Folders ----
# make directories
if(dir.exists("./obsidian-vault/")) unlink("./obsidian-vault/", recursive = TRUE) 
dir.create("./obsidian-vault/")

# make obsidian folders
if(!dir.exists("./obsidian-vault/topics/")) dir.create("./obsidian-vault/topics/") # topics dir
if(!dir.exists("./obsidian-vault/frex-stems/")) dir.create("./obsidian-vault/frex-stems/") # frex-stems dir
if(!dir.exists("./obsidian-vault/lift-stems/")) dir.create("./obsidian-vault/lift-stems/") # frex-stems dir
if(!dir.exists("./obsidian-vault/transcripts/")) dir.create("./obsidian-vault/transcripts/") # transcripts dir
if(!dir.exists("./obsidian-vault/snippets/")) dir.create("./obsidian-vault/snippets/") # snippets dir

# load data 
load(file = "./prepd-data/topic-model-data.RData")

# select stm model
assertthat::assert_that(all(param.k_select %in% many_models$K))
assertthat::assert_that(length(param.k_select) == 1L)
topic_model = many_models$topic_model[[which(many_models$K == param.k_select)]]

# Create Topic Folder Contents ----
topic_model.summary = summary(topic_model)
frex_words = as_tibble(topic_model.summary$frex, .name_repair = "unique") %>% 
  mutate(K = row_number()) %>% 
  pivot_longer(data = ., cols = -K, names_to = "dummy", values_to = "frex_word") %>% 
  select(-dummy)

topic_numbers = sort(unique(frex_words$K))
topic_labels = paste0("topic_", str_pad(string = topic_numbers, width = 2, side = "left", pad = "0"))
for(i in seq_along(topic_numbers)){
  .n = topic_numbers[i]
  .lab = topic_labels[i] 
  
  .frex_words = filter(.data = frex_words, K == .n) %$% frex_word
  .frex_words = paste0("[[", .frex_words, "]]")
  .frex_words = c(.lab, # document title
                  paste0("#", .lab), # document tag
                  .frex_words # frex-words
                  )

  write_lines(x = .frex_words, 
              file = paste0("./obsidian-vault/topics/",
                            .lab, ".md"))
  
}

# Make Main Topics Map ----
theta = as_tibble(topic_model$theta)
names(theta) = paste0("topic_",str_pad(seq(ncol(theta)), width = 2, side = "left", pad = "0"))
theta$document = rownames(sparse_data)
theta = select(.data = theta, document, everything())

main_topic = theta %>% 
  pivot_longer(data = ., cols = contains("topic"), 
               names_to = "topic_id", values_to = "theta") %>% 
  group_by(document) %>% 
  slice_max(.data = ., order_by = theta, n =  1) %>% 
  left_join(x = .,
            y = select(.data = titles, ref, title),
            by = c("document" = "ref")) %>% 
  mutate(topic_id = factor(topic_id))

# Create Frex Stems Folder Contents ----
stem_map = data %>% 
  left_join(x = ., 
            y = select(main_topic, document, topic_id), 
            by = c("ref" = "document")
            ) %>% 
  mutate(temp = as.numeric(str_extract(topic_id, "\\d+$")),
         temp = paste0(temp,token) %in% paste0(frex_words$K, frex_words$frex_word)) %>% 
  filter(temp) %>% 
  select(-temp)

pb = progress::progress_bar$new(total = length(topic_labels))
topic_buds = vector(mode = "list", length = 0L)
for(i in seq_along(topic_labels)){
  .stems = stem_map %>% 
    filter(topic_id == topic_labels[i]) %>% 
    select(topic_id, token, word) %>% 
    unique() 
  
  .lab = as.character(unique(.stems$topic_id))
  .tokens = unique(.stems$token)
  
  for(j in seq_along(.tokens)){
    .token = .tokens[j]
    .topic_buds = .stems %>% 
      filter(token == .token) %$%
      word %>% 
      str_remove_all(., "[\\.\\?\\!,]+$") %>% 
      unique()
    
    .topic_buds = c(.topic_buds, tolower(.topic_buds), stringr::str_to_title(.topic_buds), toupper(.topic_buds))
    .topic_buds = unique(.topic_buds)
    
    .topic_buds_list = list(.topic_buds)
    names(.topic_buds_list) = .token
    topic_buds = c(topic_buds, .topic_buds_list)
    
    .yaml_aliases = paste0(.topic_buds, collapse = ", ") %>%
      paste0("aliases: [", ., "]") %>%
      c("---",. , "---")
    
    .text_out = c(.yaml_aliases, # document yaml
                  paste0("#", .lab) # topic tag
                  )
    
    write_lines(x = .text_out, 
                file = paste0("./obsidian-vault/frex-stems/", .token, ".md")
                )
    
  }
  
  pb$tick()
}

# Create Transcripts Folder Contents: Punctuated Documents ----
documents_ids = filter(.data = punct_summary, .punctd)
for(j in seq_along(documents_ids)){
  
  
  
  
}

# Create Transcripts Folder Contents: Non-Punctuated Documents ----
documents_ids = filter(.data = punct_summary, !.punctd)

# Create Snippets Folder Contents ----





