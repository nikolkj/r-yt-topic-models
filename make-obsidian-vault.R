# Start-up ----
# pkgs 
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(stm)) # required for `summary(topic_model)`

# params
param.k_select = 10

# load data 
load(file = "./prepd-data/topic-model-data.RData")

# select stm model
assertthat::assert_that(all(param.k_select %in% many_models$K))
assertthat::assert_that(length(param.k_select) == 1L)
topic_model = many_models$topic_model[[which(many_models$K == param.k_select)]]

# Make Obsidian Folders ----
# make directories
if(dir.exists("./obsidian-vault/")) unlink("./obsidian-vault/", recursive = TRUE) 
dir.create("./obsidian-vault/")

# make main obsidian folders
if(!dir.exists("./obsidian-vault/topics/")) dir.create("./obsidian-vault/topics/") # topics dir
# if(!dir.exists("./obsidian-vault/frex-stems/")) dir.create("./obsidian-vault/frex-stems/") # frex-stems dir dynamic
# if(!dir.exists("./obsidian-vault/lift-stems/")) dir.create("./obsidian-vault/lift-stems/") # frex-stems dir dynamic
if(!dir.exists("./obsidian-vault/transcripts/")) dir.create("./obsidian-vault/transcripts/") # transcripts dir
if(!dir.exists("./obsidian-vault/snippets/")) dir.create("./obsidian-vault/snippets/") # snippets dir

# Map Document Transcript Topics ----
theta = as_tibble(topic_model$theta)
names(theta) = paste0("topic_",str_pad(seq(ncol(theta)), width = 2, side = "left", pad = "0"))
theta$ref = rownames(sparse_data)
theta = select(.data = theta, ref, everything())

transcript_topics = theta %>% 
  pivot_longer(data = ., cols = contains("topic"), 
               names_to = "topic_id", values_to = "theta") %>% 
  group_by(ref) %>% 
  slice_max(.data = ., order_by = theta, n =  1) %>% 
  left_join(x = .,
            y = select(.data = titles, ref, title),
            by = "ref")

topic_ids = sort(unique(transcript_topics$topic_id))
assertthat::assert_that(any(!grepl('[*"\\/<>:|]', topic_ids))) # obsidian doc title char restricts

# Create Topic Folder Contents ----
# Each topic page contains: 
#  - Title: `topic_id`
#  - Topic Tag: "#topic_id" notation
#  - Associated Token Links: FREX, LIFT, etc. tokens in "[[token]]" notation

topic_model.summary = summary(topic_model)
topic_model.tokens = list("frex" = as_tibble(topic_model.summary$frex, .name_repair = "unique"),
                         "lift" = as_tibble(topic_model.summary$lift, .name_repair = "unique")) %>% 
  bind_rows(., .id = "word_type") %>% 
  group_by(word_type) %>% 
  mutate(topic_id = row_number()) %>% 
  ungroup() %>% 
  mutate(topic_id = paste0("topic_", str_pad(topic_id, width = 2, side = "left", pad = "0"))) %>% 
  pivot_longer(data = ., cols = -c(topic_id, word_type), names_to = "dummy_var", values_to = "token") %>% 
  select(-dummy_var)
  
pb = progress::progress_bar$new(total = length(topic_ids))
for(i in seq_along(topic_ids)){
  .id = topic_ids[i]

  .title = .id
  .tag = paste0("#", .id)
  
  .tokens = topic_model.tokens %>% 
    filter(topic_id == .id) %$%
    token %>% 
    paste0("[[", ., "]]")
  
  .path = paste0("./obsidian-vault/topics/", .id, ".md")
  
  .content = c(.title, 
               .tag, 
               .tokens
               )
  
  write_lines(x = .content, 
              file = .path)
  
  pb$tick()
  
}

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





