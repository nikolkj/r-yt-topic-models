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
if(!dir.exists("./obsidian-vault/stems/")) dir.create("./obsidian-vault/stems/") # stems dir 
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

# Create Token Stem Folders ----
# Each topic page contains: 
#  - Topic Tag: "#topic_id" notation
#  - Word-type Tag: "#type" notation
#  - Aliases YAML: "aliases: [word1, word2, ...]" notation
#   |- must be clean: no trailing punctuation, non-apostrophe characters, etc.
#   |- derived from `data$word`
#
# Retained: 
# - map of token to obsidian aliased-link mapping, "[[word1|word2|]]" notation

stems = topic_model.tokens %>% 
  group_by(token) %>% 
  nest() %>% 
  ungroup()

alias_maps = list()
pb = progress::progress_bar$new(total = length(stems$token))
for(i in seq_along(stems$token)){
  
  .token = stems$token[i]
  .topic_ids = unique(stems$data[[i]]$topic_id)
  .word_types = unique(stems$data[[i]]$word_type)
  
  aliases = data %>% 
    filter(token == .token) %>% 
    left_join(x = ., 
              y = select(.data = transcript_topics, ref, topic_id),
              by = "ref")
  
  if(TRUE) filter(.data = aliases, topic_id %in% .topic_ids) # optional, only keep words from associated docs
  
  # TODO this is SLOW; refactor
  alias_map = aliases %>% 
    select(token, word) %>% 
    unique() %>% 
    mutate(alias = str_remove_all(word, "[\\.\\?\\!,-]+$"), 
           # variations 
           alias = tolower(alias),
           upper_alias = toupper(alias), 
           title_alias = str_to_title(alias)
           ) %>% 
    pivot_longer(data = ., cols = contains("alias"), names_to = "alias_type", values_to = "alias")
  
  
    .aliases = unique(alias_map$alias)
    .yaml = paste0("aliases: [",paste(.aliases, collapse = ", "),"]") %>% 
      c("---", ., "---")
    .path = paste0("./obsidian-vault/stems/", .token, ".md")
    
    .content = c(.yaml, # YAML
                 paste0("#",.word_types), # word type tags
                 paste0("#", .topic_ids) # topic tags
                 )
    
    # write to file
    write_lines(x = .content, 
                file = .path)
    
    # archive alias map
    alias_maps = c(alias_maps, list(alias_map))
    
    pb$tick()
}


# Create Transcripts Folder Contents: Punctuated Documents ----
documents_ids = filter(.data = punct_summary, .punctd)
for(j in seq_along(documents_ids)){
  
  
  
  
}

# Create Transcripts Folder Contents: Non-Punctuated Documents ----
documents_ids = filter(.data = punct_summary, !.punctd)

# Create Snippets Folder Contents ----





