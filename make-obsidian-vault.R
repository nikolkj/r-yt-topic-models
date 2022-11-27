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

readline(prompt = "Ready?")

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
pb = progress::progress_bar$new(total = nrow(documents_ids))
for(i in seq_along(documents_ids$ref)){
  
  .ref = documents_ids$ref[i]
  
  # transcript meta data
  .title = titles$title[which(titles$ref == .ref)] 
  .md_title = .title %>% 
    str_replace_all(string = ., '[\\*"\\\\/<>:|\\?]+', "~")
    assertthat::assert_that(length(.title) == 1L)
    assertthat::assert_that((!grepl('[*"\\/<>:|]', .md_title))) # obsidian doc title char restricts

  .channel = titles$channel[which(titles$ref == .ref)]  
    assertthat::assert_that(length(.channel) == 1L)
    
  .link = titles$url[which(titles$ref == .ref)]  
    assertthat::assert_that(length(.link) == 1L)
    
  .topic_id = transcript_topics$topic_id[which(transcript_topics$ref == .ref)]
    assertthat::assert_that(length(.topic_id) == 1L)
  
  # transcript contents
  .data = filter(.data = data, ref == .ref) %>%
    mutate(punct = str_extract(word, "[[:punct:]]+$")) %>% 
    # # ignores topic relationship
    # mutate(model_token = token %in% topic_model.tokens$token) %>% 
    # respects topic relationship
    mutate(model_token = token %in% filter(.data = topic_model.tokens, topic_id == .topic_id)$token) %>% 
    mutate(tagged_word = ifelse(test = model_token, 
                                yes = paste0("[[", token, "|", 
                                             str_remove(word, "[[:punct:]]+$"),
                                             "]]", ifelse(!is.na(punct), punct, "")), 
                                no = word)
           )
  
  .transcript = paste(.data$tagged_word, collapse = " ") 
  .transcript_eos = as_tibble(str_locate_all(.transcript, "[\\.\\?!]+\\s+")[[1]])
  .transcript_eos$sentence_start = lag(.transcript_eos$end, n = 1)
  .transcript_eos$sentence_start[1] = 1
  .transcript_eos$sentence_end = .transcript_eos$end -1
  .transcript_eos = select(.data = .transcript_eos, contains("sentence"))
  .transcript = apply(.transcript_eos, 1, function(x){str_sub(.transcript, x[[1]], x[[2]])}) %>% 
    trimws()
  
  
  # write to file
  .page_title = paste("#", .title)
  .tags = paste(paste0("[[", .channel, "]]"),
                paste0("#", .topic_id)
                )
  
  .link = paste0("https://www.youtube.com", .link) %>% 
    paste0("[Watch on YouTube](", ., ")")
  
  .content = .transcript
  
  .path = paste0("./obsidian-vault/transcripts/", .md_title, ".md")
  
  write_lines(x = c(.page_title,
                    .tags,
                    .link,
                    .content),
              
              file = .path, 
              sep = "\n\n")
  
  pb$tick()
  
  
}

# Create Transcripts Folder Contents: Non-Punctuated Documents ----
documents_ids = filter(.data = punct_summary, !.punctd)
pb = progress::progress_bar$new(total = nrow(documents_ids))
for(i in seq_along(documents_ids$ref)){
  
  .ref = documents_ids$ref[i]
  
  # transcript meta data
  .title = titles$title[which(titles$ref == .ref)] 
  .md_title = .title %>% 
    str_replace_all(string = ., '[\\*"\\\\/<>:|\\?]+', "~")
  assertthat::assert_that(length(.title) == 1L)
  assertthat::assert_that((!grepl('[*"\\/<>:|]', .md_title))) # obsidian doc title char restricts
  
  .channel = titles$channel[which(titles$ref == .ref)]  
  assertthat::assert_that(length(.channel) == 1L)
  
  .link = titles$url[which(titles$ref == .ref)]  
  assertthat::assert_that(length(.link) == 1L)
  
  .topic_id = transcript_topics$topic_id[which(transcript_topics$ref == .ref)]
  assertthat::assert_that(length(.topic_id) == 1L)
  
  # transcript contents
  .data = filter(.data = data, ref == .ref) %>%
    mutate(punct = str_extract(word, "[[:punct:]]+$")) %>% 
    # # ignores topic relationship
    # mutate(model_token = token %in% topic_model.tokens$token) %>% 
    # respects topic relationship
    mutate(model_token = token %in% filter(.data = topic_model.tokens, topic_id == .topic_id)$token) %>% 
    mutate(tagged_word = ifelse(test = model_token, 
                                yes = paste0("[[", token, "|", 
                                             str_remove(word, "[[:punct:]]+$"),
                                             "]]", ifelse(!is.na(punct), punct, "")), 
                                no = word)
    )
  
  .transcript_order = transcripts %>% 
    filter(ref == .ref) %>% 
    select(text) %>% 
    mutate(tokens = str_count(string = text, pattern = "\\s+") + 1) %>%
    mutate(token_end = cumsum(tokens), 
           token_start = token_end - tokens + 1) %>% 
    select(token_start, token_end)
  
  .transcript = apply(.transcript_order, 1, function(x){
    # TODO very slow, refactor
    .data %>%
      select(word_index, tagged_word) %>% 
      filter(word_index >= x[[1]] & word_index <= x[[2]]) %$% 
      tagged_word %>% 
      paste(., collapse = " ")
  })
  
  # write to file
  .page_title = paste("#", .title)
  .tags = paste(paste0("[[", .channel, "]]"),
                paste0("#", .topic_id)
  )
  
  .link = paste0("https://www.youtube.com", .link) %>% 
    paste0("[Watch on YouTube](", ., ")")
  
  .content = .transcript
  
  .path = paste0("./obsidian-vault/transcripts/", .md_title, ".md")
  
  write_lines(x = c(.page_title,
                    .tags,
                    .link,
                    .content),
              
              file = .path, 
              sep = "\n\n")
  
  pb$tick()
  
  
}

# Create Snippets Folder Contents ----





