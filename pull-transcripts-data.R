require(tidyverse)
require(rvest)

# Make folders and workspace ----
if(!dir.exists("./raw-data/")) dir.create("./raw-data/") # main input dir 
if(!dir.exists("./prepd-data/")) dir.create("./prepd-data/") # main output dir
if(!dir.exists("./prepd-data/transcripts/")) dir.create("./prepd-data/transcripts/") # individual transcripts

# Content Note ----
# full content will not load if you pull channel path with read_html
# ... manually load and scroll to bottom to load all videos
# ... r'click inspect
# ... r'click <html> then select "copy outer HTML"
# ... paste and save to file

# Julia Silge Content ----
silge_contents = rvest::read_html(x = "./raw-data/julia-silge_video-library-contents.html", # outerHTML content of fully loaded page
                                 encoding = "UTF-8")

temp = silge_contents %>% 
  html_elements(xpath = '//*[@id="video-title-link"]')

silge_titles = temp %>% 
  html_text2()

silge_links = temp %>% 
  html_attr("href")

silge_data = tibble(title = silge_titles, 
                     url = silge_links)

silge_data = filter(.data = silge_data, !is.na(url))
# silge_data = silge_data[-c(1:which(is.na(silge_data$url))[1]),] # not necessary here

silge_data$channel = "@JuliaSilge"

# Load Lander Content ----
lander_contents =  rvest::read_html(x = "./raw-data/lander-analytics_video-library-contents.html", # outerHTML content of fully loaded page
                                    encoding = "UTF-8") 
temp = lander_contents %>% 
  html_elements(xpath = '//*[@id="video-title-link"]')

lander_titles = temp %>% 
  html_text2()
  
lander_links = temp %>% 
  html_attr("href")

lander_data = tibble(title = lander_titles, 
                     url = lander_links)
lander_data = lander_data[-c(1:which(is.na(lander_data$url))[1]),] # remove suggested videos block
lander_data$channel = "@landeranalytics9162"

# Load Posit Content ----
posit_contents = read_html(x = "./raw-data/posit-pbc_video-library-content.html", # outerHTML content of fully loaded page
                           encoding = "UTF-8")

temp = posit_contents %>% 
  html_elements(xpath = '//*[@id="video-title-link"]')

posit_titles = temp %>% 
  html_text2()

posit_links = temp %>% 
  html_attr("href")

posit_data = tibble(title = posit_titles, 
                     url = posit_links)

posit_data = posit_data[-c(1:which(is.na(posit_data$url))[1]),] # remove suggested videos block
posit_data$channel = "@PositPBC"

# Combine all datasets ----
rm(list = grep("(_data$)", ls(), value = TRUE, invert = TRUE))
data = bind_rows(posit_data, lander_data, silge_data)
data = filter(.data = data, !is.na(url))

data$ref = str_remove(data$url, "/watch\\?v=")

# Pull Transacript ----
transcripts = vector(mode = "list", length=nrow(data))
names(transcripts) = data$ref

pb = progress::progress_bar$new(total = length(transcripts))
for(i in seq_along(data$url)){
  # multi-pass w/ debug, output archived to read back into list and prevent duplication of effort
  # fyi, very slow
  
  .url = data$url[i]
  .title = data$ref[i]
  
  
  if(paste0(.title, ".rds") %in% dir(path = "./prepd-data/transcripts/")){
    # check if transcript already pulled and archived
    transcripts[[i]] = read_rds(file = paste0("./prepd-data/transcripts/", .title, ".rds"))
    pb$tick()
    next
  }
  
  # pull transcripts
  .temp = tryCatch(expr = {httr::GET(url = paste0("https://www.youtube.com", .url))},
                   error = function(e){return(NA)})
  
  if(any(is.na(.temp))){
    # timeout error 
    pb$tick()
    closeAllConnections()
    next
    } 
  
  
  .temp = httr::content(x = .temp, as = "text") # read video page as HTML teext
  .temp = .temp %>% 
    # parse and clean-up TimedText API url
    str_extract(., '"baseUrl":"https://www.youtube.com/api/timedtext[^"]*lang=en') %>%
    str_remove(string = ., ".*baseUrl.*://") %>% 
    str_replace_all(string = ., pattern = "\\\\u0026", replacement = "&")
     
  if(any(is.na(.temp))){
    # no TimedText API url
    pb$tick()
    closeAllConnections()
    next
  }  
  
  # Access TimedText API url
  .temp = tryCatch(expr = {read_html(paste0("https://",.temp))},
                   error = function(e){return(NA)}) 
  
  if(is.na(.temp)){
    # timeout error 
    pb$tick()
    closeAllConnections()
    next  
  } 
  
  # Parse TimedText API response
  .text = .temp %>% 
    # grab text
    html_nodes("text") %>% 
    html_text() %>% 
    # clean-up
    str_remove_all(string = ., pattern = "&#\\d+;") %>% # HTML characters, eg. &#39;
    str_replace_all(string = ., pattern = "\\n", " ")
  
  .data = .temp %>% 
    # grab time-stamps
    html_elements("text") %>% 
    html_attrs() %>% 
    bind_rows() %>%
    # update data types
    mutate_all(.tbl = ., .funs = as.numeric) %>% 
    # add in text response
    mutate(text = .text)
    
  write_rds(x = .data, file = paste0("./prepd-data/transcripts/", .title, ".rds"))
  transcripts[[i]] = .data
  
  closeAllConnections()
  Sys.sleep(.1)
  pb$tick()
  
}

# Combine transcripts and archive output ----
transcripts = bind_rows(transcripts, .id = "ref")

write_rds(x = data, file = "./prepd-data/yt_titles-and-links.rds")
write_rds(x = transcripts, file = "./prepd-data/yt_transcripts.rds")
writexl::write_xlsx(x = data, path = "./prepd-data/yt_titles-and-links.xlsx")
writexl::write_xlsx(x = transcripts, path = "./prepd-data/yt_transcripts.xlsx")
