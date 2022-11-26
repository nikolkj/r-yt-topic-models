# Start-up ----
# pkgs 
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(rvest))

# params
param.k_select = 10

# Make Obsidian Folders ----
# make directories
if(!dir.exists("./obsidian-vault/")) dir.create("./obsidian-vault/") # main input dir 

# empty vault 
.obs_files = dir(path = "./obsidian-vault/", full.names = TRUE, recursive = TRUE)
file.remove(.obs_files)

# make obsidian folders
if(!dir.exists("./obsidian-vault/topics/")) dir.create("./obsidian-vault/topics/") # topics dir
if(!dir.exists("./obsidian-vault/transcripts/")) dir.create("./obsidian-vault/transcripts/") # topics dir
if(!dir.exists("./obsidian-vault/snippets/")) dir.create("./obsidian-vault/snippets/") # snippets dir

# load data 
load(file = "./prepd-data/topic-model-data.RData")

# Create Topic Folder Contents ----

# Create Transcripts Folder Contents ----

# Create Snippets Folder Contents ----



