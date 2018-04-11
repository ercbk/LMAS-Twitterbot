# las_twitterbot


library(tidyverse)
library(jsonlite)
library(httr)
library(rtweet)




# ==== Pull data from API ====


# Will need to register at site and get api key and secret

# URL <- "http://api.petfinder.com/pet.find"
# args <- list(key = "<key>", location = "<City, State Abbreviation>", format = "json", output = "full", count = "2")
# api_json <- GET(url = URL, query = args)

# lets you know if any errors occurred in the GET request
# stop_for_status(api_json)

# creates character vector that's needed for fromJSON
# content_json <- content(api_json, as = "text", encoding = "UTF-8")



# ==== Manipulate data into message for bot ====


# character vector
content_json <- read_rds("data/vector_json.rds")

# Get a sense of the nested structure. It's long so you can see why I only pulled two records
content_json %>% prettify


# creates list of nested data.frames
obj_json <- fromJSON(content_json)

# Think this is redundant since we used the stop_for_status function in the commented API section. If not, may need an if-stop check.
api_message <- obj_json$header$status$message

# flatten creates a df; some cols have ".$t" in their names
pet_df <- flatten(obj_json$petfinder$pets$pet) %>%
      rename_at(vars(ends_with(".$t")), ~str_replace(., "\\.\\$t", "")) %>%
      sample_n(size = 1) %>%
      mutate(lastUpdate = as.POSIXct(lastUpdate),
             link = paste0("https://www.petfinder.com/petdetail/", id),
             sex = recode(sex, "F" = "Female", "M" = "Male"),
             size = recode(size, "L" = "Large", "S" = "Small",
                           "M" = "Medium", "XL" = "Extra Large"),
             status = recode(status, "A" = "Adoptable", "H" = "Hold",
                             "P" = "Pending", "X" = "Adopted/Removed"))


# Have to unnest options and breeds separately

pet_options <- pet_df %>% 
      select(options.option) %>% 
      unnest %>% 
      rename(options = `$t`) %>% 
      summarize(options = glue::collapse(options, sep = ", "))

pet_breeds <- pet_df %>% 
      select(breeds.breed) %>% 
      unnest %>% 
      rename_at(vars(matches("\\$t")), ~str_replace(., "\\$t", "breeds")) %>%
      rename_at(vars(matches("breeds.breed")), ~str_replace(., "breeds.breed", "breeds")) %>% 
      summarize(breeds = glue::collapse(breeds, sep = ", "))

pet_img <- pet_df %>% 
      select(media.photos.photo) %>% 
      unnest %>% 
      slice(3) %>% 
      mutate(`$t` = paste0("http://jpg.party/", `$t`)) %>% 
      select(img = `$t`)


bot_df <- pet_df %>% 
      select(status, age, size, name, sex, mix, animal, link) %>% 
      bind_cols(pet_breeds, pet_options, pet_img)

# Form format used by BMG in their bot
message <- bot_df %>% 
      mutate(message = glue::glue("
                            {name} is:
                            {animal}
                            {breeds}
                            {sex}
                            {age}
                            {size}
                            #adoptdontshop #rescue # adoptme #shelterpets
                            {link}
                            {img}")) %>% 
      select(message)



# ==== Get twitter token using tutorial ====


# # Currently only getting a read-only token but we need a token with write permission. I created an issue at rtweet but still waiting to hear back

# # *** Workaround method in the next section ***

# # Code taken from tutorial, http://rtweet.info/articles/auth.html 

# appname <- "<app name>"
# 
# key <- "<key>"
# 
# secret <- "<secret>"
# 
# # create token
# twitter_token <- create_token(
#       app = appname,
#       consumer_key = key,
#       consumer_secret = secret
# )
# 
# # save token to rds file in home directory
# home_directory <- path.expand("~/")
# 
# file_name <- file.path(home_directory, "las_bot_token.rds")
# 
# saveRDS(twitter_token, file = file_name)
# 
# 
# # create environment variable so rtweet doesnt need key explicitly in order to work.
# # Getting error when restarting RStudio. RStudio starts but ignores .Renviron.
#
# cat(paste0("<home directory path>//las_bot_token.rds"),
#     file = file.path(home_directory, ".Renviron"),
#     append = TRUE,
#     fill = TRUE)
# 
# 
# post_tweet(message[[1]])



# ==== Alternate Token Method ====


# Follow tutorial (linked above) all the way until after this code chunk

appname <- "<app name>"

key <- "<key>"

secret <- "<secret>"

# create token
twitter_token <- create_token(
      app = appname,
      consumer_key = key,
      consumer_secret = secret
)

# Shows Home Directory path
path.expand("~/")

# create_token currently creates a read-only token but if you regenerate your token at the apps.twitter.com site, you'll have a read and write token. copy the new token and new token secret. create_token also creates .rtweet_token.rds which we can load and replace the old token values with the newly created ones.

token_rds <- read_rds("<home directory path>.rtweet_token.rds")

token_rds$credentials$oauth_token <- "<new token>"
token_rds$credentials$oauth_token_secret <- "<new token secret>"

write_rds(token_rds, "<home directory path>.rtweet_token.rds")

# Next restart R session (and possibly RStudio). You should be ready to tweet after that.

post_tweet(message[[1]])


