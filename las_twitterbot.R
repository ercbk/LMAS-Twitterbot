# las_twitterbot

# sections: Pull data, Create message and tweet


library(tidyverse)
library(jsonlite)
library(httr)
library(rtweet)



# ==== Pull data from API ====


# Will need to register at site and get api key and secret

# See https:/www.petfinder.com/developers/api-docs. You can use API method "shelter.find" to figure out your shelter's id. Example: Louisville Metro Animal Service's id = "KY102". Once you have the id, you can use the "shelter.getPets" method as illustrated below. I've used count = "2" for exploratory purposes. For the bot, you'll want more depending on the size of the shelter.


# URL <- "http://api.petfinder.com/shelter.getPets"
# args <- list(key = "<key>", id = "<id>", format = "json", output = "full", count = "2")
# api_json <- GET(url = URL, query = args)

# lets you know if any errors occurred in the GET request
# stop_for_status(api_json)

# creates character vector that's needed for fromJSON
# content_json <- content(api_json, as = "text", encoding = "UTF-8")



# ==== Manipulate data into message for bot ====


# example data; character vector
content_json <- read_rds("data/vector_json.rds") # 2 records
# content_json <- read_rds("data/vector_json2.rds") # 200 records


# Get a sense of the nested structure. It's long which is why I only pulled two records.
# content_json %>% prettify


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
                             "P" = "Pending", "X" = "Adopted/Removed")
             )



# Have to unnest options and breeds separately because they may have different nrows

# Dataframe to add columns to
bot_df <- pet_df %>% 
      select(name, animal, age, sex, size, link)

# Different colnames depending on nrows unnested

if(!is.na(pet_df$options.option)) {
      pet_options <- pet_df %>% 
            select(options.option) %>% 
            unnest %>% 
            rename_at(vars(matches("\\$t")), ~str_replace(., "\\$t", "options")) %>%
            rename_at(vars(matches("options.option")), ~str_replace(., "options.option", "options")) %>% 
            summarize(misc = glue::collapse(options, sep = ", "))
      
      bot_df <- bot_df %>% 
            bind_cols(pet_options) %>% 
            select(name, animal, misc, everything())
}

if(!is.na(pet_df$breeds.breed)) {
      pet_breeds <- pet_df %>% 
            select(breeds.breed) %>% 
            unnest %>% 
            rename_at(vars(matches("\\$t")), ~str_replace(., "\\$t", "breeds")) %>%
            rename_at(vars(matches("breeds.breed")), ~str_replace(., "breeds.breed", "breeds")) %>% 
            summarize(`breed(s)` = glue::collapse(breeds, sep = ", "))
      
      bot_df <- bot_df %>% 
            bind_cols(pet_breeds) %>% 
            select(name, animal, `breed(s)`, everything())
}


message <- bot_df %>% 
      select_if(~!is.na(.)) %>% 
      gather %>%
      add_row(key = "tags", value = "#adoptdontshop #rescue #adoptme #shelterpets") %>% 
      mutate(message = glue::glue_data(., "
                                       {key}: {value}
                                       ")) %>% 
      summarize(message = glue::collapse(message, sep = "\n"))


# Twitter doesn't display images from image links so we have to jump through some hoops and create a temp jpg file to feed to post_tweet

# Some images are blurry on twitter, but rows 3 seems to work alright

if(is.null(pet_df$media.photos.photo[[1]])) {
      # Default photo if no photo is provided
      img_url <- "http://www.dogsinpictures.com/images/dog-cat-bunny-bird-love.jpg"
      image_obj <- magick::image_read(img_url)
      tmp <- tempfile(fileext=".jpg")
      magick::image_write(image_obj, path = tmp, format = "jpg")
}else{
      img_df <- pet_df %>%
            select(media.photos.photo) %>%
            unnest %>%
            slice(3)
      img_url <- img_df$`$t`[[1]]
      
      image_obj <- magick::image_read(img_url)
      tmp <- tempfile(fileext=".jpg")
      magick::image_write(image_obj, path = tmp, format = "jpg")
}


# You'll need to create an app at apps.twitter.com and generate a token first. See get_token.R for details.
post_tweet(message[[1]], media = tmp)


