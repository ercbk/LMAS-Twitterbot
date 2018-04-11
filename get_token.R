# Get token

# sections: tutorial method, alternate method



# ==== Get twitter token using tutorial ====


# # Currently only getting a read-only token but we need a token with write permission. I created an issue at rtweet but still waiting to hear back

#

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


# Follow tutorial (http://rtweet.info/articles/auth.html) all the way until after this code chunk

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

# Create_token currently creates a read-only token but if you regenerate your token at the apps.twitter.com site, you'll have a read and write token. Copy the new token and new token secret. Create_token also creates .rtweet_token.rds which we can load and replace the old token values with the newly created ones.

token_rds <- read_rds("<home directory path>.rtweet_token.rds")

token_rds$credentials$oauth_token <- "<new token>"
token_rds$credentials$oauth_token_secret <- "<new token secret>"

write_rds(token_rds, "<home directory path>.rtweet_token.rds")

# Next restart R session (and possibly RStudio). You should be ready to tweet after that. You shouldn't have to recreate t