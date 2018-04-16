# Get token

# Currently only getting a read-only token but we need a token with write permission. I created an issue at rtweet but still waiting to hear back. I've added a couple lines to this tutorial, http://rtweet.info/articles/auth.html to bypass the problem.

library(tidyverse)
library(rtweet)



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
home_directory <- path.expand("~/")

# create_token currently creates a read-only token but if you regenerate your token at the apps.twitter.com site, you'll have a read and write token. Copy the new token and new token secret and paste below.

twitter_token$credentials$oauth_token <- "<new token>"
twitter_token$credentials$oauth_token_secret <- "<new token secret>"

write_rds(twitter_token, "<home directory path>twitter_token.rds")


file_name <- file.path(home_directory, "twitter_token.rds")


# create environment variable so rtweet doesnt need key explicitly in order to work.
cat(paste0("TWITTER_PAT=", file_name),
    file = file.path(home_directory, ".Renviron"),
    append = TRUE,
    fill = TRUE)

# Now unless you're using Windows Task Scheduler, you should be ready to tweet everytime you open R without explicitly loading twitter_token.