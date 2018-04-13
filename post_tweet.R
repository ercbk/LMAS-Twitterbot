# post_tweet function




#' Posts status update to user's Twitter account
#'
#' @param status Character, tweet status. Must be 140
#'   characters or less.
#' @param media File path to image or video media to be
#'   included in tweet.
#' @param token OAuth token. By default \code{token = NULL}
#'   fetches a non-exhausted token from an environment
#'   variable tokens.
#' @param in_reply_to_status_id Status ID of tweet to which you'd like to reply.
#'   Note: in line with the Twitter API, this parameter is ignored unless the
#'   author of the tweet this parameter references is mentioned within the
#'   status text.
#' @examples
#' \dontrun{
#' x <- rnorm(300)
#' y <- x + rnorm(300, 0, .75)
#' col <- c(rep("#002244aa", 50), rep("#440000aa", 50))
#' bg <- c(rep("#6699ffaa", 50), rep("#dd6666aa", 50))
#' tmp <- tempfile(fileext = "png")
#' png(tmp, 6, 6, "in", res = 127.5)
#' par(tcl = -.15, family = "Inconsolata",
#'     font.main = 2, bty = "n", xaxt = "l", yaxt = "l",
#'     bg = "#f0f0f0", mar = c(3, 3, 2, 1.5))
#' plot(x, y, xlab = NULL, ylab = NULL, pch = 21, cex = 1,
#'      bg = bg, col = col,
#'      main = "This image was uploaded by rtweet")
#' grid(8, lwd = .15, lty = 2, col = "#00000088")
#' dev.off()
#' browseURL(tmp)
#' post_tweet(".Call(\"oops\", ...)",
#'            media = tmp)
#'
#' # example of replying within a thread
#' post_tweet(status="first in a thread")
#' my_timeline <- get_timeline(self_user_name, n=1, token=twitter_token)
#' reply_id <- my_timeline[1,]$status_id
#' post_tweet(status="second in the thread", in_reply_to_status_id=reply_id)
#' }
#' @family post
#' @aliases post_status
#' @export
post_tweet <- function(status = "my first rtweet #rstats",
                       media = NULL,
                       token = NULL,
                       in_reply_to_status_id = NULL) {
      
      ## validate
      stopifnot(is.character(status))
      stopifnot(length(status) == 1)
      query <- "statuses/update"
      if (all(nchar(status) > 280, !grepl("http", status))) {
            stop("cannot exceed 280 characters.", call. = FALSE)
      }
      if (length(status) > 1) {
            stop("can only post one status at a time",
                 call. = FALSE)
      }
      token <- check_token(token)
      
      ## media if provided
      if (!is.null(media)) {
            r <- vector("list", length(media))
            media_id_string <- vector("list", length(media))
            for (i in seq_along(media)) {
                  r[[i]] <- upload_media_to_twitter(media[[i]], token)
                  if (has_name_(r[[i]], "media_id_string")) {
                        media_id_string[[i]] <- r[[i]]$media_id_string
                  } else {
                        stop(paste("media file number", i, "failed to upload"), call. = FALSE)
                  }
            }
            media_id_string <- paste(media_id_string, collapse = ",")
            params <- list(
                  status = status,
                  media_ids = media_id_string
            )
      } else {
            params <- list(
                  status = status
            )
      }
      query <- "statuses/update"
      if (!is.null(in_reply_to_status_id)) {
            params[["in_reply_to_status_id"]] <- in_reply_to_status_id
      }
      
      url <- make_url(query = query, param = params)
      
      r <- TWIT(get = FALSE, url, token)
      
      if (r$status_code != 200) {
            return(httr::content(r))
      }
      message("your tweet has been posted!")
      invisible(r)
}



upload_media_to_twitter <- function(media, token) {
      media2upload <- httr::upload_file(media)
      query <- "media/upload"
      rurl <- paste0(
            "https://upload.twitter.com/1.1/media/upload.json"
      )
      r <- httr::POST(rurl, body = list(media = media2upload), token)
      httr::content(r)
}



has_name_ <- function(x, ...) {
      vars <- c(...)
      stopifnot(is.character(vars))
      if (!is.recursive(x)) {
            return(FALSE)
      }
      all(vars %in% names(x))
}



#' make_url
#'
#' @param restapi logical Default \code{restapi = TRUE}
#'   indicates the provided URL components should be
#'   specify Twitter's REST API. Set this to FALSE if you wish
#'   to make a request URL designed for Twitter's streaming api.
#' @param query Twitter's subsetting/topic identifiers.
#'   Although the httr package refers to this as "path",
#'   query is used here to maintain consistency with
#'   Twitter API's excellent documentation.
#' @param param Additional parameters (arguments) passed
#'   along. If none, NULL (default).
#' @return URL used in httr call.
#' @keywords internal
#' @noRd
make_url <- function(restapi = TRUE, query, param = NULL) {
      if (restapi) {
            hostname <- "api.twitter.com"
      } else {
            hostname <- "stream.twitter.com"
      }
      structure(
            list(
                  scheme = "https",
                  hostname = hostname,
                  port = NULL,
                  path = paste0("1.1/", query, ".json"),
                  query = param,
                  params = NULL,
                  fragment = NULL,
                  username = NULL,
                  password = NULL),
            class = "url")
}



#' TWIT
#'
#' @description Base function responsible for formulating GET and
#'   POST requests to Twitter API's.
#'
#' @param get Logical with the default, \code{get = TRUE},
#'   indicating whether the provided url should be passed along via
#'   a GET or POST request.
#' @param url Character vector designed to operate like
#'   parse_url and build_url functions in the httr package.
#'   The easiest way to do this is to work through
#'   the call-specific functions as they are designed to simplify
#'   the process. However, if one were interested in reverse-
#'   engingeering such a thing, I would recommend checking out
#'   \code{make_url}.
#' @param \dots Further named parameters, such as config, token,
#'   etc, passed on to modify_url in the httr package.
#' @note Occasionally Twitter does recommend using POST requests
#'   for data retrieval calls. This is usually the case when requests
#'   can involve long strings (containing up to 100 user_ids). For
#'   the most part, or at least for any function-specific requests
#'   (e.g., \code{get_friends}, take reflect these changes.
#' @return json response object
#' @importFrom httr GET POST timeout write_disk progress
#' @keywords internal
#' @noRd
TWIT <- function(get = TRUE, url, ...) {
      if (get) {
            GET(url, ...)
      } else {
            POST(url, ...)
      }
}