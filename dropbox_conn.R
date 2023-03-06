dropAuthRefresh <- function(new_user = FALSE,
                            key = "mmhfsybffdom42w",
                            secret = "l8zeqqqgm1ne5z0",
                            cache = TRUE,
                            rdstoken = "droptoken.rds") {
  if (!new_user & !is.na(rdstoken)) {
    if (file.exists(rdstoken)) {
      dropbox_token <- readRDS(rdstoken)
    } else {
      stop("Token file not found")
    }
  } else {
    if (new_user && file.exists(".httr-oauth")) {
      message("Removing old credentials...")
      file.remove(".httr-oauth")
    }
    dropbox <- httr::oauth_endpoint(
      authorize = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
      access = "https://api.dropbox.com/oauth2/token"
    )
    dropbox_app <- httr::oauth_app("dropbox", key, secret)
    dropbox_token <- httr::oauth2.0_token(dropbox, dropbox_app, cache = cache)
    if (!inherits(dropbox_token, "Token2.0")) {
      stop("Something went wrong, try again")
    }
  }
  return(dropbox_token)
}


loadDataFromDropbox <- function(path = "shinystats/data.rds") {
  drop_download(path, overwrite = TRUE, dtoken = dropAuthRefresh())
}


saveDataToDropbox <- function(dat) {
  write_rds(dat, "data.rds")
  drop_upload("data.rds", path = "shinystats", dtoken = dropAuthRefresh())
}
