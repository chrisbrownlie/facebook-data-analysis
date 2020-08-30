source("init.R")

#' Get likes from external sites
#' 
#' @param folder the name of the data folder (in the project root directory)
#'  
#' @return data frame of likes from external sites
get_external_likes <- function(folder = "data") {
  
  external_likes <- fromJSON(file = file.path(folder, "likes_and_reactions", "likes_on_external_sites.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(timestamp),
              title = title,
              url = url,
              attachment_name = attachments.data.external_context.name,
              attachment_url = attachments.url,
              attachment_source = attachments.data.external_context.source,
              attachment_root_url = attachments.data.external_context.url)
  return(external_likes)
}


#' Get 'liked' pages
#' 
#' @param folder the name of the data folder (in the project root directory)
#'  
#' @return data frame of liked pages
get_liked_pages <- function(folder = "data") {
  
  pages <- fromJSON(file = file.path(folder, "likes_and_reactions", "pages.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(timestamp),
              name = name)
  return(pages)
}


#' Get posts and comments
#'
#' @param folder the name of the data folder (in the project root directory)
#' 
#' @return data frame of all posts and comments made
get_posts_and_comments <- function(folder = "data") {
  
  posts_and_comments <- fromJSON(file = file.path(folder, "likes_and_reactions", "posts_and_comments.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(timestamp),
              reaction = data.reaction.reaction,
              reacted_to = str_extract(title, pattern = "(?<=(reacted to |likes |liked ))[[:alnum:][:space:][:punct:]]+?(?='s|$)"),
              content = str_extract(title, pattern = "(?<='s) [[:alpha:]]+"),
              content_name = str_extract(title, pattern = "(?<=:)[[:print:]]+"),
              location = str_extract(title, pattern = "(?<= in | on )[[:print:]]+"))
  
  return(posts_and_comments)
}
