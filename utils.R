#' Get country from IP address
#' 
#' @param ipa the IP address to find country for, using db-ip.com
#' 
#' @return a character vector giving the location (of the ISP) and a country
lookup_ip_address <- function(ipa) {
  
  html <- read_html(paste0("https://db-ip.com/", ipa)) %>%
    html_nodes("td") %>%
    html_text()
  
  country <- trimws(html[[8]])
  region <- trimws(str_replace(html[[9]], pattern = "\n", replacement = ""))
  city <- trimws(str_replace(html[[10]], pattern = "\n", replacement = ""))
  coordinates <- trimws(str_replace(html[[14]], pattern = "\n", replacement = ""))
  c("country" = country,
    "region" = region,
    "city" = city,
    "coordinates" = coordinates)
}