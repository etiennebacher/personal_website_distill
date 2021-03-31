library(htmltools)
library(httr)
library(magick)

##########
## Create cards for the "Projects" section ##
##########

make_grid_project <- function(...) {
  
  shiny::tags$div(class = "grid-container", ...)
  
}

make_card_project <- function(
  title = NULL,
  title_link = NULL,
  description = NULL,
  button = "See more",
  button_link = NULL
) {
  
  x <- shiny::withTags(
    div(class = "card",
        div(class = "card-text",
            h4(a(target="_blank")),
            div(class = "article-style",
                p(description)
            ),
            a(target="_blank")
        )
    )
  )
  
  # Can't add directly in a()
  
  x$children[[1]]$children[[1]]$children[[1]]$attribs[["href"]] <- title_link
  x$children[[1]]$children[[1]]$children[[1]]$children <- title
  
  x$children[[1]]$children[[3]]$attribs[["href"]] <- button_link
  x$children[[1]]$children[[3]]$children <- button
  
  return(x)
}


##########
## Create the layout for the "Gallery" section ##
##########

get_tt_image <- function(year, week) {
  
  if (is.numeric(year)) year <- as.character(year)
  if (is.numeric(week)) week <- as.character(week)
  
  ### Get the link to download the image I want
  req <- GET("https://api.github.com/repos/etiennebacher/tidytuesday/git/trees/master?recursive=1")
  stop_for_status(req)
  file_list <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  png_list <- grep(".png", file_list, value = TRUE, fixed = TRUE)
  png_wanted <- grep(year, png_list, value = TRUE)
  png_wanted <- grep(paste0("W", week), png_wanted, value = TRUE)
  origin <- paste0(
    "https://raw.githubusercontent.com/etiennebacher/tidytuesday/master/",
    png_wanted
  )
  destination <- paste0("_gallery/img/", year, "-", week, "-", 
                        trimws(basename(origin)))
  
  if (!file.exists(destination)) {
    download.file(origin, destination)
  }
  
  return(destination)
  
}

layout_tt_image <- function() {
  
  images_wanted <- sapply()
  
  tagList(
    lapply(
      
    )
  )
  
}


resize_image <- function(image) {
  
  imFile <- image_read(paste0("_gallery/img/", image))
  imFile_resized <- magick::image_resize(imFile, "5%")
  magick::image_write(imFile_resized, paste0("_gallery/img/thumb-", image))
  
}
