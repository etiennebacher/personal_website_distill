library(htmltools)
library(httr)
library(magick)

##########
## Create cards for the "Projects" section ##
##########

make_grid_project <- function(...) {
  
  htmltools::tags$div(class = "grid-container", ...)
  
}

make_card_project <- function(
  title = NULL,
  title_link = NULL,
  description = NULL,
  button = "See more",
  button_link = NULL
) {
  
  x <- htmltools::withTags(
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
  if (nchar(week) == 1) week <- paste0("0", week)
  
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
  
  if (!file.exists(here::here(destination))) {
    if (!file.exists(here::here("_gallery/img"))) {
      dir.create(here::here("_gallery/img"))
    }
    download.file(origin, here::here(destination))
  }
  
  thumb_destination <- paste0("_gallery/img/thumb-", year, "-", week, "-", 
                        trimws(basename(origin)))
  if (!file.exists(thumb_destination)) {
    resize_image(paste0(year, "-", week, "-", trimws(basename(origin))))
  }
 
}

resize_image <- function(image) {
  
  imFile <- image_read(here::here(paste0("_gallery/img/", image)))
  imFile_resized <- magick::image_resize(imFile, "6%")
  magick::image_write(imFile_resized, here::here(paste0("_gallery/img/thumb-", image)))
  
}

make_gallery_layout <- function() {
  
  images <- list.files("_gallery/img")
  images_full_size <- grep("thumb", images, 
                           value = TRUE, invert = TRUE)
  images_thumb <- grep("thumb", images, value = TRUE)
  
  images <- data.frame(images_thumb = images_thumb,
                       images_full_size = images_full_size)
  
  tagList(apply(images, 1, function(x) {
      year <- substr(x[["images_full_size"]], 1, 4)
      gh_link <- paste0(
        "https://github.com/etiennebacher/tidytuesday/tree/master/R/",
        year, "/W", substr(x[["images_full_size"]], 6, 
                          nchar(x[["images_full_size"]])-4)
      )
      tags$a(
        href = paste0("_gallery/img/", x[["images_full_size"]]),
        `data-sub-html`= tags$p(
          "See code and image", 
          tags$a(href = gh_link, "here"),
          class = "gallery-link"
        ),
        tags$img(src = paste0("_gallery/img/", x[["images_thumb"]]))
      )
  }))
  
}


