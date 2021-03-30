library(htmltools)

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

