notif <- function(..., id, icon = icon("envelope-o"),
                  status = "primary", expanded = FALSE,
                  label_type = "badge") {
  
  len <- length(...)
  
  tags$li(
    class = if (expanded) "dropdown open" else "dropdown",
    role = "presentation",
    tags$a(
      class = "dropdown-toggle info-number",
      href = "javascript:;",
      `data-toggle` = "dropdown",
      `aria-expanded` = expanded,
      icon,
      switch(label_type,
             "badge" = label(name = len, status = status, mode = "badge"),
             label(name = label_type, status = status, mode = "label"))
    ),
    # content
    tags$ul(
      class = "dropdown-menu list-unstyled msg_list",
      role = "menu",
      id = id,
      ...
    )
  )
}

notifItem <- function(..., title = NULL, date = NULL, img = NULL, ref = NULL) {
  tags$li(
    tags$a(
      tags$span(
        class = "image",
        tags$img(src = img),
        tags$span(
          tags$span(title),
          tags$span(class = "time", date)
        ),
        tags$span(class = "message", ...)
      ),
      href = ref
    )
  )
}
