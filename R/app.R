#' @importFrom shiny shinyApp tags div uiOutput verbatimTextOutput reactiveValues
#' @importFrom shiny observe observeEvent updateTextInput showModal modalDialog removeModal
#' @importFrom shiny renderUI renderText actionButton selectInput textInput HTML addResourcePath
#' @importFrom shiny reactive p actionLink modalButton textAreaInput tagList h5
#' @importFrom bslib page_sidebar sidebar nav_panel navset_tab bs_theme
#' @importFrom shinyjs useShinyjs
#' @importFrom jsonlite read_json write_json toJSON
#' @importFrom magrittr %>%
#' @importFrom base64enc dataURI
#' @importFrom fs path_home
#' @importFrom markdown renderMarkdown
#' @importFrom later later
#' @importFrom shinyFiles shinyDirChoose shinyDirButton
#' @importFrom tools file_path_sans_ext
#' @importFrom utils globalVariables
NULL

# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# Declare global variables to avoid R CMD check NOTEs
globalVariables(".")

# ============================================================================
# DATA STRUCTURE FUNCTIONS
# ============================================================================

#Initialize empty digitized data structure
#'
#Creates a list with empty data frames for all digitization types.
#Internal keys use full names, but JSON export uses abbreviated names.
#'
#@return List with landmarks, polylines_open, polylines_closed, curves_open, curves_closed
init_digitized_data <- function() {
  list(
    landmarks = list(),
    polylines_open = list(),
    polylines_closed = list(),
    curves_open = list(),
    curves_closed = list()
  )
}

# ============================================================================
# POINT ADDITION FUNCTIONS
# ============================================================================

# Add a landmark point to the digitized data
#
# @param dd Digitized data structure
# @param x X coordinate
#  @param y Y coordinate
#  @param pid Partition ID (default: 1)
#  @return Updated digitized data structure
add_landmark <- function(dd, x, y, pid = 1) {
  # Ensure partition exists
  while (length(dd$landmarks) < pid) {
    dd$landmarks[[length(dd$landmarks) + 1]] <- data.frame(x = numeric(0), y = numeric(0))
  }
  # Add point to partition
  dd$landmarks[[pid]] <- rbind(dd$landmarks[[pid]], data.frame(x = x, y = y))
  dd
}

#  Add a polyline point (open or closed)
#
#  @param dd Digitized data structure
#  @param x X coordinate
#  @param y Y coordinate
#  @param ptype Type: "open" or "closed"
#  @param pid Partition ID (default: 1)
#  @return Updated digitized data structure
add_polyline_point <- function(dd, x, y, ptype, pid = 1) {
  lname <- paste0("polylines_", ptype)
  # Ensure partition exists
  while (length(dd[[lname]]) < pid) {
    dd[[lname]][[length(dd[[lname]]) + 1]] <- data.frame(x = numeric(0), y = numeric(0))
  }
  # Add point to partition
  dd[[lname]][[pid]] <- rbind(dd[[lname]][[pid]], data.frame(x = x, y = y))
  dd
}

#  Add a curve point with automatic corner detection
#
#  Automatically detects corners based on angle threshold between consecutive segments.
#
#  @param dd Digitized data structure
#  @param x X coordinate
#  @param y Y coordinate
#  @param ctype Type: "open" or "closed"
#  @param pid Partition ID (default: 1)
#  @return Updated digitized data structure
add_curve_point <- function(dd, x, y, ctype, pid = 1) {
  lname <- paste0("curves_", ctype)
  # Ensure partition exists
  while (length(dd[[lname]]) < pid) {
    dd[[lname]][[length(dd[[lname]]) + 1]] <- data.frame(x = numeric(0), y = numeric(0), is_corner = logical(0))
  }
  df <- dd[[lname]][[pid]]
  # Detect if this point is a corner
  is_corner <- FALSE
  if (nrow(df) >= 2) {
    is_corner <- detect_corner(df$x, df$y, x, y, angle_threshold = 30)
  }
  # Add point with corner flag
  dd[[lname]][[pid]] <- rbind(df, data.frame(x = x, y = y, is_corner = is_corner))
  dd
}

#  Detect if a new point forms a corner
#
#  Uses angle between vectors to determine if point is a corner.
#
#  @param xs X coordinates of existing points
#  @param ys Y coordinates of existing points
#  @param new_x X coordinate of new point
#  @param new_y Y coordinate of new point
#  @param angle_threshold Angle threshold in degrees (default: 30)
#  @return TRUE if corner detected, FALSE otherwise
detect_corner <- function(xs, ys, new_x, new_y, angle_threshold = 30) {
  if (length(xs) < 2) return(FALSE)

  # Get last three points
  p1_x <- xs[length(xs) - 1]
  p1_y <- ys[length(ys) - 1]
  p2_x <- xs[length(xs)]
  p2_y <- ys[length(ys)]
  p3_x <- new_x
  p3_y <- new_y

  # Calculate vectors
  v1_x <- p1_x - p2_x
  v1_y <- p1_y - p2_y
  v2_x <- p3_x - p2_x
  v2_y <- p3_y - p2_y

  # Calculate vector lengths
  len1 <- sqrt(v1_x^2 + v1_y^2)
  len2 <- sqrt(v2_x^2 + v2_y^2)
  if (len1 < 1e-6 || len2 < 1e-6) return(FALSE)

  # Normalize vectors
  v1_x <- v1_x / len1
  v1_y <- v1_y / len1
  v2_x <- v2_x / len2
  v2_y <- v2_y / len2

  # Calculate angle between vectors
  dot <- pmax(-1, pmin(1, v1_x * v2_x + v1_y * v2_y))
  angle <- acos(dot) * 180 / pi

  return(angle < angle_threshold)
}

# ============================================================================
# POINT MANIPULATION FUNCTIONS
# ============================================================================

#  Update point coordinates
#
#  @param dd Digitized data structure
#  @param dtype Data type (e.g., "landmarks", "polylines_open")
#  @param pid Partition ID
#  @param ptidx Point index
#  @param x New X coordinate
#  @param y New Y coordinate
#  @return Updated digitized data structure
update_point <- function(dd, dtype, pid, ptidx, x, y) {
  if (pid > length(dd[[dtype]]) || ptidx > nrow(dd[[dtype]][[pid]])) return(dd)
  dd[[dtype]][[pid]][ptidx, c("x", "y")] <- c(x, y)
  dd
}

#  Toggle corner flag on a curve point
#
#  @param dd Digitized data structure
#  @param dtype Data type (must be curves_open or curves_closed)
#  @param pid Partition ID
#  @param ptidx Point index
#  @return Updated digitized data structure
toggle_corner <- function(dd, dtype, pid, ptidx) {
  if (pid > length(dd[[dtype]]) || ptidx > nrow(dd[[dtype]][[pid]])) return(dd)
  df <- dd[[dtype]][[pid]]
  if (!"is_corner" %in% names(df)) df$is_corner <- FALSE
  df[ptidx, "is_corner"] <- !df[ptidx, "is_corner"]
  dd[[dtype]][[pid]] <- df
  dd
}

#  Delete a point
#
#  @param dd Digitized data structure
#  @param dtype Data type
#  @param pid Partition ID
#  @param ptidx Point index
#  @return Updated digitized data structure
delete_point <- function(dd, dtype, pid, ptidx) {
  if (pid > length(dd[[dtype]]) || ptidx > nrow(dd[[dtype]][[pid]])) return(dd)
  dd[[dtype]][[pid]] <- dd[[dtype]][[pid]][-ptidx, ]
  dd
}

#  Insert a point at a specific index
#
#  @param dd Digitized data structure
#  @param dtype Data type
#  @param pid Partition ID
#  @param insert_idx Index where to insert
#  @param x X coordinate
#  @param y Y coordinate
#  @param is_corner Whether point is a corner (for curves only)
#  @return Updated digitized data structure
insert_point <- function(dd, dtype, pid, insert_idx, x, y, is_corner = FALSE) {
  if (pid > length(dd[[dtype]])) return(dd)
  df <- dd[[dtype]][[pid]]

  # Create new row based on data type
  if (dtype %in% c("curves_open", "curves_closed")) {
    new_row <- data.frame(x = x, y = y, is_corner = is_corner)
  } else {
    new_row <- data.frame(x = x, y = y)
  }

  # Validate insert index
  if (insert_idx > nrow(df) + 1) insert_idx <- nrow(df) + 1

  # Insert at appropriate position
  if (nrow(df) == 0) {
    dd[[dtype]][[pid]] <- new_row
  } else if (insert_idx > nrow(df)) {
    dd[[dtype]][[pid]] <- rbind(df, new_row)
  } else {
    dd[[dtype]][[pid]] <- rbind(df[1:(insert_idx-1),], new_row, df[insert_idx:nrow(df),])
  }

  rownames(dd[[dtype]][[pid]]) <- NULL
  dd
}

#  Remove the last point from a partition
#
#  @param dd Digitized data structure
#  @param dtype Data type
#  @param pid Partition ID
#  @return Updated digitized data structure
remove_last_point <- function(dd, dtype, pid) {
  if (pid > length(dd[[dtype]]) || nrow(dd[[dtype]][[pid]]) == 0) return(dd)
  dd[[dtype]][[pid]] <- dd[[dtype]][[pid]][-nrow(dd[[dtype]][[pid]]), ]
  dd
}

# ============================================================================
# PARTITION MANAGEMENT FUNCTIONS
# ============================================================================

#  Create a new empty partition
#
#  @param dd Digitized data structure
#  @param dtype Data type
#  @return Updated digitized data structure
create_partition <- function(dd, dtype) {
  if (dtype %in% c("curves_open", "curves_closed")) {
    dd[[dtype]][[length(dd[[dtype]]) + 1]] <- data.frame(x = numeric(0), y = numeric(0), is_corner = logical(0))
  } else {
    dd[[dtype]][[length(dd[[dtype]]) + 1]] <- data.frame(x = numeric(0), y = numeric(0))
  }
  dd
}

#  Delete an entire partition
#
#  @param dd Digitized data structure
#  @param dtype Data type
#  @param pid Partition ID
#  @return Updated digitized data structure
delete_partition <- function(dd, dtype, pid) {
  if (pid <= length(dd[[dtype]])) dd[[dtype]] <- dd[[dtype]][-pid]
  dd
}

#  Count non-empty partitions for each data type
#
#  @param dd Digitized data structure
#  @return Named list with counts for each data type
count_partitions <- function(dd) {
  count_with_points <- function(partitions) {
    if (length(partitions) == 0) return(0)
    sum(sapply(partitions, function(p) nrow(p) > 0))
  }
  list(
    landmarks = count_with_points(dd$landmarks),
    polylines_open = count_with_points(dd$polylines_open),
    polylines_closed = count_with_points(dd$polylines_closed),
    curves_open = count_with_points(dd$curves_open),
    curves_closed = count_with_points(dd$curves_closed)
  )
}

# ============================================================================
# DATA VALIDATION FUNCTIONS
# ============================================================================

#  Check if digitized data structure has any points
#
#  @param dd Digitized data structure
#  @return TRUE if any points exist, FALSE otherwise
has_digitized_data <- function(dd) {
  count_total_points <- function(partitions) {
    if (length(partitions) == 0) return(0)
    sum(sapply(partitions, function(p) nrow(p)))
  }
  total <- count_total_points(dd$landmarks) +
    count_total_points(dd$polylines_open) +
    count_total_points(dd$polylines_closed) +
    count_total_points(dd$curves_open) +
    count_total_points(dd$curves_closed)
  total > 0
}

#  Check if a JSON file contains complete digitized data
#
#  @param json_path Path to JSON file
#  @return TRUE if file exists and contains data, FALSE otherwise
is_json_file_complete <- function(json_path) {
  if (!file.exists(json_path)) return(FALSE)
  tryCatch({
    loaded_data <- load_from_json(json_path)
    dd <- loaded_data$digitized
    has_digitized_data(dd)
  }, error = function(e) FALSE)
}

# ============================================================================
# JSON IMPORT/EXPORT FUNCTIONS
# ============================================================================

#  Export digitized data to JSON file
#
#  Saves data with abbreviated field names for compact storage:
#  - landmarks -> ldk
#  - polylines_open -> ply
#  - polylines_closed -> pgn
#  - curves_open -> crv
#  - curves_closed -> out
#
#  @param dd Digitized data structure
#  @param img_path Path to image file
#  @param out_dir Output directory (default: same as image)
#  @param comment Comment string to include
#  @return NULL (invisible)
export_to_json <- function(dd, img_path, out_dir = NULL, comment = "") {
  if (is.null(out_dir)) out_dir <- dirname(img_path)

  has_data <- has_digitized_data(dd)
  has_comment <- nchar(trimws(comment)) > 0

  # Only save if there's data OR a comment
  if (!has_data && !has_comment) {
    json_path <- paste0(tools::file_path_sans_ext(img_path), ".json")
    if (file.exists(json_path)) file.remove(json_path)
    return(invisible(NULL))
  }

  json_file <- paste0(tools::file_path_sans_ext(basename(img_path)), ".json")
  json_path <- file.path(out_dir, json_file)

  # Convert data frames to list format, skip empty partitions
  to_list <- function(pl, inc_corner = FALSE) {
    result <- lapply(pl, function(df) {
      if (nrow(df) == 0) return(NULL)
      if (inc_corner) {
        lapply(seq_len(nrow(df)), function(i) {
          list(x = as.numeric(df[i, "x"]),
               y = as.numeric(df[i, "y"]),
               is_corner = as.logical(df[i, "is_corner"]))
        })
      } else {
        lapply(seq_len(nrow(df)), function(i) {
          list(x = as.numeric(df[i, "x"]), y = as.numeric(df[i, "y"]))
        })
      }
    })
    Filter(Negate(is.null), result)
  }

  # Ensure comment is a clean string
  clean_comment <- if (is.character(comment) && length(comment) == 1) comment else ""

  # Create JSON with abbreviated field names
  json_data <- list(
    image = basename(img_path),
    comment = clean_comment,
    ldk = to_list(dd$landmarks),
    ply = to_list(dd$polylines_open),
    pgn = to_list(dd$polylines_closed),
    crv = to_list(dd$curves_open, TRUE),
    out = to_list(dd$curves_closed, TRUE)
  )

  tryCatch({
    write_json(json_data, json_path, pretty = TRUE);
    cat("Saved:", json_path, "\n")
  }, error = function(e) cat("Error:", e$message, "\n"))
}

#  Load digitized data from JSON file
#
#  Reads JSON with abbreviated field names and converts to internal structure.
#
#  @param json_path Path to JSON file
#  @return List with digitized data structure and comment
load_from_json <- function(json_path) {
  if (!file.exists(json_path)) {
    return(list(digitized = init_digitized_data(), comment = ""))
  }

  tryCatch({
    jd <- read_json(json_path)

    # Convert list format to data frames
    from_list <- function(pl, inc_corner = FALSE) {
      if (length(pl) == 0) return(list())
      lapply(pl, function(pts) {
        if (is.null(pts) || length(pts) == 0) {
          if (inc_corner) {
            data.frame(x = numeric(0), y = numeric(0), is_corner = logical(0))
          } else {
            data.frame(x = numeric(0), y = numeric(0))
          }
        } else {
          if (!is.list(pts[[1]])) pts <- list(pts)
          if (inc_corner) {
            df <- do.call(rbind, lapply(pts, function(p) {
              data.frame(
                x = as.numeric(p$x %||% 0),
                y = as.numeric(p$y %||% 0),
                is_corner = as.logical(p$is_corner %||% FALSE)
              )
            }))
          } else {
            df <- do.call(rbind, lapply(pts, function(p) {
              data.frame(x = as.numeric(p$x %||% 0), y = as.numeric(p$y %||% 0))
            }))
          }
          rownames(df) <- NULL
          df
        }
      })
    }

    # Load with abbreviated field names
    digitized <- list(
      landmarks = from_list(jd$ldk %||% list()),
      polylines_open = from_list(jd$ply %||% list()),
      polylines_closed = from_list(jd$pgn %||% list()),
      curves_open = from_list(jd$crv %||% list(), TRUE),
      curves_closed = from_list(jd$out %||% list(), TRUE)
    )

    # Handle comment - ensure it's a clean string
    comment <- jd$comment %||% ""
    if (is.list(comment)) comment <- ""
    if (!is.character(comment)) comment <- as.character(comment)
    comment <- if (length(comment) > 0) comment[1] else ""

    list(digitized = digitized, comment = comment)
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    list(digitized = init_digitized_data(), comment = "")
  })
}

# ============================================================================
# JAVASCRIPT COMMUNICATION FUNCTIONS
# ============================================================================

#  Convert digitized data to JavaScript format
#
#  Prepares data for transmission to canvas JavaScript component.
#
#  @param dd Digitized data structure
#  @return List formatted for JavaScript consumption
data_to_js <- function(dd) {
  to_js <- function(pl, inc_corner = FALSE) {
    lapply(seq_along(pl), function(pid) {
      df <- pl[[pid]]
      if (nrow(df) == 0) return(NULL)
      if (inc_corner) {
        coords <- lapply(seq_len(nrow(df)), function(i) {
          list(
            x = as.numeric(df[i, "x"]),
            y = as.numeric(df[i, "y"]),
            is_corner = as.logical(df[i, "is_corner"])
          )
        })
      } else {
        coords <- lapply(seq_len(nrow(df)), function(i) {
          list(x = as.numeric(df[i, "x"]), y = as.numeric(df[i, "y"]))
        })
      }
      list(partition_id = pid, coords = coords)
    }) %>% Filter(Negate(is.null), .)
  }

  list(
    landmarks = to_js(dd$landmarks),
    polylines_open = to_js(dd$polylines_open),
    polylines_closed = to_js(dd$polylines_closed),
    curves_open = to_js(dd$curves_open, TRUE),
    curves_closed = to_js(dd$curves_closed, TRUE)
  )
}

#  Convert JavaScript data to internal digitized data structure
#
#  Receives data from canvas JavaScript component.
#
#  @param js_data Data from JavaScript
#  @return Digitized data structure
js_to_dd <- function(js_data) {
  if (is.null(js_data)) return(init_digitized_data())

  from_js <- function(pl, inc_corner = FALSE) {
    if (is.null(pl)) return(list())
    lapply(pl, function(part) {
      if (is.null(part$coords) || length(part$coords) == 0) {
        if (inc_corner) {
          data.frame(x = numeric(0), y = numeric(0), is_corner = logical(0))
        } else {
          data.frame(x = numeric(0), y = numeric(0))
        }
      } else {
        coords_list <- part$coords
        if (inc_corner) {
          df <- do.call(rbind, lapply(coords_list, function(c) {
            data.frame(
              x = as.numeric(c$x),
              y = as.numeric(c$y),
              is_corner = as.logical(c$is_corner %||% FALSE)
            )
          }))
        } else {
          df <- do.call(rbind, lapply(coords_list, function(c) {
            data.frame(x = as.numeric(c$x), y = as.numeric(c$y))
          }))
        }
        rownames(df) <- NULL
        df
      }
    }) %>% Filter(function(df) nrow(df) > 0, .)
  }

  list(
    landmarks = from_js(js_data$landmarks),
    polylines_open = from_js(js_data$polylines_open),
    polylines_closed = from_js(js_data$polylines_closed),
    curves_open = from_js(js_data$curves_open, TRUE),
    curves_closed = from_js(js_data$curves_closed, TRUE)
  )
}

# ============================================================================
# SHINY SERVER
# ============================================================================

server <- function(input, output, session) {

  # Reactive values store
  rv <- reactiveValues(
    batch = NULL,                          # List of image paths
    current_idx = 1,                       # Current image index
    current_digitized = init_digitized_data(),  # Current digitization data
    mode = "landmarks",                    # Current digitization mode
    active_partition = 1,                  # Active partition ID
    image_base64 = NULL,                   # Base64 encoded image
    image_name = "",                       # Current image filename
    image_path = "",                       # Current image path
    image_dir = NULL,                      # Image directory path
    saved_state = "clean",                 # Track if changes are saved
    zoom_initial = 1,                      # Initial zoom level
    pan_x_initial = 0,                     # Initial pan X
    pan_y_initial = 0,                     # Initial pan Y
    filter_mode = "all",                   # Filter: "all", "done", "undone"
    action_text = "",                      # Action bar text
    is_shuffled = FALSE,                   # Shuffle mode flag
    comment = ""                           # Current comment
  )

  # Configure folder browser roots
  roots <- c(
    Home = fs::path_home(),
    Desktop = "~/Desktop",
    Documents = "~/Documents",
    Downloads = "~/Downloads"
  )
  shinyDirChoose(input, "folder_browser", roots = roots, filetypes = c(""))

  # ---------------------------------------------------------------------------
  # IMAGE LOADING
  # ---------------------------------------------------------------------------

  #  Load current image and its associated JSON data
  load_current_image <- function() {
    if (is.null(rv$batch) || rv$current_idx > rv$batch$n) return()

    rv$image_path <- rv$batch$paths[rv$current_idx]
    rv$image_name <- basename(rv$image_path)
    rv$active_partition <- 1
    rv$comment <- ""

    cat("=== Loading image", rv$current_idx, "/", rv$batch$n, ":", rv$image_name, "===\n")

    # Load existing JSON if available
    json_path <- paste0(tools::file_path_sans_ext(rv$image_path), ".json")
    loaded_data <- load_from_json(json_path)
    rv$current_digitized <- loaded_data$digitized
    rv$comment <- loaded_data$comment

    # Load and encode image
    tryCatch({
      raw_img <- readBin(rv$image_path, "raw", file.info(rv$image_path)$size)
      b64_string <- base64enc::base64encode(raw_img)
      ext <- tolower(tools::file_ext(rv$image_path))
      mime_type <- switch(
        ext,
        jpg = "image/jpeg",
        jpeg = "image/jpeg",
        png = "image/png",
        tiff = "image/tiff",
        tif = "image/tiff",
        "image/jpeg"
      )
      rv$image_base64 <- paste0("data:", mime_type, ";base64,", b64_string)

      # Send to canvas
      session$sendCustomMessage("load_image", list(
        image = rv$image_base64,
        digitized = data_to_js(rv$current_digitized)
      ))
      session$sendCustomMessage("reset_view", list())
    }, error = function(e) {
      cat("Error:", e$message, "\n")
      shinyjs::alert(paste("Error:", e$message))
    })

    rv$saved_state <- "clean"
  }

  # ---------------------------------------------------------------------------
  # FILTER MANAGEMENT
  # ---------------------------------------------------------------------------

  #  Apply current filter to image list
  apply_filter <- function() {
    if (is.null(rv$image_dir) || !dir.exists(rv$image_dir)) return()

    valid_exts <- c("jpg", "jpeg", "png", "tiff", "tif")
    all_files <- list.files(rv$image_dir, full.names = TRUE)
    image_files <- all_files[tolower(tools::file_ext(all_files)) %in% valid_exts]
    image_files <- sort(image_files)

    # Apply filter
    if (rv$filter_mode == "done") {
      has_json <- sapply(image_files, function(f) {
        is_json_file_complete(paste0(tools::file_path_sans_ext(f), ".json"))
      })
      image_files <- image_files[has_json]
    } else if (rv$filter_mode == "undone") {
      has_json <- sapply(image_files, function(f) {
        is_json_file_complete(paste0(tools::file_path_sans_ext(f), ".json"))
      })
      image_files <- image_files[!has_json]
    }

    if (length(image_files) == 0) {
      shinyjs::alert("No images found for this filter")
      return()
    }

    # Apply shuffle if enabled
    if (rv$is_shuffled) image_files <- sample(image_files)

    rv$batch <- list(paths = image_files, n = length(image_files), current_idx = 1)
    rv$current_idx <- 1
    load_current_image()
  }

  # ---------------------------------------------------------------------------
  # AUTO-LOAD ON STARTUP
  # ---------------------------------------------------------------------------

  observe({
    if (exists(".momacs_folder", envir = .GlobalEnv)) {
      folder_path <- get(".momacs_folder", envir = .GlobalEnv)
      cat("\nAuto-loading folder:", folder_path, "\n")

      valid_exts <- c("jpg", "jpeg", "png", "tiff", "tif")
      all_files <- list.files(folder_path, full.names = TRUE)
      image_files <- all_files[tolower(tools::file_ext(all_files)) %in% valid_exts]

      if (length(image_files) > 0) {
        rv$batch <- list(paths = sort(image_files), n = length(image_files), current_idx = 1)
        rv$image_dir <- folder_path
        rv$filter_mode <- "all"
        rv$current_idx <- 1
        load_current_image()
        cat("Batch loaded:", rv$batch$n, "images\n")
      } else {
        cat("No image files found\n")
      }

      rm(".momacs_folder", envir = .GlobalEnv)
    }
  })

  # ---------------------------------------------------------------------------
  # FOLDER BROWSER
  # ---------------------------------------------------------------------------

  observeEvent(input$folder_browser, {
    tryCatch({
      folder_path <- shinyFiles::parseDirPath(roots, input$folder_browser)
      if (is.null(folder_path) || length(folder_path) == 0) return()
      if (!dir.exists(folder_path)) {
        shinyjs::alert(paste("Folder does not exist:", folder_path))
        return()
      }

      valid_exts <- c("jpg", "jpeg", "png", "tiff", "tif")
      all_files <- list.files(folder_path, full.names = TRUE)
      image_files <- all_files[tolower(tools::file_ext(all_files)) %in% valid_exts]

      if (length(image_files) == 0) {
        shinyjs::alert("No image files found")
        return()
      }

      rv$batch <- list(paths = sort(image_files), n = length(image_files), current_idx = 1)
      rv$image_dir <- folder_path
      rv$filter_mode <- "all"
      rv$current_idx <- 1
      load_current_image()
    }, error = function(e) shinyjs::alert(paste("Error:", e$message)))
  })

  # ---------------------------------------------------------------------------
  # CANVAS INTERACTION HANDLERS
  # ---------------------------------------------------------------------------

  observeEvent(input$canvas_click, {
    click <- input$canvas_click
    if (is.null(click)) return()

    x <- as.numeric(click$x)
    y <- as.numeric(click$y)

    # Add point based on current mode
    if (rv$mode == "landmarks") {
      rv$current_digitized <- add_landmark(rv$current_digitized, x, y, rv$active_partition)
    } else if (rv$mode == "polylines_open") {
      rv$current_digitized <- add_polyline_point(rv$current_digitized, x, y, "open", rv$active_partition)
    } else if (rv$mode == "polylines_closed") {
      rv$current_digitized <- add_polyline_point(rv$current_digitized, x, y, "closed", rv$active_partition)
    } else if (rv$mode == "curves_open") {
      rv$current_digitized <- add_curve_point(rv$current_digitized, x, y, "open", rv$active_partition)
    } else if (rv$mode == "curves_closed") {
      rv$current_digitized <- add_curve_point(rv$current_digitized, x, y, "closed", rv$active_partition)
    }

    rv$saved_state <- "dirty"
    session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
  })

  observeEvent(input$canvas_undo, {
    js_data <- input$canvas_undo$digitized
    if (!is.null(js_data)) {
      rv$current_digitized <- js_to_dd(js_data)
      rv$saved_state <- "dirty"
      # Send back without adding to history (prevents ghost points)
      session$sendCustomMessage("update_digitized_no_history", js_data)
    }
  })

  # ---------------------------------------------------------------------------
  # MODE SELECTION
  # ---------------------------------------------------------------------------

  observeEvent(input$mode_landmark, {
    rv$mode <- "landmarks"
    session$sendCustomMessage("set_mode", "landmarks")
  })
  observeEvent(input$mode_polyline, {
    rv$mode <- "polylines_open"
    session$sendCustomMessage("set_mode", "polylines_open")
  })
  observeEvent(input$mode_polygon, {
    rv$mode <- "polylines_closed"
    session$sendCustomMessage("set_mode", "polylines_closed")
  })
  observeEvent(input$mode_curve_open, {
    rv$mode <- "curves_open"
    session$sendCustomMessage("set_mode", "curves_open")
  })
  observeEvent(input$mode_curve_closed, {
    rv$mode <- "curves_closed"
    session$sendCustomMessage("set_mode", "curves_closed")
  })

  # ---------------------------------------------------------------------------
  # SHUFFLE TOGGLE
  # ---------------------------------------------------------------------------

  output$shuffle_btn <- renderUI({
    actionButton(
      "toggle_shuffle",
      HTML('<img src="momx/svg/shuffle.svg" width="20" height="20" />'),
      title = "Toggle shuffle",
      class = if (rv$is_shuffled) "active" else ""
    )
  })

  observeEvent(input$toggle_shuffle, {
    rv$is_shuffled <- !rv$is_shuffled
    action_text <- if (rv$is_shuffled) {
      "Shuffle: ON - images randomized"
    } else {
      "Shuffle: OFF - sequential order"
    }
    rv$action_text <- action_text
    apply_filter()
  })

  # ---------------------------------------------------------------------------
  # COMMENT MODAL
  # ---------------------------------------------------------------------------

  observeEvent(input$comment_btn, {
    session$sendCustomMessage("comment_modal_opening", list())
    showModal(modalDialog(
      title = "Add or Edit Comment",
      textAreaInput(
        "modal_comment",
        label = NULL,
        value = rv$comment,
        rows = 5,
        placeholder = "Add notes about scale, treatment, specimen ID, etc."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_comment", "Save", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$save_comment, {
    rv$comment <- input$modal_comment
    rv$saved_state <- "dirty"
    removeModal()
    session$sendCustomMessage("comment_modal_closing", list())
  })

  # ---------------------------------------------------------------------------
  # CLEAR ALL
  # ---------------------------------------------------------------------------

  observeEvent(input$flush_partitions, {
    rv$current_digitized <- init_digitized_data()
    rv$saved_state <- "dirty"
    session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
  })

  # ---------------------------------------------------------------------------
  # ACTION STATUS
  # ---------------------------------------------------------------------------

  observeEvent(input$action_status, {
    rv$action_text <- input$action_status
  }, ignoreInit = TRUE)

  observeEvent(input$store_view_state, {
    rv$zoom_initial <- input$store_view_state$zoom
    rv$pan_x_initial <- input$store_view_state$panX
    rv$pan_y_initial <- input$store_view_state$panY
  })

  # ---------------------------------------------------------------------------
  # FILTER COUNTS
  # ---------------------------------------------------------------------------

  get_filter_counts <- reactive({
    if (is.null(rv$image_dir) || !dir.exists(rv$image_dir)) {
      return(list(all = 0, done = 0, undone = 0))
    }

    valid_exts <- c("jpg", "jpeg", "png", "tiff", "tif")
    all_files <- list.files(rv$image_dir, full.names = TRUE)
    image_files <- all_files[tolower(tools::file_ext(all_files)) %in% valid_exts]

    all_count <- length(image_files)
    done_count <- sum(sapply(image_files, function(f) {
      json_path <- paste0(tools::file_path_sans_ext(f), ".json")
      is_json_file_complete(json_path)
    }))
    undone_count <- all_count - done_count

    list(all = all_count, done = done_count, undone = undone_count)
  })

  # ---------------------------------------------------------------------------
  # FILTER BUTTONS
  # ---------------------------------------------------------------------------

  observeEvent(input$filter_all, {
    rv$filter_mode <- "all"
    session$sendCustomMessage("set_filter_mode", "all")
    apply_filter()
  })

  observeEvent(input$filter_done, {
    rv$filter_mode <- "done"
    session$sendCustomMessage("set_filter_mode", "done")
    apply_filter()
  })

  observeEvent(input$filter_undone, {
    rv$filter_mode <- "undone"
    session$sendCustomMessage("set_filter_mode", "undone")
    apply_filter()
  })

  # ---------------------------------------------------------------------------
  # PARTITION COUNTS UPDATE
  # ---------------------------------------------------------------------------

  observe({
    parts <- count_partitions(rv$current_digitized)
    session$sendCustomMessage("update_partition_counts", parts)
    session$sendCustomMessage("setup_partition_clicks", parts)
  })

  # ---------------------------------------------------------------------------
  # IMAGE NAVIGATION
  # ---------------------------------------------------------------------------

  observe({
    if (is.null(rv$batch)) return()
    for (i in 1:rv$batch$n) {
      local({
        idx <- i
        observeEvent(input[[paste0("jump_to_image_", idx)]], {
          if (is.null(rv$batch)) return()
          export_to_json(rv$current_digitized, rv$image_path, rv$image_dir, rv$comment)
          rv$current_idx <- idx
          load_current_image()
        })
      })
    }
  })

  # ---------------------------------------------------------------------------
  # PARTITION OPERATIONS
  # ---------------------------------------------------------------------------

  observeEvent(input$new_partition, {
    rv$current_digitized <- create_partition(rv$current_digitized, rv$mode)
    rv$active_partition <- length(rv$current_digitized[[rv$mode]])
    rv$saved_state <- "dirty"
    session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
  })

  observeEvent(input$delete_partition_by_point, {
    pt <- input$delete_partition_by_point
    if (!is.null(pt)) {
      rv$current_digitized <- delete_partition(rv$current_digitized, pt$data_type, pt$partition_id)
      rv$active_partition <- max(1, rv$active_partition - 1)
      rv$saved_state <- "dirty"
      session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
    }
  })

  # ---------------------------------------------------------------------------
  # POINT OPERATIONS
  # ---------------------------------------------------------------------------

  observeEvent(input$update_point, {
    pt <- input$update_point
    if (!is.null(pt)) {
      rv$current_digitized <- update_point(
        rv$current_digitized,
        pt$data_type,
        pt$partition_id,
        pt$point_idx,
        pt$x,
        pt$y
      )
      rv$saved_state <- "dirty"
    }
  })

  observeEvent(input$delete_point, {
    pt <- input$delete_point
    if (!is.null(pt)) {
      rv$current_digitized <- delete_point(
        rv$current_digitized,
        pt$data_type,
        pt$partition_id,
        pt$point_idx
      )
      rv$saved_state <- "dirty"
      session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
    }
  })

  observeEvent(input$insert_point, {
    pt <- input$insert_point
    if (!is.null(pt)) {
      rv$current_digitized <- insert_point(
        rv$current_digitized,
        pt$data_type,
        pt$partition_id,
        pt$insert_idx,
        pt$x,
        pt$y,
        pt$is_corner %||% FALSE
      )
      rv$saved_state <- "dirty"
      session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
    }
  })

  observeEvent(input$remove_last_point, {
    rv$current_digitized <- remove_last_point(
      rv$current_digitized,
      rv$mode,
      rv$active_partition
    )
    rv$saved_state <- "dirty"
    session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
  })

  observeEvent(input$toggle_corner, {
    pt <- input$toggle_corner
    if (!is.null(pt)) {
      rv$current_digitized <- toggle_corner(
        rv$current_digitized,
        pt$data_type,
        pt$partition_id,
        pt$point_idx
      )
      rv$saved_state <- "dirty"
      session$sendCustomMessage("update_digitized", data_to_js(rv$current_digitized))
    }
  })

  # ---------------------------------------------------------------------------
  # IMAGE NAVIGATION BUTTONS
  # ---------------------------------------------------------------------------

  observeEvent(input$next_image, {
    if (is.null(rv$batch)) return()
    export_to_json(rv$current_digitized, rv$image_path, rv$image_dir, rv$comment)
    if (rv$current_idx < rv$batch$n) {
      rv$current_idx <- rv$current_idx + 1
      load_current_image()
    }
  })

  observeEvent(input$prev_image, {
    if (is.null(rv$batch)) return()
    export_to_json(rv$current_digitized, rv$image_path, rv$image_dir, rv$comment)
    if (rv$current_idx > 1) {
      rv$current_idx <- rv$current_idx - 1
      load_current_image()
    }
  })

  # ---------------------------------------------------------------------------
  # UI OUTPUTS
  # ---------------------------------------------------------------------------

  output$shortcuts_display <- renderUI({
    if (file.exists("manual.md")) {
      md_content <- paste(readLines("manual.md"), collapse = "\n")
      HTML(markdown::markdownToHTML(text = md_content, fragment.only = TRUE))
    } else {
      p("Manual not available", style = "color: #999; font-style: italic;")
    }
  })

  output$filter_buttons <- renderUI({
    counts <- get_filter_counts()
    session$sendCustomMessage("update_filter_counts", counts)

    div(
      class = "nav-buttons",
      actionButton(
        "filter_all",
        HTML(sprintf(
          '<img src="momx/svg/all.svg" width="20" height="20" /><span class="partition-count">%d</span>',
          counts$all
        )),
        class = if (rv$filter_mode == "all") "active" else "",
        title = "All images"
      ),
      actionButton(
        "filter_undone",
        HTML(sprintf(
          '<img src="momx/svg/undone.svg" width="20" height="20" /><span class="partition-count">%d</span>',
          counts$undone
        )),
        class = if (rv$filter_mode == "undone") "active" else "",
        title = "Undone images"
      ),
      actionButton(
        "filter_done",
        HTML(sprintf(
          '<img src="momx/svg/done.svg" width="20" height="20" /><span class="partition-count">%d</span>',
          counts$done
        )),
        class = if (rv$filter_mode == "done") "active" else "",
        title = "Done images"
      )
    )
  })

  output$image_list <- renderUI({
    if (is.null(rv$batch)) return(NULL)

    valid_exts <- c("jpg", "jpeg", "png", "tiff", "tif")
    all_files <- list.files(rv$image_dir, full.names = TRUE)
    image_files <- all_files[tolower(tools::file_ext(all_files)) %in% valid_exts]
    image_files <- sort(image_files)

    # Apply current filter
    if (rv$filter_mode == "done") {
      has_json <- sapply(image_files, function(f) {
        is_json_file_complete(paste0(tools::file_path_sans_ext(f), ".json"))
      })
      image_files <- image_files[has_json]
    } else if (rv$filter_mode == "undone") {
      has_json <- sapply(image_files, function(f) {
        is_json_file_complete(paste0(tools::file_path_sans_ext(f), ".json"))
      })
      image_files <- image_files[!has_json]
    }

    if (length(image_files) == 0) return(NULL)

    image_names <- basename(image_files)
    list_items <- lapply(seq_along(image_files), function(i) {
      is_current <- (rv$image_path == image_files[i])
      actionLink(
        paste0("jump_to_image_", which(rv$batch$paths == image_files[i])),
        image_names[i],
        class = if (is_current) "image-item active" else "image-item"
      )
    })

    div(class = "image-list-container", do.call(tagList, list_items))
  })

  output$image_header <- renderUI({
    if (is.null(rv$batch)) return(NULL)

    stats <- input$canvas_stats
    dims_text <- if (is.null(stats)) "-, -" else sprintf("%d, %d", stats$imageWidth, stats$imageHeight)
    cursor_text <- if (is.null(stats)) "-, -" else sprintf("%d, %d", stats$cursorX, stats$cursorY)
    zoom_text <- if (is.null(stats)) "-" else stats$zoom

    # Generate inline summary of digitized data
    summarize_inline <- function(dd) {
      parts <- list()

      landmarks_with_data <- Filter(function(p) nrow(p) > 0, dd$landmarks)
      if (length(landmarks_with_data) > 0) {
        point_counts <- sapply(landmarks_with_data, nrow)
        counts_str <- paste0("(", paste(point_counts, collapse = ", "), ")")
        parts <- c(parts, sprintf(
          "%d landmark%s %s",
          length(landmarks_with_data),
          if (length(landmarks_with_data) > 1) "s" else "",
          counts_str
        ))
      }

      polylines_open_with_data <- Filter(function(p) nrow(p) > 0, dd$polylines_open)
      if (length(polylines_open_with_data) > 0) {
        point_counts <- sapply(polylines_open_with_data, nrow)
        counts_str <- paste0("(", paste(point_counts, collapse = ", "), ")")
        parts <- c(parts, sprintf(
          "%d polyline%s %s",
          length(polylines_open_with_data),
          if (length(polylines_open_with_data) > 1) "s" else "",
          counts_str
        ))
      }

      polylines_closed_with_data <- Filter(function(p) nrow(p) > 0, dd$polylines_closed)
      if (length(polylines_closed_with_data) > 0) {
        point_counts <- sapply(polylines_closed_with_data, nrow)
        counts_str <- paste0("(", paste(point_counts, collapse = ", "), ")")
        parts <- c(parts, sprintf(
          "%d polygon%s %s",
          length(polylines_closed_with_data),
          if (length(polylines_closed_with_data) > 1) "s" else "",
          counts_str
        ))
      }

      curves_open_with_data <- Filter(function(p) nrow(p) > 0, dd$curves_open)
      if (length(curves_open_with_data) > 0) {
        point_counts <- sapply(curves_open_with_data, nrow)
        counts_str <- paste0("(", paste(point_counts, collapse = ", "), ")")
        parts <- c(parts, sprintf(
          "%d curve%s %s",
          length(curves_open_with_data),
          if (length(curves_open_with_data) > 1) "s" else "",
          counts_str
        ))
      }

      curves_closed_with_data <- Filter(function(p) nrow(p) > 0, dd$curves_closed)
      if (length(curves_closed_with_data) > 0) {
        point_counts <- sapply(curves_closed_with_data, nrow)
        counts_str <- paste0("(", paste(point_counts, collapse = ", "), ")")
        parts <- c(parts, sprintf(
          "%d outline%s %s",
          length(curves_closed_with_data),
          if (length(curves_closed_with_data) > 1) "s" else "",
          counts_str
        ))
      }

      if (length(parts) == 0) return("")
      paste(" |>", paste(parts, collapse = " | "))
    }

    summary_text <- summarize_inline(rv$current_digitized)

    div(
      class = "info-bar",
      sprintf(
        "Current image: %s  |  Dimensions: [%s]  |  Cursor: (%s)  |  Zoom: %s%%%s",
        rv$image_name,
        dims_text,
        cursor_text,
        zoom_text,
        summary_text
      )
    )
  })

  output$action_bar <- renderUI({
    div(class = "action-bar", rv$action_text)
  })

  output$json_display <- renderText({
    toJSON(list(
      image = rv$image_name,
      comment = rv$comment,
      ldk = lapply(rv$current_digitized$landmarks, function(df) {
        if (nrow(df) > 0) apply(df, 1, as.list) else list()
      }),
      ply = lapply(rv$current_digitized$polylines_open, function(df) {
        if (nrow(df) > 0) apply(df, 1, as.list) else list()
      }),
      pgn = lapply(rv$current_digitized$polylines_closed, function(df) {
        if (nrow(df) > 0) apply(df, 1, as.list) else list()
      }),
      crv = lapply(rv$current_digitized$curves_open, function(df) {
        if (nrow(df) > 0) apply(df, 1, as.list) else list()
      }),
      out = lapply(rv$current_digitized$curves_closed, function(df) {
        if (nrow(df) > 0) apply(df, 1, as.list) else list()
      })
    ), pretty = TRUE)
  })
}

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  theme = bs_theme(preset = "darkly"),
  useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = "momx/momx_styles.css"
    ),
    tags$script(src = "momx/momx_digitizer.js")
  ),
  sidebar = sidebar(
    h5("Momacs", style = "font-weight: 700; letter-spacing: 0.5px; margin-bottom: 20px;"),

    # Folder and filter section
    div(
      class = "sidebar-section",
      div(
        class = "nav-buttons",
        shinyDirButton("folder_browser", "+", title = "Select folder"),
        uiOutput("shuffle_btn")
      ),
      uiOutput("filter_buttons"),
      uiOutput("image_list")
    ),

    # Navigation section
    div(
      class = "sidebar-section",
      div(
        class = "nav-buttons",
        actionButton("prev_image", "<-", title = "Previous"),
        actionButton("flush_partitions", ".", title = "Clear all"),
        actionButton("next_image", "->", title = "Next")
      )
    ),

    # Mode selection section
    div(
      class = "sidebar-section",
      div(
        class = "mode-grid-single",
        actionButton(
          "mode_landmark",
          HTML('<img src="momx/svg/landmarks.svg" width="22" height="22" />'),
          class = "mode-btn active",
          title = "Landmark (L)"
        ),
        actionButton(
          "mode_polyline",
          HTML('<img src="momx/svg/polyline.svg" width="22" height="22" />'),
          class = "mode-btn",
          title = "Polyline (P)"
        ),
        actionButton(
          "mode_polygon",
          HTML('<img src="momx/svg/polygon.svg" width="22" height="22" />'),
          class = "mode-btn",
          title = "Polygon (G)"
        ),
        actionButton(
          "mode_curve_open",
          HTML('<img src="momx/svg/curve.svg" width="22" height="22" />'),
          class = "mode-btn",
          title = "Curve (C)"
        ),
        actionButton(
          "mode_curve_closed",
          HTML('<img src="momx/svg/outline.svg" width="22" height="22" />'),
          class = "mode-btn",
          title = "Outline (O)"
        )
      ),
      actionButton(
        "comment_btn",
        HTML('<img src="momx/svg/comment.svg" width="20" height="20" />'),
        title = "Add or edit comment"
      )
    ),

    # Footer
    div(
      class = "sidebar-footer",
      HTML("momacs is part of <a href='https://momx.github.io/' target='_blank'>momx</a> &bull; <a href='https://momx.github.io/momacs' target='_blank'>docs</a>")
    )
  ),

  # Main content area
  div(
    style = "height: calc(100vh - 250px); display: flex; flex-direction: column;",
    navset_tab(
      nav_panel(
        "Image",
        uiOutput("image_header"),
        uiOutput("action_bar"),
        div(
          class = "canvas-container",
          tags$canvas(id = "canvas", width = 800, height = 600)
        )
      ),
      nav_panel(
        "JSON",
        div(
          style = "padding: 20px; height: 100%; overflow-y: auto;",
          verbatimTextOutput("json_display", placeholder = TRUE)
        )
      ),
      nav_panel(
        "Manual",
        div(
          style = "padding: 20px; height: 100%; overflow-y: auto;",
          uiOutput("shortcuts_display")
        )
      )
    )
  )
)

