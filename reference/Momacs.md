# Launch Momacs Application

Starts the Shiny application for morphometric digitization of images.
Supports landmarks, polylines, polygons, curves, and closed outlines.

## Usage

``` r
momacs(folder_path = NULL)
```

## Arguments

- folder_path:

  Optional path to folder containing images to digitize. If NULL, use
  the Browse button in the app to select a folder.

## Value

Invisibly returns the Shiny app object

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch with folder selection in the app
momacs()

# Launch with a specific folder
momacs("~/my_images")
} # }
```
