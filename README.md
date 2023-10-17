# ClickMetrics

This project is a R package. The aim of ClickMetrics is to help on the extraction of data by mouse clicking on images. First it will be covered common situations in agronomy/biology: length, area, counts.

This package has shiny dashboards built to help scientists measure things in images/photos, e.g. mycelial diameter, nematode body length, number of spores on plate dishes, damaged leaf area, distribution of point patterns. Based on coordinate of mouse clicks, the measures are obtained after data pos processing.

This package is under continuous development, so function names, function headers and return values, object classes, etc, can be modified at any moment to improve usability and cohesion.

To install the package from its GitHub repository, run the code below.

```
# Install `devtools` if you don't have it.
install.packages("devtools")

# Load `devtools` package.
library(devtools)

# Install from source on GitHub.
install_github("Kowalsq/ClickMetrics")

# Load `ClickMetrics`.
library(ClickMetrics)
```
