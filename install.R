# Install required packages ----
package_list <- c(
  "tidyverse",
  "leaflet",
  "leaflet.extras",
  "leaflet.minicharts"
)

for(i in 1:length(package_list)) {
  install.packages(package_list[i])
}
