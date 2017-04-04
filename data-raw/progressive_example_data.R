# Reads Peter's example file and converts colour specifications of the
# form 'rgb(123, 123, 123)' to hex values. 

path <- file.path("data-raw", "input2.txt")
bacteria <- read.delim(path, stringsAsFactors = FALSE)

# local version of rgb function
rgb <- function(r,g,b) grDevices::rgb(r,g,b, maxColorValue = 255)

# evaluate input colour specs using the local function
cols <- sapply( bacteria$colour, function(x) eval(parse(text = x)) )

# replace colour column and save to data directory
bacteria$colour <- cols
devtools::use_data(bacteria, overwrite = TRUE)