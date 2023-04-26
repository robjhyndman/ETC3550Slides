source("renv/activate.R")
options(languageserver.formatting_style = function(options) {
  styler::tidyverse_style(scope = "indention", indent_by = options$tabSize)
})

# Set some defaults
# Colours to be viridis for continuous scales and Okabe for discrete scales
options(
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)

ggplot2::theme_set(ggplot2::theme_get() + ggplot2::theme(text = ggplot2::element_text(family = 'Source Sans Pro')))
