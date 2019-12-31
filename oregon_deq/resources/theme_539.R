# file theme_539.R

# this file creates a variant of ggthemes' "fivethirtyeight" theme
# with a transparent background and a few other details changed.
# this theme is a good match for DEQ's official style guidelines,
# which require an Arial/Helvetica font

# it also defines a color palette based on DEQ preferred colors, 
# converted to R color names.

# creating a custom theme
theme_539 <- function() {
  theme_fivethirtyeight() +
  theme(rect=element_rect(fill="transparent"))
}

# creating a palette using "official" Oregon DEQ colors
# for colors and fills
deq_pal <- 
  c("aquamarine4", "steelblue4", "lightseagreen", 
    "yellowgreen", "sienna1", "darkseagreen", 
    "slateblue2", "powderblue", "khaki", "darksalmon", 
    "dodgerblue4", "darkslateblue", "maroon4", "cyan4", 
    "darkgoldenrod3")
names(deq_pal) <- deq_pal
