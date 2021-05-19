# dput(desire_line_town) getting lat long
origin = c(-0.214849749698027,51.5586119006974)
destination = c( -0.214888955159809,51.5564466729046)

a = cyclestreets::journey(
  origin,
  destination 
)
mapview::mapview(a)
