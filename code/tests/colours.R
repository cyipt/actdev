# colours_3 = c('#DFDCD9', '#F18D6F', '#B60B27')
# colorspace::demoplot(colours_3)
# 
# colours_5 = c('#DFDCD9', '#F7B295', '#F18D6F', '#D34C40', '#B60B27')
# colorspace::demoplot(colours_5)

colours_8 = c('#DFDCD9', '#F4C7B1', '#F7B295', '#F18D6F', '#E7735A', '#D34C40', '#C43032', '#B60B27')
# colorspace::demoplot(colours_8)
colorspace::hclplot(colours_8)
colorspace::cvd_emulator(colours_8)


# ramp = colorspace::choose_palette()
# colours = ramp(5)
# colours_10 = ramp(10)
# dput(colours_10)

colours = c("#A93154", "#BA4B8E", "#BC6EB9", "#B494D5", "#AEB6E5")
colours_10 = c("#A93154", "#B23A70", "#B94789", "#BC569E", "#BD66B0", "#BB77C0", 
            "#B788CD", "#B398D7", "#B0A8DF", "#AEB6E5")

colours
colours_10
colorspace::demoplot(colours)
colorspace::demoplot(colours_10)

colorspace::hclplot(colours_10)

# google
stplanr::geo_code("trumpington")

# nominatim
osmextract:::oe_search("trumpington")
