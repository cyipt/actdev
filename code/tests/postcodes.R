txt = c("blurb blurb HR4 7BP",
        "blurb 34blurb",
        "LS2 9JT",
        " LS2 9JT",
        "LS2 9JT "
        )
rgx = r"(([A-Z][A-HJ-Y]?\d[A-Z\d]? ?\d[A-Z]{2}|GIR ?0A{2}))"
stringr::str_extract(txt, rgx)
