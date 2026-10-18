# ---------------------------------------------------------------------------
# Builds the saydo hex logo and the pkgdown favicons.
#
# The motif is the package in one picture: lines of transcript with
# NVivo-style coding stripes beside them, and on the right a small forest
# plot whose points share the stripes' colours, i.e. each code linked to its
# effect on a metric. Flat indigo risograph-style colours, a light "say" and a
# heavy "do" (the two halves of the motif), and neu-data.com along the
# lower-right edge in the style of the tidyverse stickers.
#
# Run from the package root:  source("data-raw/make_logo.R")
# Needs ragg, systemfonts, magick and jsonlite, and the Segoe UI font.
# ---------------------------------------------------------------------------

library(grid)

PALETTE <- "indigo"

systemfonts::register_variant("SegoeLight", "Segoe UI", weight = "light")
systemfonts::register_variant("SegoeHeavy", "Segoe UI", weight = "heavy")
systemfonts::register_variant("SegoeSemi", "Segoe UI", weight = "semibold")

W <- 518
H <- 600
CX <- 259
CY <- 300

al <- function(col, a) {
  m <- grDevices::col2rgb(col)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], alpha = a * 255, maxColorValue = 255)
}

# bg: sticker background; rim: border; ink: text lines, plot and name;
# card: panel behind the transcript; codes: the three stripe/point colours;
# ring: outline round the plot points.
palettes <- list(
  paper = list(bg = "#F3EBDD", rim = "#1F2A44", ink = "#1F2A44", card = al("#1F2A44", 0.06),
               codes = c("#E4572E", "#F2A541", "#4F8A6B"), ring = "#F3EBDD"),
  forest = list(bg = "#26463A", rim = "#16291F", ink = "#F3E9D2", card = al("#F3E9D2", 0.08),
                codes = c("#E9B44C", "#E4572E", "#9BC1BC"), ring = "#26463A"),
  terracotta = list(bg = "#C65D3B", rim = "#7E3420", ink = "#FBF1E3", card = al("#FBF1E3", 0.12),
                    codes = c("#F2C14E", "#2D3047", "#8DB596"), ring = "#C65D3B"),
  indigo = list(bg = "#27346B", rim = "#151D40", ink = "#F7F3E3", card = al("#F7F3E3", 0.08),
                codes = c("#FF6F59", "#FFC857", "#7FD1B9"), ring = "#27346B")
)

hex_pts <- function(r, cx = CX, cy = CY) {
  a <- seq(90, 390, by = 60)[1:6] * pi / 180
  list(x = cx + r * cos(a), y = cy + r * sin(a))
}

rline <- function(x0, x1, y, col, lwd) {
  grid.lines(c(x0, x1), c(y, y), default.units = "native",
             gp = gpar(col = col, lwd = lwd, lineend = "round"))
}

# Rounded rectangle drawn as a polygon (grid.roundrect misbehaves with
# native units).
rrect <- function(x, y, w, h, fill, r = 8, col = NA, lwd = 1) {
  r <- min(r, w / 2, h / 2)
  a <- seq(0, pi / 2, length.out = 12)
  cx <- c(x + w / 2 - r, x - w / 2 + r, x - w / 2 + r, x + w / 2 - r)
  cy <- c(y + h / 2 - r, y + h / 2 - r, y - h / 2 + r, y - h / 2 + r)
  px <- c(cx[1] + r * cos(a), cx[2] + r * cos(a + pi / 2),
          cx[3] + r * cos(a + pi), cx[4] + r * cos(a + 3 * pi / 2))
  py <- c(cy[1] + r * sin(a), cy[2] + r * sin(a + pi / 2),
          cy[3] + r * sin(a + pi), cy[4] + r * sin(a + 3 * pi / 2))
  grid.polygon(px, py, default.units = "native", gp = gpar(fill = fill, col = col, lwd = lwd))
}

draw_logo <- function(p) {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))

  o <- hex_pts(299)
  grid.polygon(o$x, o$y, default.units = "native", gp = gpar(fill = p$rim, col = NA))
  i <- hex_pts(284)
  grid.polygon(i$x, i$y, default.units = "native", gp = gpar(fill = p$bg, col = NA))

  # transcript card and lines
  rrect(196, 378, 236, 232, p$card, r = 22)
  ly <- seq(462, 294, length.out = 7)
  lw <- c(132, 104, 140, 88, 128, 112, 76)
  x0 <- 138
  for (k in seq_along(ly)) rline(x0, x0 + lw[k], ly[k], p$ink, 10)

  # coding stripes, each spanning the lines it codes
  spans <- list(c(1, 3), c(3, 5), c(5, 7))
  for (k in seq_along(spans)) {
    y1 <- ly[spans[[k]][1]] + 9
    y2 <- ly[spans[[k]][2]] - 9
    rrect(118 - (k - 1) * 17, (y1 + y2) / 2, 11, y1 - y2, p$codes[k], r = 5.5)
  }

  # forest plot: each code's effect on a metric, in the same colours
  zx <- 384
  grid.lines(c(zx, zx), c(282, 470), default.units = "native",
             gp = gpar(col = al(p$ink, 0.55), lwd = 3.5, lty = "22", lineend = "round"))
  est <- c(56, 24, -16)
  lo <- c(28, 0, -46)
  hi <- c(84, 48, 14)
  for (k in 1:3) {
    yy <- 436 - (k - 1) * 62
    rline(zx + lo[k], zx + hi[k], yy, p$codes[k], 7)
    grid.circle(zx + est[k], yy, r = unit(15, "native"), default.units = "native",
                gp = gpar(fill = p$codes[k], col = p$ring, lwd = 5))
  }

  # wordmark: light "say" + heavy "do", centred together
  size <- 96
  w1 <- convertWidth(grobWidth(textGrob("say", gp = gpar(fontfamily = "SegoeLight", fontsize = size))),
                     "native", valueOnly = TRUE)
  w2 <- convertWidth(grobWidth(textGrob("do", gp = gpar(fontfamily = "SegoeHeavy", fontsize = size))),
                     "native", valueOnly = TRUE)
  start <- CX - (w1 + w2 + 3) / 2
  grid.text("say", start, 176, just = c("left", "centre"), default.units = "native",
            gp = gpar(fontfamily = "SegoeLight", fontsize = size, col = al(p$ink, 0.8)))
  grid.text("do", start + w1 + 3, 176, just = c("left", "centre"), default.units = "native",
            gp = gpar(fontfamily = "SegoeHeavy", fontsize = size, col = p$ink))

  grid.text("neu-data.com", 386, 116, rot = 30, default.units = "native",
            gp = gpar(fontfamily = "SegoeSemi", fontsize = 15, col = al(p$ink, 0.7)))
  popViewport()
}

# High-resolution master, downsampled so edges are smooth.
render_master <- function(p, file) {
  ragg::agg_png(file, width = W * 4, height = H * 4, res = 288, background = "transparent")
  draw_logo(p)
  invisible(grDevices::dev.off())
  magick::image_read(file)
}

if (!isTRUE(getOption("saydo.logo_preview"))) {
  logo <- render_master(palettes[[PALETTE]], tempfile(fileext = ".png"))
  dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
  magick::image_write(magick::image_resize(logo, paste0(W, "x", H)), "man/figures/logo.png", format = "png")
  # pkgdown prefers logo.svg when it exists; the PNG is the master now.
  unlink("man/figures/logo.svg")

  # Favicons, cut from the master so they match it exactly.
  dir.create("pkgdown/favicon", showWarnings = FALSE, recursive = TRUE)
  square <- function(size) {
    img <- magick::image_resize(logo, paste0("x", size))
    magick::image_extent(img, paste0(size, "x", size), color = "none", gravity = "center")
  }
  magick::image_write(square(180), "pkgdown/favicon/apple-touch-icon.png", format = "png")
  magick::image_write(square(96), "pkgdown/favicon/favicon-96x96.png", format = "png")
  magick::image_write(square(192), "pkgdown/favicon/web-app-manifest-192x192.png", format = "png")
  magick::image_write(square(512), "pkgdown/favicon/web-app-manifest-512x512.png", format = "png")
  magick::image_write(magick::image_join(square(16), square(32), square(48)),
                      "pkgdown/favicon/favicon.ico", format = "ico")
  b64 <- function(path) {
    raw <- readBin(path, "raw", file.info(path)$size)
    paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
  }
  writeLines(sprintf(paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="96" height="96" viewBox="0 0 96 96">',
    '<image width="96" height="96" href="%s"/></svg>'), b64("pkgdown/favicon/favicon-96x96.png")),
    "pkgdown/favicon/favicon.svg")
  writeLines(c(
    "{",
    '  "name": "saydo",',
    '  "short_name": "saydo",',
    '  "icons": [',
    '    {"src": "/web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png", "purpose": "maskable"},',
    '    {"src": "/web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png", "purpose": "maskable"}',
    "  ],",
    sprintf('  "theme_color": "%s",', palettes[[PALETTE]]$rim),
    sprintf('  "background_color": "%s",', palettes[[PALETTE]]$bg),
    '  "display": "standalone"',
    "}"
  ), "pkgdown/favicon/site.webmanifest")
}
