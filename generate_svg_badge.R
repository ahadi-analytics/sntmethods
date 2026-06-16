# generate_svg_badge.R
# Creates a static SVG badge file

# Calculate coverage. Use covr::package_coverage(type = "tests") directly:
# it is the canonical programmatic API and respects .covrignore. (The
# interactive devtools::test_coverage() can report a misleading figure when
# run non-interactively.)
coverage_results <- covr::package_coverage(type = "tests")
percent <- round(covr::percent_coverage(coverage_results))

# Determine badge color
color <- dplyr::case_when(
  percent >= 70 ~ "#4CAF50",
  percent >= 60 ~ "#dfb317",
  percent >= 40 ~ "#fe7d37",
  TRUE ~ "#e05d44"
)

# Create SVG badge
badge_svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="104" height="20">',
  '<linearGradient id="b" x2="0" y2="100%">',
  '<stop offset="0" stop-color="#bbb" stop-opacity=".1"/>',
  '<stop offset="1" stop-opacity=".1"/>',
  '</linearGradient>',
  '<mask id="a">',
  '<rect width="104" height="20" rx="3" fill="#fff"/>',
  '</mask>',
  '<g mask="url(#a)">',
  '<path fill="#555" d="M0 0h63v20H0z"/>',
  '<path fill="',
  color,
  '" d="M63 0h41v20H63z"/>',
  '<path fill="url(#b)" d="M0 0h104v20H0z"/>',
  '</g>',
  '<g fill="#fff" text-anchor="middle" font-family="DejaVu Sans,Verdana,Geneva,sans-serif" font-size="11">',
  '<text x="31.5" y="15" fill="#010101" fill-opacity=".3">coverage</text>',
  '<text x="31.5" y="14">coverage</text>',
  '<text x="82.5" y="15" fill="#010101" fill-opacity=".3"> ',
  percent,
  '% </text>',
  '<text x="82.5" y="14"> ',
  percent,
  '% </text>',
  '</g>',
  '</svg>'
)

# Create badges directory
if (!dir.exists("man/badges")) {
  dir.create("man/badges")
}

# Save badge
writeLines(badge_svg, "man/badges/coverage.svg")
