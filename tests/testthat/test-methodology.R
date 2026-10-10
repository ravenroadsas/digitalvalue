test_that("methodology renders in both languages with all placeholders resolved", {
  cfg <- test_cfg()
  for (lang in methodology_languages) {
    md <- methodology_markdown(lang, cfg)
    expect_false(grepl("{{", md, fixed = TRUE), info = lang)
    r <- render_methodology(md)
    expect_gt(nchar(r$html), 5000)
    expect_true(all(sprintf('id="%s"', r$toc$id) %in% regmatches(r$html, gregexpr('id="[^"]+"', r$html))[[1]]))
    expect_false(anyDuplicated(r$toc$id) > 0)
  }
})

test_that("English and Spanish have the same structure: summary, principles and every step", {
  cfg <- test_cfg()
  en <- render_methodology(methodology_markdown("en", cfg))$toc
  es <- render_methodology(methodology_markdown("es", cfg))$toc
  expect_equal(en$level, es$level)
  expect_equal(en$title[1:2], c("Summary", "Principles"))
  expect_equal(es$title[1:2], c("Resumen", "Principios"))
  for (k in 1:6) {
    expect_true(any(startsWith(en$title, paste(k, "·"))), info = k)
    expect_true(any(startsWith(es$title, paste(k, "·"))), info = k)
  }
})

test_that("numbers in the methodology follow the configuration", {
  cfg <- test_cfg()
  cfg$params$gate3_value_min_mm_usd <- 7.5
  cfg$params$netback_usd_bbl <- 42
  en <- methodology_markdown("en", cfg)
  es <- methodology_markdown("es", cfg)
  expect_match(en, "7.5 mm USD", fixed = TRUE)
  expect_match(es, "7,5 mm USD", fixed = TRUE)
  expect_match(en, "net back 42 USD/bbl", fixed = TRUE)
  v <- methodology_values(cfg, "en")
  expect_match(v[["rice_weights_table"]], "Moonshot = 0.2", fixed = TRUE)
  expect_match(methodology_values(cfg, "es")[["rice_weights_table"]], "Moonshot = 0,2", fixed = TRUE)
  expect_equal(v[["example_workover_bopd"]], "30")
  expect_error(methodology_markdown("fr", cfg))
})

test_that("unresolved placeholders are reported", {
  cfg <- test_cfg()
  d <- withr::local_tempdir()
  writeLines("## Summary\n\nValue {{unknown_key}}", file.path(d, "methodology_en.md"))
  expect_error(methodology_markdown("en", cfg, d), "unknown_key")
})
