context("Barcode patterns")

test_that("tss pattern", {
  tss <- .barcodes$tissueSourceSite$TSS.Code
  expect_true(all(str_detect(tss, .pattern$tss)))
})


test_that("center pattern", {
  centers <- .barcodes$centerCode$Code
  expect_true(all(str_detect(centers, .pattern$center)))
})


test_that("sample pattern", {
  samples <- c("01", "02B")
  expect_true(all(str_detect(samples, .pattern$sample)))
  expect_false(str_detect("2B", .pattern$sample))
})



context("Parsed barcodes")

test_that("Aliquote barcodes are parsed correctly", {
    barcodes <- c('TCGA-EJ-7321-11A-01R-2263-07', 'TCGA-EJ-7321-11A-01R-2263-07')  
    expect <- c("project", "tss", "participant", "sample", "portion", "plate", "center")
    parsed <- parse_barcodes(barcodes)
    expect_identical(nrow(parsed), length(barcodes))
    expect_identical(names(parsed), expect)
})
