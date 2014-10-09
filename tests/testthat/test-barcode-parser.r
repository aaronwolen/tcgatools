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

barcodes <- list(tss         = "TCGA-02",
                 participant = "TCGA-02-0001",
                 drug        = "TCGA-02-0001-C1",
                 exam        = "TCGA-02-0001-E3124",
                 surgery     = "TCGA-02-0001-S145",
                 radiation   = "TCGA-02-0001-R2", 
                 sample      = "TCGA-02-0001-01",
                 portion     = "TCGA-02-0001-01C-01",
                 shipped     = "TCGA-CM-5341-01A-21-1933-20",
                 slide       = "TCGA-02-0001-01C-01-TS1",
                 analyte     = "TCGA-02-0001-01C-01D",
                 aliquote    = "TCGA-02-0001-01C-01D-0182-01")

test_that("TSS barcode parsed correctly", {
    expect <- names(.type$tss)
    parsed <- parse_barcodes(barcodes$tss)
    expect_identical(names(parsed), expect)
})

test_that("Participant barcode parsed correctly", {
  expect <- names(.type$participant)
  parsed <- parse_barcodes(barcodes$participant)
  expect_identical(names(parsed), expect)
})

test_that("Drug barcode parsed correctly", {
  expect <- names(.type$drug)
  parsed <- parse_barcodes(barcodes$drug)
  expect_identical(names(parsed), expect)
})

test_that("Examination barcode parsed correctly", {
  expect <- names(.type$exam)
  parsed <- parse_barcodes(barcodes$exam)
  expect_identical(names(parsed), expect)
})

test_that("Surgery barcode parsed correctly", {
  expect <- names(.type$surgery)
  parsed <- parse_barcodes(barcodes$surgery)
  expect_identical(names(parsed), expect)
})

test_that("Radiation barcode parsed correctly", {
  expect <- names(.type$radiation)
  parsed <- parse_barcodes(barcodes$radiation)
  expect_identical(names(parsed), expect)
})

test_that("Sample barcode parsed correctly", {
  expect <- names(.type$sample)
  parsed <- parse_barcodes(barcodes$sample)
  expect_identical(names(parsed), expect)
})

test_that("Portion barcode parsed correctly", {
  expect <- names(.type$portion)
  parsed <- parse_barcodes(barcodes$portion)
  expect_identical(names(parsed), expect)
})

test_that("Shipped portion barcode parsed correctly", {
  expect <- names(.type$aliquot)
  parsed <- parse_barcodes(barcodes$shipped)
  expect_identical(names(parsed), expect)
})

test_that("Slide barcode parsed correctly", {
  expect <- names(.type$slide)
  parsed <- parse_barcodes(barcodes$slide)
  expect_identical(names(parsed), expect)
})

test_that("Analyte barcode parsed correctly", {
  expect <- names(.type$portion)
  parsed <- parse_barcodes(barcodes$analyte)
  expect_identical(names(parsed), expect)
})

test_that("Aliquot barcode parsed correctly", {
  expect <- names(.type$aliquot)
  parsed <- parse_barcodes(barcodes$aliquote)
  expect_identical(names(parsed), expect)
})






