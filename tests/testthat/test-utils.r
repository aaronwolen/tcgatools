context("Utilities")

test_that("extract_split returns data.frames", {
  
  options(stringsAsFactors = FALSE)
  
  ab2 <- data.frame(a = paste0(1, letters[1:2]), b = paste0(2, letters[1:2]))
  
  abc2 <- data.frame(a = ab2$a, b = "2", c = letters[1:2])
  acb2 <- data.frame(a = "1", c = letters[1:2], b = ab2$b)
  
  expect_equal(abc2, extract_split(ab2, "b", "c"))
  expect_equal(acb2, extract_split(ab2, "a", "c"))
  
  ab1 <- ab2[1, ]
  abc1 <- data.frame(a = ab1$a, b = "2", c = letters[1])
  acb1 <- data.frame(a = "1", c = letters[1], b = ab1$b)
  
  expect_equal(abc1, extract_split(ab1, "b", "c"))
  expect_equal(acb1, extract_split(ab1, "a", "c"))
})



