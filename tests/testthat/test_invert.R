context("inversion")

test_that("Inverts correctly", {
  expect_equal(invertItem(1:4, 1, 4), 4:1)
  expect_equal(invertItem(3:-3, -4, 4), -3:3)
})

test_that("Inverter throws errors", {
  expect_error(invertItem(c(1:3, "4"), 1, 4))
  expect_error(invertItem(1:4, "1", 4))
  expect_error(invertItem(1:4, 1, "4"))
  expect_error(invertItem(1:4, 4, 1))
  expect_error(invertItem(1:5, 1, 4))

})
