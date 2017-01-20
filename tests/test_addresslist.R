source('./app/addresslist.R')

testthat::test_that("numbers are described", {
  testthat::expect_equal(describe_numbers(c(1)), "1")
  testthat::expect_equal(describe_numbers(c(1:8)), "1-8")
  testthat::expect_equal(describe_numbers(c(1:5, 7:8)), "1-5, 7-8")
  testthat::expect_equal(describe_numbers(c(1:5, 7:8, 6)), "1-8")
  testthat::expect_equal(describe_numbers(c(1:5, 7:8, 10, 12)), "1-5, 7-8, 10, 12")
})

addresses1 = data.frame(
  street=rep('Teststr.', 12),
  no=c(1:5, 7, 9, 11:15)
)

addresses2 = data.frame(
  street=c(rep('Teststr.', 12), 'Straßentest'),
  no=c(1:5, 7, 9, 11:15, 42)
)

testthat::test_that("to_addresslist", {
  testthat::expect_equal(to_addresslist(addresses1), c('Teststr. 1-5, 7, 9, 11-15'))
  testthat::expect_equal(sort(to_addresslist(addresses2)), sort(c('Teststr. 1-5, 7, 9, 11-15', 'Straßentest 42')))
})
