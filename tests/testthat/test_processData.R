library(SoilDataR)
context('Testing dummy data')

test_that("does data load",{
  temp <- processData_ISCN3('../data/ISCN3')
})
