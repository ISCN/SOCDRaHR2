library(SoilDataR)
library(testthat)

#devtools::test() ##to run
context('Testing structure of dummy data')

test_that("known empty standard", {
  empty_snd <- processData_emptyStandard()
})


test_that("does data load",{
  temp <- processData_ISCN3('../data/ISCN3', warningMsg=FALSE)
})


empty_snd <- processData_emptyStandard()

testFormat <- list(ISCN3 = processData_ISCN3('../data/ISCN3', warningMsg=FALSE),
                   Treat2015 = processData_Treat2015('../data/Treat_2015'))

for(testCaseStr in names(testFormat)){

  temp <- testFormat[[testCaseStr]]

  test_that("does the main list have the standard data frames",{
    expect_true(all(names(empty_snd) %in% names(temp)), info=testCaseStr)
    expect_true(all(names(temp) %in% names(empty_snd)), info=testCaseStr)
  })

  test_that("do the data frames have the minimum columns",{
    expect_true(all(names(empty_snd$study) %in% names(temp$study)), info=testCaseStr)
    expect_true(all(names(empty_snd$field) %in% names(temp$field)), info=testCaseStr)
    expect_true(all(names(empty_snd$lab) %in% names(temp$lab)), info=testCaseStr)
    expect_true(all(names(empty_snd$fieldTreatment) %in% names(temp$fieldTreatment)), info=testCaseStr)
    expect_true(all(names(empty_snd$measurement) %in% names(temp$measurement)), info=testCaseStr)
    expect_true(all(names(empty_snd$sample) %in% names(temp$sample)), info=testCaseStr)
  })
}
