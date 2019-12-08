#To run just this file
#test_file('tests/testthat/test_formatDataTables.R')

library(testthat)
context('Make keys')

test_that('check current key names',{
  currentKeys <- makeKeys()
  testthat::expect_equivalent(names(currentKeys), c('ISCN', 'ISCN3', 'ISCN2016'))
})

test_that('check ISCN3 variables match ISCN master',{
  keys.ls <- makeKeys()
  testthat::expect_true(all(keys.ls$ISCN3[!is.na(variable), variable] %in% keys.ls$ISCN$variable))
})

test_that('check ISCN2016 variables are in ISCN master',{
  keys.ls <- makeKeys()
  testthat::expect_true(all(keys.ls$ISCN2016[!is.na(variable), variable] %in% keys.ls$ISCN$variable))
})