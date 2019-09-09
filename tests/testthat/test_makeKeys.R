#To run just this file
#test_file('tests/testthat/test_formatDataTables.R')

library(testthat)
context('makeKeys.R')

test_that('check current key names',{
  currentKeys <- makeKeys()
  testthat::expect_equivalent(names(currentKeys), c('ISCN', 'ISCN3'))
})

##TODO test ISCN3 keys have all the tables defined