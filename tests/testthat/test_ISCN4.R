#To run just this file
#test_file('tests/testthat/test_formatDataTables.R')

library(testthat)
context('ISCN4.R')

test_that('check ISCN4 construction',{
  skip() 
  testISCN4 <- SOCDRaH2::ISCN4(dataDir = '~/Documents/Datasets/ISCN/', onlyNewData = TRUE)
})
