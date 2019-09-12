#To run just this file
#test_file('tests/testthat/test_formatDataTables.R')

library(testthat)
context('ISCN3.R')

test_that('check ISCN3 download',{
  skip('run testISCN3 <- ISCN3() manually if desired')
  testISCN3 <- ISCN3()
})

test_that('check ISCN3 dimentions',{
  skip_if(!file.exists('~/Documents/Datasets/ISCN_3/'), 'ISCN3 folder not found') 
  testISCN3 <- SOCDRaH2::ISCN3(dataDir = '~/Documents/Datasets/ISCN_3/', orginalFormat=TRUE)
  
  expect_equal(dim(testISCN3$citation), c(67, 12))
  expect_equal(dim(testISCN3$dataset), c(45, 19))
  expect_equal(dim(testISCN3$profile), c(96722, 44))
  expect_equal(dim(testISCN3$layer), c(445829, 95))
  expect_equal(ncol(testISCN3$key), 5)
  
})

test_that('check ISCN3 key matches table headers',{
  skip_if(!file.exists('~/Documents/Datasets/ISCN_3/'), 'ISCN3 folder not found') 
  testISCN3 <- SOCDRaH2::ISCN3(dataDir = '~/Documents/Datasets/ISCN_3/', orginalFormat=TRUE)
  
  expect_equal(names(testISCN3$key), c("table", "header", "variable", "type", "entry"))
  expect_equal(names(testISCN3$citation), testISCN3$key[table == 'citation', header])
  expect_equal(names(testISCN3$dataset), testISCN3$key[table == 'dataset', header])
  expect_equal(names(testISCN3$profile), testISCN3$key[table == 'profile', header])
  expect_true(names(testISCN3$layer) %in% testISCN3$key[table == 'layer', header])
  
  #expect_equal(dim(testISCN3$layer), c(445829, 95))
  #expect_equal(ncol(testISCN3$key), 5)
})

test_that('check that long format runs', {
  skip('run as manual check, long runtime')
  skip_if(!file.exists('~/Documents/Datasets/ISCN_3/'), 'ISCN3 folder not found') 
  ISCN3.long <- ISCN3(dataDir = '~/Documents/Datasets/ISCN_3/')
  
})