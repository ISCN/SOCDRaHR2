#To run just this file
#test_file('tests/testthat/test_formatLongTable.R')

library(testthat)
context('formatLongTable.R')

testthat::test_that("test one variable one table",{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~SOC,
                               'river', 'S1', '5.2',
                               'lake', 'S2', '3')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                             'T1', 'siteID', 'site_name', 'id', '',
                             'T1', 'sampleID', 'layer_name', 'id', '',
                             'T1', 'SOC', 'soc', 'value', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                               'sample', 'site_name',
                               'sample', 'layer_name',
                               'sample', 'soc')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <- list(sample=
                           as.data.table(read.csv(text = '"layer_name_id","site_name_id","header","entry","variable","type"
"S1","river","SOC","5.2","soc","value"
"S2","lake","SOC","3","soc","value"', colClasses = 'character')))
  #setkey(expectedOutput$sample, 'header')
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match number of tables
  testthat::expect_equal(names(expectedOutput), names(output))
  
  #match sample table
  testthat::expect_identical(expectedOutput$sample, output$sample)
})

testthat::test_that('one header assigned to two variables [[wishlist]]',{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~sampleID, ~depth,
                    'S1', '1',
                    'S1', '3')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T1', 'sampleID', 'profile_name', 'id', '',
                    'T1', 'depth', 'layer_name', 'value', '',
                    'T1', 'depth', 'layer_bottom', 'value', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                               'sample', 'profile_name',
                               'sample', 'layer_name',
                               'sample', 'layer_bottom')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
})

testthat::test_that("test one variable, two methods, one table",{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~SOC, ~SOC_flag, ~SOC_method,
                               'river', 'S1', '5.2', 'ISCN', 'gap filled',
                               'lake', 'S2', '3', NA, 'provided')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T1', 'siteID', 'site_name', 'id', '',
                    'T1', 'sampleID', 'layer_name', 'id', '',
                    'T1', 'SOC', 'soc', 'value', '',
                    'T1', 'SOC_flag', 'soc', 'method', '',
                    'T1', 'SOC_method', 'soc', 'method', ''
    ))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                               'sample', 'site_name',
                               'sample', 'layer_name',
                               'sample', 'soc')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  #write.csv(output$sample, row.names=FALSE)
  expectedOutput <- list(sample=data.table::as.data.table(
    read.csv(text = '"layer_name_id","site_name_id","header","entry","variable","type"
"S1","river","SOC","5.2","soc","value"
"S2","lake","SOC","3","soc","value"
"S1","river","SOC_flag","ISCN","soc","method"
"S1","river","SOC_method","gap filled","soc","method"
"S2","lake","SOC_method","provided","soc","method"', stringsAsFactors=FALSE)))
  #setkey(expectedOutput$sample, 'header')
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match number of tables
  testthat::expect_equal(names(expectedOutput), names(output))
  
  #match sample table
  testthat::expect_identical(expectedOutput$sample, output$sample)
})

testthat::test_that('test two variables with one shared unit column',{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~SOC, ~BD, ~unit,
                               'river', 'S1', '5.2', '1', 'g cm-3',
                               'lake', 'S2', '3', '2', 'kg m-3')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T1', 'siteID', 'site_name', 'id', '',
                    'T1', 'sampleID', 'layer_name', 'id', '',
                    'T1', 'SOC', 'soc', 'value', '',
                    'T1', 'BD', 'bulk_density', 'value', '',
                    'T1', 'unit', 'soc', 'unit', '',
                    'T1', 'unit', 'bulk_density', 'unit', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                     'sample', 'site_name',
                     'sample', 'layer_name',
                     'sample', 'soc',
                     'sample', 'bulk_density')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <-list(sample = data.table::as.data.table(
    tibble::tribble(~layer_name_id, ~site_name_id, ~header, ~entry, ~variable, ~type,
                    "S1","river","BD","1","bulk_density","value",
                    "S2","lake","BD","2","bulk_density","value",
                    "S1","river","SOC","5.2","soc","value",
                    "S2","lake","SOC","3","soc","value",
                    "S1","river","unit","g cm-3","bulk_density","unit",
                    "S2","lake","unit","kg m-3","bulk_density","unit",
                    "S1","river","unit","g cm-3","soc","unit",
                    "S2","lake","unit","kg m-3","soc","unit")
  ))
  
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match number of tables
  testthat::expect_equal(names(expectedOutput), names(output))
})

testthat::test_that("test two variable one table",{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~SOC, ~BD,
                               'river', 'S1', '5.2', '1.5',
                               'lake', 'S2', '3', '1.1')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T1', 'siteID', 'site_name', 'id', '',
                    'T1', 'sampleID', 'layer_name', 'id', '',
                    'T1', 'SOC', 'soc', 'value', '',
                    'T1', 'BD', 'bulk_density', 'value', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                               'sample', 'site_name',
                               'sample', 'layer_name',
                               'sample', 'soc',
                                'sample', 'bulk_density')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <- list(sample=data.table::as.data.table(
    tibble::tribble(~layer_name_id, ~site_name_id,~header,~entry,~variable,~type,
                    "S1","river","BD","1.5","bulk_density","value",
                    "S2","lake","BD","1.1","bulk_density","value",
                    "S1","river","SOC","5.2","soc","value",
                    "S2","lake","SOC","3","soc","value")))
  #setkey(expectedOutput$sample, 'header')
  
  
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match tables
  testthat::expect_equal(expectedOutput, output)
})

testthat::test_that('test hard entries',{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~SOC,
                               'river', 'S1', '5.2',
                               'lake', 'S2', '3')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T1', 'siteID', 'site_name', 'id', '',
                    'T1', 'sampleID', 'layer_name', 'id', '',
                    'T1', 'SOC', 'soc', 'value', '',
                    'T1', '', 'soc', 'unit', 'g cm-3'))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                               'sample', 'site_name',
                               'sample', 'layer_name',
                               'sample', 'soc')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <- list(sample=data.table::as.data.table(
    tibble::tribble(~layer_name_id,~site_name_id,~header,~entry,~variable,~type,
"S1","river","SOC","5.2","soc","value",
"S2","lake","SOC","3","soc","value")))
  #setkey(expectedOutput$sample, 'header')
  
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]

  #match sample table
  testthat::expect_equal(expectedOutput, output)
})

testthat::test_that("test two target table",{
  testInput <- list(T1 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~MAT, ~SOC, ~BD,
                               'river', 'S1', '15', '5.2', '1.5',
                               'lake', 'S2', '13', '3', '1.1')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T1', 'siteID', 'site_name', 'id', '',
                    'T1', 'sampleID', 'layer_name', 'id', '',
                    'T1', 'SOC', 'soc', 'value', '',
                    'T1', 'BD', 'bulk_density', 'value', '',
                    'T1', 'MAT', 'mat', 'value', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                     'site', 'site_name',
                     'site', 'mat',
                     'sample', 'site_name',
                     'sample', 'layer_name',
                     'sample', 'soc',
                     'sample', 'bulk_density')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <- list(
    site = data.table::as.data.table(
      tibble::tribble(~site_name_id, ~header,~entry,~variable,~type,
"river","MAT","15","mat","value",
"lake","MAT","13","mat","value")),
    sample=data.table::as.data.table(
      tibble::tribble(~layer_name_id,~site_name_id,~header,~entry,~variable,~type,
"S1","river","BD","1.5","bulk_density","value",
"S2","lake","BD","1.1","bulk_density","value",
"S1","river","SOC","5.2","soc","value",
"S2","lake","SOC","3","soc","value")))
  #setkey(expectedOutput$site, 'header')
  #setkey(expectedOutput$sample, 'header')
  
  
  cols <- setdiff(names(expectedOutput$site), c('entry'))
  expectedOutput$site[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match number of tables
  testthat::expect_equal(expectedOutput, output)
  testthat::expect_equivalent(expectedOutput$sample, output$sample)
  testthat::expect_equivalent(expectedOutput$site, output$site)
})

testthat::test_that("test two source and two target table",{
  testInput <- list(
    T1 =data.table::as.data.table(
    tibble::tribble(~siteID,  ~MAT,
                               'river','15', 
                               'lake', '13')),
    T2 = data.table::as.data.table(
    tibble::tribble(~siteID, ~sampleID, ~SOC, ~BD,
                               'river', 'S1', '5.2', '1.5',
                               'lake', 'S2', '3', '1.1')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'T2', 'siteID', 'site_name', 'id', '',
                    'T1', 'siteID', 'site_name', 'id', '',
                    'T2', 'sampleID', 'layer_name', 'id', '',
                    'T2', 'SOC', 'soc', 'value', '',
                    'T2', 'BD', 'bulk_density', 'value', '',
                    'T1', 'MAT', 'mat', 'value', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                     'site', 'site_name',
                     'site', 'mat',
                     'sample', 'site_name',
                     'sample', 'layer_name',
                     'sample', 'soc',
                     'sample', 'bulk_density')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <- list(
    site = data.table::as.data.table(
      tibble::tribble(~site_name_id,~header,~entry,~variable,~type,
                      "river",NA,NA,NA,NA,
                      "lake",NA,NA,NA,NA,
                      "river","MAT","15","mat","value",
                      "lake","MAT","13","mat","value")),
    sample=data.table::as.data.table(
      tibble::tribble(~site_name_id,~layer_name_id, ~header,~entry, ~variable, ~type,
                      "river",NA,NA,NA,NA,NA,
                      "lake",NA,NA,NA,NA,NA,
                      "river","S1","BD","1.5","bulk_density","value",
                      "lake","S2","BD","1.1","bulk_density","value",
                      "river","S1","SOC","5.2","soc","value",
                      "lake","S2","SOC","3","soc","value")))
  
  
  cols <- setdiff(names(expectedOutput$site), c('entry'))
  expectedOutput$site[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match number of tables
  testthat::expect_equal(expectedOutput, output)
})

testthat::test_that("quasi real test",{
  testInput <- list(
    site = data.table::as.data.table(
    tibble::tribble(~siteID,  ~site_description, ~state, ~country, ~lat, ~lon, ~datum,
                    'lake', 'up by the lake', 'Maine', 'USA', '45.8200', '-68.8706',  'WGS84',
                    'river', 'down by the river', 'New Hampshire', 'USA', '46.52016', '-68.37050', 'NAD83')),
    layer = data.table::as.data.table(
      tibble::tribble(~siteID, ~sampleID, ~SOC, ~BD, ~BD_notes, ~color,
                      'river', 'S1', '5.2', '1.5', 'dry, not seive', 'black', 
                      'lake', 'S1', '3', '1.1', 'dry and 2mm seive', 'brown')))
  
  inputKey <- data.table::as.data.table(
    tibble::tribble(~table, ~header, ~variable, ~type, ~entry,
                    'site', 'siteID', 'site_name', 'id', '',
                    'site', 'site_description', 'description', 'string', '',
                    'site', 'state', 'state', 'string', '',
                    'site', 'country', 'country', 'string','',
                    'site', 'lat', 'latitude', 'value', '',
                    'site', 'datum', 'latitude', 'unit', '',
                    'site', 'lon', 'longitude', 'value', '',
                    'site', 'datum', 'longitude', 'unit', '',
                    'layer', 'siteID', 'site_name', 'id', '',
                    'layer', 'sampleID', 'layer_name', 'id', '',
                    'layer', 'SOC', 'soc', 'value', '',
                    'layer', '','soc', 'unit', 'g cm-3',
                    'layer', 'BD', 'bulk_density', 'value', '',
                    'layer', 'BD_notes', 'bulk_density', 'method', '',
                    'layer', 'color', 'color', 'string', ''))
  
  outputKey <- data.table::as.data.table(
    (tibble::tribble(~table, ~variable,
                     'site', 'site_name',
                     'site', 'description',
                     'site', 'state',
                     'site', 'country',
                     'site', 'latitude',
                     'site', 'longitude',
                     'sample', 'site_name',
                     'sample', 'layer_name',
                     'sample', 'soc',
                     'sample', 'bulk_density',
                     'sample', 'color')))
  
  
  output <- formatLongTable(data.ls = testInput, sourceKey = inputKey, targetKey = outputKey)
  
  expectedOutput <- list(
    site = data.table::as.data.table(
      tibble::tribble(~site_name_id, ~header, ~entry, ~variable, ~type,
"river",NA,NA,NA,NA,
"lake",NA,NA,NA,NA,
"lake","country","USA","country","string",
"river","country","USA","country","string",
"lake","datum","WGS84","latitude","unit",
"river","datum","NAD83","latitude","unit",
"lake","datum","WGS84","longitude","unit",
"river","datum","NAD83","longitude","unit",
"lake","lat","45.8200","latitude","value",
"river","lat","46.52016","latitude","value",
"lake","lon","-68.8706","longitude","value",
"river","lon","-68.37050","longitude","value",
"lake","site_description","up by the lake","description","string",
"river","site_description","down by the river","description","string",
"lake","state","Maine","state","string",
"river","state","New Hampshire","state","string")
    ),
    sample=data.table::as.data.table(
      tibble::tribble(~site_name_id,~layer_name_id,~header,~entry,~variable,~type,
"lake",NA,NA,NA,NA,NA,
"river",NA,NA,NA,NA,NA,
"river","S1","BD","1.5","bulk_density","value",
"lake","S1","BD","1.1","bulk_density","value",
"river","S1","BD_notes","dry, not seive","bulk_density","method",
"lake","S1","BD_notes","dry and 2mm seive","bulk_density","method",
"river","S1","color","black","color","string",
"lake","S1","color","brown","color","string",
"river","S1","SOC","5.2","soc","value",
"lake","S1","SOC","3","soc","value")))
  
  cols <- setdiff(names(expectedOutput$site), c('entry'))
  expectedOutput$site[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  cols <- setdiff(names(expectedOutput$sample), c('entry'))
  expectedOutput$sample[,(cols) := lapply(.SD, as.factor), .SDcols=cols]
  
  #match number of tables
  testthat::expect_equal(expectedOutput, output)
})
