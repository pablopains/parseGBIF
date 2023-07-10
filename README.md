
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parseGBIF

<!-- badges: start -->

[![R-CMD-check](https://github.com/p/parseGBIF/pablopains/R-CMD-check/badge.svg)](https://github.com/pablopains/parseGBIF/actions)
[![Codecov test
coverage](https://codecov.io/gh/pablopains/parseGBIF/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pablopains/parseGBIF?branch=main)
[![R-CMD-check](https://github.com/pablopains/parseGBIF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pablopains/parseGBIF/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

parseGBIF package is designed to convert [Global Biodiversity
Information Facility - GBIF](https://www.gbif.org/) species occurrence
data to a more comprehensible format to be used for further analysis,
e.g. spatial. The package provides tools for verifying and standardizing
species scientific names and for selecting the most informative species
records when duplicates are available. The Manual provides a brief
introduction to parseGBIF, with more information available from Help
pages accessed via help(function_name).

## Installation

You can install the development version of parseGBIF from
[GitHub](https://github.com/pablopains/parseGBIF). To install parseGBIF,
run

``` r
devtools::install_github("pablopains/parseGBIF")
```

Please site parseGBIF as:

``` r
print(citation("parseGBIF"), bibtex = FALSE)
```

## Example

**parseGBIF makes it easy to get species occurrence records based on
GBIF.**

### 1. GBIF data preparation

#### 1.1. Obtaining occurrence data of the species records from GBIF

1.1.1. Access a registered account in [GBIF](gbif.org)

1.1.2. Filter occurrences using available fields, for instance:

- Basis of record: *Preserved specimen*
- Occurrence status: *present*
- Scientific name: *Botanical family name* (e.g. Achatocarpaceae) or
  **filter by other fields**

1.1.3. Request to download information in **DARWIN CORE ARCHIVE FORMAT**

1.1.4. Download compressed file and unzip downloaded file

1.1.5. Use the **occurrence.txt** file as input to the
prepare_gbif_occurrence_data(gbif_occurrece_file = ‘occurrence.txt’)
function

#### 1.2. Preparing occurrence data downloaded from GBIF

``` r
  library(parseGBIF)
  
  occ_file <- 'https://raw.githubusercontent.com/pablopains/parseGBIF/main/dataGBIF/Achatocarpaceae/occurrence.txt'
  
  occ <- parseGBIF::prepare_gbif_occurrence_data(gbif_occurrece_file = occ_file, columns = 'standard')
```

``` r
  col_standard <- parseGBIF::select_gbif_fields(columns = 'standard')
  
  str(col_standard)
```

#### 1.3. Extracting GBIF issue

``` r
  data(EnumOccurrenceIssue)

  colnames(EnumOccurrenceIssue)
```

``` r
 occ_gbif_issue <- parseGBIF::extract_gbif_issue(occ = occ)
```

### 2. Check species names against WCVP database

``` r
  wcvp_names <- get_wcvp(read_only_to_memory = TRUE)$wcvp_names
  
  colnames(wcvp_names)
```

Species’ names can be checked against WCVP database one by one or in a
batch mode. To verify individual names, the function checkName_wcvp is
used.

``` r
  name.checked <- checkName_wcvp(searchedName = 'Achatocarpus mollis H.Walter',
                 wcvp_names = wcvp_names,
                 if_author_fails_try_without_combinations = TRUE)
  name.checked[,c(3:5,22,23,40)]
```

``` r
  names.checked <- batch_checkName_wcvp(occ = occ,
                                                   wcvp_names =  wcvp_names,
                                                   if_author_fails_try_without_combinations = TRUE,
                                                   wcvp_selected_fields = 'standard')
  
```

``` r
# hybrid separator
standardize_scientificName('Leucanthemum ×superbum (Bergmans ex J.W.Ingram) D.H.Kent')

# variety 
standardize_scientificName('Platymiscium pubescens subsp. fragrans (Rusby) Klitg.')
```

``` r
# library(parseGBIF)
get_lastNameRecordedBy('Melo, P.H.A, Bystriakova, N. & Monro, A.')

get_lastNameRecordedBy('Monro, A.; Bystriakova, N. & Melo, P.H.A')

get_lastNameRecordedBy('Bystriakova, N., Monro, A.,Melo, P.H.A')
```

### 3. Collectors Dictionary

#### 3.1 Prepare dictionary collectors

``` r

collectorsDictionary.dataset <- prepare_collectorsDictionary(occ = occ)
```

#### 3.2 Check the main collector’s last name

``` r

  file.collectorsDictionary.dataset <-  'file_collectorsDictionary_dataset.csv'

  write.csv(collectorsDictionary.dataset,
            file.collectorsDictionary.dataset, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")
```

#### 3.3 Update dictionary collectors

``` r

  occ_collectorsDictionary <- update_collectorsDictionary(occ=occ,
                                        collectorDictionary_checked_file = file.collectorsDictionary.dataset)
```

### 4. Select digital voucher

``` r

  occ_digital_voucher <- select_digital_voucher(occ = occ,
  occ_gbif_issue = occ_gbif_issue$occ_gbif_issue,
  occ_checkName_wcvp = names.checked$occ_checkName_wcvp ,
  occ_collectorsDictionary = occ_collectorsDictionary$occ_collectorsDictionary)
```

### 5. Export of results

``` r

  occ_resuts <-  export_data(occ_digital_voucher_file = file.occ_digital_voucher )
  
  names(occ_resuts)

  NROW(occ_resuts$occ_in)
  
  NROW(occ_resuts$occ_out_to_recover)
  
  NROW(occ_resuts$occ_dup)
  
  
  file.in <-  'parseGBIF_occ_in.csv'
  write.csv(occ_resuts$occ_in,
            file.in, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")

  file.occ_out_to_recover <-  'parseGBIF_occ_out_to_recover.csv'
  write.csv(occ_resuts$occ_out_to_recover,
            file.occ_out_to_recover, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")


  file.dup <-  'parseGBIF_occ_dup.csv'
  write.csv(occ_resuts$occ_dup,
            file.dup, 
            row.names = FALSE, 
            fileEncoding = "UTF-8", 
            na = "")
```
