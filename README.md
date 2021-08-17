# nih_reporter_api
R code to work with v2 of the NIH RePORTER API

## Overview

These functions allow you to obtain award data from the [NIH RePORTER API](https://api.reporter.nih.gov/) in R and parse the resulting JSON data into a data frame. In general, you create a search query using the `create_query()` function and then pass that query into the `get_nih_reporter()` function to request the awards that match that query. The `get_nih_reporter()` function will then send the query to the API, automatically loop through the pages of results to download all of the records matching that query, save the resulting JSON to a file, and parse the results into a data frame. 

The `pmid_match()` function allows you to match core project numbers or application IDs to the PMIDs that are associated with those numbers in RePORTER, or vice versa. 

These functions are still very much in development, so there may be bugs. Of special note, the `create_query()` function only allows you to search a limited number of the available fields in NIH RePORTER. This is due to the complexity, and seeming arbitrariness, of the API request format. More search fields will be added to the function over time. If there are fields that you need that are not yet available in the function, please let me know. 

These functions use the `httr`, `jsonlite`, and `tm` packages, so you'll need to have them installed for the functions to work. The `httr` and `jsonlite` packages are included in the `tidyverse`, so you probably already have them, but `tm` is a special case, so you'll probably need to install it separately. 

## Usage

### Loading the .r file

First, load the get_nih_reporter.r file with source(): 
```r
source("get_nih_reporter.r")
```
This is just like loading a package with `library()`, except you're loading the functions from a local file rather than from a package.
    
### Creating queries

Then use the `create_query()` function to create the JSON query you want to use. For example, if you want to search for active awards, excluding subprojects, that are administered by NCCIH, you would do: 
```r
my_query <- create_query(IC = "NCCIH", is_admin_ic = TRUE, include_active = TRUE, exclude_subprojects = TRUE)
```
Or, to search for all R44 awards from fiscal year 2019, including subprojects, you would do 
```r
my_query <- create_query(fiscal_year = "2019", activity_code = "R44")
```
To search for multiple inputs in a single field, use `c()` to create a vector of inputs. So to search for all awards administered by NCCIH from fiscal years 2018-2020, you would do: 
```r
my_query <- create_query(IC = "NCCIH", FY = c("2020", "2019", "2018"), is_admin_ic = TRUE)
```
And so on. 

As of 10 May 2021, you can now search by [NIH spending category](https://report.nih.gov/funding/categorical-spending#/) by using the `spending_cats` argument, but note that the field requires numeric category codes, not the names of the spending categories. So to search for projects with the NIH spending category of 'Pregnancy', you would do: 
```r
my_query <- create_query(spending_cats = "3920", FY = "2019", exclude_subprojects = TRUE)
```
because the 'Pregnancy' spending category has the numeric code of 3920. As far as I am aware, there is no comprehensive list of spending category codes available.

Also as of 10 May 2021, you can now run text searches in the API. To search for the boolean string 'pregnancy OR pregnant', you can either do something like
```r
my_query <- create_query(FY = "2019", IC = "NHLBI", text_search_string = "pregnancy pregnant", text_search_operator = "or")
```
or you can pass the boolean string directly by setting the `text_search_operator` argument to ""
```r
my_query <- create_query(FY = "2019", IC = "NHLBI", text_search_string = "pregnancy OR pregnant", text_search_operator = "")
```
### Requesting data

Once you have the query set, you then pass it to the `get_nih_reporter()` function to run it against the API and download the resulting award data. 
```r
awards <- get_nih_reporter(my_query, "reporter_data.txt")
 ```
If necessary, the function will automatically make additional requests until all of the awards that match your query have been downloaded. The resulting `awards` object will then be a data frame of the requested awards with one award per row. The raw JSON data will also be saved to the outfile, in this case specified as reporter_data.txt.

Note, however, that the API can currently only process the first 10,000 results for any given query. If you try to download more than 10,000 records for a single query, the API will throw an error when you try to request record 10,001 and above. To get around this, you have to break large search queries up into smaller batches so that each batch returns less than 10,000 results. 

### Matching awards to publication IDs

With the release of v2 of the API, there is now a `publications` endpoint that allows you to match awards (either by their application IDs or core project numbers) to the PMIDs that are associated with those awards in RePORTER. This endpoint is operationalized here with the `pmid_match()` function. To use it, simply pass a vector of award numbers (or PMIDs) to the function and specify what kind of ID ("appl_id", "core_project_num", or "pmid") you're starting with in the `idtype` argument. So to search for PMIDs matched to the awards in your `awards` data frame, simply do
```r
pub_link <- pmid_match(awards$core_project_num, idtype = "core_project_num")
```
and the resulting `pub_link()` data frame will contain a long-form link table that matches the `appl_id`, `core_project_num`, and `pmid` for each of the awards in your data frame. You can then use other API packages or functions to retrieve the publication metadata for those PMIDs from publication databases like PubMed, Web of Science, or Scopus.

### Parsing .txt files

The RePORTER data often contains strange characters or embedded nulls in the project abstract and/or public health relevance fields that can cause errors later on. This is especially true when you use `write.csv()` to write the data frame created by the `get_nih_reporter()` function and then try to read it back into R at a later date. So I've added a convenience function, `extract_reporter()`, to read the outfile created by the `get_nih_reporter()` function back into R.

To use it, simply run the function on the outfile created by the `get_nih_reporter()` function:
```r
awards <- extract_reporter("reporter_data.txt")
```
And the fuction will return the same data frame that was originally created by the `get_nih_reporter()` function.
