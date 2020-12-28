# nih_reporter_api
R code to work with the new NIH RePORTER API

## Overview

These functions allow you to obtain award data from the NIH RePORTER API (https://api.reporter.nih.gov/) in R and parse the resulting JSON data into a data frame. In general, you create a search query using the create_query() function and then pass that query into the get_nih_reporter() function to request the awards that match that query. The get_nih_reporter() function will then send the query to the API, automatically loop through the pages of results to download all of the records matching that query, save the resulting JSON to a file, and parse the results into a data frame. 

These functions are still very much in development, so there may be bugs. Of special note, the create_query() function only allows you to search a limited number of the available fields in NIH RePORTER. This is due to the complexity, and seeming arbitrariness, of the API request format. More search fields will be added to the function over time. If there are fields that you need that are not yet available in the function, please let me know. 

## Usage

### Loading the .r file

First, load the get_nih_reporter.r file with source(): 

    source("get_nih_reporter.r")
    
This is just like loading a package with library(), except you're loading the functions from a local file rather than from a package.
    
### Creating queries

Then use the create_query() function to create the JSON query you want to use. For example, if you want to search for active awards, excluding subprojects, that are administered by NCCIH, you would do: 

    my_query <- create_query(IC = "NCCIH", is_admin_ic = TRUE, include_active = TRUE, exclude_subprojects = TRUE)
    
Or, to search for all R44 awards from fiscal year 2019, including subprojects, you would do 

    my_query <- create_query(fiscal_year = "2019", activity_code = "R44")
    
To search for multiple inputs in a single field, use c() to create a vector of inputs. So to search for all awards administered by NCCIH from fiscal years 2018-2020, you would do: 

    my_query <- create_query(IC = "NCCIH", FY = c("2020", "2019", "2018"), is_admin_ic = TRUE)
    
And so on. 
    
### Requesting data

Once you have the query set, you then pass it to the get_nih_reporter() function to run it against the API and download the resulting award data. 

    awards <- get_nih_reporter(my_query, "reporter_data.txt")
    
The API limits you to donwloading 50 awards at a time, but the function will automatically make additional requests until all of the available awards have been downloaded. The resulting awards object will then be a data frame of the requested awards with one award per row. The raw JSON data will also be saved to the outfile, in this case specified as reporter_data.txt. 
