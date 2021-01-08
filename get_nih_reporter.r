
#    "advancedTextSearch": {
#      "searchText": "string",
#      "operator": "string",
#      "searchRegion": "string",
#      "searchField": "string"
#    },

create_query <- function(FY = "", IC = "", is_admin_ic = "", is_funding_ic = "", include_active = "", pi_name = "", org_names = "", exclude_subprojects = "", activity_code = "", funding_mechanism = "", foa = "", project_number = "", covid_response = "") {
	theQ <- list(
		criteria = list(
		   fiscal_years = FY, 
		   agencies = IC, 
		   is_agency_admin = jsonlite::unbox(is_admin_ic), 
		   is_agency_funding = jsonlite::unbox(is_funding_ic), 
		   include_active_projects = jsonlite::unbox(include_active), 
		   pi_names = data.frame(any_name = jsonlite::unbox(pi_name)), 
		   org_names = org_names, 
		   exclude_subprojects = jsonlite::unbox(exclude_subprojects), 
		   ## advancedTextSearch = data.frame(searchText = jsonlite::unbox(search_text), operator = jsonlite::unbox(search_operator)), 
		   ## spending_categories, 
		   activity_codes = activity_code, 
		   funding_mechanism = funding_mechanism,  ## input values include IAA, IM, NSRDC, OR, RC, RP, SB, SRDC, TI, TR
		   foa = foa, 
		   project_nums = project_number, 
		   covid_response = covid_response ## input values: Reg-CV, CV, C3, C4
		), 
		offset = jsonlite::unbox(0), 
		limit = jsonlite::unbox(50)
	)
	if (pi_name == "") {theQ$criteria$pi_names <- NULL}
	#if (search_text == "") {theQ$criteria$advancedTextSearch <- NULL}
	theQ$criteria <- theQ$criteria[!theQ$criteria == ""]
	theQ <- jsonlite::toJSON(theQ)
	return(theQ)
}
## usage examples
## my_query <- create_query(FY = "2019", funding_mechanism = "RC")
## my_query <- create_query(IC = "NICHD", is_admin_ic = TRUE, include_active = TRUE, exclude_subprojects = TRUE)
## my_query <- create_query(IC = "NCI", activity_code = c("R01", "R21"))
## my_query <- create_query(IC = c("NCI", "NIAID"), FY = "2020")
## my_query <- create_query(IC = "NICHD", FY = c("2020", "2019", "2018"))
## my_query <- create_query(IC = "NICHD", covid_response = c("Reg-CV", "CV", "C3", "C4"))
## etc.

get_nih_reporter <- function(my_query, outfile) {
	thePages <- list()
	theJ <- list()
	querya <- jsonlite::fromJSON(my_query)
	mStart <- as.numeric(querya$offset)
	perPage <- as.numeric(querya$limit)
	message("Retrieving results ", mStart + 1, " to ", perPage)
	theURL <- httr::POST("https://api.reporter.nih.gov/v1/projects/Search", httr::accept("text/plain"), httr::content_type_json(), body = my_query)
	theData <- httr::content(theURL, as = "text")
	if (httr::http_error(theURL) == TRUE) {
		message("HTTP error.")
		print(httr::http_status(theURL))
		break
	}
	newData <- jsonlite::fromJSON(theData)
	resultCount <- as.numeric(newData$meta$total)
	pagesNeeded <- ceiling(resultCount / perPage)
	thePages[[1]] <- newData$results
	theJ[[1]] <- theData
	Sys.sleep(1)
	if (pagesNeeded > 1) {
	for (i in 2:pagesNeeded) {
		oStart <- mStart
		mStart <- mStart + perPage
		gettingPage <- mStart + perPage
		my_query <- gsub(paste0("\"offset\":", oStart), paste0("\"offset\":", mStart), my_query)
		message("Retrieving results ", mStart, " to ", gettingPage, " of ", resultCount)
		theURL <- httr::POST("https://api.reporter.nih.gov/v1/projects/Search", httr::accept("text/plain"), httr::content_type_json(), body = my_query)
		theData <- httr::content(theURL, as = "text")
		if (httr::http_error(theURL) == TRUE) {
			message("HTTP error.")
			print(httr::http_status(theURL))
		}
		newData <- jsonlite::fromJSON(theData)
		thePages[[i]] <- newData$results
		theJ[[i]] <- theData
		Sys.sleep(1)
	}
	message("Finished retrieving results. Formatting and saving results.")
	}
	writeLines(unlist(theJ), con = outfile)
	thePages <- thePages[which(lapply(thePages, is.data.frame) == TRUE)]
	thePages <- lapply(thePages, jsonlite::flatten)
	thePages <- jsonlite::rbind_pages(thePages)
	thePages$spending_categories_desc <- gsub("; ", ";", thePages$spending_categories_desc)
	thePages$spending_categories <- sapply(1:nrow(thePages), function(x) paste(thePages$spending_categories[x][[1]], collapse = ";"))
	t1 <- data.frame(first_name = NA, middle_name = NA, last_name = NA, full_name = NA, email = NA, stringsAsFactors = FALSE)
	prob <- thePages$program_officers[which(sapply(thePages$program_officers, length) == 0)]
	if (length(prob) > 0) {
	  prob <- lapply(1:length(prob), function(x) prob[[x]] <- t1)
	  thePages$program_officers[which(sapply(thePages$program_officers, length) == 0)] <- prob
	}
	pi_profile_ids <- sapply(thePages$principal_investigators, "[[", 1)
	pi_profile_ids <- sapply(pi_profile_ids, paste, collapse = ";")
	thePages$pi_profile_ids <- pi_profile_ids
	pi_first_names <- sapply(thePages$principal_investigators, "[[", 2)
	pi_first_names <- sapply(pi_first_names, paste, collapse = ";")
	thePages$pi_first_names <- pi_first_names
	pi_middle_names <- sapply(thePages$principal_investigators, "[[", 3)
	pi_middle_names <- sapply(pi_middle_names, paste, collapse = ";")
	thePages$pi_middle_names <- pi_middle_names
	pi_last_names <- sapply(thePages$principal_investigators, "[[", 4)
	pi_last_names <- sapply(pi_last_names, paste, collapse = ";")
	thePages$pi_last_names <- pi_last_names
	pi_is_contact_pi <- sapply(thePages$principal_investigators, "[[", 5)
	pi_is_contact_pi <- sapply(pi_is_contact_pi, paste, collapse = ";")
	thePages$pi_is_contact_pi <- pi_is_contact_pi
	pi_full_names <- sapply(thePages$principal_investigators, "[[", 6)
	pi_full_names <- sapply(pi_full_names, paste, collapse = ";")
	thePages$pi_full_names <- pi_full_names
	pi_titles <- sapply(thePages$principal_investigators, "[[", 7)
	pi_titles <- sapply(pi_titles, paste, collapse = ";")
	thePages$pi_titles <- pi_titles
	pi_emails <- sapply(thePages$principal_investigators, "[[", 8)
	pi_emails <- sapply(pi_emails, paste, collapse = ";")
	thePages$pi_emails <- pi_emails
	thePages$principal_investigators <- NULL
	po_first_names <- sapply(thePages$program_officers, "[", 1, 1)
	po_first_names <- sapply(po_first_names, paste, collapse = ";")
	thePages$po_first_names <- po_first_names
	po_middle_names <- sapply(thePages$program_officers, "[", 1, 2)
	po_middle_names <- sapply(po_middle_names, paste, collapse = ";")
	thePages$po_middle_names <- po_middle_names
	po_last_names <- sapply(thePages$program_officers, "[", 1, 3)
	po_last_names <- sapply(po_last_names, paste, collapse = ";")
	thePages$po_last_names <- po_last_names
	po_full_names <- sapply(thePages$program_officers, "[", 1, 4)
	po_full_names <- sapply(po_full_names, paste, collapse = ";")
	thePages$po_full_names <- po_full_names
	po_email <- sapply(thePages$program_officers, "[", 1, 5)
	po_email <- sapply(po_email, paste, collapse = ";")
	thePages$po_email <- po_email
	thePages$program_officers <- NULL
	t2 <- data.frame(fy = NA, code = NA, name = NA, abbreviation = NA, total_cost = NA, stringsAsFactors = FALSE)
	prob <- thePages$agency_ic_fundings[which(sapply(thePages$agency_ic_fundings, is.data.frame) == FALSE)]
	if (length(prob) > 0) {
	  prob <- lapply(1:length(prob), function(x) prob[[x]] <- t2)
	  thePages$agency_ic_fundings[which(sapply(thePages$agency_ic_fundings, is.data.frame) == FALSE)] <- prob
	}
	agency_ic_fundings_fy <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$fy)
	agency_ic_fundings_fy <- sapply(agency_ic_fundings_fy, paste, collapse = ";")
	thePages$agency_ic_fundings_fy <- agency_ic_fundings_fy
	agency_ic_fundings_code <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$code)
	agency_ic_fundings_code <- sapply(agency_ic_fundings_code, paste, collapse = ";")
	thePages$agency_ic_fundings_code <- agency_ic_fundings_code
	agency_ic_fundings_name <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$name)
	agency_ic_fundings_name <- sapply(agency_ic_fundings_name, paste, collapse = ";")
	thePages$agency_ic_fundings_name <- agency_ic_fundings_name
	agency_ic_fundings_abbreviation <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$abbreviation)
	agency_ic_fundings_abbreviation <- sapply(agency_ic_fundings_abbreviation, paste, collapse = ";")
	thePages$agency_ic_fundings_abbreviation <- agency_ic_fundings_abbreviation
	agency_ic_fundings_total_cost <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$total_cost)
	agency_ic_fundings_total_cost <- sapply(agency_ic_fundings_total_cost, paste, collapse = ";")
	thePages$agency_ic_fundings_total_cost <- agency_ic_fundings_total_cost
	thePages$agency_ic_fundings <- NULL
	canTask <- sapply(thePages$can_task, paste, collapse = ";")
	thePages$can_task <- canTask
	specTopic <- sapply(thePages$special_topic_code, paste, collapse = ";")
	thePages$special_topic_code <- specTopic
	covid <- sapply(thePages$covid_response, paste, collapse = ";")
	thePages$covid_response <- covid
	## uses the tm package
	thePages$abstract_text <- tm::stripWhitespace(thePages$abstract_text)
	thePages$phr_text <- tm::stripWhitespace(thePages$phr_text)
	## end tm package
	message("Done.")
	return(thePages)
}

## usage
## source("H:/bibliometrics/r/functions/getnihreporter_dev.r")
## nca <- create_query(IC = "NICHD", is_admin_ic = TRUE, include_active = TRUE, exclude_subprojects = TRUE)
## nichd <- get_nih_reporter(nca, "nichd_active.txt")


extract_reporter <- function(theFile) {
	theFile <- scan(theFile, what = "varchar", sep = "\n")
	thePages <- lapply(theFile, jsonlite::fromJSON)
	thePages <- lapply(1:length(thePages), function(x) thePages[[x]]$results)
	thePages <- thePages[which(lapply(thePages, is.data.frame) == TRUE)]
	thePages <- lapply(thePages, jsonlite::flatten)
	thePages <- jsonlite::rbind_pages(thePages)
	thePages$spending_categories_desc <- gsub("; ", ";", thePages$spending_categories_desc)
	thePages$spending_categories <- sapply(1:nrow(thePages), function(x) paste(thePages$spending_categories[x][[1]], collapse = ";"))
	t1 <- data.frame(first_name = NA, middle_name = NA, last_name = NA, full_name = NA, email = NA, stringsAsFactors = FALSE)
	prob <- thePages$program_officers[which(sapply(thePages$program_officers, length) == 0)]
	if (length(prob) > 0) {
	  prob <- lapply(1:length(prob), function(x) prob[[x]] <- t1)
	  thePages$program_officers[which(sapply(thePages$program_officers, length) == 0)] <- prob
	}
	pi_profile_ids <- sapply(thePages$principal_investigators, "[[", 1)
	pi_profile_ids <- sapply(pi_profile_ids, paste, collapse = ";")
	thePages$pi_profile_ids <- pi_profile_ids
	pi_first_names <- sapply(thePages$principal_investigators, "[[", 2)
	pi_first_names <- sapply(pi_first_names, paste, collapse = ";")
	thePages$pi_first_names <- pi_first_names
	pi_middle_names <- sapply(thePages$principal_investigators, "[[", 3)
	pi_middle_names <- sapply(pi_middle_names, paste, collapse = ";")
	thePages$pi_middle_names <- pi_middle_names
	pi_last_names <- sapply(thePages$principal_investigators, "[[", 4)
	pi_last_names <- sapply(pi_last_names, paste, collapse = ";")
	thePages$pi_last_names <- pi_last_names
	pi_is_contact_pi <- sapply(thePages$principal_investigators, "[[", 5)
	pi_is_contact_pi <- sapply(pi_is_contact_pi, paste, collapse = ";")
	thePages$pi_is_contact_pi <- pi_is_contact_pi
	pi_full_names <- sapply(thePages$principal_investigators, "[[", 6)
	pi_full_names <- sapply(pi_full_names, paste, collapse = ";")
	thePages$pi_full_names <- pi_full_names
	pi_titles <- sapply(thePages$principal_investigators, "[[", 7)
	pi_titles <- sapply(pi_titles, paste, collapse = ";")
	thePages$pi_titles <- pi_titles
	pi_emails <- sapply(thePages$principal_investigators, "[[", 8)
	pi_emails <- sapply(pi_emails, paste, collapse = ";")
	thePages$pi_emails <- pi_emails
	thePages$principal_investigators <- NULL
	po_first_names <- sapply(thePages$program_officers, "[", 1, 1)
	po_first_names <- sapply(po_first_names, paste, collapse = ";")
	thePages$po_first_names <- po_first_names
	po_middle_names <- sapply(thePages$program_officers, "[", 1, 2)
	po_middle_names <- sapply(po_middle_names, paste, collapse = ";")
	thePages$po_middle_names <- po_middle_names
	po_last_names <- sapply(thePages$program_officers, "[", 1, 3)
	po_last_names <- sapply(po_last_names, paste, collapse = ";")
	thePages$po_last_names <- po_last_names
	po_full_names <- sapply(thePages$program_officers, "[", 1, 4)
	po_full_names <- sapply(po_full_names, paste, collapse = ";")
	thePages$po_full_names <- po_full_names
	po_email <- sapply(thePages$program_officers, "[", 1, 5)
	po_email <- sapply(po_email, paste, collapse = ";")
	thePages$po_email <- po_email
	thePages$program_officers <- NULL
	t2 <- data.frame(fy = NA, code = NA, name = NA, abbreviation = NA, total_cost = NA, stringsAsFactors = FALSE)
	prob <- thePages$agency_ic_fundings[which(sapply(thePages$agency_ic_fundings, is.data.frame) == FALSE)]
	if (length(prob) > 0) {
	  prob <- lapply(1:length(prob), function(x) prob[[x]] <- t2)
	  thePages$agency_ic_fundings[which(sapply(thePages$agency_ic_fundings, is.data.frame) == FALSE)] <- prob
	}
	agency_ic_fundings_fy <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$fy)
	agency_ic_fundings_fy <- sapply(agency_ic_fundings_fy, paste, collapse = ";")
	thePages$agency_ic_fundings_fy <- agency_ic_fundings_fy
	agency_ic_fundings_code <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$code)
	agency_ic_fundings_code <- sapply(agency_ic_fundings_code, paste, collapse = ";")
	thePages$agency_ic_fundings_code <- agency_ic_fundings_code
	agency_ic_fundings_name <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$name)
	agency_ic_fundings_name <- sapply(agency_ic_fundings_name, paste, collapse = ";")
	thePages$agency_ic_fundings_name <- agency_ic_fundings_name
	agency_ic_fundings_abbreviation <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$abbreviation)
	agency_ic_fundings_abbreviation <- sapply(agency_ic_fundings_abbreviation, paste, collapse = ";")
	thePages$agency_ic_fundings_abbreviation <- agency_ic_fundings_abbreviation
	agency_ic_fundings_total_cost <- sapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$total_cost)
	agency_ic_fundings_total_cost <- sapply(agency_ic_fundings_total_cost, paste, collapse = ";")
	thePages$agency_ic_fundings_total_cost <- agency_ic_fundings_total_cost
	thePages$agency_ic_fundings <- NULL
	canTask <- sapply(thePages$can_task, paste, collapse = ";")
	thePages$can_task <- canTask
	specTopic <- sapply(thePages$special_topic_code, paste, collapse = ";")
	thePages$special_topic_code <- specTopic
	covid <- sapply(thePages$covid_response, paste, collapse = ";")
	thePages$covid_response <- covid
	## uses the tm package
	thePages$abstract_text <- tm::stripWhitespace(thePages$abstract_text)
	thePages$phr_text <- tm::stripWhitespace(thePages$phr_text)
	## end tm package
	message("Done.")
	return(thePages)
}