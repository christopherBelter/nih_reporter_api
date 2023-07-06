# for v2 of the NIH RePORTER API

## note: requests for result number 10,001 and up will fail, so break queries up into batches of less than 10,000 to ensure you get everything
## new search fields: award_types, pi_profile_ids, org_cities, org_states, org_countries, cong_dists, newly_added_projects_only

create_query <- function(FY = "", IC = "", is_admin_ic = "", is_funding_ic = "", include_active = "", pi_name = "", pi_profile_ids = "", org_names = "", org_cities = "", org_states = "", org_countries = "", org_dept_types = "", cong_dists = "", exclude_subprojects = "", activity_code = "", funding_mechanism = "", foa = "", project_number = "", appl_ids = "", award_types = "", covid_response = "", text_search_operator = "and", text_search_field = "all", text_search_string = "", spending_cats = "", match_all_cats = "true", newly_added_projects_only = "") {
	theQ <- list(
		criteria = list(
		   fiscal_years = FY, 
		   agencies = IC, 
		   is_agency_admin = jsonlite::unbox(is_admin_ic), 
		   is_agency_funding = jsonlite::unbox(is_funding_ic), 
		   include_active_projects = jsonlite::unbox(include_active), 
		   pi_names = data.frame(any_name = jsonlite::unbox(pi_name)), 
		   pi_profile_ids = pi_profile_ids,
		   org_names = org_names, 
		   org_cities = org_cities, 
		   org_states = org_states, 
		   cong_dists = cong_dists, 
		   org_countries = org_countries,
		   dept_types = org_dept_types,
		   exclude_subprojects = jsonlite::unbox(exclude_subprojects), 
		   advanced_text_search = list(operator = jsonlite::unbox(text_search_operator), search_field = jsonlite::unbox(text_search_field), search_text = jsonlite::unbox(text_search_string)), 
		   spending_categories = list(Values = spending_cats, match_all = jsonlite::unbox(match_all_cats)), ## note: requires numeric codes as inputs, not actual category names
		   activity_codes = activity_code, 
		   funding_mechanism = funding_mechanism,  ## input values include IAA, IM, NSRDC, OR, RC, RP, SB, SRDC, TI, TR
		   foa = foa, 
		   project_nums = project_number, 
		   appl_ids = appl_ids, 
		   award_types = award_types, 
		   covid_response = covid_response, ## input values: Reg-CV, CV, C3, C4, C5, C6
		   newly_added_projects_only = jsonlite::unbox(newly_added_projects_only)
		), 
		offset = jsonlite::unbox(0), 
		limit = jsonlite::unbox(500)
	)
	if (pi_name == "") {theQ$criteria$pi_names <- NULL}
	if (text_search_string == "") {theQ$criteria$advanced_text_search <- NULL}
	if (any(spending_cats == "")) {theQ$criteria$spending_categories <- NULL}
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
## my_query <- create_query(IC = "NICHD", covid_response = c("Reg-CV", "CV", "C3", "C4", "C5"))
## etc.

get_nih_reporter <- function(my_query, outfile) {
	thePages <- list()
	theJ <- list()
	querya <- jsonlite::fromJSON(my_query)
	mStart <- as.numeric(querya$offset)
	perPage <- as.numeric(querya$limit)
	message("Retrieving results ", mStart + 1, " to ", perPage)
	theURL <- httr::POST("https://api.reporter.nih.gov/v2/projects/search", httr::accept("text/plain"), httr::content_type_json(), body = my_query)
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
		theURL <- httr::POST("https://api.reporter.nih.gov/v2/projects/Search", httr::accept("text/plain"), httr::content_type_json(), body = my_query)
		theData <- httr::content(theURL, as = "text")
		if (httr::http_error(theURL) == TRUE) {
			message("HTTP error.")
			print(httr::http_status(theURL))
			writeLines(unlist(theJ), con = outfile)
			message("Partial results saved to the specified outfile.")
			break
		}
		newData <- jsonlite::fromJSON(theData)
		thePages[[i]] <- newData$results
		theJ[[i]] <- theData
		Sys.sleep(1)
	}
	message("Finished retrieving results. Formatting and saving results.")
	}
	writeLines(unlist(theJ), con = outfile)
	if (resultCount == 0) { return(NA) }
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
	pi_profile_ids <- lapply(thePages$principal_investigators, "[[", 1)
	pi_profile_ids <- sapply(pi_profile_ids, paste, collapse = ";")
	thePages$pi_profile_ids <- pi_profile_ids
	pi_first_names <- lapply(thePages$principal_investigators, "[[", 2)
	pi_first_names <- sapply(pi_first_names, paste, collapse = ";")
	thePages$pi_first_names <- pi_first_names
	pi_middle_names <- lapply(thePages$principal_investigators, "[[", 3)
	pi_middle_names <- sapply(pi_middle_names, paste, collapse = ";")
	thePages$pi_middle_names <- pi_middle_names
	pi_last_names <- lapply(thePages$principal_investigators, "[[", 4)
	pi_last_names <- sapply(pi_last_names, paste, collapse = ";")
	thePages$pi_last_names <- pi_last_names
	pi_is_contact_pi <- lapply(thePages$principal_investigators, "[[", 5)
	pi_is_contact_pi <- sapply(pi_is_contact_pi, paste, collapse = ";")
	thePages$pi_is_contact_pi <- pi_is_contact_pi
	pi_full_names <- lapply(thePages$principal_investigators, "[[", 6)
	pi_full_names <- sapply(pi_full_names, paste, collapse = ";")
	thePages$pi_full_names <- pi_full_names
	pi_titles <- lapply(thePages$principal_investigators, "[[", 7)
	pi_titles <- sapply(pi_titles, paste, collapse = ";")
	thePages$pi_titles <- pi_titles
	#pi_emails <- sapply(thePages$principal_investigators, "[[", 8)
	#pi_emails <- sapply(pi_emails, paste, collapse = ";")
	#thePages$pi_emails <- pi_emails
	thePages$principal_investigators <- NULL
	po_first_names <- lapply(thePages$program_officers, "[", 1, 1)
	po_first_names <- sapply(po_first_names, paste, collapse = ";")
	thePages$po_first_names <- po_first_names
	po_middle_names <- lapply(thePages$program_officers, "[", 1, 2)
	po_middle_names <- sapply(po_middle_names, paste, collapse = ";")
	thePages$po_middle_names <- po_middle_names
	po_last_names <- lapply(thePages$program_officers, "[", 1, 3)
	po_last_names <- sapply(po_last_names, paste, collapse = ";")
	thePages$po_last_names <- po_last_names
	po_full_names <- lapply(thePages$program_officers, "[", 1, 4)
	po_full_names <- sapply(po_full_names, paste, collapse = ";")
	thePages$po_full_names <- po_full_names
	po_email <- lapply(thePages$program_officers, "[", 1, 5)
	po_email <- sapply(po_email, paste, collapse = ";")
	thePages$po_email <- po_email
	thePages$program_officers <- NULL
	t2 <- data.frame(fy = NA, code = NA, name = NA, abbreviation = NA, total_cost = NA, stringsAsFactors = FALSE)
	prob <- thePages$agency_ic_fundings[which(sapply(thePages$agency_ic_fundings, is.data.frame) == FALSE)]
	if (length(prob) > 0) {
	  prob <- lapply(1:length(prob), function(x) prob[[x]] <- t2)
	  thePages$agency_ic_fundings[which(sapply(thePages$agency_ic_fundings, is.data.frame) == FALSE)] <- prob
	}
	agency_ic_fundings_fy <- lapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$fy)
	agency_ic_fundings_fy <- sapply(agency_ic_fundings_fy, paste, collapse = ";")
	thePages$agency_ic_fundings_fy <- agency_ic_fundings_fy
	agency_ic_fundings_code <- lapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$code)
	agency_ic_fundings_code <- sapply(agency_ic_fundings_code, paste, collapse = ";")
	thePages$agency_ic_fundings_code <- agency_ic_fundings_code
	agency_ic_fundings_name <- lapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$name)
	agency_ic_fundings_name <- sapply(agency_ic_fundings_name, paste, collapse = ";")
	thePages$agency_ic_fundings_name <- agency_ic_fundings_name
	agency_ic_fundings_abbreviation <- lapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$abbreviation)
	agency_ic_fundings_abbreviation <- sapply(agency_ic_fundings_abbreviation, paste, collapse = ";")
	thePages$agency_ic_fundings_abbreviation <- agency_ic_fundings_abbreviation
	agency_ic_fundings_total_cost <- lapply(1:nrow(thePages), function(x) thePages$agency_ic_fundings[[x]]$total_cost)
	agency_ic_fundings_total_cost <- sapply(agency_ic_fundings_total_cost, paste, collapse = ";")
	thePages$agency_ic_fundings_total_cost <- agency_ic_fundings_total_cost
	thePages$agency_ic_fundings <- NULL
	#thePages$organization_org_ueis <- sapply(thePages$organization_org_ueis, paste, collapse = ";")
	if ("can_task" %in% colnames(thePages)) {
	  canTask <- sapply(thePages$can_task, paste, collapse = ";")
	  thePages$can_task <- canTask
	}
	if ("special_topic_code" %in% colnames(thePages)) {
	  specTopic <- sapply(thePages$special_topic_code, paste, collapse = ";")
	  thePages$special_topic_code <- specTopic
	}
	covid <- sapply(thePages$covid_response, paste, collapse = ";")
	thePages$covid_response <- covid
	## uses the tm package
	thePages$abstract_text <- tm::stripWhitespace(as.character(thePages$abstract_text))
	thePages$phr_text <- tm::stripWhitespace(as.character(thePages$phr_text))
	## end tm package
	colnames(thePages) <- gsub("\\.", "_", colnames(thePages))
	if (is.list(thePages$organization_org_duns) == TRUE) {
		thePages$organization_org_duns <- sapply(thePages$organization_org_duns, paste, collapse = ";")
	}
	if (is.list(thePages$organization_org_ueis) == TRUE) {
		thePages$organization_org_ueis <- sapply(thePages$organization_org_ueis, paste, collapse = ";")
	}
	message("Done.")
	return(thePages)
}

## usage
## source("H:/bibliometrics/r/functions/getnihreporter_dev.r")
## nca <- create_query(IC = "NICHD", is_admin_ic = TRUE, include_active = TRUE, exclude_subprojects = TRUE)
## nichd <- get_nih_reporter(nca, "nichd_active.txt")


extract_reporter <- function(theFile) {
	theFile <- scan(theFile, what = "varchar", sep = "\n", quiet = TRUE)
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
	pi_profile_ids <- lapply(thePages$principal_investigators, "[[", 1)
	pi_profile_ids <- sapply(pi_profile_ids, paste, collapse = ";")
	thePages$pi_profile_ids <- pi_profile_ids
	pi_first_names <- lapply(thePages$principal_investigators, "[[", 2)
	pi_first_names <- sapply(pi_first_names, paste, collapse = ";")
	thePages$pi_first_names <- pi_first_names
	pi_middle_names <- lapply(thePages$principal_investigators, "[[", 3)
	pi_middle_names <- sapply(pi_middle_names, paste, collapse = ";")
	thePages$pi_middle_names <- pi_middle_names
	pi_last_names <- lapply(thePages$principal_investigators, "[[", 4)
	pi_last_names <- sapply(pi_last_names, paste, collapse = ";")
	thePages$pi_last_names <- pi_last_names
	pi_is_contact_pi <- lapply(thePages$principal_investigators, "[[", 5)
	pi_is_contact_pi <- sapply(pi_is_contact_pi, paste, collapse = ";")
	thePages$pi_is_contact_pi <- pi_is_contact_pi
	pi_full_names <- lapply(thePages$principal_investigators, "[[", 6)
	pi_full_names <- sapply(pi_full_names, paste, collapse = ";")
	thePages$pi_full_names <- pi_full_names
	pi_titles <- lapply(thePages$principal_investigators, "[[", 7)
	pi_titles <- sapply(pi_titles, paste, collapse = ";")
	thePages$pi_titles <- pi_titles
	#pi_emails <- sapply(thePages$principal_investigators, "[[", 8)
	#pi_emails <- sapply(pi_emails, paste, collapse = ";")
	#thePages$pi_emails <- pi_emails
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
	if ("can_task" %in% colnames(thePages)) {
	  canTask <- sapply(thePages$can_task, paste, collapse = ";")
	  thePages$can_task <- canTask
	}
	if ("special_topic_code" %in% colnames(thePages)) {
	  specTopic <- sapply(thePages$special_topic_code, paste, collapse = ";")
	  thePages$special_topic_code <- specTopic
	}
	covid <- sapply(thePages$covid_response, paste, collapse = ";")
	thePages$covid_response <- covid
	## uses the tm package
	thePages$abstract_text <- tm::stripWhitespace(as.character(thePages$abstract_text))
	thePages$phr_text <- tm::stripWhitespace(as.character(thePages$phr_text))
	## end tm package
	colnames(thePages) <- gsub("\\.", "_", colnames(thePages))
	if (is.list(thePages$organization_org_duns) == TRUE) {
		thePages$organization_org_duns <- sapply(thePages$organization_org_duns, paste, collapse = ";")
	}
	if (is.list(thePages$organization_org_ueis) == TRUE) {
	   thePages$organization_org_ueis <- sapply(thePages$organization_org_ueis, paste, collapse = ";")
	}
	return(thePages)
}

pmid_match <- function(the_ids, idtype) {
	## Create a list of JSON queries corresponding to the IDs requested
	idList <- split(the_ids, ceiling(seq_along(the_ids)/20))
	queryList <- list()
	for (i in 1:length(idList)) {
		queryList[[i]] <- list(
			criteria = list(
				appl_ids = "",
				pmids = "",
				core_project_nums = ""
			),
			offset =  jsonlite::unbox(0),
			limit =  jsonlite::unbox(100)
		)
		if (idtype == "appl_id") {
			queryList[[i]]$criteria$appl_ids <- idList[[i]]
		}
		else if (idtype == "pmid") {
			queryList[[i]]$criteria$pmids <- idList[[i]]
		}
		else if (idtype == "core_project_num") {
			queryList[[i]]$criteria$core_project_nums <- idList[[i]]
		}
		else {stop("Invalid idtype. Valid idtypes are 'appl_id', 'pmid', or 'core_project_num'")}
		queryList[[i]]$criteria <- queryList[[i]]$criteria[!queryList[[i]]$criteria == ""]
		queryList[[i]] <- jsonlite::toJSON(queryList[[i]])
	}
	## create a list to store the resulting data in
	theD <- list()
	## Request data from the API for each query and store the results to theD
	for (i in 1:length(queryList)) {
		message(paste("Requesting data for query set", i, "of", length(queryList)))
		theURL <- httr::POST("https://api.reporter.nih.gov/v2/publications/search", httr::accept("text/plain"), httr::content_type_json(), body = queryList[[i]])
		theJ <- httr::content(theURL, as = "text")
		if (httr::http_error(theURL) == TRUE) {
			message("HTTP error.")
			print(httr::http_status(theURL))
			break
		}
		tempD <- jsonlite::fromJSON(theJ)
		theD[[i]] <- tempD$results
		num_results <- tempD$meta$total
		num_retrieved <- 100
		mStart <- 0
		Sys.sleep(1)
		message(paste("Retrieving", num_results, "PMID-Appl ID matches for query set", i))
		## if there are more than 100 rows of matches for the query, loop to get additional sets 
		while(num_results > num_retrieved) {
			oStart <- mStart
			mStart <- mStart + 100
			queryList[[i]] <- gsub(paste0("\"offset\":", oStart), paste0("\"offset\":", mStart), queryList[[i]])
			theURL <- httr::POST("https://api.reporter.nih.gov/v2/publications/search", httr::accept("text/plain"), httr::content_type_json(), body = queryList[[i]])
			theJ <- httr::content(theURL, as = "text")
			if (httr::http_error(theURL) == TRUE) {
				message("HTTP error.")
				print(httr::http_status(theURL))
				break
			}
			tempD <- jsonlite::fromJSON(theJ)
			theD[[i]] <- rbind(theD[[i]], tempD$results)
			num_retrieved <- num_retrieved + 100
			Sys.sleep(1)
		}
	}
	## put the results together into a single data frame, and then return it
	theD <- do.call(rbind, theD)
	message("Done")
	return(theD)
}