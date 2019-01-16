capture log close
clear

log using "_get_hiv_prev", t replace
local start = "$S_TIME"

* ------------------------------------------------------------------------------
* HIV XWAS merge
* Eric Chow, 8-28-2018
*
* goes through DHS live files on cluster and calculates the HIV prevalance
* ------------------------------------------------------------------------------

set more off

clear 
clear mata
clear matrix
set maxvar 18000 // IR: 4268, MR: 867, PR: 379 vars = 5514 total

cd  "D:\EricChow\DHS_live"          // where the merged output goes

* ------------------------------------------------------------------------------
* initialize final output file
* ------------------------------------------------------------------------------
 
//capture file close merge_master_file
//file open merge_master_file using "HIV_prevalence.csv", write replace
* write the header:
							  // country,  survey,   prevalence
//file write merge_master_file    "country,  survey,   prev" _newline
 
* ------------------------------------------------------------------------------
* go through directories
* ------------------------------------------------------------------------------

local ttl = 0
local mrg = 0
local country_dirs: dir . dirs "*" // get list of directories (countries)
	foreach cdirectory of local country_dirs {  // country-level: ie: Zambia
// forvalues i = `num_bgn'/`num_end' {  // country-level: ie: Zambia
// 	local cdirectory : word `i' of `country_dirs'
	di "`cdirectory':"
	quietly cd "`cdirectory'" // go into country folfer
	
	local survey_dirs: dir . dirs "*" // get list of surveys
	foreach sdirectory of local survey_dirs {  //survey-level (standrd DHS 2013)
		di "    `sdirectory':" _continue
		quietly cd "`sdirectory'" // go into survey

		
		* ----------------------------------------------------------------------
		* SEARCH FOR FLATTENED FILE
		* ----------------------------------------------------------------------
		
		* gets flattened file
		local flattened_file: dir "./`idirectory'" files "*flattened.dta"	
		
		* ----------------------------------------------------------------------
		* CALCULATE THE PREVALENCE
		* ----------------------------------------------------------------------
		
		local filename = subinstr(`"`flattened_file'"', ".dta", "",1)
 		use `filename', clear

		// does it have hiv03?
		capture confirm variable hiv03
		if !_rc {
			// if it does, count negative
			quietly count if hiv03 == 0
			local hiv_neg = r(N)
			
			// count positive
			quietly count if hiv03 == 1
			local hiv_pos = r(N) 
			
			// calcluate prevalance
			local prev_ = `hiv_neg' / (`hiv_neg' + `hiv_pos')
			local mrg = `mrg' + 1
			
        }
		else {
		   // no hiv03 var
		   local prev_ = ""
		   local hiv_neg = ""
		   local hiv_pos = ""
		}
		
		// write to log
		di   "`filename', `hiv_neg', `hiv_pos', `prev_'"
		
		local ttl = `ttl' + 1
		
		
		
		* ----------------------------------------------------------------------
		* WRITE OUT PREVALENCE LOG
		* ----------------------------------------------------------------------
		//file write merge_master_file    "`cdirectory', `sdirectory',  `prev_'" _newline

			
		quietly cd "../"    // leave survey directory
	}
	quietly cd "../"		// leave country directory
}
//file close merge_master_file
		
di "Number of country-surveys searched: `ttl'"
di "country-surveys for which HIV prevalance calculated: `mrg' (" `mrg'/`ttl'*100 "%)"

 
log close

*         ~ fin ~
