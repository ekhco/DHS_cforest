capture log close
clear

cd  "/Users/echow/DHS_live_abstract/DHS_live_abstract"          // where the merged output goes
log using "surveys_v12", t replace
local start = "$S_TIME"

* ------------------------------------------------------------------------------
* HIV XWAS merge
* Eric Chow, 8-28-2018
*
* turns all the DHS files into v12
* ------------------------------------------------------------------------------

set more off

clear 
clear mata
clear matrix
set maxvar 18000 // IR: 4268, MR: 867, PR: 379 vars = 5514 total

 
* ------------------------------------------------------------------------------
* get list of all files
* ------------------------------------------------------------------------------

* gets flattened file
local files: dir "." files "*.dta"	


foreach file of local files {
	use `file'
}
 
log close

*         ~ fin ~
