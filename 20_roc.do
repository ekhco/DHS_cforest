// ROC for DHS random forests

capture log close
clear

cd "~/QSU/DHS_boruta"

log using "20_roc", t replace
local start = "$S_TIME"

* ------------------------------------------------------------------------------
* ROC for DHS surveys
* Eric Chow, 09-25-2018
* appends validation (20%) predicted probabilities and does ROC
* ------------------------------------------------------------------------------

set more off




* look for outpat files
local roc_files: dir "data" files "*.dta"
local i = 1

* compress and save out 700 dta files
foreach file of local roc_files {
	di "`file'"
	if (`i' == 1) {
		use "data/`file'", clear
	}
	if (`i' > 1) {
		append using "data/`file'"
	}
	local i = `i' + 1 // increment 1
}

saveold "data/roc_ALL.dta", v(12) replace




if (1==0) {
				gen keep = 0
			    replace keep = 1 if inlist(survey, "ci50", "gh4a", "ke52", "ke51", "ls41", "ls61", "mw4a", "mw61")
				replace keep = 1 if inlist("mz51", "nm61", "rw51", "rw61", "sz51", "tz4a", "tz51", "tz6a", "ug6a")
				replace keep = 1 if inlist("zm51", "zw51", "zw61")
				
				drop if keep == 0
				
				replace ref = ref - 1
				label define ref1 0 "HIV-" 1 "HIV+"
				label values ref ref1

				roccomp ref prob if survey=="mw61", by(sex) graph summary aspect(1)
}









log close

*         ~ fin ~
