************************************************************************
*	Do-file:			researchproject.do
*	Project:			HRQoL Research Project
*	Data used:			"HUI3.dta"
* 	Purpose:  			To conduct analyses for research project
*	Date:				01 August 2021
************************************************************************

set more off
capture log close
version 

cd "/Users/chrisselman/University/Masters/Thesis/Datasets"

// Open the dataset
use "HUI3_original.dta", clear

// Create log file to keep copy of commands/results
log using researchproj.log, replace text 

//Only include relevant IDs 
drop if ep_elbw == .
drop if seen18 == 2

// Rename variables 
rename A0104 id
rename zuk_bweight bwtstd
rename HUI3_Scale_Score_18 HUI3_18
rename HUI3_Scale_Score_25 HUI3_25

// Create intellectual disability variable
gen iqim = . 

// Find mean IQ and SD for term-born controls at age 18
summ WASIiqstn if ep_elbw == 0

// Use to define intellectual disability variable
replace iqim = 1 if WASIiqstn < 92.60409
replace iqim = 0 if WASIiqstn >= 92.60409
replace iqim = . if WASIiqstn == .

// Generate new birthweight variable to ensure readable interpretations (convert kg to g)
gen bwtg = bwtkg*1000

// Ensure variables are in range
replace arithstn = . if arithstn == 9999 | arithstn == 7777
replace spellstn = . if spellstn == 9999 | spellstn == 7777
replace readstnd = . if readstnd == 9999 | readstnd == 7777

// BASELINE CHARACTERISTICS FOR THOSE WITH/WITHOUT FOLLOW-UP DATA AT AGE 25 YEARS
// Create missingness indicator for outcome
gen missin = .
replace missin = 1 if HUI3_25 == . & ep_elbw != .
replace missin = 0 if HUI3_25 != . & ep_elbw != .

// Comparison of participants with/without follow-up data at age 25 years
summ HUI3_25 if ep_elbw == 1, detail
summ HUI3_25 if ep_elbw == 0, detail

// Categorical variables (for each birth group)
foreach var of varlist male caesar sevpvl ivh3or4 surgeryn pnster lowmated8 preeclampsia anster disab2 disab5 disab8 cpyes8 lowsocialclass8 asthma8 blind8 deaf8 iqim o236week lowmated8 {
	tab `var' if HUI3_25 != . & ep_elbw == 1
	tab `var' if HUI3_25 == . & ep_elbw == 1
	tab `var' if HUI3_25 != . & ep_elbw == 0
	tab `var' if HUI3_25 == . & ep_elbw == 0
}

// Continuous variables (for each birth group)
foreach var of varlist gestage bwtg fscale8 utility8 HUI3_18 WASIiqstn  {
	summ `var' if HUI3_25 != . & ep_elbw == 1, detail
	summ `var' if HUI3_25 == . & ep_elbw == 1, detail
	summ `var' if HUI3_25 != . & ep_elbw == 0, detail
	summ `var' if HUI3_25 == . & ep_elbw == 0, detail
}

// Calculate ORs for predictors of missingness in HUI-3, correlation with HUI-3, and number of missing values in the original variable (by birth group)
foreach var of varlist male gestage bwtg bwtstd pnster ivh3or4 sevpvl brain_inj surgeryn o236week mdi pdi disab2 disab5 disab8 fscale5 cpyes8 iqim fscale8 WASIiqstn HUI3_18 utility8 spellstn readstnd arithstn abc8_rawscore utiltot8 utiliq8 totalchq psychsoc physical lowmated8 lowsocialclass8  {
	logit missin `var' if ep_elbw == 1, or 
	corr HUI3_25 `var' if ep_elbw == 1
}


foreach var of varlist male gestage bwtg bwtstd disab8 fscale5 iqim fscale8 WASIiqstn HUI3_18 utility8 spellstn readstnd arithstn abc8_rawscore totalchq psychsoc physical lowmated8 lowsocialclass8 {
	logit missin `var' if ep_elbw == 0, or 
	corr HUI3_25 `var' if ep_elbw == 0
}


// MULTIPLE IMPUTATION
// Comparing HRQoL at age 25 years 
// Create separate datasets for EP/ELBW and NBW group
use "HUI3.dta", clear
drop if ep_elbw == 1 | ep_elbw == .
save "HUI3_nbw.dta", replace

use "HUI3.dta", clear
drop if ep_elbw == 0 | ep_elbw == .
save "HUI3_elbw.dta", replace

//
use "HUI3_elbw.dta", clear
summ HUI3_18to25
// 45% had data for both 18 and 25

use "HUI3_nbw.dta", clear
summ HUI3_18to25
// 40% had data for both 18 and 25 

//MAR analyses
// Set imputation model for EP/ELBW group
* Create missing data indicators 
use "HUI3_elbw.dta", clear
gen miss_HUI3_25 = missing(HUI3_25)
gen miss_HUI3_18 = missing(HUI3_18)
gen miss_disab8 = missing(disab8)
gen miss_fscale5 = missing(fscale5)
gen miss_fscale8 = missing(fscale8)
gen miss_utility8 = missing(utility8)
gen miss_WASIiqstn = missing(WASIiqstn)
gen miss_spellstn = missing(spellstn)
gen miss_arithstn = missing(arithstn)
gen miss_readstnd = missing(readstnd)
gen miss_abc8_rawscore = missing(abc8_rawscore)
gen miss_physical = missing(physical)
gen miss_psychsoc = missing(psychsoc)
gen miss_lowsocialclass8 = missing(lowsocialclass8)
save "HUI3_elbw.dta", replace

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) HUI3_25 fscale8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(trace, replace) rseed(5423)
save "HUI3_elbw_im.dta", replace

// Set imputation model for term-born/NBW group
* Create missing data indicators 
use "HUI3_nbw.dta", clear
gen miss_HUI3_25 = missing(HUI3_25)
gen miss_HUI3_18 = missing(HUI3_18)
gen miss_disab8 = missing(disab8)
gen miss_fscale5 = missing(fscale5)
gen miss_fscale8 = missing(fscale8)
gen miss_utility8 = missing(utility8)
gen miss_WASIiqstn = missing(WASIiqstn)
gen miss_spellstn = missing(spellstn)
gen miss_arithstn = missing(arithstn)
gen miss_readstnd = missing(readstnd)
gen miss_abc8_rawscore = missing(abc8_rawscore)
gen miss_physical = missing(physical)
gen miss_psychsoc = missing(psychsoc)
gen miss_lowsocialclass8 = missing(lowsocialclass8)
save "HUI3_nbw.dta", replace

// Set imputation model for NBW group
mi set flong 
mi register regular gestage bwtstd male multiple_yesno miss_fscale5 miss_WASIiqstn miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 lowsocialclass8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowmated8

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) HUI3_25 fscale8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 fscale5 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) savetrace(trace_nbw, replace) burnin(10) rseed(5423)
save "HUI3_nbw_im.dta", replace

//Analysis
use "HUI3_nbw_im.dta", clear
mi append using HUI3_elbw_im

//Unadjusted 
mi estimate: qreg HUI3_25 i.ep_elbw, vce(robust)
mi estimate (_b[_cons] + _b[ep_elbw]): qreg HUI3_25 ep_elbw, vce(robust) quantile(0.5)

mi estimate: qreg HUI3_18 i.ep_elbw, vce(robust)
mi estimate (_b[_cons] + _b[ep_elbw]): qreg HUI3_18 ep_elbw, vce(robust) quantile(0.5)



// Adjusted analysis (quantile regression) - 18 and 25 years
mi estimate: qreg HUI3_25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)
mi estimate: qreg HUI3_18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)


//Complete case analysis - 18 and 25 years
use "HUI3.dta", clear

qreg HUI3_25 i.ep_elbw i.lowmated8 i.lowsocialclass8, vce(robust)
qreg HUI3_18 i.ep_elbw i.lowmated8 i.lowsocialclass8, vce(robust)

//Unadjusted - 18 and 25 years:
qreg HUI3_25 i.ep_elbw, vce(robust)
qreg HUI3_18 i.ep_elbw, vce(robust)


//Sensitivity analyses
// MSP = -0.2, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.099
gen offset_hui3_18 = miss_HUI3_18*-0.019	
// CSPs: -0.099, -0.019

* Prepare dataset
mi set flong 

mi register regular gestage bwtstd male brain_inj surgeryn pnster o236week multiple_yesno miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 (ologit) disab8 = gestage bwtstd i.male i.brain_inj i.surgeryn i.pnster i.o236week i.multiple_yesno i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens1, replace) rseed(5423)

save "HUI3_elbw_im_0.20.dta", replace

use "HUI3_elbw_im_0.20.dta", clear

//Check we reach a tolerance of 0.005 for our MSPs
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.1, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.037
gen offset_hui3_18 = miss_HUI3_18*0.046

// CSPs: -0.037, 0.046
* Prepare dataset
mi set flong 

mi register regular gestage bwtstd male brain_inj surgeryn pnster o236week multiple_yesno  miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 (ologit) disab8 = gestage bwtstd i.male i.brain_inj i.surgeryn i.pnster i.o236week i.multiple_yesno i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens2, replace) rseed(5423)

save "HUI3_elbw_im_0.10.dta", replace

use "HUI3_elbw_im_0.10.dta", clear

//Check we reach a tolerance of 0.005 for our MSPs
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18

// MSP = -0.05, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.001
gen offset_hui3_18 = miss_HUI3_18*0.078
// CSPs: -0.001; 0.078
* Prepare dataset
mi set flong 

mi register regular gestage bwtstd male brain_inj surgeryn pnster o236week multiple_yesno miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 (ologit) disab8 = gestage bwtstd i.male i.brain_inj i.surgeryn i.pnster i.o236week i.multiple_yesno i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens3, replace) rseed(5423)

save "HUI3_elbw_im_0.05.dta", replace

use "HUI3_elbw_im_0.05.dta", clear

//Check we reach a tolerance of 0.005 for our MSPs
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.03, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*0.01
gen offset_hui3_18 = miss_HUI3_18*0.092
// CSPs: 0.01, 0.092
* Prepare dataset
mi set flong 

mi register regular gestage bwtstd male brain_inj surgeryn pnster o236week multiple_yesno 
mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8 lowmated8

mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 (ologit) disab8 = gestage bwtstd i.male i.brain_inj i.surgeryn i.pnster i.o236week i.multiple_yesno i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens4, replace) rseed(5423)

save "HUI3_elbw_im_0.03.dta", replace
use "HUI3_elbw_im_0.03.dta", clear

//Check we reach a tolerance of 0.005 for our MSPs
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.01, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*0.02
gen offset_hui3_18 = miss_HUI3_18*0.109
//CSPs: 0.02, 0.109

* Prepare dataset
mi set flong 

mi register regular gestage bwtstd male brain_inj surgeryn pnster o236week multiple_yesno miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8 

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 (ologit) disab8 = gestage bwtstd i.male i.brain_inj i.surgeryn i.pnster i.o236week i.multiple_yesno i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens5, replace) rseed(5423)

save "HUI3_elbw_im_0.01.dta", replace
use "HUI3_elbw_im_0.01.dta", clear

//Check we reach a tolerance of 0.005 for our MSPs
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// NBW
	//Sensitivity analyses
	* Prepare dataset
	// MSP = -0.20, tolerance = 0.005
	use "HUI3_nbw", clear

	* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
	gen offset_hui3_25 = miss_HUI3_25*-0.156
	gen offset_hui3_18 = miss_HUI3_18*-0.092
	//CSPs: -0.156, -0.092

	mi set flong 

	mi register regular gestage bwtstd male multiple_yesno miss_fscale5 miss_WASIiqstn miss_psychsoc miss_lowsocialclass8

	mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

	mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens6, replace) rseed(5423)

	save "HUI3_nbw_im_0.20.dta", replace

	use "HUI3_nbw_im_0.20.dta", clear
	
//Confirm MSP is within 0.005 of elicited value
	mi estimate: reg HUI3_25 miss_HUI3_25
	mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.10, tolerance = 0.005
use "HUI3_nbw", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.063
gen offset_hui3_18 = miss_HUI3_18*-0.005
//CSPs: -0.063; 0.005

mi set flong 

mi register regular gestage bwtstd male multiple_yesno miss_fscale5 miss_WASIiqstn miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

	mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens7, replace) rseed(5423)

save "HUI3_nbw_im_0.10.dta", replace

use "HUI3_nbw_im_0.10.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.05, tolerance = 0.005
use "HUI3_nbw", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.02
gen offset_hui3_18 = miss_HUI3_18*0.042
//CSPs: -0.02; 0.042

mi set flong 

mi register regular gestage bwtstd male multiple_yesno miss_fscale5 miss_WASIiqstn miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

	mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens8, replace) rseed(5423)

save "HUI3_nbw_im_0.05.dta", replace

use "HUI3_nbw_im_0.05.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.03, tolerance = 0.005
use "HUI3_nbw", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.002
gen offset_hui3_18 = miss_HUI3_18*0.064
//CSPs: -0.002, 0.064

mi set flong 

mi register regular gestage bwtstd male multiple_yesno miss_fscale5 miss_WASIiqstn miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

	mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens9, replace) rseed(5423)

save "HUI3_nbw_im_0.03.dta", replace

use "HUI3_nbw_im_0.03.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MSP = -0.01, tolerance = 0.005
use "HUI3_nbw", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*0.02
gen offset_hui3_18 = miss_HUI3_18*0.08
//CSPs: 0.02; 0.08

mi set flong 

mi register regular gestage bwtstd male multiple_yesno miss_fscale5 miss_WASIiqstn miss_psychsoc miss_lowsocialclass8

mi register imputed HUI3_25 disab8 fscale5 fscale8 utility8 HUI3_18 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

	mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(10) savetrace(tracesens10, replace) rseed(5423)

save "HUI3_nbw_im_0.01.dta", replace

use "HUI3_nbw_im_0.01.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18


// MNAR Analyses
// MSP = -0.20
use "HUI3_elbw_im_0.20.dta", clear
mi append using "HUI3_nbw_im_0.20.dta"

// Adjusted analysis (quantile regression)
mi estimate: qreg HUI3_25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

mi estimate: qreg HUI3_18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

// MSP = -0.10
use "HUI3_elbw_im_0.10.dta", clear
mi append using "HUI3_nbw_im_0.10.dta"

// Adjusted analysis (quantile regression)
mi estimate: qreg HUI3_25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

mi estimate: qreg HUI3_18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)


// MSP = -0.05
use "HUI3_elbw_im_0.05.dta", clear
mi append using "HUI3_nbw_im_0.05.dta"

// Adjusted analysis (quantile regression)
mi estimate: qreg HUI3_25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

mi estimate: qreg HUI3_18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

// MSP = -0.03
use "HUI3_elbw_im_0.03.dta", clear
mi append using "HUI3_nbw_im_0.03.dta"

// Adjusted analysis (quantile regression)
mi estimate: qreg HUI3_25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

mi estimate: qreg HUI3_18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)


// MSP = -0.01
use "HUI3_elbw_im_0.01.dta", clear
mi append using "HUI3_nbw_im_0.01.dta"

// Adjusted analysis (quantile regression)
mi estimate: qreg HUI3_25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)

mi estimate: qreg HUI3_18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust)


// INTELLECTUAL DISABILITY 
use "HUI3_elbw.dta", clear
//Complete case analysis
qreg HUI3_25 i.iqim, vce(robust) 
qreg HUI3_18 i.iqim, vce(robust) 


//Adjusted 
qreg HUI3_25 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
qreg HUI3_18 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) 


//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 HUI3_18 iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8

// Run the imputation model 
mi impute chained (pmm, knn(10)) HUI3_25 fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 HUI3_18 (logit) lowmated8 iqim lowsocialclass8 (ologit) disab8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(10) savetrace(traceid, replace) rseed(5423)

save "HUI3_elbw_dis_im.dta", replace

use "HUI3_elbw_dis_im.dta", clear

// Find difference in median HUI3 between intellectual disability groups - MAR
mi estimate: qreg HUI3_25 i.iqim, vce(robust) 
mi estimate: qreg HUI3_18 i.iqim, vce(robust) 

mi estimate (_b[_cons] + _b[iqim]): qreg HUI3_25 iqim, vce(robust) quantile(0.5)
mi estimate (_b[_cons] + _b[iqim]): qreg HUI3_18 iqim, vce(robust) quantile(0.5)


//Adjusted 
mi estimate: qreg HUI3_25 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
mi estimate: qreg HUI3_18 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) 



//MNAR Analysis 

//Sensitivity analyses
// MSP = -0.2, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.099
gen offset_hui3_18 = miss_HUI3_18*-0.015
// CSPs: -0.099, -0.015

// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 HUI3_18 iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8

// Run the imputation model 
mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 (ologit) disab8 = gestage bwtstd male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(10) savetrace(traceid1, replace) rseed(5423)

save "HUI3_elbw_dis_im_0.20.dta", replace

use "HUI3_elbw_dis_im_0.20.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18

// Find difference in median HUI3 between intellectual disability groups - MNAR
mi estimate: qreg HUI3_25 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
mi estimate: qreg HUI3_18 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 


// MSP = -0.1, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.04
gen offset_hui3_18 = miss_HUI3_18*0.049
// CSPs: -0.04; 0.049

// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 HUI3_18 iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8

// Run the imputation model 
mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) iqim lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(10) savetrace(traceid2, replace) rseed(5423)

save "HUI3_elbw_dis_im_0.10.dta", replace

use "HUI3_elbw_dis_im_0.10.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18

// Find difference in median HUI3 between intellectual disability groups - MNAR
mi estimate: qreg HUI3_25 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
mi estimate: qreg HUI3_18 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 


// MSP = -0.05, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*-0.007
gen offset_hui3_18 = miss_HUI3_18*0.08
// CSPs: -0.007, 0.08

// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 HUI3_18 iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8

// Run the imputation model 
mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 (ologit) disab8 = gestage bwtstd male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(10) savetrace(traceid3, replace) rseed(5423)

save "HUI3_elbw_dis_im_0.05.dta", replace

use "HUI3_elbw_dis_im_0.05.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18

// Find difference in median HUI3 between intellectual disability groups - MNAR
mi estimate: qreg HUI3_25 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
mi estimate: qreg HUI3_18 iqim i.lowmated8 i.lowsocialclass8, vce(robust)  


// MSP = -0.03, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*0.004
gen offset_hui3_18 = miss_HUI3_18*0.0965
// CSPs: 0.004, 0.0965

// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 HUI3_18 iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8

// Run the imputation model 
mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 (ologit) disab8 = gestage bwtstd male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(10) savetrace(traceid4, replace) rseed(5423)

save "HUI3_elbw_dis_im_0.03.dta", replace

use "HUI3_elbw_dis_im_0.03.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18

// Find difference in median HUI3 between intellectual disability groups - MNAR
mi estimate: qreg HUI3_25 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
mi estimate: qreg HUI3_18 iqim i.lowmated8 i.lowsocialclass8, vce(robust)  



// MSP = -0.01, tolerance = 0.005
use "HUI3_elbw.dta", clear

* Using the elicited delta values, create "offset" variables that will be incorporated into the NARFCS procedure
gen offset_hui3_25 = miss_HUI3_25*0.02
gen offset_hui3_18 = miss_HUI3_18*0.106
// CSPs: 0.02, 0.106

// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUI3_25 HUI3_18 iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8

// Run the imputation model 
mi impute chained (regress, offset(offset_hui3_25)) HUI3_25 (regress, offset(offset_hui3_18)) HUI3_18 (pmm, knn(10)) fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 (ologit) disab8 = gestage bwtstd male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(10) savetrace(traceid5, replace) rseed(5423)

save "HUI3_elbw_dis_im_0.01.dta", replace

use "HUI3_elbw_dis_im_0.01.dta", clear

//Confirm MSP is within 0.005 of elicited value
mi estimate: reg HUI3_25 miss_HUI3_25
mi estimate: reg HUI3_18 miss_HUI3_18

// Find difference in median HUI3 between intellectual disability groups - MNAR
mi estimate: qreg HUI3_25 iqim i.lowmated8 i.lowsocialclass8, vce(robust) 
mi estimate: qreg HUI3_18 iqim i.lowmated8 i.lowsocialclass8, vce(robust)  


// GRAPHS 
// Use data from output (entered into its own dataset) - birth group
use "plots1.dta", clear

twoway rcap lci uci age if group == 1, lc(black) || ///
rcap lci uci age if group == 2, lc(gs8) legend(label(1 "Control") label(2 "ELBW/EP"))  ///
|| scatter medianhui3 age if group == 1, mcolor(black)  ///
|| scatter medianhui3 age if group == 2, mcolor(gs8) ytitle("HUI-3") ylabel(0.82 "0.82" 0.84 "0.84" 0.86 "0.86" 0.88 "0.88" 0.90 "0.90" 0.92 "0.92" 0.94 "0.94" 0.96 "0.96" 0.98 "0.98" 1 "1.0", angle(horizontal) labsize(small))  xlabels(18 25) yscale(range(0.82 1)) xscale(range(17 26)) || line medianhui3 age if group == 1, lc(black) lpattern(dot) || line medianhui3 age if group == 2, lc(gs8) xtitle("Age at Assessment (Years)") graphregion(color(white)) bgcolor(white) legend(order(1 "Control" 2 "ELBW/EP")) lpattern(dot)


// Graph for intellectual disability groups
use "plots2.dta", clear

twoway rcap lci uci age if group == 1, lc(black) || ///
rcap lci uci age if group == 2, lc(gs8) ///
|| scatter medianhui3 age if group == 1, mcolor(black)  ///
|| scatter medianhui3 age if group == 2, mcolor(gs8) ytitle("HUI-3") ylabel(0.76 "0.76" 0.78 "0.78" 0.80 "0.80" 0.82 "0.82" 0.84 "0.84" 0.86 "0.86" 0.88 "0.88" 0.90 "0.90" 0.92 "0.92" 0.94 "0.94" 0.96 "0.96" 0.98 "0.98" 1 "1.0", angle(horizontal) labsize(small)) xlabels(18 25) yscale(range(0.79 1)) xscale(range(17 26)) || line medianhui3 age if group == 1, lc(black) lpattern(dot) || line medianhui3 age if group == 2, lc(gs8) xtitle("Age at Assessment (Years)") graphregion(color(white)) bgcolor(white) legend(order(1 "Higher IQ" 2 "Lower IQ")) lpattern(dot)


// Sub-scale analysis 
use "VICS 9192_HUIsubscales.dta", clear
rename a0104 id
rename twinid twinID 

keep id HUIMvis_recode_18yr HUIMhear_recode_18yr HUIMspe_recode_18yr HUIMmove_recode_18yr HUIMdext_recode_18yr HUIMemo_recode_18yr HUIMcog_recoded_18yr HUIMpain_recoded_18yr huivisrc_25yr huihearrc_25yr huispeechrc_25yr huiamburc_25yr huidextrc_25yr huiemorc_25yr huicognrc_25yr huipainrc_25yr

//Merge datasets 
merge 1:1 id using "HUI3.dta" 
drop if ep_elbw == .


save "HUI3_secondary.dta", replace 

// Create separate datasets for EP/ELBW and NBW group
drop if ep_elbw == 1 | ep_elbw == .
save "HUI3_nbw_s.dta", replace

use "HUI3_secondary.dta", clear
drop if ep_elbw == 0 | ep_elbw == .
save "HUI3_elbw_s.dta", replace



//MAR analyses
// Set imputation model for EP/ELBW group
* Create missing data indicators 
use "HUI3_elbw_s.dta", clear
gen miss_disab8 = missing(disab8)
gen miss_fscale5 = missing(fscale5)
gen miss_fscale8 = missing(fscale8)
gen miss_utility8 = missing(utility8)
gen miss_WASIiqstn = missing(WASIiqstn)
gen miss_spellstn = missing(spellstn)
gen miss_arithstn = missing(arithstn)
gen miss_readstnd = missing(readstnd)
gen miss_abc8_rawscore = missing(abc8_rawscore)
gen miss_physical = missing(physical)
gen miss_psychsoc = missing(psychsoc)
gen miss_lowsocialclass8 = missing(lowsocialclass8)
save "HUI3_elbw_s.dta", replace

use "HUI3_elbw_s.dta", clear
// Imputation model for vision 
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week 
 
mi register imputed disab8 fscale5 fscale8 utility8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8 huivisrc_25yr HUIMvis_recode_18yr

mi xtset, clear 

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) huivisrc_25yr HUIMvis_recode_18yr fscale5 fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_im_s_vis.dta", replace



// Imputation model for hearing 
use "HUI3_elbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week 

mi register imputed disab8 fscale5 fscale8 utility8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8 huihearrc_25yr HUIMhear_recode_18yr

mi xtset, clear 

// Run imputation model for EP/ELBW group
// We have removed disability and IQ at 5 to ensure the model reached convergence 
mi impute chained (pmm, knn(10)) huihearrc_25yr HUIMhear_recode_18yr fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 (logit) lowsocialclass8 lowmated8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week, augment add(50)  burnin(20) rseed(5423)

save "HUI3_elbw_im_hear.dta", replace


// Imputation model for speech
use "HUI3_elbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMspe_recode_18yr disab8 fscale8 utility8 huispeechrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) huispeechrc_25yr fscale8 HUIMspe_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_im_s_speech.dta", replace


// Imputation model for ambulation 
use "HUI3_elbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMmove_recode_18yr disab8 fscale5 fscale8 utility8 huiamburc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) huiamburc_25yr fscale8 HUIMmove_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(20) rseed(5423)

save "HUI3_elbw_im_s_ambul.dta", replace



// Imputation model for emotion 
use "HUI3_elbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMemo_recode_18yr disab8 fscale5 fscale8 utility8 huiemorc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) huiemorc_25yr fscale8 HUIMemo_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(20) rseed(5423)

save "HUI3_elbw_im_s_em.dta", replace




// Imputation model for cognition 
use "HUI3_elbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMcog_recoded_18yr disab8 fscale5 fscale8 utility8 huicognrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) huicognrc_25yr fscale8 HUIMcog_recoded_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(20) rseed(5423)

save "HUI3_elbw_im_s_cog.dta", replace


// Imputation model for pain  
use "HUI3_elbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMpain_recoded_18yr disab8 fscale5 fscale8 utility8 huipainrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for EP/ELBW group
mi impute chained (pmm, knn(10)) huipainrc_25yr fscale8 HUIMpain_recoded_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowsocialclass8 lowmated8 (ologit) disab8 = gestage bwtstd i.male i.multiple_yesno i.brain_inj i.surgeryn i.pnster i.o236week i.miss_fscale5 i.miss_fscale8 i.miss_WASIiqstn i.miss_spellstn i.miss_physical i.miss_psychsoc i.miss_lowsocialclass8, augment add(50)  burnin(20) rseed(5423)

save "HUI3_elbw_im_s_pain.dta", replace
 
 

//MAR analyses
// Set imputation model for NBW group
* Create missing data indicators 
use "HUI3_nbw_s.dta", clear
gen miss_disab8 = missing(disab8)
gen miss_fscale5 = missing(fscale5)
gen miss_fscale8 = missing(fscale8)
gen miss_utility8 = missing(utility8)
gen miss_WASIiqstn = missing(WASIiqstn)
gen miss_spellstn = missing(spellstn)
gen miss_arithstn = missing(arithstn)
gen miss_readstnd = missing(readstnd)
gen miss_abc8_rawscore = missing(abc8_rawscore)
gen miss_physical = missing(physical)
gen miss_psychsoc = missing(psychsoc)
gen miss_lowsocialclass8 = missing(lowsocialclass8)
save "HUI3_nbw_s.dta", replace

use "HUI3_nbw_s.dta", clear
// Imputation model for vision 
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed disab8 fscale5 fscale8 utility8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8 HUIMvis_recode_18yr huivisrc_25yr

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) HUIMvis_recode_18yr huivisrc_25yr fscale8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_s_vis.dta", replace



// Imputation model for hearing 
use "HUI3_nbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week 

mi register imputed disab8 fscale5 fscale8 utility8 WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8 HUIMhear_recode_18yr huihearrc_25yr

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) HUIMhear_recode_18yr huihearrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_hear.dta", replace



// Imputation model for speech
use "HUI3_nbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMspe_recode_18yr disab8 fscale5 fscale8 utility8 huispeechrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) huispeechrc_25yr fscale8 HUIMspe_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_s_speech.dta", replace


// Imputation model for ambulation 
use "HUI3_nbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMmove_recode_18yr disab8 fscale5 fscale8 utility8 huiamburc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) huiamburc_25yr fscale8 HUIMmove_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 fscale5 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_s_ambul.dta", replace


// Imputation model for dexterity
// Model did not run for NBW due to perfect prediction, so we will run the imputation model
// by including birth group as a variable (and not impute separately) just for this sub-scale
use "HUI3_secondary.dta", clear 
drop if ep_elbw == .

mi set flong 

mi register regular gestage male multiple_yesno brain_inj surgeryn pnster o236week ep_elbw bwtstd
 
mi register imputed HUIMdext_recode_18yr disab8 fscale5 fscale8 utility8 huidextrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8 

mi xtset, clear 

// Run imputation model for NBW group - 
mi impute chained (pmm, knn(10)) huidextrc_25yr HUIMdext_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 fscale5 (logit) lowmated8 lowsocialclass8 = gestage i.male i.multiple_yesno bwtstd, augment add(50) burnin(20) rseed(5423)

save "HUI3_im_s_dex.dta", replace



// Imputation model for emotion 
use "HUI3_nbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMemo_recode_18yr disab8 fscale5 fscale8 utility8 huiemorc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) huiemorc_25yr fscale8 HUIMemo_recode_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 fscale5 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_s_em.dta", replace



// Imputation model for cognition 
use "HUI3_nbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMcog_recoded_18yr disab8 fscale5 fscale8 utility8 huicognrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) huicognrc_25yr fscale8 HUIMcog_recoded_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 fscale5 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_s_cog.dta", replace


// Imputation model for pain  
use "HUI3_nbw_s.dta", clear

mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed HUIMpain_recoded_18yr disab8 fscale5 fscale8 utility8 huipainrc_25yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc lowsocialclass8 lowmated8

mi xtset, clear 

// Run imputation model for NBW group
mi impute chained (pmm, knn(10)) huipainrc_25yr fscale8 HUIMpain_recoded_18yr WASIiqstn spellstn arithstn readstnd abc8_rawscore physical psychsoc utility8 fscale5 (logit) lowmated8 lowsocialclass8 = gestage bwtstd i.male i.multiple_yesno i.miss_fscale5 i.miss_WASIiqstn i.miss_psychsoc i.miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_nbw_im_s_pain.dta", replace
 
 
  
//Append imputation datasets 
//Vision 
//Analysis
use "HUI3_elbw_im_s_vis.dta", clear
mi append using HUI3_nbw_im_s_vis

mi passive: generate visoptimal25 = 1 if huivisrc_25yr < 1 
mi passive: replace visoptimal25 = 0 if huivisrc_25yr == 1

//18 yrs 
mi passive: gen visoptimal18 = 1 if HUIMvis_recode_18yr < 1 
mi passive: replace visoptimal18 = 0 if HUIMvis_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg visoptimal25 i.ep_elbw i.lowmated8 i.lowsocialclass8, vce(robust) rd 
mi estimate: binreg visoptimal18 i.ep_elbw i.lowmated8 i.lowsocialclass8, vce(robust) rd 


** REMOVED SOCIAL CLASS **
//Hearing
//Analysis
use "HUI3_elbw_im_hear.dta", clear
mi append using HUI3_nbw_im_hear 

mi passive: generate hearoptimal25 = 1 if huihearrc_25yr < 1 
mi passive: replace hearoptimal25 = 0 if huihearrc_25yr == 1

//18 yrs 
mi passive: gen hearoptimal18 = 1 if HUIMhear_recode_18yr < 1 
mi passive: replace hearoptimal18 = 0 if HUIMhear_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg hearoptimal25 i.ep_elbw i.lowmated8, vce(robust) rd 
mi estimate: binreg hearoptimal18 i.ep_elbw i.lowmated8, vce(robust) rd  


** REMOVED SOCIAL CLASS **
//Speech
//Analysis
use "HUI3_elbw_im_s_speech.dta", clear
mi append using HUI3_nbw_im_s_speech

mi passive: generate speechoptimal25 = 1 if huispeechrc_25yr < 1 
mi passive: replace speechoptimal25 = 0 if huispeechrc_25yr == 1

//18 yrs 
mi passive: gen speechoptimal18 = 1 if HUIMspe_recode_18yr < 1 
mi passive: replace speechoptimal18 = 0 if HUIMspe_recode_18yr == 1


// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg speechoptimal25 i.ep_elbw i.lowmated8, vce(robust) rd
mi estimate: binreg speechoptimal18 i.ep_elbw i.lowmated8, vce(robust) rd  



//Ambulation 
//Analysis
use "HUI3_elbw_im_s_ambul.dta", clear
mi append using HUI3_nbw_im_s_ambul

mi passive: generate amboptimal25 = 1 if huiamburc_25yr < 1 
mi passive: replace amboptimal25 = 0 if huiamburc_25yr == 1

//18 yrs 
mi passive: gen amboptimal18 = 1 if HUIMmove_recode_18yr < 1 
mi passive: replace amboptimal18 = 0 if HUIMmove_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg amboptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg amboptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Dexterity  
//Analysis
use "HUI3_im_s_dex.dta", clear

mi passive: generate dextoptimal25 = 1 if huidextrc_25yr < 1 
mi passive: replace dextoptimal25 = 0 if huidextrc_25yr == 1

//18 yrs 
mi passive: gen dextoptimal18 = 1 if HUIMdext_recode_18yr < 1 
mi passive: replace dextoptimal18 = 0 if HUIMdext_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg dextoptimal25 i.ep_elbw i.lowmated8 i.lowsocialclass8, vce(robust) rd
mi estimate: binreg dextoptimal18 i.ep_elbw i.lowmated8 i.lowsocialclass, vce(robust) rd  


//Emotion 
//Analysis
use "HUI3_elbw_im_s_em.dta", clear
mi append using HUI3_nbw_im_s_em 

mi passive: generate emoptimal25 = 1 if huiemorc_25yr < 1 
mi passive: replace emoptimal25 = 0 if huiemorc_25yr == 1

//18 yrs 
mi passive: gen emoptimal18 = 1 if HUIMemo_recode_18yr < 1 
mi passive: replace emoptimal18 = 0 if HUIMemo_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg emoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg emoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Cognition    
//Analysis
use "HUI3_elbw_im_s_cog.dta", clear
mi append using HUI3_nbw_im_s_cog 

mi passive: generate cogoptimal25 = 1 if huicognrc_25yr < 1 
mi passive: replace cogoptimal25 = 0 if huicognrc_25yr == 1

//18 yrs 
mi passive: gen cogoptimal18 = 1 if HUIMcog_recoded_18yr < 1 
mi passive: replace cogoptimal18 = 0 if HUIMcog_recoded_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg cogoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg cogoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Pain   
//Analysis
use "HUI3_elbw_im_s_pain.dta", clear
mi append using HUI3_nbw_im_s_pain 

mi passive: generate painoptimal25 = 1 if huipainrc_25yr < 1 
mi passive: replace painoptimal25 = 0 if huipainrc_25yr == 1

//18 yrs 
mi passive: gen painoptimal18 = 1 if HUIMpain_recoded_18yr < 1 
mi passive: replace painoptimal18 = 0 if HUIMpain_recoded_18yr == 1 

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg painoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg painoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  



//Complete case analysis - 18 and 25 years
// Sub-scale analysis 
use "HUI3_secondary.dta", clear

//Vision 
gen visoptimal25 = 1 if huivisrc_25yr < 1 
replace visoptimal25 = 0 if huivisrc_25yr == 1

//18 yrs 
gen visoptimal18 = 1 if HUIMvis_recode_18yr < 1 
replace visoptimal18 = 0 if HUIMvis_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg visoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg visoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd 

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg visoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg visoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd 


//Hearing
gen hearoptimal25 = 1 if huihearrc_25yr < 1 
replace hearoptimal25 = 0 if huihearrc_25yr == 1

//18 yrs 
gen hearoptimal18 = 1 if HUIMhear_recode_18yr < 1 
replace hearoptimal18 = 0 if HUIMhear_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg hearoptimal25 i.ep_elbw i.lowmated8, vce(robust) rd
binreg hearoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg hearoptimal25 i.iqim i.lowmated8, vce(robust) rd
binreg hearoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd 


//Speech
gen speechoptimal25 = 1 if huispeechrc_25yr < 1 
replace speechoptimal25 = 0 if huispeechrc_25yr == 1

//18 yrs 
gen speechoptimal18 = 1 if HUIMspe_recode_18yr < 1
replace speechoptimal18 = 0 if HUIMspe_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg speechoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg speechoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg speechoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg speechoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Ambulation 
gen amboptimal25 = 1 if huiamburc_25yr < 1 
replace amboptimal25 = 0 if huiamburc_25yr == 1

//18 yrs 
gen amboptimal18 = 1 if HUIMmove_recode_18yr < 1 
replace amboptimal18 = 0 if HUIMmove_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg amboptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg amboptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg amboptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg amboptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Dexterity  
gen dextoptimal25 = 1 if huidextrc_25yr < 1 
replace dextoptimal25 = 0 if huidextrc_25yr == 1

//18 yrs 
gen dextoptimal18 = 1 if HUIMdext_recode_18yr < 1 
replace dextoptimal18 = 0 if HUIMdext_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg dextoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg dextoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg dextoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg dextoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Emotion 
gen emoptimal25 = 1 if huiemorc_25yr < 1
replace emoptimal25 = 0 if huiemorc_25yr == 1

//18 yrs 
gen emoptimal18 = 1 if HUIMemo_recode_18yr < 1 
replace emoptimal18 = 0 if HUIMemo_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg emoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg emoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg emoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg emoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  



//Cognition    
gen cogoptimal25 = 1 if huicognrc_25yr < 1 
replace cogoptimal25 = 0 if huicognrc_25yr == 1

//18 yrs 
gen cogoptimal18 = 1 if HUIMcog_recoded_18yr < 1 
replace cogoptimal18 = 0 if HUIMcog_recoded_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg cogoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg cogoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg cogoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg cogoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Pain   
gen painoptimal25 = 1 if huipainrc_25yr < 1 
replace painoptimal25 = 0 if huipainrc_25yr == 1

//18 yrs 
gen painoptimal18 = 1 if HUIMpain_recoded_18yr < 1 
replace painoptimal18 = 0 if HUIMpain_recoded_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
binreg painoptimal25 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg painoptimal18 i.ep_elbw i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ 
// Adjusted analysis (binomial regression) - 18 and 25 years
binreg painoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
binreg painoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  

//Low IQ analysis 
use "HUI3_elbw_s.dta", clear

//Vision 
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed huivisrc_25yr HUIMvis_recode_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huivisrc_25yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 HUIMvis_recode_18yr (logit) lowmated8 iqim lowsocialclass8 (ologit) disab8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_vis.dta", replace

//Vision 
//Analysis
use "HUI3_elbw_dis_im_vis.dta", clear

mi passive: generate visoptimal25 = 1 if huivisrc_25yr < 1 
mi passive: replace visoptimal25 = 0 if huivisrc_25yr == 1

//18 yrs 
mi passive: gen visoptimal18 = 1 if HUIMvis_recode_18yr < 1 
mi passive: replace visoptimal18 = 0 if HUIMvis_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg visoptimal25 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) rd 
mi estimate: binreg visoptimal18 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) rd 



use "HUI3_elbw_s.dta", clear

//Hearing  
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week
 
mi register imputed huihearrc_25yr HUIMhear_recode_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huihearrc_25yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 HUIMhear_recode_18yr (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_hear.dta", replace

//Hearing  
//Analysis
use "HUI3_elbw_dis_im_hear.dta", clear

mi passive: generate hearoptimal25 = 1 if huihearrc_25yr < 1 
mi passive: replace hearoptimal25 = 0 if huihearrc_25yr == 1

//18 yrs 
mi passive: gen hearoptimal18 = 1 if HUIMhear_recode_18yr < 1 
mi passive: replace hearoptimal18 = 0 if HUIMhear_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg hearoptimal25 i.iqim i.lowmated8, vce(robust) rd 
mi estimate: binreg hearoptimal18 i.iqim i.lowmated8, vce(robust) rd  


//Speech 
//MAR 
use "HUI3_elbw_s.dta", clear
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed huispeechrc_25yr HUIMspe_recode_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huispeechrc_25yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 HUIMspe_recode_18yr (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_speech.dta", replace

//Speech
//Analysis
use "HUI3_elbw_dis_im_speech.dta", clear

mi passive: generate speechoptimal25 = 1 if huispeechrc_25yr < 1 
mi passive: replace speechoptimal25 = 0 if huispeechrc_25yr == 1

//18 yrs 
mi passive: gen speechoptimal18 = 1 if HUIMspe_recode_18yr < 1 
mi passive: replace speechoptimal18 = 0 if HUIMspe_recode_18yr == 1


// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg speechoptimal25 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) rd
mi estimate: binreg speechoptimal18 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) rd


// Ambulation
use "HUI3_elbw_s.dta", clear 
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed huiamburc_25yr HUIMmove_recode_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huiamburc_25yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 HUIMmove_recode_18yr (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_amb.dta", replace

//Ambulation 
//Analysis
use "HUI3_elbw_dis_im_amb.dta", clear

mi passive: generate amboptimal25 = 1 if huiamburc_25yr < 1 
mi passive: replace amboptimal25 = 0 if huiamburc_25yr == 1

//18 yrs 
mi passive: gen amboptimal18 = 1 if HUIMmove_recode_18yr < 1 
mi passive: replace amboptimal18 = 0 if HUIMmove_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg amboptimal25 i.iqim i.lowmated8, vce(robust) rd
mi estimate: binreg amboptimal18 i.iqim i.lowmated8, vce(robust) rd 


//Dexterity 
use "HUI3_elbw_s.dta", clear 
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed huidextrc_25yr HUIMdext_recode_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huidextrc_25yr HUIMdext_recode_18yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_dex.dta", replace


//Dexterity  
//Analysis
use "HUI3_elbw_dis_im_dex.dta", clear

mi passive: generate dextoptimal25 = 1 if huidextrc_25yr < 1 
mi passive: replace dextoptimal25 = 0 if huidextrc_25yr == 1

//18 yrs 
mi passive: gen dextoptimal18 = 1 if HUIMdext_recode_18yr < 1 
mi passive: replace dextoptimal18 = 0 if HUIMdext_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg dextoptimal25 i.iqim i.lowmated8 i.lowsocialclass8, vce(robust) rd
mi estimate: binreg dextoptimal18 i.iqim i.lowmated8 i.lowsocialclass, vce(robust) rd  


//Emotion 
use "HUI3_elbw_s.dta", clear
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed huiemorc_25yr HUIMemo_recode_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huiemorc_25yr HUIMemo_recode_18yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_emo.dta", replace

//Emotion 
//Analysis
use "HUI3_elbw_dis_im_emo.dta", clear

mi passive: generate emoptimal25 = 1 if huiemorc_25yr < 1 
mi passive: replace emoptimal25 = 0 if huiemorc_25yr == 1

//18 yrs 
mi passive: gen emoptimal18 = 1 if HUIMemo_recode_18yr < 1 
mi passive: replace emoptimal18 = 0 if HUIMemo_recode_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg emoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg emoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Cognition 
use "HUI3_elbw_s.dta", clear
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8
 
mi register imputed huicognrc_25yr HUIMcog_recoded_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huicognrc_25yr HUIMcog_recoded_18yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week miss_fscale5 miss_fscale8 miss_WASIiqstn miss_spellstn miss_physical miss_psychsoc miss_lowsocialclass8, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_cog.dta", replace

//Cognition    
//Analysis
use "HUI3_elbw_dis_im_cog.dta", clear

mi passive: generate cogoptimal25 = 1 if huicognrc_25yr < 1 
mi passive: replace cogoptimal25 = 0 if huicognrc_25yr == 1

//18 yrs 
mi passive: gen cogoptimal18 = 1 if HUIMcog_recoded_18yr < 1 
mi passive: replace cogoptimal18 = 0 if HUIMcog_recoded_18yr == 1

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg cogoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg cogoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  


//Pain 
use "HUI3_elbw_s.dta", clear
//MAR 
// Set up the imputation model
mi set flong 

mi register regular gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week 
 
mi register imputed huipainrc_25yr HUIMpain_recoded_18yr iqim lowsocialclass8  disab8 fscale5 fscale8 utility8 spellstn arithstn readstnd abc8_rawscore physical psychsoc utiltot8 lowmated8 

mi xtset, clear 

// Run the imputation model 
mi impute chained (pmm, knn(10)) huipainrc_25yr HUIMpain_recoded_18yr fscale8 spellstn arithstn readstnd abc8_rawscore physical psychsoc fscale5 utility8 (logit) lowmated8 iqim lowsocialclass8 = gestage bwtstd male multiple_yesno brain_inj surgeryn pnster o236week, augment add(50) burnin(20) rseed(5423)

save "HUI3_elbw_dis_im_pain.dta", replace

//Analysis 
use "HUI3_elbw_dis_im_pain.dta", clear 
mi passive: generate painoptimal25 = 1 if huipainrc_25yr < 1 
mi passive: replace painoptimal25 = 0 if huipainrc_25yr == 1

//18 yrs 
mi passive: gen painoptimal18 = 1 if HUIMpain_recoded_18yr < 1 
mi passive: replace painoptimal18 = 0 if HUIMpain_recoded_18yr == 1 

// Adjusted analysis (binomial regression) - 18 and 25 years
mi estimate: binreg painoptimal25 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd
mi estimate: binreg painoptimal18 i.iqim i.lowsocialclass8 i.lowmated8, vce(robust) rd  


