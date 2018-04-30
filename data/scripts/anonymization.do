
/*

	In order to protect against backward identification, we should censor the age 
	information, since this information is detailed in terms of year, and the groups
	are singular for some older students. 
	
	We want to bin age into groups and replace the actual age by group averages.

	To evaluate whether the anonymization changes the meaning of the dataset, we
	run and compare the regressions in Table 2 - Treatment effects with background
	variables and compare the pre-anonymization to the binned results.

*/


// First we need to read data and prepare for the regression:
use ../processed/mmwinner_secure, clear

gen shareY = 1-shareX    // Reformulate in terms of share to winner.
recode T (1 = 3) (2 = 1) (3 = 4) (4 = 2) , generate(treatment)
label define treatment 1 "WTA" 3 "WTA-No Choice" 4 "WTA-No Exp." 2 "Base"
label value treatment treatment 
gen WTA = treatment==1
gen WTA_no_choice = treatment==3
gen WTA_no_exp = treatment==4
gen byte female = (sex==2)
tabstat age, by(treatment)
gen c_right_wing = inlist(political,4,5)
gen byte all_to_Y = (shareY==1)
encode sessionid, gen(sessionidn)

// Cutting age into 6 groups, which means that only the lowest group and the highest group
// represents more than one unique age.
egen age_c6 = cut(age), group(6)
bys age_c6: egen age_a = mean(age)
// New censored age distribution:
tab age_a

// Dropping situations without a loser:
drop if e1!=0


// Original regressions
reg shareY WTA WTA_no_choice WTA_no_exp age female c_right_wing i.sessionidn, robust
est store share_orig
reg all_to_Y WTA WTA_no_choice WTA_no_exp age female c_right_wing i.sessionidn , robust
est store all_orig




// Running the regressions on anonymized data:
// replace age 
replace age = age_a

reg shareY WTA WTA_no_choice WTA_no_exp age female c_right_wing i.sessionidn, robust
est store share_a
reg all_to_Y WTA WTA_no_choice WTA_no_exp age female c_right_wing i.sessionidn , robust
est store all_a

// Output of regressions: Columns can be compared for effect of censoring age.
esttab share_orig share_a all_orig all_a, b(3) se(3) ar2 keep(WTA* age* c_right_wing) star(* 0.1 ** 0.05 *** 0.01)

esttab share_orig share_a all_orig all_a using anonymization.tex, b(3) se(3) ar2 keep(WTA* age* c_right_wing) star(* 0.1 ** 0.05 *** 0.01) booktabs replace
clear

/*
	Instead of removing all remnants, of past data definitions, I start with new data
*/
use ../processed/mmwinner_secure, clear
egen age_c6 = cut(age), group(6)
bys age_c6: egen age_a = mean(age)
replace age = age_a
drop age_c6 age_a
saveold ../mmwinner, replace
outsheet using ../mmwinner.csv, comma nolabel replace





