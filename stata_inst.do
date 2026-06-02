* Install some new Stata versions that otherwise might fail
set more off

cap ado uninstall ivreg2
cap ado uninstall ranktest
cap ado uninstall avar

ssc install ivreg2, replace
ssc install ranktest, replace
ssc install avar, replace

mata: mata mlib index
discard

which ivreg2, all
which ranktest, all
which avar, all
mata: mata mlib query

exit, clear STATA
