cd "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA"


capture log close
capture log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\DATA_MANAGEMENT.smcl"



***************************DATA MANAGEMENT********************************************

//STEP 1: GENERATE AGE, TIMES, SEX, RACE AND OTHER DEMGRAPHICS VARIABLE//


use n_eid n_31_0_0  n_21022_0_0  n_34_0_0 n_52_0_0 n_6138_0_0 n_189_0_0 s_53_0_0 s_40000_0_0 s_40000_1_0  n_21000_0_0 n_709_0_0 n_738_0_0   n_1239_0_0 n_3456_0_0 n_22507_0_0  n_1279_0_0 n_1269_0_0 n_20162_0_0 n_1558_0_0 n_100022_0_0 n_981_0_0 ///
n_971_0_0 n_894_0_0 n_884_0_0 n_914_0_0 n_904_0_0 n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 ///
n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_2654_0_0   n_1458_0_0 n_1468_0_0 ///
n_1478_0_0 n_1478_0_0 n_1548_0_0 n_30890_0_0 n_30070_0_0 n_1031_0_0 n_6160_0_* n_2110_0_0 n_135_0_0 n_134_0_0 n_2178_0_0 n_21001_0_0 n_21001_0_0 n_48_0_0 n_49_0_0 n_30600_0_0 n_30710_0_0 n_30690_0_0 n_30760_0_0 n_30750_0_0 n_102_0_0 n_4080_0_0 n_4079_0_0 ///
n_21022_0_0 n_20009_0_* n_20002_0_* s_41202_0_* n_34_0_0 n_52_0_0 s_53_0_0 s_40000_*_0  n_40007_*_0 s_42018_0_0 s_42020_0_0  n_20023_0_0 n_399_0_1 n_399_0_2  n_400_0_1 n_400_0_2 n_20003_0_* n_20004_0_* n_6153_0_0 n_25010_2_0 n_25008_2_0 n_25006_2_0 n_25006_2_0 n_2501*_2_0 n_2502*_2_0 n_257*_2_0 n_258*_2_0 n_259*_2_0  s_53_0_0 s_53_2_0 n_250*_2_0 n_251*_2_0 n_262*_0_0 n_2443_0_0 n_1160_0_0 n_20023_0_0 s_42018_0_0 s_42020_0_0 n_26521_2_0 s_130* n_130* s_131* n_131* s_132* n_132* n_23074_0_0 n_23050_0_0 n_23051_0_0  n_23052_0_0 n_23053_0_0 n_23054_0_0 n_23055_0_0 n_23058_0_0 n_23059_0_0 n_23060_0_0 n_23061_0_0 n_23062_0_0 n_23063_0_0 n_23064_0_0 n_23065_0_0 n_23066_0_0 n_23067_0_0 n_23075_0_0 n_23069_0_0 n_23071_0_0 n_23039_0_0 n_23043_0_0 n_23041_0_0 n_23042_0_0 n_23044_0_0 n_23040_0_0 using "E:\UKB_DATA\UKB_DATA_BASKET2\STATA\DTA_FILES\LARGESTUKBDATASET_FIRST100K.dta" 

save "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA\SMALLERDATASETUKB_FIRST100K.dta", replace


use n_eid n_31_0_0  n_21022_0_0  n_34_0_0 n_52_0_0 n_6138_0_0 n_189_0_0 s_53_0_0 s_40000_0_0 s_40000_1_0  n_21000_0_0 n_709_0_0 n_738_0_0   n_1239_0_0 n_3456_0_0 n_22507_0_0  n_1279_0_0 n_1269_0_0 n_20162_0_0 n_1558_0_0 n_100022_0_0 n_981_0_0 ///
n_971_0_0 n_894_0_0 n_884_0_0 n_914_0_0 n_904_0_0 n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 ///
n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_2654_0_0   n_1458_0_0 n_1468_0_0 ///
n_1478_0_0 n_1478_0_0 n_1548_0_0 n_30890_0_0 n_30070_0_0 n_1031_0_0 n_6160_0_* n_2110_0_0 n_135_0_0 n_134_0_0 n_2178_0_0 n_21001_0_0 n_21001_0_0 n_48_0_0 n_49_0_0 n_30600_0_0 n_30710_0_0 n_30690_0_0 n_30760_0_0 n_30750_0_0 n_102_0_0 n_4080_0_0 n_4079_0_0 ///
n_21022_0_0 n_20009_0_* n_20002_0_* s_41202_0_* n_34_0_0 n_52_0_0 s_53_0_0 s_40000_*_0  n_40007_*_0 s_42018_0_0 s_42020_0_0  n_20023_0_0 n_399_0_1 n_399_0_2  n_400_0_1 n_400_0_2 n_20003_0_* n_20004_0_* n_6153_0_0 n_25010_2_0 n_25008_2_0 n_25006_2_0 n_25006_2_0 n_2501*_2_0 n_2502*_2_0 n_257*_2_0 n_258*_2_0 n_259*_2_0  s_53_0_0 s_53_2_0 n_250*_2_0 n_251*_2_0 n_262*_0_0 n_2443_0_0 n_1160_0_0 n_20023_0_0 s_42018_0_0 s_42020_0_0 n_26521_2_0 s_130* n_130* s_131* n_131* s_132* n_132* n_23074_0_0 n_23050_0_0 n_23051_0_0  n_23052_0_0 n_23053_0_0 n_23054_0_0 n_23055_0_0 n_23058_0_0 n_23059_0_0 n_23060_0_0 n_23061_0_0 n_23062_0_0 n_23063_0_0 n_23064_0_0 n_23065_0_0 n_23066_0_0 n_23067_0_0 n_23075_0_0 n_23069_0_0 n_23071_0_0 n_23039_0_0 n_23043_0_0 n_23041_0_0 n_23042_0_0 n_23044_0_0 n_23040_0_0 using "E:\UKB_DATA\UKB_DATA_BASKET2\STATA\DTA_FILES\LARGESTUKBDATASET_SECOND100K.dta" 




save "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA\SMALLERDATASETUKB_SECOND100K.dta", replace






use n_eid n_31_0_0  n_21022_0_0  n_34_0_0 n_52_0_0 n_6138_0_0 n_189_0_0 s_53_0_0 s_40000_0_0 s_40000_1_0  n_21000_0_0 n_709_0_0 n_738_0_0   n_1239_0_0 n_3456_0_0 n_22507_0_0  n_1279_0_0 n_1269_0_0 n_20162_0_0 n_1558_0_0 n_100022_0_0 n_981_0_0 ///
n_971_0_0 n_894_0_0 n_884_0_0 n_914_0_0 n_904_0_0 n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 ///
n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_2654_0_0   n_1458_0_0 n_1468_0_0 ///
n_1478_0_0 n_1478_0_0 n_1548_0_0 n_30890_0_0 n_30070_0_0 n_1031_0_0 n_6160_0_* n_2110_0_0 n_135_0_0 n_134_0_0 n_2178_0_0 n_21001_0_0 n_21001_0_0 n_48_0_0 n_49_0_0 n_30600_0_0 n_30710_0_0 n_30690_0_0 n_30760_0_0 n_30750_0_0 n_102_0_0 n_4080_0_0 n_4079_0_0 ///
n_21022_0_0 n_20009_0_* n_20002_0_* s_41202_0_* n_34_0_0 n_52_0_0 s_53_0_0 s_40000_*_0  n_40007_*_0 s_42018_0_0 s_42020_0_0  n_20023_0_0 n_399_0_1 n_399_0_2  n_400_0_1 n_400_0_2 n_20003_0_* n_20004_0_* n_6153_0_0 n_25010_2_0 n_25008_2_0 n_25006_2_0 n_25006_2_0 n_2501*_2_0 n_2502*_2_0 n_257*_2_0 n_258*_2_0 n_259*_2_0  s_53_0_0 s_53_2_0 n_250*_2_0 n_251*_2_0 n_262*_0_0 n_2443_0_0 n_1160_0_0 n_20023_0_0 s_42018_0_0 s_42020_0_0 n_26521_2_0 s_130* n_130* s_131* n_131* s_132* n_132* n_23074_0_0 n_23050_0_0 n_23051_0_0  n_23052_0_0 n_23053_0_0 n_23054_0_0 n_23055_0_0 n_23058_0_0 n_23059_0_0 n_23060_0_0 n_23061_0_0 n_23062_0_0 n_23063_0_0 n_23064_0_0 n_23065_0_0 n_23066_0_0 n_23067_0_0 n_23075_0_0 n_23069_0_0 n_23071_0_0 n_23039_0_0 n_23043_0_0 n_23041_0_0 n_23042_0_0 n_23044_0_0 n_23040_0_0 using "E:\UKB_DATA\UKB_DATA_BASKET2\STATA\DTA_FILES\LARGESTUKBDATASET_THIRD100K.dta" 




save "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA\SMALLERDATASETUKB_THIRD100K.dta", replace




use n_eid n_31_0_0  n_21022_0_0  n_34_0_0 n_52_0_0 n_6138_0_0 n_189_0_0 s_53_0_0 s_40000_0_0 s_40000_1_0  n_21000_0_0 n_709_0_0 n_738_0_0   n_1239_0_0 n_3456_0_0 n_22507_0_0  n_1279_0_0 n_1269_0_0 n_20162_0_0 n_1558_0_0 n_100022_0_0 n_981_0_0 ///
n_971_0_0 n_894_0_0 n_884_0_0 n_914_0_0 n_904_0_0 n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 /// 
n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_2654_0_0   n_1458_0_0 n_1468_0_0 ///
n_1478_0_0 n_1478_0_0 n_1548_0_0 n_30890_0_0 n_30070_0_0 n_1031_0_0 n_6160_0_* n_2110_0_0 n_135_0_0 n_134_0_0 n_2178_0_0 n_21001_0_0 n_21001_0_0 n_48_0_0 n_49_0_0 n_30600_0_0 n_30710_0_0 n_30690_0_0 n_30760_0_0 n_30750_0_0 n_102_0_0 n_4080_0_0 n_4079_0_0 ///
n_21022_0_0 n_20009_0_* n_20002_0_* s_41202_0_* n_34_0_0 n_52_0_0 s_53_0_0 s_40000_*_0  n_40007_*_0 s_42018_0_0 s_42020_0_0  n_20023_0_0 n_399_0_1 n_399_0_2  n_400_0_1 n_400_0_2 n_20003_0_* n_20004_0_* n_6153_0_0 n_25010_2_0 n_25008_2_0 n_25006_2_0 n_25006_2_0 n_2501*_2_0 n_2502*_2_0 n_257*_2_0 n_258*_2_0 n_259*_2_0  s_53_0_0 s_53_2_0 n_250*_2_0 n_251*_2_0 n_262*_0_0 n_2443_0_0 n_1160_0_0 n_20023_0_0 s_42018_0_0 s_42020_0_0 n_26521_2_0 s_130* n_130* s_131* n_131* s_132* n_132* n_23074_0_0 n_23050_0_0 n_23051_0_0  n_23052_0_0 n_23053_0_0 n_23054_0_0 n_23055_0_0 n_23058_0_0 n_23059_0_0 n_23060_0_0 n_23061_0_0 n_23062_0_0 n_23063_0_0 n_23064_0_0 n_23065_0_0 n_23066_0_0 n_23067_0_0 n_23075_0_0 n_23069_0_0 n_23071_0_0 n_23039_0_0 n_23043_0_0 n_23041_0_0 n_23042_0_0 n_23044_0_0 n_23040_0_0 using "E:\UKB_DATA\UKB_DATA_BASKET2\STATA\DTA_FILES\LARGESTUKBDATASET_FOURTH100K.dta" 




save "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA\SMALLERDATASETUKB_FOURTH100K.dta", replace



use n_eid n_31_0_0  n_21022_0_0  n_34_0_0 n_52_0_0 n_6138_0_0 n_189_0_0 s_53_0_0 s_40000_0_0 s_40000_1_0  n_21000_0_0 n_709_0_0 n_738_0_0   n_1239_0_0 n_3456_0_0 n_22507_0_0  n_1279_0_0 n_1269_0_0 n_20162_0_0 n_1558_0_0 n_100022_0_0 n_981_0_0 ///
n_971_0_0 n_894_0_0 n_884_0_0 n_914_0_0 n_904_0_0 n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 ///
n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_2654_0_0   n_1458_0_0 n_1468_0_0 ///
n_1478_0_0 n_1478_0_0 n_1548_0_0 n_30890_0_0 n_30070_0_0 n_1031_0_0 n_6160_0_* n_2110_0_0 n_135_0_0 n_134_0_0 n_2178_0_0 n_21001_0_0 n_21001_0_0 n_48_0_0 n_49_0_0 n_30600_0_0 n_30710_0_0 n_30690_0_0 n_30760_0_0 n_30750_0_0 n_102_0_0 n_4080_0_0 n_4079_0_0 ///
n_21022_0_0 n_20009_0_* n_20002_0_* s_41202_0_* n_34_0_0 n_52_0_0 s_53_0_0 s_40000_*_0  n_40007_*_0 s_42018_0_0 s_42020_0_0  n_20023_0_0 n_399_0_1 n_399_0_2  n_400_0_1 n_400_0_2 n_20003_0_* n_20004_0_* n_6153_0_0 n_25010_2_0 n_25008_2_0 n_25006_2_0 n_25006_2_0 n_2501*_2_0 n_2502*_2_0 n_257*_2_0 n_258*_2_0 n_259*_2_0  s_53_0_0 s_53_2_0 n_250*_2_0 n_251*_2_0 n_262*_0_0 n_2443_0_0 n_1160_0_0 n_20023_0_0 s_42018_0_0 s_42020_0_0 n_26521_2_0 s_130* n_130* s_131* n_131* s_132* n_132* n_23074_0_0 n_23050_0_0 n_23051_0_0  n_23052_0_0 n_23053_0_0 n_23054_0_0 n_23055_0_0 n_23058_0_0 n_23059_0_0 n_23060_0_0 n_23061_0_0 n_23062_0_0 n_23063_0_0 n_23064_0_0 n_23065_0_0 n_23066_0_0 n_23067_0_0 n_23075_0_0 n_23069_0_0 n_23071_0_0 n_23039_0_0 n_23043_0_0 n_23041_0_0 n_23042_0_0 n_23044_0_0 n_23040_0_0 using "E:\UKB_DATA\UKB_DATA_BASKET2\STATA\DTA_FILES\LARGESTUKBDATASET_FIFTH100K.dta" 




save "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA\SMALLERDATASETUKB_FIFTH100K.dta", replace


cd "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\DATA"

use SMALLERDATASETUKB_FIRST100K,clear
append using SMALLERDATASETUKB_SECOND100K
append using SMALLERDATASETUKB_THIRD100K
append using SMALLERDATASETUKB_FOURTH100K
append using SMALLERDATASETUKB_FIFTH100K

save SMALLERDATASETUKB, replace


************************************************************************************************************************

**Format time-series variables using the do file**
use SMALLERDATASETUKB,clear
sort n_eid
capture drop _merge
save, replace

capture gen double ts_53_0_0 = date(s_53_0_0,"DMY")
capture format ts_53_0_0 %td
capture label variable ts_53_0_0 "Date of attending assessment centre"

capture gen double ts_53_0_0 = date(s_53_0_0,"DMY")
capture format ts_53_0_0 %td
capture label variable ts_53_0_0 "Date of attending assessment centre"

capture gen double ts_53_1_0 = date(s_53_1_0,"DMY")
capture format ts_53_1_0 %td
capture label variable ts_53_1_0 "Date of attending assessment centre"

capture gen double ts_53_2_0 = date(s_53_2_0,"DMY")
capture format ts_53_2_0 %td
capture label variable ts_53_2_0 "Date of attending assessment centre"

capture gen double ts_53_3_0 = date(s_53_3_0,"DMY")
capture format ts_53_3_0 %td
capture label variable ts_53_3_0 "Date of attending assessment centre"

capture gen double ts_40000_0_0 = date(s_40000_0_0,"DMY")
capture format ts_40000_0_0 %td
capture label variable ts_40000_0_0 "Date of death"

capture gen double ts_40000_1_0 = date(s_40000_1_0,"DMY")
capture format ts_40000_1_0 %td
capture label variable ts_40000_1_0 "Date of death"


format %18.14f n_40007_0_0
format %18.14f n_40007_1_0

capture gen double ts_42018_0_0 = date(s_42018_0_0,"DMY")
capture format ts_42018_0_0 %td
capture label variable ts_42018_0_0 "Date of all cause dementia report"

capture gen double ts_42020_0_0 = date(s_42020_0_0,"DMY")
capture format ts_42020_0_0 %td
capture label variable ts_42020_0_0 "Date of alzheimers disease report"


save, replace



su ts_53_*_0 ts_40000_0_0 ts_40000_1_0 ts_42018_0_0 ts_42020_0_0

save SMALLERDATASETUKBfin, replace



capture log close

capture log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\DATA_MANAGEMENT2.smcl"

**************************DEMOGRAPHICS, SES: Fix this section in all papers*********************************************

use SMALLERDATASETUKBfin, clear


keep n_eid n_31_0_0 n_21022_0_0  n_34_0_0 n_52_0_0 n_6138_0_0 n_189_0_0 ts_53_0_0 ts_40000_0_0 ts_40000_1_0 n_21000_0_0 n_709_0_0 n_738_0_0 n_1239_0_0 

save DEMOG_SES_UKB,replace



**Exogenous variables: age (@@fix baseline age), gender, household size**


*Sex*
capture drop sex
gen sex=n_31_0_0 
recode sex 0=2 1=1


*Age*
capture drop Age
gen Age=n_21022_0_0 

*Years of Birth*
capture drop birthyear
gen birthyear=n_34_0_0 

*Month of Birth*
capture drop birthmonth
gen birthmonth=n_52_0_0 

*Date of birth*
capture drop birthdate
gen birthdate=mdy(birthmonth,14,birthyear)

*Baseline Cohort Start Date*
capture drop startdate
gen startdate=ts_53_0_0 
format startdate %d

*Baseline age**
capture drop baselineage
gen baselineage=(startdate-birthdate)/365.25




**Household size**
label var n_709_0_0	"Number in household"

capture drop householdsize
gen householdsize=n_709_0_0
label var householdsize "Number of household members"

**-1	Do not know
**-3	Prefer not to answer

recode householdsize (-1=. ) (-3=.)

*Ethnicity*
capture drop ethnicity

gen ethnicity = 1 if inlist(n_21000_0_0,1,1001,1002,1003) 
replace ethnicity = 2 if inlist(n_21000_0_0,3,3001,3002,3003,3004)
replace ethnicity = 3 if inlist(n_21000_0_0,5)
replace ethnicity = 4 if inlist(n_21000_0_0,4,4001,4002,4003)
replace ethnicity = 5 if inlist(n_21000_0_0,2,6,2001,2002,2003,2004)
replace ethnicity = 6 if ethnicity ==.

label var ethnicity "ethnic group"
label define ethnicityL 1 "white" 2 "south asian" 3 "east asian" 4 "black" 5 "other/mixed" 6 "unknown"
label values ethnicity ethnicityL

capture drop ethnicity2
gen ethnicity2=.
replace ethnicity2=ethnicity
recode ethnicity2 (1=0) (4=1) (2=2) (3=3) (5=3) (6=3) 

label var ethnicity2 "ethnic grouping 2"
label define ethnicityL2 0 "white" 1 "black" 2 "south asian" 3 "other" 
label values ethnicity2 ethnicityL2

capture drop RACE_ETHN=ethnicity2

capture drop NonWhite
gen NonWhite=.
replace NonWhite=ethnicity2
recode NonWhite (0=0) (1=1) (2=1) (3=1)

label var NonWhite "ethnic grouping 3"
label define ethnicityL3 0 "white" 1 "Non-White"  
label values NonWhite ethnicityL3


******************DATES*******************************************

*Years of Birth*
capture drop birthyear
gen  birthyear=n_34_0_0 

*Month of Birth*
capture drop birthmonth
gen  birthmonth=n_52_0_0 

*Death Date*
capture drop deathdate
gen deathdate = ts_40000_0_0 
replace deathdate =  ts_40000_1_0 if deathdate ==.
format deathdate %d
label var deathdate "date of death"

*Baseline Cohort Start Date*
capture drop startdate
gen startdate=ts_53_0_0 

********************************SES******************************

*Education, qualification*
capture rename n_6138_0_0 education
recode education -7=. -3=. 7=. 
label define educationL 0 "none" 1 "College/University" 2 "A/AS Levels/Equivalent" 3 "O Levels/GCSEs/Equivalent" 4 "CSEs/Equivalent" 5 "NVQ/HND/HNC/Equivalent" 6 "Other professional qual" 7 "Unknown"
label values education educationL

capture drop educationbr
gen educationbr=.
replace educationbr=0 if (education==0 | education==4 | education==5 | education==6)
replace educationbr=1 if (education==2 | education==3)
replace educationbr=2 if education==1

*Deprivation*
capture rename n_189_0_0 townsend


**Household income before tax**
label var n_738_0_0	"Average total household income before tax"

**1	Less than 18,000
**2	18,000 to 30,999
**3	31,000 to 51,999
**4	52,000 to 100,000
**5	Greater than 100,000
**-1	Do not know
**-3	Prefer not to answer



capture drop householdincome
gen householdincome=n_738_0_0	
label var householdincome "Average total household income before tax"

recode householdincome (-1=.) (-3=.)


save, replace


//STEP 2: GENERATE SES VARIABLE//


**SES: educationbr, householdincome, townsend

capture drop zeducationbr 
egen zeducationbr=std(educationbr)

capture drop zhouseholdincome 
egen zhouseholdincome=std(householdincome)

capture drop ztownsend
egen ztownsend=std(townsend)

capture drop ztownsendinv
gen ztownsendinv=ztownsend*-1

capture drop rowmissSES
egen rowmissSES=rowmiss(zeducationbr zhouseholdincome ztownsendinv)

tab rowmissSES

capture drop SES
egen SES=rowmean(zeducationbr zhouseholdincome ztownsendinv) if rowmissSES<=1


save, replace  


//STEP 3: GENERATE LIFESTYLE VARIABLES: SMOKING, ALCOHOL, AND PA//

use SMALLERDATASETUKBfin,clear

keep n_eid n_1239_0_0 n_3456_0_0 n_22507_0_0  n_1279_0_0 n_1269_0_0 n_20162_0_0 n_1558_0_0 n_100022_0_0 n_981_0_0 ///
n_971_0_0 n_894_0_0 n_884_0_0 n_914_0_0 n_904_0_0 n_21022_0_0

save LIFESTYLENODIET_UKB,replace

capture drop Age
gen Age=n_21022_0_0 


*Current Smoking Status*
capture drop smoking
gen smoking = 0 if  n_1239_0_0 == 0 | n_1239_0_0 == -3
replace smoking = 1 if n_22507_0_0 != . 
replace smoking = 2 if n_1239_0_0 == 1 | n_1239_0_0 == 2
label var smoking "current smoking status"
label define smokingL 0 "never smoker" 1 "ex-smoker" 2 "current smoker"
label values smoking smokingL
recode smoking .=0

*Cigarettes Per Day*
capture drop cigperday
gen cigperday = n_3456_0_0 if smoking == 2    //missing values exist - consider imputation
replace cigperday = 0 if smoking == 0 | smoking == 1
replace cigperday = . if cigperday < 0
label var cigperday "if smoking, number of cig per day"

*Aged Stopped Smoking Cigarettes*
capture drop stopsmoke agestop
gen stopsmoke = Age - n_22507_0_0 
label var stopsmoke "intermediate var: baseline age - age stopped smoking"
gen agestop = 1 if stopsmoke < 5
replace agestop = 2 if stopsmoke >= 5 & stopsmoke <= 10
replace agestop = 3 if stopsmoke > 10 & stopsmoke !=.
label var agestop "how long ago did you quit smoking in years"
label define stopL 1 "< 5 years" 2 " between 5-10 years" 3 "> 10 years"
label values agestop stopL  

*Combine Current Smoking with Ex-smoking duration for protective effects*
capture drop smokingstatus
gen smokingstatus = 0 if smoking == 2
replace smokingstatus = agestop if smokingstatus ==.
replace smokingstatus = 4 if smoking == 0
label var smokingstatus "complete smoking status"
label define statusL 0 "current smoker" 1 "ex-smoker < 5 years" 2 "ex-smoker between 5-10 years" 3 "ex-smoker > 10 years" 4 "never smoker"
label values smokingstatus statusL
recode smokingstatus .=4

*Environmental tobacco smoke*
capture drop etsmoke
replace n_1279_0_0 = . if n_1279_0_0 < 0
replace n_1269_0_0  = . if n_1269_0_0 < 0
order n_1279_0_0, after(n_1269_0_0)
egen ets = rowtotal(n_1269_0_0-n_1279_0_0)
gen etsmoke = ets
label var etsmoke "environmental tobacco exposures (hours per week)"

**Pack-years of smoking**
capture drop packyearssmoke
gen packyearssmoke=n_20162_0_0
replace packyearssmoke=0 if smoking==0



**Use smokingstatus, etsmoke and packyearssmoke as measured variables for SMOKING LATENT VARIABLE OR AVERAGE Z-SCORE*****


capture drop zsmokingstatus zetsmoke zpackyearssmoke
egen zsmokingstatus=std(smokingstatus)
egen zetsmoke=std(etsmoke)
egen zpackyearssmoke=std(packyearssmoke)


capture drop rowmissSMOKE
egen rowmissSMOKE=rowmiss(zsmokingstatus zetsmoke zpackyearssmoke)

tab rowmissSMOKE

egen SMOKE=rmean(zsmokingstatus zetsmoke zpackyearssmoke) if rowmissSMOKE<=1


save, replace


************************************ALCOHOL: fixed code 6 to missing***************************************************
*https://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=100098

*Usual Alcohol intake*

capture drop alcohol
gen alcohol = 0 if  n_1558_0_0 == 6
replace alcohol = 1 if  n_1558_0_0 == 5
replace alcohol = 2 if  n_1558_0_0 == 4
replace alcohol = 3 if  n_1558_0_0 == 3
replace alcohol = 4 if  n_1558_0_0 == 2
replace alcohol = 5 if  n_1558_0_0 == 1
replace alcohol = . if  n_1558_0_0 == -3
replace alcohol = . if  n_1558_0_0 ==.
label var alcohol "alcohol intake frequency"
label define alcoholL 0 "never" 1 "special occasions only" 2 "1-3 times per month" 3 "1-3 times per week" 4 "3-4 times per week" 5 "daily or almost daily" 
label values alcohol alcoholL



**use alcohol ALCOHOL LATENT VARIABLE OR AVERAGE Z-SCORE***

egen zalcohol=std(alcohol)

capture drop ALCOHOL
gen ALCOHOL=zalcohol

*************************************PHYSICAL ACTIVITY**********************************************************************

***https://biobank.ctsu.ox.ac.uk/crystal/search.cgi?wot=0&srch=physical+activity&sta0=on&sta1=on&sta2=on&sta3=on&str0=on&str3=on&fit0=on&fit10=on&fit20=on&fit30=on&fvt11=on&fvt21=on&fvt22=on&fvt31=on&fvt41=on&fvt51=on&fvt61=on&fvt101=on


*Physical activity - use the IPAQ scoring guidelines*

*Walking MET-min/week - use median time of each category*
capture drop walkperday
gen walkperday = 7.5 if n_981_0_0 == 1
replace walkperday = 22.5 if n_981_0_0 == 2
replace walkperday = 45 if n_981_0_0 == 3
replace walkperday = 75 if n_981_0_0 == 4
replace walkperday = 105 if n_981_0_0 == 5
replace walkperday = 150 if n_981_0_0 == 6
replace walkperday = 210 if n_981_0_0 == 7
label var walkperday "walking minutes per day"

*Frequency of Walking (no days per week) - use median time of each category*
capture drop walkperweek
gen walkperweek = 0.25 if n_971_0_0 == 1
replace walkperweek = 0.625 if n_971_0_0 == 2
replace walkperweek = 1 if n_971_0_0 == 3
replace walkperweek = 2.5 if n_971_0_0 == 4
replace walkperweek = 4.5 if n_971_0_0 == 5
replace walkperweek = 7 if n_971_0_0 == 6
label var walkperweek "walking days per week"

*Calculate walking MET-min/week*
capture drop walkMETmin
gen walkMETmin = 3.3 * walkperday * walkperweek
label var walkMETmin "MET-min per week walking"

*Moderate exercise MET-min/week*
capture drop modperday
gen modperday = n_894_0_0 if n_894_0_0 >= 0
label var modperday "moderate exercise minutes per day"

*Frequency of moderate exercise (no days per week)*
capture drop modperweek
gen modperweek =  n_884_0_0 if  n_884_0_0 >= 0
label var modperweek "moderate exercise days per week"

*Calculate moderate exercise MET-min/week*
capture drop modMETmin
gen modMETmin = 4.0 * modperday * modperweek
label var modMETmin "MET-min per week moderate exercise"

*Vigorous exercise MET-min/week*
capture drop vigperday
gen vigperday = n_914_0_0 if n_914_0_0 >= 0
label var vigperday "vigorous exercise minutes per day"

*Frequency of vigorous exercise (no days per week)*
capture drop vigperweek
gen vigperweek = n_904_0_0 if n_904_0_0 >= 0
label var vigperweek "vigorous exercise days per week"

*Calculate vigorous exercise MET-min/week*
capture drop vigMETmin
gen vigMETmin = 8.0 * vigperday * vigperweek
label var vigMETmin "MET-min per week vigorous exercise"

****Calculate TOTAL MET-min per week*******
capture drop METmin
order modMETmin, before(vigMETmin)
order walkMETmin, before(modMETmin)
egen METmin = rowtotal(walkMETmin modMETmin vigMETmin)
label var METmin "MET-min per week total exercise" // missing values exist - consider imputation

////USE METmin as the PA measure, z-scored////

capture drop PA
egen PA=std(METmin)

save, replace


//STEP 4: GENERATE LIFESTYLE VARIABLES: DIET//

use SMALLERDATASETUKBfin,clear

keep n_eid n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 ///
n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_2654_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_1458_0_0 n_1468_0_0 ///
n_1478_0_0 n_1478_0_0 n_1548_0_0  n_6144_0_0

**(ALL ITEMS UNDER THE DIET CATEGORY)
**URL: https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100052


save LIFESTYLEDIET_UKB,replace

**https://www.ahajournals.org/doi/10.1161/CIRCULATIONAHA.115.018585#d3e341


**********Recoding as missing data fields and -10 as 0.50***********************
**-10:<1 --> to 0.5 throughout
**-3: prefer not to answer
**-1: do not know

capture drop n_1289_0_0r n_1299_0_0r n_1309_0_0r n_1319_0_0r n_1329_0_0r n_1339_0_0r n_1349_0_0r n_1359_0_0r n_1369_0_0r n_1379_0_0r n_1389_0_0r 
capture drop n_6144_0_0r n_6144_0_1r n_6144_0_2r n_6144_0_3r n_10855_0_0r n_1408_0_0r n_1418_0_0r n_2654_0_0r n_1438_0_0r n_1448_0_0r n_1458_0_0r 
capture drop n_1468_0_0r n_1478_0_0r n_1478_0_0r


foreach x of varlist n_1289_0_0 n_1299_0_0 n_1309_0_0 n_1319_0_0 n_1329_0_0 n_1339_0_0 n_1349_0_0 n_1359_0_0 n_1369_0_0 n_1379_0_0 n_1389_0_0 n_6144_0_0 n_6144_0_1 n_6144_0_2 n_6144_0_3 n_10855_0_0 n_1408_0_0 n_1418_0_0 n_2654_0_0 n_1428_0_0 n_1438_0_0 n_1448_0_0 n_1458_0_0 n_1468_0_0 n_1478_0_0  {
	gen `x'r=`x'
}

foreach y of varlist n_1289_0_0r n_1299_0_0r n_1309_0_0r n_1319_0_0r n_1329_0_0r n_1339_0_0r n_1349_0_0r n_1359_0_0r n_1369_0_0r n_1379_0_0r n_1389_0_0r n_6144_0_0r n_6144_0_1r n_6144_0_2r n_6144_0_3r n_10855_0_0r n_1408_0_0r n_1418_0_0r n_1428_0_0r n_2654_0_0r n_1438_0_0r n_1448_0_0r n_1458_0_0r n_1468_0_0r n_1478_0_0r n_1478_0_0r {
replace `y'=. if `y'==-3 | `y'==-1 | `y'==-5
}



foreach y of varlist n_1289_0_0r n_1299_0_0r n_1309_0_0r n_1319_0_0r n_1329_0_0r n_1339_0_0r n_1349_0_0r n_1359_0_0r n_1369_0_0r n_1379_0_0r n_1389_0_0r  n_6144_0_0r n_6144_0_1r n_6144_0_2r n_6144_0_3r n_10855_0_0r n_1408_0_0r n_1418_0_0r n_2654_0_0r n_1438_0_0r n_1448_0_0r n_1458_0_0r n_1468_0_0r n_1478_0_0r n_1478_0_0r {
replace `y'=0.5 if `y'==-10 
}

save, replace


/////////////////////FRUITS COMPONENT //////////////////////


**Servings per day for all fruits**

**1 piece of dried fruit (e.g. apricot)~=2.5 TBSP, 1 TBSP= 0.063 cups; ½ cup of dried fruit (1 serving) is 3 pieces of dried fruit.

**1 medium sized fruit is one serving.

capture drop allfruits
gen allfruits=.
replace allfruits=(n_1309_0_0r+(n_1319_0_0r/3))

**>=3 servings per day**


capture drop allfruits_component1
gen allfruits_component1=.
replace allfruits_component1=1 if allfruits>=3 & allfruits~=.
replace allfruits_component1=0 if allfruits_component1~=1 & allfruits~=.



//////////////////////VEGETABLES COMPONENT/////////////////////

**Servings per day for all vegetables**

**1 cup of raw leafy vegetables is 16 TBSP. ½ cup of cooked or non-leafy raw vegetables is 8 TBSP. 

**1 serving of raw leafy/non-leafy vegetables is ~=12 TBSP; 1 serving of cooked vegetables is ~=8 TBSP


capture drop allvegs
gen allvegs=.
replace allvegs=((n_1289_0_0r/8))+((n_1299_0_0r/12))

**>=3 servings per day**

capture drop allvegs_component2
gen allvegs_component2=.
replace allvegs_component2=1 if allvegs>=3 & allvegs~=.
replace allvegs_component2=0 if allvegs_component2~=1 & allvegs~=.

//////////////WHOLE GRAINS///////////////////////////////////

**Bread intake, slices/week: n_1438_0_0r
**Type of bread: n_1448_0_0r	

**1 White
**2	Brown
**3	Wholemeal or wholegrain
**4	Other type of bread


**Cereal intake, bowls/week: n_1458_0_0r


**Cereal type:n_1448_0_0r

**1	Bran cereal (e.g. All Bran, Branflakes)
**2	Biscuit cereal (e.g. Weetabix)
**3	Oat cereal (e.g. Ready Brek, porridge)
**4	Muesli
**5	Other (e.g. Cornflakes, Frosties)

capture drop wholegrain_bread
gen wholegrain_bread=.
replace wholegrain_bread=n_1438_0_0r if n_1448_0_0r==3
replace wholegrain_bread=0 if n_1448_0_0r~=3 & n_1438_0_0r~=.

capture drop wholegrain_bread_day
gen wholegrain_bread_day=.
replace wholegrain_bread_day=wholegrain_bread/7 

capture drop wholegrain_cereal
gen wholegrain_cereal=.
replace wholegrain_cereal=n_1458_0_0r if n_1448_0_0r==1 | n_1448_0_0r==2 | n_1448_0_0r==3 | n_1448_0_0r==4
replace wholegrain_cereal=0 if n_1448_0_0r~=3 & n_1458_0_0r~=.

capture drop wholegrain_cereal_day
gen wholegrain_cereal_day=.
replace wholegrain_cereal_day=wholegrain_cereal/7 


capture drop wholegrain
gen wholegrain=(wholegrain_bread_day+wholegrain_cereal_day)

capture drop wholegrain_component3
gen wholegrain_component3=.
replace wholegrain_component3=1 if wholegrain>=3 & wholegrain~=.
replace wholegrain_component3=0 if wholegrain_component3~=1 & wholegrain~=.

//////////////FISH/SHELLFISH////////////////////////////////

**non-oily fish, times/week: n_1329_0_0r
**oily fish, times/week: n_1339_0_0r


**0	Never
**1	Less than once a week
**2	Once a week
**3	2-4 times a week
**4	5-6 times a week
**5	Once or more daily


capture drop fish
gen fish=n_1329_0_0r+n_1339_0_0r

capture drop fish_component4
gen fish_component4=.
replace fish_component4=1 if fish>=3 & fish~=.
replace fish_component4=0 if fish_component4~=1 & fish~=.

//////////////DAIRY PRODUCTS///////////////////////////////


**Never eat eggs, dairy, weat, sugar: n_6144_0_0r
**1	Eggs or foods containing eggs
**2	Dairy products
**3	Wheat products
**4	Sugar or foods/drinks containing sugar
**5	I eat all of the above

**Cheese intake: n_1408_0_0r**
**0	Never
**1	Less than once a week
**2	Once a week
**3	2-4 times a week
**4	5-6 times a week
**5	Once or more daily

**Milk type used: n_1418_0_0r
**1	Full cream
**2	Semi-skimmed
**3	Skimmed
**4	Soya
**5	Other type of milk
**6	Never/rarely have milk


capture drop dairy_component5
gen dairy_component5=.
replace dairy_component5=1 if n_6144_0_0r~=2 & n_1408_0_0r==5 & n_1418_0_0r~=6 |n_6144_0_1r~=2 & n_1408_0_0r==5 & n_1418_0_0r~=6 |n_6144_0_2r~=2 & n_1408_0_0r==5 & n_1418_0_0r~=6 | n_6144_0_3r~=2 & n_1408_0_0r==5 & n_1418_0_0r~=6
replace dairy_component5=0 if dairy_component5~=1 & n_6144_0_0r~=. & n_1408_0_0r~=. & n_1418_0_0r~=.

/////////////VEGETABLE OILS//////////////////////////////

**Other types of non-butter spreads: n_2654_0_0r**

**4	Soft (tub) margarine
**5	Hard (block) margarine
**6	Olive oil based spread (eg: Bertolli)
**7	Polyunsaturated/sunflower oil based spread (eg: Flora)
**2	Flora Pro-Active or Benecol
**8	Other low or reduced fat spread
**9	Other type of spread/margarine


capture drop vegoil_component6
gen vegoil_component6=.
replace vegoil_component6=1 if n_2654_0_0r==6 | n_2654_0_0r==7 
replace vegoil_component6=0 if vegoil_component6~=1 


////////////REFINED GRAINS, STARCHES, ADDED SUGARS///////

**Bread intake, slices/week: n_1438_0_0r
**Type of bread: n_1448_0_0r	

**1 White
**2	Brown
**3	wholemeal or wholegrain
**4	Other type of bread


**Cereal intake, bowls/week: n_1458_0_0r


**Cereal type:n_1448_0_0r

**1	Bran cereal (e.g. All Bran, Branflakes)
**2	Biscuit cereal (e.g. Weetabix)
**3	Oat cereal (e.g. Ready Brek, porridge)
**4	Muesli
**5	Other (e.g. Cornflakes, Frosties)

capture drop nonwholegrain_bread
gen nonwholegrain_bread=.
replace nonwholegrain_bread=n_1438_0_0r if n_1448_0_0r~=3 & n_1448_0_0r~=.
replace nonwholegrain_bread=0 if n_1448_0_0r==3 & n_1438_0_0r~=.

capture drop nonwholegrain_bread_day
gen nonwholegrain_bread_day=.
replace nonwholegrain_bread_day=nonwholegrain_bread/7 

capture drop nonwholegrain_cereal
gen nonwholegrain_cereal=.
replace nonwholegrain_cereal=n_1458_0_0r if n_1448_0_0r==5 
replace nonwholegrain_cereal=0 if n_1448_0_0r~=5 & n_1458_0_0r~=.

capture drop nonwholegrain_cereal_day
gen nonwholegrain_cereal_day=.
replace nonwholegrain_cereal_day=nonwholegrain_cereal/7 


capture drop nonwholegrain
gen nonwholegrain=nonwholegrain_bread_day+nonwholegrain_cereal_day

capture drop nonwholegrain_component7
gen nonwholegrain_component7=.
replace nonwholegrain_component7=1 if nonwholegrain<1.5 & nonwholegrain~=.
replace nonwholegrain_component7=0 if nonwholegrain_component7~=1 & nonwholegrain~=.



////////////PROCESSED MEATS/////////////////////////////



**0	Never
**1	Less than once a week
**2	Once a week
**3	2-4 times a week
**4	5-6 times a week
**5	Once or more daily

capture drop processed_meat
gen processed_meat=n_1349_0_0r

capture drop processed_meat_component8
gen processed_meat_component8=.
replace processed_meat_component8=1 if processed_meat==0 | processed_meat==1 | processed_meat==2
replace processed_meat_component8=0 if processed_meat==3 | processed_meat==4 | processed_meat==5



///////////UNPROCESSED RED MEATS/////////////////////////////

**Poultry: n_1359_0_0r: White meat
**Lamb/mutton: n_1379_0_0r: Red meat
**Beef:n_1369_0_0r: Red meat
**Pork:n_1389_0_0r: Red meat


**0	Never
**1	Less than once a week
**2	Once a week
**3	2-4 times a week
**4	5-6 times a week
**5	Once or more daily

capture drop unprocessed_meats
gen unprocessed_meats=(n_1369_0_0r+n_1379_0_0r+n_1389_0_0r)


capture drop unprocessed_meats_component9
gen unprocessed_meats_component9=.
replace unprocessed_meats_component9=1 if unprocessed_meats<3 & unprocessed_meats~=.
replace unprocessed_meats_component9=0 if unprocessed_meats_component9~=1 & unprocessed_meats~=.


////////////INDUSTRIAL TRANS FAT////////////////////////////

**Type of spread: n_1428_0_0r

**1	Butter/spreadable butter
**3	Other type of spread/margarine
**0	Never/rarely use spread
**2	Flora Pro-Active/Benecol

**Non-butter spread type details: n_2654_0_0r

**4	Soft (tub) margarine
**5	Hard (block) margarine
**6	Olive oil based spread (eg: Bertolli)
**7	Polyunsaturated/sunflower oil based spread (eg: Flora)
**2	Flora Pro-Active or Benecol
**8	Other low or reduced fat spread
**9	Other type of spread/margarine

capture drop transfat_component10
gen transfat_component10=.
replace transfat_component10=1 if n_1428_0_0r==0
replace transfat_component10=0 if transfat_component10~=1 & n_1428_0_0r~=.


////////////SUGAR SWEETENDED BEVERAGES/////////////////////

**Never eat eggs, dairy, wheat, sugar: n_6144_0_0r
**1	Eggs or foods containing eggs
**2	Dairy products
**3	Wheat products
**4	Sugar or foods/drinks containing sugar
**5	I eat all of the above

capture drop sugar_bev_component11
gen sugar_bev_component11=.
replace sugar_bev_component11=0 if n_6144_0_0r==4 & n_6144_0_0r~=. | n_6144_0_0r==4 & n_6144_0_1r~=. | n_6144_0_0r==4 & n_6144_0_2r~=. | n_6144_0_0r==4 & n_6144_0_3r~=.
replace sugar_bev_component11=1 if sugar_bev_component11~=0 & n_6144_0_0r~=.


////////////SODIUM////////////////////////////////////////

**Salt added to food: n_1478_0_0r
**1	Never/rarely
**2	Sometimes
**3	Usually
**4	Always


capture drop sodium_component12
gen sodium_component12=.
replace sodium_component12=1 if n_1478_0_0r==1 | n_1478_0_0r==2
replace sodium_component12=0 if sodium_component12~=1 & n_1478_0_0r~=.

//////////HDI_TOTALSCORE//////////////////

capture drop rowmissHDI
egen rowmissHDI=rowmiss(allfruits_component1 allvegs_component2 wholegrain_component3 fish_component4 dairy_component5 vegoil_component6 nonwholegrain_component7 processed_meat_component8 unprocessed_meats_component9 transfat_component10 sugar_bev_component11 sodium_component12)

tab rowmissHDI


capture drop HDI_TOTALSCORE
egen HDI_TOTALSCORE=rowmean(allfruits_component1 allvegs_component2 wholegrain_component3 fish_component4 dairy_component5 vegoil_component6 nonwholegrain_component7 processed_meat_component8 unprocessed_meats_component9 transfat_component10 sugar_bev_component11 sodium_component12) if rowmissHDI<=6

replace HDI_TOTALSCORE=HDI_TOTALSCORE*12 

////////////////////

///////////////////RECODING FOR VARIATION IN DIET///////////////


**Coding	Meaning
**1	Never/rarely
**2	Sometimes
**3	Often
**-1	Do not know
**-3	Prefer not to answer

capture drop DIET_VARIATION
gen DIET_VARIATION=.
replace DIET_VARIATION=1 if n_1548_0_0==1
replace DIET_VARIATION=2 if n_1548_0_0==2
replace DIET_VARIATION=3 if n_1548_0_0==3
replace DIET_VARIATION=. if n_1548_0_0==-1
replace DIET_VARIATION=. if n_1548_0_0==-3


////////////DIET z-score///////////////

capture drop zHDI_TOTALSCORE
egen zHDI_TOTALSCORE=std(HDI_TOTALSCORE)

capture drop zDIET_VARIATION
egen zDIET_VARIATION=std(DIET_VARIATION)


capture drop DIET
gen DIET=zHDI_TOTALSCORE

save, replace


//STEP 5: GENERATE LIFESTYLE VARIABLES: NUTR//


use SMALLERDATASETUKBfin,clear

keep n_eid n_30890_0_0 n_30070_0_0

save NUTR_UKB,replace

label var n_30890_0_0 "Vitamin D"
label var n_30070_0_0 "Red blood cell (erythrocyte) distribution width"


pwcorr n_30890_0_0 n_30070_0_0, sig

capture drop vitamind
gen vitamind= n_30890_0_0 


capture drop rdw
gen rdw=n_30070_0_0 

capture drop zvitamind
egen zvitamind=std(vitamind)

capture drop zrdw
egen zrdw=std(rdw)

capture drop zrdwinv
gen zrdwinv=zrdw*-1

capture drop rowmissNUTR
egen rowmissNUTR=rowmiss(zvitamind zrdwinv)

tab rowmissNUTR

capture drop NUTR
egen NUTR=rmean(zvitamind zrdwinv) if rowmissNUTR<=1

save, replace


//STEP 6: GENERATE LIFESTYLE VARIABLES: SS//

use SMALLERDATASETUKBfin,clear

keep n_eid n_1031_0_0 n_6160_0_* n_2110_0_0

save SS_UKB,replace


//////////SOCIAL SUPPORT VARIABLE 1/////

**Frequency of friend/family visits**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=1031

**WP11 How often do you visit friends or family
**or have them visit you?
**SELECT one of 9 from
**1 : Almost daily
**2 : 2-4 times a week
**3 : About once a week
**4 : About once a month
**5 : Once every few months
**6 : Never or almost never
**7 : No friends/family
**outside household
**-1 : Do not know
**-3 : Prefer not to answer
**Goto WP12 If this varies, please give an average of
**how often you visit or have had visits in
**the last year. Include meeting with
**friends or family in environments
**outside of the home such as in the park,
**at a sports field, at a restaurant or pub.

capture drop SS_friendsfamily
gen SS_friendsfamily=.
replace SS_friendsfamily=n_1031_0_0
replace SS_friendsfamily=. if n_1031_0_0==-1
replace SS_friendsfamily=. if n_1031_0_0==-3
recode SS_friendsfamily (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)


///SOCIAL SUPPORT VARIABLE 2/////
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=6160

**WP12 Which of the following do you attend once
**a week or more often?
**(You can select more than one)
**TOGGLE of 7 choices
**1 : Sports club or gym
**2 : Pub or social club
**3 : Religious group
**4 : Adult education class
**5 : Other group activity
**-7 : None of the above
**-3 : Prefer not to answer
**Require ≥1
**choices
**-7 : is exclusive
**-3 : is exclusive
**Goto WP12A If this varies, please think about
**activities in the last year.


capture drop SS_leisuresocial1
gen SS_leisuresocial1=.
replace SS_leisuresocial1=1 if n_6160_0_0~=. & n_6160_0_0~=-7 & n_6160_0_0~=-3
replace SS_leisuresocial1=0 if SS_leisuresocial1~=1 & n_6160_0_0~=-7 & n_6160_0_0~=-3


capture drop SS_leisuresocial2
gen SS_leisuresocial2=.
replace SS_leisuresocial2=1 if n_6160_0_1~=. & n_6160_0_1~=-7 & n_6160_0_1~=-3
replace SS_leisuresocial2=0 if SS_leisuresocial2~=1 & n_6160_0_1~=-7 & n_6160_0_1~=-3


capture drop SS_leisuresocial3
gen SS_leisuresocial3=.
replace SS_leisuresocial3=1 if n_6160_0_2~=. & n_6160_0_2~=-7 & n_6160_0_2~=-3
replace SS_leisuresocial3=0 if SS_leisuresocial3~=1 & n_6160_0_2~=-7 & n_6160_0_2~=-3


capture drop SS_leisuresocial4
gen SS_leisuresocial4=.
replace SS_leisuresocial4=1 if n_6160_0_3~=. & n_6160_0_3~=-7 & n_6160_0_3~=-3
replace SS_leisuresocial4=0 if SS_leisuresocial4~=1 & n_6160_0_3~=-7 & n_6160_0_3~=-3


capture drop SS_leisuresocial5
gen SS_leisuresocial5=.
replace SS_leisuresocial5=1 if n_6160_0_4~=. & n_6160_0_4~=-7 & n_6160_0_4~=-3
replace SS_leisuresocial5=0 if SS_leisuresocial5~=1 & n_6160_0_4~=-7 & n_6160_0_4~=-3

capture drop rowmissSS1
egen rowmissSS1=rowmiss(SS_leisuresocial1 SS_leisuresocial2 SS_leisuresocial3 SS_leisuresocial4 SS_leisuresocial5)

tab rowmissSS1

capture drop SS_leisuresocial
egen SS_leisuresocial=rmean(SS_leisuresocial1 SS_leisuresocial2 SS_leisuresocial3 SS_leisuresocial4 SS_leisuresocial5) if rowmissSS1<=5

replace SS_leisuresocial=SS_leisuresocial*5

save, replace


**SOCIAL SUPPORT VARIABLE 3**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=2110

**E1 How often are you able to confide in
**someone close to you?
**SELECT one of 8 from
**5 : Almost daily
**4 : 2-4 times a week
**3 : About once a week
**2 : About once a month
**1 : Once every few months
**0 : Never or almost never
**-1 : Do not know
**-3 : Prefer not to answer


capture drop SS_abilityconfide
gen SS_abilityconfide=.
replace SS_abilityconfide=n_2110_0_0
replace SS_abilityconfide=. if n_2110_0_0==-1 | n_2110_0_0==-3


save, replace

capture drop zSS_friendsfamily
egen zSS_friendsfamily=std(SS_friendsfamily)

capture drop zSS_leisuresocial
egen zSS_leisuresocial=std(SS_leisuresocial)

capture drop zSS_abilityconfide
egen zSS_abilityconfide=std(SS_abilityconfide)


capture drop rowmissSS
egen rowmissSS=rowmiss(zSS_friendsfamily zSS_leisuresocial zSS_abilityconfide)

tab rowmissSS



capture drop SS
egen SS=rmean(zSS_friendsfamily zSS_leisuresocial zSS_abilityconfide) if rowmissSS<=1


save, replace

//STEP 7: GENERATE HEALTH VARIABLES//

use SMALLERDATASETUKBfin,clear

keep n_eid n_135_0_0 n_134_0_0 n_2178_0_0 n_21001_0_0 n_21001_0_0 n_48_0_0 n_49_0_0 n_30600_0_0 n_30710_0_0 n_30690_0_0 n_30760_0_0 n_30750_0_0 n_102_0_0 n_4080_0_0 n_4079_0_0 n_31_0_0 n_2443_0_0 n_1160_0_0

save HEALTH_UKB,replace

capture drop sex
gen sex=n_31_0_0 
recode sex 0=2 1=1


********************CO-MORBIDITY INDEX**********

**NUMBER OF SELF-REPORTED NON-CANCER ILLNESSES:***

**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=135

capture drop comorbid_noncancer
gen comorbid_noncancer=.
replace comorbid_noncancer=n_135_0_0


***NUMBER OF SELF-REPORTED CANCERS*****

**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=134

capture drop comorbid_cancer
gen comorbid_cancer=.
replace comorbid_cancer=n_134_0_0

**NUMBER OF CO-MORBIDITIES**

capture drop comorbid
gen comorbid=comorbid_noncancer+comorbid_cancer


***********SELF-RATED HEALTH***************
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=2178

capture drop srh
gen srh=n_2178_0_0

capture drop srhbr
gen srhbr=.
replace srhbr=srh if srh~=-1 & srh~=-3



***********BODY MASS INDEX****************
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=23104

capture drop bmi
gen bmi=n_21001_0_0 
label var bmi "body mass index kg/m^2" //missing values exist - consider imputation




***********ALLOSTATIC LOAD****************

***WAIST TO HIP RATIO***
**https://biobank.ctsu.ox.ac.uk/crystal/search.cgi?wot=0&srch=waist+circumference&sta0=on&sta1=on&sta2=on&sta3=on&str0=on&str3=on&fit0=on&fit10=on&fit20=on&fit30=on&fvt11=on&fvt21=on&fvt22=on&fvt31=on&fvt41=on&fvt51=on&fvt61=on&fvt101=on&yfirst=2000&ylast=2021

su n_48_0_0 n_49_0_0, det

capture drop waist
gen waist=n_48_0_0
label var waist "waist circumference cm" //missing values exist - consider imputation

capture drop hip
gen hip=n_49_0_0
label var hip "hip circumference cm" //missing values exist - consider imputation

capture drop waisthipratio
gen waisthipratio=waist/hip
label var waisthipratio "waist-hip ratio" //missing values exist - consider imputation

capture drop whr_high
gen whr_high=.
replace whr_high=1 if waisthipratio>0.90 & waisthipratio~=. & sex==1 | waisthipratio>0.85 & waisthipratio~=. & sex==2
replace whr_high=0 if whr_high~=1 & waisthipratio~=. 

label var whr_high "elevated, sex-specific waist-hip ratio: 0.90 for men, 0.85 for women"

tab whr_high 

**LOW SERUM ALBUMIN, <38 g/L**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30600**

su n_30600_0_0, det

capture drop albumin_low
gen albumin_low=.
replace albumin_low=1 if n_30600_0_0<38 & n_30600_0_0~=.
replace albumin_low=0 if albumin_low~=1 & n_30600_0_0~=. 
label var albumin_low "low serum albumin, <38 g/l"

tab albumin_low

**HIGH CRP, >=3 mg/dL**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30710

su n_30710_0_0,det

capture drop crp_high
gen crp_high=. 
replace crp_high=1 if n_30710_0_0>=3 & n_30710_0_0~=.
replace crp_high=0 if crp_high~=1 & n_30710_0_0~=. 
label var crp_high "elevated C-reactive protein, >=3 g/l"

tab crp_high


**ELEVATED TC, >=240 mg/dL: divide by 38.67 to get mmol/L: >=6.21 mmol/L

**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30690,det
**https://www.omnicalculator.com/health/cholesterol-units

su n_30690_0_0, det

capture drop tc_high
gen tc_high=.
replace tc_high=1 if n_30690_0_0>=6.21 & n_30690_0_0~=. 
replace tc_high=0 if tc_high~=1 & n_30690_0_0~=. 
label var tc_high "elevated total cholesterol, >=6.21 mmol/L"


tab tc_high

**LOW HDL-C, <40 mg/dL: divide by 38.67 to get mmol/L: <1.034 mmol/L**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30760
**https://www.omnicalculator.com/health/cholesterol-units

su n_30760_0_0, det


capture drop hdl_low
gen hdl_low=.
replace hdl_low=1 if n_30760_0_0<1.034 & n_30760_0_0~=.
replace hdl_low=0 if hdl_low~=1 & n_30760_0_0~=.
label var hdl_low "low HDL-C, <1.034 mmol/L"

tab hdl_low



**Glycated hemoglobin, % change to mmol/mol:  cutoff in % is 6.4% --> 46 mmol/mol
**https://www.hba1cnet.com/hba1c-calculator/
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30750

su n_30750_0_0,det

capture drop ghp_high
gen ghp_high=.
replace ghp_high=1 if n_30750_0_0>=46 & n_30750_0_0~=. 
replace ghp_high=0 if ghp_high~=1 & n_30750_0_0~=. 
label var ghp_high "High glycated hemoglobin A1c, >=46 mmol/mol"


tab ghp_high


**PULSE RATE, automated reading, beats/min:
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=102


su n_102_0_0,det

capture drop rhr_high
gen rhr_high=.
replace rhr_high=1 if n_102_0_0>=90 & n_102_0_0~=.
replace rhr_high=0 if rhr_high~=1 & n_102_0_0~=.
label var rhr_high "Resting pulse rate, >=90 beats/min"

tab rhr_high

**Blood pressure, automated reading:

**Systolic blood pressure, automated reading, mm Hg, >=140 mm Hg: 
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4080

su n_4080_0_0,det

capture drop sbp_high
gen sbp_high=.
replace sbp_high=1 if n_4080_0_0>=140 & n_4080_0_0~=.
replace sbp_high=0 if sbp_high~=1 & n_4080_0_0~=.
label var sbp_high "Systolic blood pressure >=140 mm Hg"

tab sbp_high


**Diastolic blood pressure, automated reading, mm Hg, >=90 mm Hg: 
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4079

su n_4079_0_0,det


capture drop dbp_high
gen dbp_high=.
replace dbp_high=1 if n_4079_0_0>=90 & n_4079_0_0~=.
replace dbp_high=0 if dbp_high~=1 & n_4079_0_0~=.
label var dbp_high "Diastolic blood pressure >=90 mm Hg"

tab dbp_high

**allostatic load, total score**

capture drop allostatic
gen allostatic=.
replace allostatic=albumin_low+crp_high+whr_high+tc_high+hdl_low+ghp_high+rhr_high+sbp_high+dbp_high


save, replace


capture drop zcomorbid
egen zcomorbid=std(comorbid)

capture drop zbmi
egen zbmi=std(bmi)

capture drop zsrh
egen zsrh=std(srhbr)

capture drop zallostatic
egen zallostatic=std(allostatic)

capture drop rowmissHEALTH
egen rowmissHEALTH=rowmiss(zcomorbid zbmi zsrh zallostatic)

tab rowmissHEALTH

capture drop HEALTH
egen HEALTH=rmean(zcomorbid zbmi zsrh zallostatic) if rowmissHEALTH<=2

save, replace



**************************Diabetes*************************
capture drop diabetes
gen diabetes = n_2443_0_0
recode diabetes -3 = 0 -1 =0
label var diabetes "previously diagnosed with diabetes"



********************Sleep duration***********************

capture drop sleep_duration
gen sleep_duration=n_1160_0_0 if n_1160_0_0>0

save, replace


****************************STEP 8: MERGE FILES TOGETHER FOR LIFE'S ESSENTIAL 8****************************

use DEMOG_SES_UKB,clear
sort n_eid
capture drop _merge
save, replace

use LIFESTYLENODIET_UKB,clear
sort n_eid
capture drop _merge
save, replace


use LIFESTYLEDIET_UKB,clear
sort n_eid
capture drop _merge
save, replace


use SS_UKB,clear
sort n_eid
capture drop _merge
save, replace

use HEALTH_UKB,clear
sort n_eid
capture drop _merge
save, replace


use NUTR_UKB,clear
sort n_eid
capture drop _merge
save, replace



use DEMOG_SES_UKB,clear
merge n_eid using LIFESTYLENODIET_UKB
sort n_eid
capture drop _merge
merge n_eid using LIFESTYLEDIET_UKB
sort n_eid
capture drop _merge
merge n_eid using SS_UKB
sort n_eid
capture drop _merge
merge n_eid using HEALTH_UKB
sort n_eid
capture drop _merge
merge n_eid using NUTR_UKB
sort n_eid
capture drop _merge

save LE8_UKB, replace


//STEP 9: GET MEDICATION VARIABLES: STATINS, ANTI-HYPERTENSIVES AND OTHER MEDICATIONS AND MERGE WITH LE8_UKB///////

use SMALLERDATASETUKBfin,clear

keep n_eid n_20003_0_* n_20004_0_* n_6153_0_0 

save MEDICATIONS_UKB, replace

*Aspirin*
capture drop aspirin
gen aspirin = 0
forval i=0/47 {
replace aspirin = 1 if n_20003_0_`i' == 1140868226 
}
label var aspirin "currently taking aspirin"
label define aspirinL 0 "no" 1 "yes"
label values aspirin aspirinL

*Warfarin*
capture drop warfarin
gen warfarin = 0
forval i=0/47 {
replace warfarin = 1 if  n_20003_0_`i' == 1140888266
}
label var warfarin "currently taking warfarin"
label values warfarin aspirinL

*Digoxin*
capture drop digoxin
gen digoxin = 0
forval i=0/47 {
replace digoxin = 1 if  n_20003_0_`i' == 2038459814
}
label var digoxin "currently taking digoxin"
label values digoxin aspirinL

*Metformin*
capture drop metformin
gen metformin = 0
forval i=0/47 {
replace metformin = 1 if  n_20003_0_`i' == 1140884600
}
label var metformin "currently taking metformin"
label values metformin aspirinL

*Radiotherapy*
capture drop radiotherapy
gen radiotherapy = 0 
forval i = 0/31 {
replace radiotherapy = 1 if n_20004_0_`i' == 1228
}

label var radiotherapy "thyroid radioablation therapy"
label define radiotherapyL 0 "no" 1 "yes"
label values radiotherapy radiotherapyL

*Menopause*
capture drop menopause
gen menopause = 0
forval i = 0/31 {
replace menopause = 1 if n_20004_0_`i' == 1665
}

label var menopause "menopause/menopausal symptoms"
label define menopauseL 0 "no" 1 "yes"
label values menopause menopauseL


*Lipid Lowering Drugs*
capture drop statins
gen statins = .
forval i = 0/0 {
replace statins = 1 if n_6153_0_`i' == 1
}
replace statins = 0 if statins ==.
label var statins "currently taking statins"
label values statins aspirinL

*Blood Pressure Treatment*
*Lipid Lowering Drugs*
capture drop bptreat
gen bptreat = .
forval i = 0/0 {
replace bptreat = 1 if n_6153_0_`i' == 2
}
replace bptreat = 0 if bptreat ==.
label var bptreat "currently on anti-hypertensives"
label values bptreat aspirinL



save,replace


use LE8_UKB,clear
sort n_eid
capture drop _merge
save, replace


use MEDICATIONS_UKB,clear
sort n_eid
capture drop _merge
save, replace

use LE8_UKB,clear
merge n_eid using MEDICATIONS_UKB
save, replace


//STEP 10: GENERATE LIFE'S ESSENTIAL 8 INDEX//////

use LE8_UKB, clear



**************************************COMPONENT 1: DIET************************************************

**HDI_TOTALSCORE

su HDI_TOTALSCORE
histogram HDI_TOTALSCORE

**>=95th percentile: 100
**75th-94th percentile: 80
**50th-74th: 50
**25th-49th: 25
**1st-24th: 0

xtile HDI_TOTALSCOREpct=HDI_TOTALSCORE,nq(100)

capture drop LE8_COMP1DIET
gen LE8_COMP1DIET=.
replace LE8_COMP1DIET=100 if HDI_TOTALSCOREpct>=95 & HDI_TOTALSCORE~=.
replace LE8_COMP1DIET=80 if HDI_TOTALSCOREpct>=75 & HDI_TOTALSCOREpct<95 & HDI_TOTALSCORE~=.
replace LE8_COMP1DIET=50 if HDI_TOTALSCOREpct>=50 & HDI_TOTALSCOREpct<75 & HDI_TOTALSCORE~=.
replace LE8_COMP1DIET=25 if HDI_TOTALSCOREpct>=25 & HDI_TOTALSCOREpct<50 & HDI_TOTALSCORE~=.
replace LE8_COMP1DIET=0 if HDI_TOTALSCOREpct>=1 & HDI_TOTALSCOREpct<25 & HDI_TOTALSCORE~=.


************************************COMPONENT 2: PHYSICAL ACTIVITY*************************************


*Moderate exercise MET-min/week*
**gen modperday = n_894_0_0 if n_894_0_0 >= 0
**label var modperday "moderate exercise minutes per day"


*Vigorous exercise MET-min/week*
**gen vigperday = n_914_0_0 if n_914_0_0 >= 0
**label var vigperday "vigorous exercise minutes per day"

**Minutes per day of moderate or greater activity**

capture drop modvigperday
egen modvigperday=rowtotal(modperday vigperday)


**100: >=150
**90: 120-149
**80: 90-119
**60: 60-89
**40: 30-59
**20: 1-29
**0: 0

capture drop LE8_COMP2PA
gen LE8_COMP2PA=.
replace LE8_COMP2PA=100 if modvigperday>=150 & modvigperday~=.
replace LE8_COMP2PA=90 if modvigperday>=120 & modvigperday<150 & modvigperday~=.
replace LE8_COMP2PA=80 if modvigperday>=90 & modvigperday<120 & modvigperday~=.
replace LE8_COMP2PA=60 if modvigperday>=60 & modvigperday<90 & modvigperday~=.
replace LE8_COMP2PA=40 if modvigperday>=30 & modvigperday<60 & modvigperday~=.
replace LE8_COMP2PA=20 if modvigperday>=1 & modvigperday<30 & modvigperday~=.
replace LE8_COMP2PA=0 if modvigperday==0 

save, replace

************************************COMPONENT 3: NICOTINE EXPOSURE*************************************

*Combine Current Smoking with Ex-smoking duration for protective effects*


**gen smokingstatus = 0 if smoking == 2
**replace smokingstatus = agestop if smokingstatus ==.
**replace smokingstatus = 4 if smoking == 0
**label var smokingstatus "complete smoking status"
**label define statusL 0 "current smoker" 1 "ex-smoker < 5 years" 2 "ex-smoker between 5-10 years" 3 "ex-smoker > 10 years" 4 "never smoker"
**label values smokingstatus statusL
**recode smokingstatus .=4

*Environmental tobacco smoke*

**replace n_1279_0_0 = . if n_1279_0_0 < 0
**replace n_1269_0_0  = . if n_1269_0_0 < 0
**order n_1279_0_0, after(n_1269_0_0)
**egen ets = rowtotal(n_1269_0_0-n_1279_0_0)
**gen etsmoke = ets
**label var etsmoke "environmental tobacco exposures (hours per week)"

save, replace

**100 Never smoker
** 75 Former smoker, quit >=5 years
** 50 Former smoker, quit 1-<5 years
**25 Former smoker, quit <1 year, or currently using inhaled NDS
**0 Current smoker

capture drop LE8_COMP3NICOTINE
gen LE8_COMP3NICOTINE=.
replace LE8_COMP3NICOTINE=100 if smokingstatus==4
replace LE8_COMP3NICOTINE=75 if smokingstatus==2 | smokingstatus==3
replace LE8_COMP3NICOTINE=37.5 if smokingstatus==1 
replace LE8_COMP3NICOTINE=0 if smokingstatus==0
replace LE8_COMP3NICOTINE=LE8_COMP3NICOTINE-20 if LE8_COMP3NICOTINE~=0 & etsmoke~=0



**[Note that 25 and 50 are combined to denote <5 years, into 37.5 ] 
save, replace



*************************************COMPONENT 4: SLEEP HEALTH*****************************************


**capture drop sleep_duration
**gen sleep_duration=n_1160_0_0 if n_1160_0_0>0


**100: 7-<9
**90: 9-<10
**70: 6-<7
**40: 5-<6 or >=10
**20: 4-<5
**0: <4


capture drop LE8_COMP4SLEEP
gen LE8_COMP4SLEEP=.
replace LE8_COMP4SLEEP=100 if sleep_duration>=7 & sleep_duration<9
replace LE8_COMP4SLEEP=90 if sleep_duration>=9 & sleep_duration<10
replace LE8_COMP4SLEEP=70 if sleep_duration>=6 & sleep_duration<7
replace LE8_COMP4SLEEP=40 if (sleep_duration>=5 & sleep_duration<6) | (sleep_duration>=10 & sleep_duration~=.)
replace LE8_COMP4SLEEP=20 if sleep_duration>=4 & sleep_duration<5
replace LE8_COMP4SLEEP=0 if sleep_duration<4

save, replace


**************************************COMPONENT 5: BODY MASS INDEX************************************

**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=23104

**capture drop bmi
**gen bmi=n_21001_0_0 
**label var bmi "body mass index kg/m^2" 

**100: <25
**70: 25-29.9
**30: 30.0-34.9
**15: 35.0-39.9
**0: >=40.0


capture drop LE8_COMP5BMI
gen LE8_COMP5BMI=.
replace LE8_COMP5BMI=100 if bmi<25 & bmi~=.
replace LE8_COMP5BMI=70 if bmi>=25 & bmi<30
replace LE8_COMP5BMI=30 if bmi>=30 & bmi<35
replace LE8_COMP5BMI=15 if bmi>=35 & bmi<40
replace LE8_COMP5BMI=0 if bmi>=40 & bmi~=.


save, replace


***************************************COMPONENT 6: Blood lipids **************************************

**ELEVATED TC, >=240 mg/dL: divide by 38.67 to get mmol/L: >=6.21 mmol/L

**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30690,det
**https://www.omnicalculator.com/health/cholesterol-units

su n_30690_0_0, det



**LOW HDL-C, <40 mg/dL: divide by 38.67 to get mmol/L: <1.034 mmol/L**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30760
**https://www.omnicalculator.com/health/cholesterol-units

su n_30760_0_0, det


capture drop nonhdlchol
gen nonhdlchol=(n_30690_0_0-n_30760_0_0)


*Lipid Lowering Drugs*
capture drop statins
gen statins = .
forval i = 0/0 {
replace statins = 1 if n_6153_0_`i' == 1
}
replace statins = 0 if statins ==.
label var statins "currently taking statins"
label values statins aspirinL


**100: <130 or <3.36
**60: 130-159 or 3.36-<4.13
**40: 160-189 or 4.13-<4.91
**20: 190-219 or 4.91-<5.69
**0: >=220 or >=5.69

capture drop LE8_COMP6LIPIDS
gen LE8_COMP6LIPIDS=.
replace LE8_COMP6LIPIDS=100 if nonhdlchol<3.36
replace LE8_COMP6LIPIDS=60 if nonhdlchol>=3.36 & nonhdlchol<4.13
replace LE8_COMP6LIPIDS=40 if nonhdlchol>=4.13 & nonhdlchol<4.91
replace LE8_COMP6LIPIDS=20 if nonhdlchol>=4.91 & nonhdlchol<5.69
replace LE8_COMP6LIPIDS=0 if nonhdlchol>=5.69 & nonhdlchol~=.
replace LE8_COMP6LIPIDS=LE8_COMP6LIPIDS-20 if LE8_COMP6LIPIDS~=0 & statins==1

save, replace

***************************************COMPONENT 7: Blood glucose or HbA1c*************************************

**Glycated hemoglobin, % change to mmol/mol: (35.3 mmol/mol is 5.4%), cutoff in % is 6.4% --> 46 mmol/mol
**Use the converter below

**https://www.hba1cnet.com/hba1c-calculator/
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=30750

su n_30750_0_0,det



*Diabetes*
**gen diabetes = n_2443_0_0
**recode diabetes -3 = 0 -1 =0
**label var diabetes "previously diagnosed with diabetes"
**label values diabetes aspirinL

**Points	Level (for HANDLS SE8)
**100	If dxDiabetes is not "Diabetes" and hbA1c < 5.7
**60	If dxDiabetes is not "Diabetes" and HbA1c 5.7–6.4
**40	HbA1c <7.0 and dxDiabetes = "Diabetes", 
**OR If dxDiabetes is not "Diabetes"(or dx missing) and HbA1C 6.4-7.0
**30	HbA1c 7.0–7.9  (regardless of dx)
**20	HbA1c 8.0–8.9  (regardless of dx)
**10	Hb A1c 9.0–9.9  (regardless of dx)
**0	HbA1c ≥10.0  (regardless of dx)


capture drop LE8_COMP7GLUC
gen LE8_COMP7GLUC=.
replace LE8_COMP7GLUC=100 if diabetes==0 & n_30750_0_0<39 
replace LE8_COMP7GLUC=60 if  diabetes==0 & (n_30750_0_0>=39 &  n_30750_0_0<46)
replace LE8_COMP7GLUC=40 if  (diabetes==1 & (n_30750_0_0<53)) | (diabetes==0 & (n_30750_0_0>=46 & n_30750_0_0<53))
replace LE8_COMP7GLUC=30 if  (n_30750_0_0>=53 & n_30750_0_0<64)
replace LE8_COMP7GLUC=20 if  (n_30750_0_0>=64 & n_30750_0_0<75)
replace LE8_COMP7GLUC=10 if  (n_30750_0_0>=75 & n_30750_0_0<86)
replace LE8_COMP7GLUC=0 if  (n_30750_0_0>=86) & n_30750_0_0~=.

save, replace

***************************************COMPONENT 8: Blood pressure************************************


**Systolic blood pressure, automated reading, mm Hg, >=140 mm Hg: 
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4080

su n_4080_0_0,det


**Diastolic blood pressure, automated reading, mm Hg, >=90 mm Hg: 
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=4079

su n_4079_0_0,det


*Blood Pressure Treatment*

**capture drop bptreat
**gen bptreat = .
**forval i = 0/3 {
**replace bptreat = 1 if n_6153_0_`i' == 2
**}
**replace bptreat = 0 if bptreat ==.
**label var bptreat "currently on anti-hypertensives"
**label values bptreat aspirinL


**100: <120/<80 (Optimal)
**75: 120-129/<80 (Elevated)
**50: 130-139 or 80-89 (Stage | HTN)
**25: 140-159 or 90-99
**0: >=160 or >=100
**Subtact 20 pionts if treated level**

capture drop LE8_COMP8BP
gen LE8_COMP8BP=.
replace LE8_COMP8BP=100 if n_4080_0_0<120 & n_4079_0_0<80
replace LE8_COMP8BP=75 if (n_4080_0_0>=120 & n_4080_0_0<130) & n_4079_0_0<80
replace LE8_COMP8BP=50 if (n_4080_0_0>=130 & n_4080_0_0<140) | (n_4079_0_0>=80 & n_4079_0_0<90)
replace LE8_COMP8BP=25 if (n_4080_0_0>=140 & n_4080_0_0<160) | (n_4079_0_0>=90 & n_4079_0_0<100)
replace LE8_COMP8BP=0 if (n_4080_0_0>=160 & n_4080_0_0~=.) | (n_4079_0_0>=100 & n_4079_0_0~=.)
replace LE8_COMP8BP=LE8_COMP8BP-20 if LE8_COMP8BP~=0 & bptreat==1



*******************************************LE8 TOTAL SCORE**********************************************
capture drop rowmissLE8TOTAL
egen rowmissLE8TOTAL=rowmiss(LE8_COMP1DIET LE8_COMP2PA LE8_COMP3NICOTINE LE8_COMP4SLEEP LE8_COMP5BMI LE8_COMP6LIPIDS LE8_COMP7GLUC LE8_COMP8BP)

tab rowmissLE8TOTAL

capture drop LE8_TOTALSCORE
egen LE8_TOTALSCORE=rmean(LE8_COMP1DIET LE8_COMP2PA LE8_COMP3NICOTINE LE8_COMP4SLEEP LE8_COMP5BMI LE8_COMP6LIPIDS LE8_COMP7GLUC LE8_COMP8BP) if rowmissLE8TOTAL<=4

replace LE8_TOTALSCORE=LE8_TOTALSCORE*8


*******************************************LE8 LIFESTYLE SCORE*******************************************
capture drop rowmissLE8LIFESTYLE
egen rowmissLE8LIFESTYLE=rowmiss(LE8_COMP1DIET LE8_COMP2PA LE8_COMP3NICOTINE LE8_COMP4SLEEP)

tab rowmissLE8LIFESTYLE

capture drop LE8_LIFESTYLE
egen LE8_LIFESTYLE=rmean(LE8_COMP1DIET LE8_COMP2PA LE8_COMP3NICOTINE LE8_COMP4SLEEP) if rowmissLE8LIFESTYLE<=2


replace LE8_LIFESTYLE=LE8_LIFESTYLE*4



********************************************LE8 BIOLOGICAL SCORE***********************************************
capture drop rowmissLE8BIOLOGICAL
egen rowmissLE8BIOLOGICAL=rowmiss(LE8_COMP5BMI LE8_COMP6LIPIDS LE8_COMP7GLUC LE8_COMP8BP)

tab rowmissLE8BIOLOGICAL

capture drop LE8_BIOLOGICAL
egen LE8_BIOLOGICAL=rmean(LE8_COMP5BMI LE8_COMP6LIPIDS LE8_COMP7GLUC LE8_COMP8BP) if rowmissLE8BIOLOGICAL<=2

replace LE8_BIOLOGICAL=LE8_BIOLOGICAL*4

 

save, replace



//STEP 11: GENERATE COGNITION VARIABLES//

use SMALLERDATASETUKBfin,clear

keep n_eid n_20023_0_0 n_399_0_1 n_399_0_2 n_400_0_1 n_400_0_2 

save COGN_UKB,replace

capture drop n_400_0_1r
capture drop n_400_0_2r

gen n_400_0_1r=n_400_0_1 
gen n_400_0_2r=n_400_0_2 

destring n_399_0_1 n_399_0_2 n_400_0_1r n_400_0_2r,replace

**Reaction time: mean time to correctly identify matches**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20023

capture drop REACTION_TIME
gen REACTION_TIME=.
replace REACTION_TIME=ln(n_20023_0_0)



**Pairs matching: number incorrect**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=399
capture drop n_399_0_mean
gen n_399_0_mean=(n_399_0_1+n_399_0_2)/2

su n_399_0_mean


capture drop PAIRSMATCHING_INC
gen PAIRSMATCHING_INC=.
replace PAIRSMATCHING_INC=ln(n_399_0_mean)

save, replace

**Pairs matching: time to complete**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=400

capture drop n_400_0_mean
gen n_400_0_mean=(n_400_0_1r+n_400_0_2r)/2


capture drop PAIRSMATCHING_TTC
gen PAIRSMATCHING_TTC=.
replace PAIRSMATCHING_TTC=ln(n_400_0_mean)


capture drop rowmissCOGN
egen rowmissCOGN=rowmiss(REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC)

tab rowmissCOGN

***PCA for cognitive performance**

pca REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC if rowmissCOGN<=1, factors(1)
predict POORCOGN
 
save, replace



//STEP 12: GENERATE AD/DEMENTIA AND RELATED TIME VARIABLES//

use SMALLERDATASETUKBfin,clear

keep n_eid n_21022_0_0 n_20009_0_* n_20002_0_* s_41202_0_* n_34_0_0 n_52_0_0 ts_53_0_0 ts_40000_*_0  n_40007_*_0  ts_42018_0_0 ts_42020_0_0 





save ADDEMENTIA_UKB,replace


**DEMENTIA AND ALZHEIMER'S DISEASE, INCIDENT CASES AND DATE OF INCIDENCE**

**https://biobank.ctsu.ox.ac.uk/crystal/search.cgi?wot=0&srch=dementia&sta0=on&sta1=on&sta2=on&sta3=on&str0=on&str3=on&fit0=on&fit10=on&fit20=on&fit30=on&fvt11=on&fvt21=on&fvt22=on&fvt31=on&fvt41=on&fvt51=on&fvt61=on&fvt101=on&yfirst=2000&ylast=2021

**Main resource: algorithmically defined outcomes: 
**https://biobank.ctsu.ox.ac.uk/crystal/ukb/docs/alg_outcome_main.pdf**: pages 14-15


**Field ID	Description	Category

**130836	Date F00 first reported (dementia in alzheimer's disease)	Mental and behavioural disorders  
**130838	Date F01 first reported (vascular dementia)	Mental and behavioural disorders  
**130840	Date F02 first reported (dementia in other diseases classified elsewhere)	Mental and behavioural disorders  
**130842	Date F03 first reported (unspecified dementia)	Mental and behavioural disorders  
**42018	Date of all cause dementia report	Dementia outcomes  
**42024	Date of frontotemporal dementia report	Dementia outcomes  
**42022	Date of vascular dementia report	Dementia outcomes  
**42019	Source of all cause dementia report	Dementia outcomes  
**42025	Source of frontotemporal dementia report	Dementia outcomes  
**130837	Source of report of F00 (dementia in alzheimer's disease)	Mental and behavioural disorders  
**130839	Source of report of F01 (vascular dementia)	Mental and behavioural disorders  
**130841	Source of report of F02 (dementia in other diseases classified elsewhere)	Mental and behavioural disorders  
**130843	Source of report of F03 (unspecified dementia)	Mental and behavioural disorders  
**42023	Source of vascular dementia report	Dementia outcomes  
**20112	Illnesses of adopted father	Family history  
**20113	Illnesses of adopted mother	Family history  
**20114	Illnesses of adopted siblings	Family history  
**20107	Illnesses of father	Family history  
**20110	Illnesses of mother	Family history  
**20111	Illnesses of siblings	Family history  
**40002	Contributory (secondary) causes of death: ICD10	Death register  
**41270	Diagnoses - ICD10	Summary Diagnoses  
**41202	Diagnoses - main ICD10	Summary Diagnoses  
**41204	Diagnoses - secondary ICD10	Summary Diagnoses  
**41201	External causes - ICD10	Summary Diagnoses  
**40006	Type of cancer: ICD10	Cancer register  
**40001	Underlying (primary) cause of death: ICD10	Death register  
**41271	Diagnoses - ICD9	Summary Diagnoses  
**41203	Diagnoses - main ICD9	Summary Diagnoses  
**41205	Diagnoses - secondary ICD9	Summary Diagnoses  
**40013	Type of cancer: ICD9	Cancer register  
**20002	Non-cancer illness code, self-reported	Medical conditions  
**41246	Treatment speciality of consultant (recoded)	Summary Administration  


**Field ID	Description	Category
**20009	Interpolated Age of participant when non-cancer illness first diagnosed	Medical conditions  
**40007	Age at death	Death register  


**Field ID	Description	Category
**131036	Date G30 first reported (alzheimer's disease)	Nervous system disorders  (**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=131036)  
**42020	        Date of alzheimer's disease report	Dementia outcomes  (**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=42020)
**42021	        Source of alzheimer's disease report	Dementia outcomes  
**130837	Source of report of F00 (dementia in alzheimer's disease)	Mental and behavioural disorders  
**131037	Source of report of G30 (alzheimer's disease)	Nervous system disorders  

/////////////////////////////////////////////////////////////////////////////////////////////////
***Field 20002: https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=6
***Field 20009: https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20009

*Years of Birth*
capture drop birthyear
gen birthyear=n_34_0_0 

*Month of Birth*
capture drop birthmonth
gen birthmonth=n_52_0_0 

*Date of birth*
capture drop birthdate
gen birthdate=mdy(birthmonth,14,birthyear)

*Baseline Cohort Start Date*
capture drop startdate
gen startdate=ts_53_0_0 
format startdate %d

*Baseline age**
capture drop baselineage
gen baselineage=(startdate-birthdate)/365.25


**Prior dementia**
forval i = 0/28 {
replace n_20009_0_`i' = . if n_20009_0_`i' < 0
}

gen dem = 0
gen agedem = .
forval i=0/28 {
replace dem = 1 if n_20002_0_`i' == 1263
	forval k=0/28 {
	replace agedem = n_20009_0_`k' if dem == 1 & n_20009_0_`k' < n_20009_0_`k-1'
	}
}

capture drop dem
capture drop priordem 
gen priordem = 1 if agedem < baselineage 
recode priordem .=0 
label var priordem "individuals with prior history of dementia"
label define priorL 0 "none" 1 "had disease before cohort start date"
label values priordem priorL

**Source: https://bmcmedicine.biomedcentral.com/track/pdf/10.1186/s12916-021-01980-z.pdf


**Dementia: A81.0, F00, F01, F02, F03, F05, G30, G31.0, G31.1, G31.8, and I67.3

**AD: (F00, G30)

**VaD: (F01, I67.3)


format ts_42018_0_0 %d 

capture drop DEMENTIA_EARLIESTDATE
gen DEMENTIA_EARLIESTDATE= ts_42018_0_0  

capture drop dem_diag
gen dem_diag=.
replace dem_diag=1 if DEMENTIA_EARLIESTDATE~=.
replace dem_diag=0 if dem_diag~=1



****Incident Alzheimer's Disease***


format ts_42020_0_0 %d 

capture drop AD_EARLIESTDATE
gen AD_EARLIESTDATE = ts_42020_0_0


capture drop ad_diag
gen ad_diag=.
replace ad_diag=1 if AD_EARLIESTDATE~=.
replace ad_diag=0 if ad_diag~=1

*Years of Birth*
capture drop birthyear
gen birthyear=n_34_0_0 

*Month of Birth*
capture drop birthmonth
gen birthmonth=n_52_0_0 

*Date of birth*
capture drop birthdate
gen birthdate=mdy(birthmonth,14,birthyear)

*Baseline Cohort Start Date*
capture drop startdate
gen startdate=ts_53_0_0 
format startdate %d

*Baseline age**
capture drop baselineage
gen baselineage=(startdate-birthdate)/365.25

*Death Date*
capture drop deathdate
gen deathdate = ts_40000_0_0 
replace deathdate =  ts_40000_1_0 if deathdate ==.
format deathdate %d
label var deathdate "date of death"

**Died vs. not**
capture drop died
gen died=.
replace died=1 if deathdate~=.
replace died=0 if deathdate==.

**Age of death**
**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=40007

capture drop deathage
gen deathage=.
replace deathage=n_40007_0_0 if n_40007_0_0~=.
replace deathage=n_40007_1_0 if n_40007_1_0~=.

**https://biobank.ndph.ox.ac.uk/ukb/exinfo.cgi?src=Data_providers_and_dates**

*Generate exit date:
capture drop doexit
gen doexit = deathdate if died==1
replace doexit = d(31oct2021) if doexit==. & died==0
format doexit %d

*Generate follow-up time between start and end among those who survived during follow-up without incident outcome*
capture drop time0_0 timeyrs0_0
gen time0_0 = doexit - startdate if died==0
gen timeyrs0_0 = time0_0/365.25
label var time0_0 "follow up time in days if no incident outcome and survived, days"
label var timeyrs0_0 "follow up time in years if no incident outcome and survived, years"

*Generate follow-up time between start and end among those who died during follow-up without incident outcome*
capture drop time1_0 timeyrs1_0
gen time1_0 = doexit - startdate if died==1
gen timeyrs1_0 = time1_0 /365.25
label var time1_0 "follow up time in days if no incident outcome and died, days"
label var timeyrs1_0 "follow up time in years if no incident outcome and died, years"


***Generate dementia earliest date for incident cases: **

**Field IDField title
**42018Date of all cause dementia report
**42019Source of all cause dementia report
**42020Date of alzheimer's disease report
**42021Source of alzheimer's disease report
**42022Date of vascular dementia report
**42023Source of vascular dementia report
**42024Date of frontotemporal dementia report
**42025Source of frontotemporal dementia r


***Generate Alzheimer's Disease earliest date for incident cases**
**42020	        Date of alzheimer's disease report	Dementia outcomes  (**https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=42020)



****Generate follow-up time between start and end among those who survived or died during follow-up but with  incident all-cause dementia*
capture drop time01_1A 
capture drop time01_1Ayears
gen time01_1A = DEMENTIA_EARLIESTDATE - startdate 
gen time01_1Ayears = time01_1A/365.25 
replace time01_1Ayears=time01_1Ayears if time01_1Ayears>-10
label var time01_1A "follow up time in days if Dementia incident outcome and died/survived, days"
label var time01_1Ayears "follow up time in days if Dementia incident outcome and died/survived, years"


****Generate follow-up time between start and end among those who survived or died during follow-up but with  incident AD*
capture drop time01_1B 
capture drop time01_1Byears
gen time01_1B = AD_EARLIESTDATE - startdate
gen time01_1Byears = time01_1B/365.25 
replace time01_1Byears=time01_1Byears if time01_1Byears>-10
label var time01_1B "follow up time in days if AD incident outcome and died/survived, days"
label var time01_1Byears "follow up time in years if AD incident outcome and died/survived, years"


**Generate time to all-cause dementia**

capture drop time_dementia
gen time_dementia=.
replace time_dementia=timeyrs1_0  if dem_diag==0 & died==1
replace time_dementia=time01_1Ayears   if dem_diag==1 
replace time_dementia=timeyrs0_0   if time_dementia==.



**Generate time to AD dementia**
capture drop time_AD
gen time_AD=.
replace time_AD=timeyrs1_0  if ad_diag==0 & died==1
replace time_AD=time01_1Byears   if ad_diag==1 
replace time_AD=timeyrs0_0   if time_AD==.


**Generate Age to incident all-cause dementia or death or end of follow-up**
capture drop Age_dementia
gen Age_dementia=.
replace Age_dementia=baselineage+time_dementia 
 

**Generate Age to incident AD dementia or death or end of follow-up**

capture drop Age_AD
gen Age_AD=baselineage+time_AD 

save, replace

//STEP 13: MERGE DATASETS TOGETHER//


use ADDEMENTIA_UKB
sort n_eid
capture drop _merge
save, replace

use COGN_UKB,clear
sort n_eid
capture drop _merge
save, replace


use LE8_UKB
sort n_eid
capture drop _merge
save, replace

merge n_eid using COGN_UKB
capture drop _merge
sort n_eid
merge n_eid using ADDEMENTIA_UKB

save UKB_PAPER5_RACESESMRI_SUBCORT_DMRI, replace

//STEP 14: H. PYLORI AND OTHER PERSISTENT INFECTIONS///////

clear all

use n_eid n_23074_0_0 n_23050_0_0 n_23051_0_0  n_23052_0_0 n_23053_0_0 n_23054_0_0 n_23055_0_0 n_23058_0_0 n_23059_0_0 n_23060_0_0 n_23061_0_0 ///
n_23062_0_0 n_23063_0_0 n_23064_0_0 n_23065_0_0 n_23066_0_0 n_23067_0_0 n_23075_0_0 n_23069_0_0 n_23071_0_0 n_23039_0_0 n_23040_0_0 n_23041_0_0 ///
n_23042_0_0 n_23043_0_0  n_23044_0_0 using SMALLERDATASETUKBfin.dta

save UKB_HPYLORIPERSIST, replace

*********************************** H. pylori antigens *****************************

**23039	CagA antigen for Helicobacter pylori	Infectious Disease Antigens  
**23043	Catalase antigen for Helicobacter pylori	Infectious Disease Antigens  
**23042	GroEL antigen for Helicobacter pylori	Infectious Disease Antigens  
**23041	OMP antigen for Helicobacter pylori	Infectious Disease Antigens  
**23044	UreA antigen for Helicobacter pylori	Infectious Disease Antigens  
**23040	VacA antigen for Helicobacter pylori	Infectious Disease Antigens  

capture drop hpyloriCagA
gen hpyloriCagA=.
replace hpyloriCagA=n_23039_0_0


capture drop hpyloriCat
gen hpyloriCat=.
replace hpyloriCat=n_23043_0_0

capture drop hpyloriGroeL
gen hpyloriGroeL=.
replace hpyloriGroeL=n_23042_0_0

capture drop hpyloriOMP
gen hpyloriOMP=.
replace hpyloriOMP=n_23041_0_0

capture drop hpyloriUreA
gen hpyloriUreA=.
replace hpyloriUreA=n_23044_0_0

capture drop hpyloriVacA
gen hpyloriVacA=.
replace hpyloriVacA=n_23040_0_0


capture drop LnhpyloriCagA LnhpyloriCat LnhpyloriGroeL LnhpyloriOMP LnhpyloriUreA LnhpyloriVacA
foreach x of varlist hpyloriCagA hpyloriCat hpyloriGroeL hpyloriOMP hpyloriUreA hpyloriVacA {
gen Ln`x'=ln(`x')
}

save, replace


************************************H. pylori seropositivity, II*******************
capture drop hpylori_seropos2
gen hpylori_seropos2=.
replace hpylori_seropos2=n_23074_0_0

******************************Infectious burden***********************************

**HSV-1 seropositivity**

capture drop hsv1_seropos
gen hsv1_seropos=n_23050_0_0

**HSV-2 seropositivity**	

capture drop hsv2_seropos
gen hsv2_seropos=n_23051_0_0

**Varicella zoster seropositivity**
capture drop vzv_seropos
gen vzv_seropos=n_23052_0_0

**EBV seropositivity**
capture drop ebv_seropos
gen ebv_seropos=n_23053_0_0

**CMV seropositivity**
capture drop cmv_seropos
gen cmv_seropos=n_23054_0_0

**HHV-6 overall seropositivity for Human Herpesvirus-6**
capture drop hhv6_seropos
gen hhv6_seropos=n_23055_0_0

**HHV-7 seropositivity for Human Herpesvirus-7**
capture drop hhv7_seropos
gen hhv7_seropos=n_23058_0_0

**KSHV seropositivity for Kaposi's Sarcoma-Associated HerpesvirusV
capture drop kshv_seropos
gen kshv_seropos=n_23059_0_0

**HBV seropositivity for Hepatitis B Virus
capture drop hbv_seropos
gen hbv_seropos=n_23060_0_0

**HCV seropositivity for Hepatitis C Virus
capture drop hcv_seropos
gen hcv_seropos=n_23061_0_0

**T. gondii seropositivity for Toxoplasma gondii
capture drop TG_seropos
gen TG_seropos=n_23062_0_0

**HTLV-1 seropositivity for Human T-Lymphotropic Virus 1
capture drop htlv1_seropos
gen htlv1_seropos=n_23063_0_0

**HIV-1 seropositivity for Human Immunodeficiency Virus**
capture drop hiv1_seropos
gen hiv1_seropos=n_23064_0_0

**BKV seropositivity for Human Polyomavirus BKV
capture drop bkv_seropos
gen bkv_seropos=n_23065_0_0

**JCV seropositivity for Human Polyomavirus JCV
capture drop jvc_seropos
gen jvc_seropos=n_23066_0_0

**MCV seropositivity for Merkel Cell Polyomavirus
capture drop mcv_seropos
gen mcv_seropos=n_23067_0_0


**HPV 16 Definition II seropositivity for Human Papillomavirus type-16
capture drop hpv16_seropos
gen hpv16_seropos=n_23075_0_0


**HPV 18 seropositivity for Human Papillomavirus type-18
capture drop hpv18_seropos
gen hpv18_seropos=n_23069_0_0


**C. trachomatis Definition II seropositivity for Chlamydia trachomatis
capture drop CT_seropos2
gen CT_seropos2=n_23071_0_0


//////////INFECTIOUS BURDEN, TOTAL///////

capture drop IB_total
gen IB_total=hpylori_seropos2+CT_seropos2+hpv18_seropos+hpv16_seropos+mcv_seropos+jvc_seropos+bkv_seropos+hiv1_seropos+htlv1_seropos+TG_seropos+hcv_seropos ////
+hbv_seropos+kshv_seropos+hhv7_seropos+hhv6_seropos+cmv_seropos+ebv_seropos+vzv_seropos+hsv2_seropos+hsv1_seropos



//////////INFECTIOUS BURDEN, MINUS H. PYLORI/////

capture drop IB_minhpylori
gen IB_minhpylori=CT_seropos2+hpv18_seropos+hpv16_seropos+mcv_seropos+jvc_seropos+bkv_seropos+hiv1_seropos+htlv1_seropos+TG_seropos+hcv_seropos ////
+hbv_seropos+kshv_seropos+hhv7_seropos+hhv6_seropos+cmv_seropos+ebv_seropos+vzv_seropos+hsv2_seropos+hsv1_seropos

save, replace


/////////////////////////////////STEP 15: BRAIN SMRI PHENOTYPES////////////////////////////////////////

use SMALLERDATASETUKBfin,clear

keep n_eid n_25010_2_0 n_25008_2_0 n_25006_2_0 n_25006_2_0 n_2501*_2_0 n_2502*_2_0 n_257*_2_0 n_258*_2_0 n_259*_2_0  ts_53_0_0 ts_53_2_0 n_26521_2_0

save UKB_BRAINSMRI, replace


////////////////////////////////////TIME_V0V2////////////////////////////////////////////////////////

capture drop TIME_V0V2
gen TIME_V0V2=ts_53_2_0-ts_53_0_0

save, replace


/////////////////////////////////////SMRI VARIABLES/////////////////////////

**Total brain volume: 25010**
**https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=25010

capture drop TOTALBRAIN
gen TOTALBRAIN=n_25010_2_0


**ICV: 26521**
**https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=26521

capture drop ICV
gen ICV=n_26521_2_0



**Total WM: 25008**
**https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=25008

capture drop WM
gen WM=n_25008_2_0


**Total GM: 25006**
**https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=25006

capture drop GM
gen GM=n_25006_2_0



**Total Lesion volume or WMH: 25781**
**https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=25781

capture drop WMH
gen WMH=n_25781_2_0

capture drop WMHpctICV
gen WMHpctICV=(WMH*100/ICV)

capture drop LnWMHpctICV
gen LnWMHpctICV=ln(WMHpctICV)



**FRONTAL GRAY MATTER, Left**

capture drop FRONTAL_GM_LEFT
gen FRONTAL_GM_LEFT=(n_25830_2_0+n_25862_2_0+n_25846_2_0+n_25782_2_0+n_25792_2_0+n_25790_2_0+n_25832_2_0+n_25788_2_0+n_25794_2_0+n_25786_2_0)


**FRONTAL GRAY MATTER, Right**

capture drop FRONTAL_GM_RIGHT
gen FRONTAL_GM_RIGHT=(n_25831_2_0+n_25863_2_0+n_25847_2_0+n_25783_2_0+n_25793_2_0+n_25791_2_0+n_25833_2_0+n_25789_2_0+n_25795_2_0+n_25787_2_0)



**All small L/R ROIs GRAY MATTER VOLUMES: FAST**

**Field ID	Description
**25888	Volume of grey matter in Amygdala (left)
**25889	Volume of grey matter in Amygdala (right)
**25822	Volume of grey matter in Angular Gyrus (left)
**25823	Volume of grey matter in Angular Gyrus (right)
**25892	Volume of grey matter in Brain-Stem
**25880	Volume of grey matter in Caudate (left)
**25881	Volume of grey matter in Caudate (right)
**25864	Volume of grey matter in Central Opercular Cortex (left)
**25865	Volume of grey matter in Central Opercular Cortex (right)
**25838	Volume of grey matter in Cingulate Gyrus, anterior division (left)
**25839	Volume of grey matter in Cingulate Gyrus, anterior division (right)
**25840	Volume of grey matter in Cingulate Gyrus, posterior division (left)
**25841	Volume of grey matter in Cingulate Gyrus, posterior division (right)
**25900	Volume of grey matter in Crus I Cerebellum (left)
**25902	Volume of grey matter in Crus I Cerebellum (right)
**25901	Volume of grey matter in Crus I Cerebellum (vermis)
**25903	Volume of grey matter in Crus II Cerebellum (left)
**25905	Volume of grey matter in Crus II Cerebellum (right)
**25904	Volume of grey matter in Crus II Cerebellum (vermis)
**25844	Volume of grey matter in Cuneal Cortex (left)
**25845	Volume of grey matter in Cuneal Cortex (right)
**25830	Volume of grey matter in Frontal Medial Cortex (left)
**25831	Volume of grey matter in Frontal Medial Cortex (right)
**25862	Volume of grey matter in Frontal Operculum Cortex (left)
**25863	Volume of grey matter in Frontal Operculum Cortex (right)
**25846	Volume of grey matter in Frontal Orbital Cortex (left)
**25847	Volume of grey matter in Frontal Orbital Cortex (right)
**25782	Volume of grey matter in Frontal Pole (left)
**25783	Volume of grey matter in Frontal Pole (right)
**25870	Volume of grey matter in Heschl's Gyrus (includes H1 and H2) (left)
**25871	Volume of grey matter in Heschl's Gyrus (includes H1 and H2) (right)
**25886	Volume of grey matter in Hippocampus (left)
**25887	Volume of grey matter in Hippocampus (right)
**25893	Volume of grey matter in I-IV Cerebellum (left)
**25894	Volume of grey matter in I-IV Cerebellum (right)
**25915	Volume of grey matter in IX Cerebellum (left)
**25917	Volume of grey matter in IX Cerebellum (right)
**25916	Volume of grey matter in IX Cerebellum (vermis)
**25792	Volume of grey matter in Inferior Frontal Gyrus, pars opercularis (left)
**25793	Volume of grey matter in Inferior Frontal Gyrus, pars opercularis (right)
**25790	Volume of grey matter in Inferior Frontal Gyrus, pars triangularis (left)
**25791	Volume of grey matter in Inferior Frontal Gyrus, pars triangularis (right)
**25808	Volume of grey matter in Inferior Temporal Gyrus, anterior division (left)
**25809	Volume of grey matter in Inferior Temporal Gyrus, anterior division (right)
**25810	Volume of grey matter in Inferior Temporal Gyrus, posterior division (left)
**25811	Volume of grey matter in Inferior Temporal Gyrus, posterior division (right)
**25812	Volume of grey matter in Inferior Temporal Gyrus, temporooccipital part (left)
**25813	Volume of grey matter in Inferior Temporal Gyrus, temporooccipital part (right)
**25784	Volume of grey matter in Insular Cortex (left)
**25785	Volume of grey matter in Insular Cortex (right)
**25828	Volume of grey matter in Intracalcarine Cortex (left)
**25829	Volume of grey matter in Intracalcarine Cortex (right)
**25832	Volume of grey matter in Juxtapositional Lobule Cortex (formerly Supplementary Motor Cortex) (left)
**25833	Volume of grey matter in Juxtapositional Lobule Cortex (formerly Supplementary Motor Cortex) (right)
**25826	Volume of grey matter in Lateral Occipital Cortex, inferior division (left)
**25827	Volume of grey matter in Lateral Occipital Cortex, inferior division (right)
**25824	Volume of grey matter in Lateral Occipital Cortex, superior division (left)
**25825	Volume of grey matter in Lateral Occipital Cortex, superior division (right)
**25852	Volume of grey matter in Lingual Gyrus (left)
**25853	Volume of grey matter in Lingual Gyrus (right)
**25788	Volume of grey matter in Middle Frontal Gyrus (left)
**25789	Volume of grey matter in Middle Frontal Gyrus (right)
**25802	Volume of grey matter in Middle Temporal Gyrus, anterior division (left)
**25803	Volume of grey matter in Middle Temporal Gyrus, anterior division (right)
**25804	Volume of grey matter in Middle Temporal Gyrus, posterior division (left)
**25805	Volume of grey matter in Middle Temporal Gyrus, posterior division (right)
**25806	Volume of grey matter in Middle Temporal Gyrus, temporooccipital part (left)
**25807	Volume of grey matter in Middle Temporal Gyrus, temporooccipital part (right)
**25860	Volume of grey matter in Occipital Fusiform Gyrus (left)
**25861	Volume of grey matter in Occipital Fusiform Gyrus (right)
**25876	Volume of grey matter in Occipital Pole (left)
**25877	Volume of grey matter in Occipital Pole (right)
**25884	Volume of grey matter in Pallidum (left)
**25885	Volume of grey matter in Pallidum (right)
**25836	Volume of grey matter in Paracingulate Gyrus (left)
**25837	Volume of grey matter in Paracingulate Gyrus (right)
**25848	Volume of grey matter in Parahippocampal Gyrus, anterior division (left)
**25849	Volume of grey matter in Parahippocampal Gyrus, anterior division (right)
**25850	Volume of grey matter in Parahippocampal Gyrus, posterior division (left)
**25851	Volume of grey matter in Parahippocampal Gyrus, posterior division (right)
**25866	Volume of grey matter in Parietal Operculum Cortex (left)
**25867	Volume of grey matter in Parietal Operculum Cortex (right)
**25868	Volume of grey matter in Planum Polare (left)
**25869	Volume of grey matter in Planum Polare (right)
**25872	Volume of grey matter in Planum Temporale (left)
**25873	Volume of grey matter in Planum Temporale (right)
**25814	Volume of grey matter in Postcentral Gyrus (left)
**25815	Volume of grey matter in Postcentral Gyrus (right)
**25794	Volume of grey matter in Precentral Gyrus (left)
**25795	Volume of grey matter in Precentral Gyrus (right)
**25842	Volume of grey matter in Precuneous Cortex (left)
**25843	Volume of grey matter in Precuneous Cortex (right)
**25882	Volume of grey matter in Putamen (left)
**25883	Volume of grey matter in Putamen (right)
**25834	Volume of grey matter in Subcallosal Cortex (left)
**25835	Volume of grey matter in Subcallosal Cortex (right)
**25786	Volume of grey matter in Superior Frontal Gyrus (left)
**25787	Volume of grey matter in Superior Frontal Gyrus (right)
**25816	Volume of grey matter in Superior Parietal Lobule (left)
**25817	Volume of grey matter in Superior Parietal Lobule (right)
**25798	Volume of grey matter in Superior Temporal Gyrus, anterior division (left)
**25799	Volume of grey matter in Superior Temporal Gyrus, anterior division (right)
**25800	Volume of grey matter in Superior Temporal Gyrus, posterior division (left)
**25801	Volume of grey matter in Superior Temporal Gyrus, posterior division (right)
**25874	Volume of grey matter in Supracalcarine Cortex (left)
**25875	Volume of grey matter in Supracalcarine Cortex (right)
**25818	Volume of grey matter in Supramarginal Gyrus, anterior division (left)
**25819	Volume of grey matter in Supramarginal Gyrus, anterior division (right)
**25820	Volume of grey matter in Supramarginal Gyrus, posterior division (left)
**25821	Volume of grey matter in Supramarginal Gyrus, posterior division (right)
**25854	Volume of grey matter in Temporal Fusiform Cortex, anterior division (left)
**25855	Volume of grey matter in Temporal Fusiform Cortex, anterior division (right)
**25856	Volume of grey matter in Temporal Fusiform Cortex, posterior division (left)
**25857	Volume of grey matter in Temporal Fusiform Cortex, posterior division (right)
**25858	Volume of grey matter in Temporal Occipital Fusiform Cortex (left)
**25859	Volume of grey matter in Temporal Occipital Fusiform Cortex (right)
**25796	Volume of grey matter in Temporal Pole (left)
**25797	Volume of grey matter in Temporal Pole (right)
**25878	Volume of grey matter in Thalamus (left)
**25879	Volume of grey matter in Thalamus (right)
**25895	Volume of grey matter in V Cerebellum (left)
**25896	Volume of grey matter in V Cerebellum (right)
**25897	Volume of grey matter in VI Cerebellum (left)
**25899	Volume of grey matter in VI Cerebellum (right)
**25898	Volume of grey matter in VI Cerebellum (vermis)
**25909	Volume of grey matter in VIIIa Cerebellum (left)
**25911	Volume of grey matter in VIIIa Cerebellum (right)
**25910	Volume of grey matter in VIIIa Cerebellum (vermis)
**25912	Volume of grey matter in VIIIb Cerebellum (left)
**25914	Volume of grey matter in VIIIb Cerebellum (right)
**25913	Volume of grey matter in VIIIb Cerebellum (vermis)
**25906	Volume of grey matter in VIIb Cerebellum (left)
**25908	Volume of grey matter in VIIb Cerebellum (right)
**25907	Volume of grey matter in VIIb Cerebellum (vermis)
**25890	Volume of grey matter in Ventral Striatum (left)
**25891	Volume of grey matter in Ventral Striatum (right)
**25918	Volume of grey matter in X Cerebellum (left)
**25919	Volume of grey matter in X Cerebellum (vermis)
**25920	Volume of grey matter in X Cerebellum (right)


**SUB-CORTICAL VOLUMES (FIRST)**

**Field ID	Description
**25023	Volume of accumbens (left)
**25024	Volume of accumbens (right)
**25021	Volume of amygdala (left)
**25022	Volume of amygdala (right)
**25013	Volume of caudate (left)
**25014	Volume of caudate (right)
**25019	Volume of hippocampus (left)
**25020	Volume of hippocampus (right)
**25017	Volume of pallidum (left)
**25018	Volume of pallidum (right)
**25015	Volume of putamen (left)
**25016	Volume of putamen (right)
**25011	Volume of thalamus (left)
**25012	Volume of thalamus (right)


**Accumbens, Left**
capture drop Accumbens_Left
gen Accumbens_Left=n_25023_2_0


**Accumbens, Right**
capture drop Accumbens_Right
gen Accumbens_Right=n_25024_2_0


**Amygdala, Left**
capture drop Amygdala_Left
gen Amygdala_Left=n_25021_2_0


**Amygdala, Right**
capture drop Amygdala_Right
gen Amygdala_Right=n_25022_2_0


**Caudate, Left**
capture drop Caudate_Left
gen Caudate_Left=n_25013_2_0

**Caudate, Right**
capture drop Caudate_Right
gen Caudate_Right=n_25014_2_0

**Hippocampus, Left**
capture drop Hippocampus_Left
gen Hippocampus_Left=n_25019_2_0


**Hippocampus, Right**
capture drop Hippocampus_Right
gen Hippocampus_Right=n_25020_2_0

**Pallidum, Left**
capture drop Pallidum_Left
gen Pallidum_Left=n_25017_2_0


**Pallidum, Right**
capture drop Pallidum_Right
gen Pallidum_Right=n_25018_2_0

**Putamen, Left**
capture drop Putamen_Left
gen Putamen_Left=n_25015_2_0


**Putamen, Right**
capture drop Putamen_Right
gen Putamen_Right=n_25016_2_0

**Thalamus, Left**
capture drop Thalamus_Left
gen Thalamus_Left=n_25011_2_0

**Thalamus, Right**
capture drop Thalamus_Right
gen Thalamus_Right=n_25012_2_0

save, replace

****************************STEP 16: BRAIN DMRI MEASURES*******************************



use SMALLERDATASETUKBfin,clear

keep n_eid n_250*_2_0 n_251*_2_0   

save UKB_BRAINDMRI, replace

**************************FRACTIONAL ANISOTROPY**********************

capture drop FA_ACR_Left
gen FA_ACR_Left=n_25079_2_0
label var FA_ACR_Left "Mean FA in anterior corona radiata on FA skeleton (left)"

capture drop FA_ACR_Right
gen FA_ACR_Right=n_25078_2_0
label var FA_ACR_Right "Mean FA in anterior corona radiata on FA skeleton (right)"

capture drop FA_ALIC_Left
gen FA_ALIC_Left=n_25073_2_0
label var FA_ALIC_Left "Mean FA in anterior limb of the internal capsule on FA skeleton (left)"


capture drop FA_ALIC_Right
gen FA_ALIC_Right=n_25072_2_0
label var FA_ALIC_Right "Mean FA in anterior limb of the internal capsule on FA skeleton (right)"


capture drop FA_BCC
gen FA_BCC=n_25059_2_0
label var FA_BCC "Mean FA in body of corpus callosum on FA skeleton"

capture drop FA_CP_Left
gen FA_CP_Left=n_25071_2_0
label var FA_CP_Left "Mean FA in body of cerebral peduncle on FA skeleton (left)"

capture drop FA_CP_Right
gen FA_CP_Right=n_25070_2_0
label var FA_CP_Right "Mean FA in body of cerebral peduncle on FA skeleton (right)"


capture drop FA_CCG_Left
gen FA_CCG_Left=n_25091_2_0
label var FA_CCG_Left "Mean FA in body of cigulum cingulate gyrus on FA skeleton (left)"

capture drop FA_CCG_Right
gen FA_CCG_Right=n_25090_2_0
label var FA_CCG_Right "Mean FA in body of cigulum cingulate gyrus on FA skeleton (right)"

capture drop FA_CH_Left
gen FA_CH_Left=n_25093_2_0
label var FA_CH_Left "Mean FA in cigulum hippocampus on FA skeleton (left)"

capture drop FA_CH_Right
gen FA_CH_Right=n_25092_2_0
label var FA_CH_Right "Mean FA in cigulum hippocampus on FA skeleton (right)"

capture drop FA_CT_Left
gen FA_CT_Left=n_25063_2_0
label var FA_CT_Left "Mean FA in corticospinal tract on FA skeleton (left)"

capture drop FA_CT_Right
gen FA_CT_Right=n_25062_2_0
label var FA_CT_Right "Mean FA in corticospinal tract on FA skeleton (right)"

capture drop FA_EC_Left
gen FA_EC_Left=n_25089_2_0
label var FA_EC_Left "Mean FA in external capsule on FA skeleton (left)"

capture drop FA_EC_Right
gen FA_EC_Right=n_25088_2_0
label var FA_EC_Right "Mean FA in external capsule on FA skeleton (right)"

capture drop FA_FCST_Left
gen FA_FCST_Left=n_25095_2_0
label var FA_FCST_Left "Mean FA in fornix cres+stria terminalis on FA skeleton (left)"

capture drop FA_FCST_Right
gen FA_FCST_Right=n_25094_2_0
label var FA_FCST_Right "Mean FA in fornix cres+stria terminalis  on FA skeleton (right)"

capture drop FA_FO
gen FA_FO=n_25061_2_0
label var FA_FO "Mean FA in fornix on FA skeleton (left)"


capture drop FA_GCC
gen FA_GCC=n_25058_2_0
label var FA_GCC "Mean FA in genu of corpus callosum on FA skeleton (left)"

capture drop FA_ICP_Left
gen FA_ICP_Left=n_25067_2_0
label var FA_ICP_Left "Mean FA in inferior cerebellar peduncle on FA skeleton (left)"

capture drop FA_ICP_Right
gen FA_ICP_Right=n_25066_2_0
label var FA_ICP_Right "Mean FA in inferior cerebellar peduncle  on FA skeleton (right)"

capture drop FA_ML_Left
gen FA_ML_Left=n_25065_2_0
label var FA_ML_Left "Mean FA in medial lemniscus on FA skeleton (left)"

capture drop FA_ML_Right
gen FA_ML_Right=n_25064_2_0
label var FA_ML_Right "Mean FA in medial lemniscus  on FA skeleton (right)"

capture drop FA_MCP
gen FA_MCP=n_25056_2_0
label var FA_MCP "Mean FA in middle cerebellar peduncle on FA skeleton (left)"

capture drop FA_PC
gen FA_PC=n_25057_2_0
label var FA_PC "Mean FA in pontine crossing tract on FA skeleton (left)"

capture drop FA_PCR_Left
gen FA_PCR_Left=n_25083_2_0
label var FA_PCR_Left "Mean FA in posterior corona radiata on FA skeleton (left)"

capture drop FA_PCR_Right
gen FA_PCR_Right=n_25082_2_0
label var FA_PCR_Right "Mean FA in posterior corona radiata  on FA skeleton (right)"

capture drop FA_PLIC_Left
gen FA_PLIC_Left=n_25075_2_0
label var FA_PLIC_Left "Mean FA in posterior limb of the internal capsule on FA skeleton (left)"

capture drop FA_PLIC_Right
gen FA_PLIC_Right=n_25074_2_0
label var FA_PLIC_Right "Mean FA in posterior limb of the internal capsule  on FA skeleton (right)"


capture drop FA_PTR_Left
gen FA_PTR_Left=n_25085_2_0
label var FA_PTR_Left "Mean FA in posterior thalamic radiation on FA skeleton (left)"

capture drop FA_PTR_Right
gen FA_PTR_Right=n_25084_2_0
label var FA_PTR_Right "Mean FA in posterior thalamic radiation  on FA skeleton (right)"

capture drop FA_RPIC_Left
gen FA_RPIC_Left=n_25077_2_0
label var FA_RPIC_Left "Mean FA in retrolenticular part of internal capsule on FA skeleton (left)"

capture drop FA_RPIC_Right
gen FA_RPIC_Right=n_25076_2_0
label var FA_RPIC_Right "Mean FA in retrolenticular part of internal capsule  on FA skeleton (right)"

capture drop FA_SS_Left
gen FA_SS_Left=n_25087_2_0
label var FA_SS_Left "Mean FA in sagittal stratum on FA skeleton (left)"

capture drop FA_SS_Right
gen FA_SS_Right=n_25086_2_0
label var FA_SS_Right "Mean FA in sagittal stratum  on FA skeleton (right)"

capture drop FA_SCC
gen FA_SCC=n_25060_2_0
label var FA_SCC "Mean FA in splenium of the corpus callosum"

capture drop FA_SCP_Left
gen FA_SCP_Left=n_25069_2_0
label var FA_SCP_Left "Mean FA in superior cerebellar peduncle on FA skeleton (left)"

capture drop FA_SCP_Right
gen FA_SCP_Right=n_25068_2_0
label var FA_SCP_Right "Mean FA in superior cerebellar peduncle  on FA skeleton (right)"


capture drop FA_CR_Left
gen FA_CR_Left=n_25081_2_0
label var FA_CR_Left "Mean FA in superior corona radiata on FA skeleton (left)"

capture drop FA_CR_Right
gen FA_CR_Right=n_25080_2_0
label var FA_CR_Right "Mean FA in superior corona radiata  on FA skeleton (right)"


capture drop FA_FOF_Left
gen FA_FOF_Left=n_25099_2_0
label var FA_FOF_Left "Mean FA in fronto-occipital fasciculus on FA skeleton (left)"

capture drop FA_FOF_Right
gen FA_FOF_Right=n_25098_2_0
label var FA_FOF_Right "Mean FA in fronto-occipital fasciculus  on FA skeleton (right)"


capture drop FA_SLF_Left
gen FA_SLF_Left=n_25097_2_0
label var FA_SLF_Left "Mean FA in super longitudinal fasciculus on FA skeleton (left)"

capture drop FA_SLF_Right
gen FA_SLF_Right=n_25096_2_0
label var FA_SLF_Right "Mean FA in super longitudinal fasciculus  on FA skeleton (right)"

capture drop FA_TP_Left
gen FA_TP_Left=n_25103_2_0
label var FA_TP_Left "Mean FA in tapetum on FA skeleton (left)"

capture drop FA_TP_Right
gen FA_TP_Right=n_25102_2_0
label var FA_TP_Right "Mean FA in tapetum  on FA skeleton (right)"

capture drop FA_UNC_Left
gen FA_UNC_Left=n_25101_2_0
label var FA_UNC_Left "Mean FA in unciate fasciculus on FA skeleton (left)"

capture drop FA_UNC_Right
gen FA_UNC_Right=n_25100_2_0
label var FA_UNC_Right "Mean FA in unciate fasciculus on FA skeleton (right)"



*************************MEAN DIFFUSIVITIY****************************
capture drop MD_ACR_Left
gen MD_ACR_Left=n_25127_2_0
label var MD_ACR_Left "Mean MD in anterior corona radiata on FA skeleton (left)"

capture drop MD_ACR_Right
gen MD_ACR_Right=n_25126_2_0
label var MD_ACR_Right "Mean MD in anterior corona radiata on FA skeleton (right)"

capture drop MD_ALIC_Left
gen MD_ALIC_Left=n_25121_2_0
label var MD_ALIC_Left "Mean MD in anterior limb of the internal capsule on FA skeleton (left)"


capture drop MD_ALIC_Right
gen MD_ALIC_Right=n_25120_2_0
label var MD_ALIC_Right "Mean MD in anterior limb of the internal capsule on FA skeleton (right)"


capture drop MD_BCC
gen MD_BCC=n_25107_2_0
label var MD_BCC "Mean MD in body of corpus callosum on FA skeleton"

capture drop MD_CP_Left
gen MD_CP_Left=n_25119_2_0
label var MD_CP_Left "Mean MD in body of cerebral peduncle on FA skeleton (left)"

capture drop MD_CP_Right
gen MD_CP_Right=n_25118_2_0
label var MD_CP_Right "Mean MD in body of cerebral peduncle on FA skeleton (right)"


capture drop MD_CCG_Left
gen MD_CCG_Left=n_25139_2_0
label var MD_CCG_Left "Mean MD in body of cigulum cingulate gyrus on FA skeleton (left)"

capture drop MD_CCG_Right
gen MD_CCG_Right=n_25138_2_0
label var MD_CCG_Right "Mean MD in body of cigulum cingulate gyrus on FA skeleton (right)"

capture drop MD_CH_Left
gen MD_CH_Left=n_25141_2_0
label var MD_CH_Left "Mean MD in cigulum hippocampus on FA skeleton (left)"

capture drop MD_CH_Right
gen MD_CH_Right=n_25140_2_0
label var MD_CH_Right "Mean MD in cigulum hippocampus on FA skeleton (right)"

capture drop MD_CT_Left
gen MD_CT_Left=n_25111_2_0
label var MD_CT_Left "Mean MD in corticospinal tract on FA skeleton (left)"

capture drop MD_CT_Right
gen MD_CT_Right=n_25110_2_0
label var MD_CT_Right "Mean MD in corticospinal tract on FA skeleton (right)"

capture drop MD_EC_Left
gen MD_EC_Left=n_25137_2_0
label var MD_EC_Left "Mean MD in external capsule on FA skeleton (left)"

capture drop MD_EC_Right
gen MD_EC_Right=n_25136_2_0
label var MD_EC_Right "Mean MD in external capsule on FA skeleton (right)"

capture drop MD_FCST_Left
gen MD_FCST_Left=n_25143_2_0
label var MD_FCST_Left "Mean MD in fornix cres+stria terminalis on FA skeleton (left)"

capture drop MD_FCST_Right
gen MD_FCST_Right=n_25142_2_0
label var MD_FCST_Right "Mean MD in fornix cres+stria terminalis  on FA skeleton (right)"

capture drop MD_FO
gen MD_FO=n_25109_2_0
label var MD_FO "Mean MD in fornix on FA skeleton (left)"


capture drop MD_GCC
gen MD_GCC=n_25106_2_0
label var MD_GCC "Mean MD in genu of corpus callosum on FA skeleton (left)"

capture drop MD_ICP_Left
gen MD_ICP_Left=n_25115_2_0
label var MD_ICP_Left "Mean MD in inferior cerebellar peduncle on FA skeleton (left)"

capture drop MD_ICP_Right
gen MD_ICP_Right=n_25114_2_0
label var MD_ICP_Right "Mean MD in inferior cerebellar peduncle  on FA skeleton (right)"

capture drop MD_ML_Left
gen MD_ML_Left=n_25113_2_0
label var MD_ML_Left "Mean MD in medial lemniscus on FA skeleton (left)"

capture drop MD_ML_Right
gen MD_ML_Right=n_25112_2_0
label var MD_ML_Right "Mean MD in medial lemniscus  on FA skeleton (right)"

capture drop MD_MCP
gen MD_MCP=n_25104_2_0
label var MD_MCP "Mean MD in middle cerebellar peduncle on FA skeleton (left)"

capture drop MD_PC
gen MD_PC=n_25105_2_0
label var MD_PC "Mean MD in pontine crossing tract on FA skeleton (left)"

capture drop MD_PCR_Left
gen MD_PCR_Left=n_25131_2_0
label var MD_PCR_Left "Mean MD in posterior corona radiata on FA skeleton (left)"

capture drop MD_PCR_Right
gen MD_PCR_Right=n_25130_2_0
label var MD_PCR_Right "Mean MD in posterior corona radiata  on FA skeleton (right)"

capture drop MD_PLIC_Left
gen MD_PLIC_Left=n_25123_2_0
label var MD_PLIC_Left "Mean MD in posterior limb of the internal capsule on FA skeleton (left)"

capture drop MD_PLIC_Right
gen MD_PLIC_Right=n_25122_2_0
label var MD_PLIC_Right "Mean MD in posterior limb of the internal capsule  on FA skeleton (right)"


capture drop MD_PTR_Left
gen MD_PTR_Left=n_25133_2_0
label var MD_PTR_Left "Mean MD in posterior thalamic radiation on FA skeleton (left)"

capture drop MD_PTR_Right
gen MD_PTR_Right=n_25132_2_0
label var MD_PTR_Right "Mean MD in posterior thalamic radiation  on FA skeleton (right)"

capture drop MD_RPIC_Left
gen MD_RPIC_Left=n_25125_2_0
label var MD_RPIC_Left "Mean MD in retrolenticular part of internal capsule on FA skeleton (left)"

capture drop MD_RPIC_Right
gen MD_RPIC_Right=n_25124_2_0
label var MD_RPIC_Right "Mean MD in retrolenticular part of internal capsule  on FA skeleton (right)"

capture drop MD_SS_Left
gen MD_SS_Left=n_25135_2_0
label var MD_SS_Left "Mean MD in sagittal stratum on FA skeleton (left)"

capture drop MD_SS_Right
gen MD_SS_Right=n_25134_2_0
label var MD_SS_Right "Mean MD in sagittal stratum  on FA skeleton (right)"

capture drop MD_SCC
gen MD_SCC=n_25108_2_0
label var MD_SCC "Mean MD in splenium of the corpus callosum"

capture drop MD_SCP_Left
gen MD_SCP_Left=n_25117_2_0
label var MD_SCP_Left "Mean MD in superior cerebellar peduncle on FA skeleton (left)"

capture drop MD_SCP_Right
gen MD_SCP_Right=n_25116_2_0
label var MD_SCP_Right "Mean MD in superior cerebellar peduncle  on FA skeleton (right)"


capture drop MD_CR_Left
gen MD_CR_Left=n_25129_2_0
label var MD_CR_Left "Mean MD in superior corona radiata on FA skeleton (left)"

capture drop MD_CR_Right
gen MD_CR_Right=n_25128_2_0
label var MD_CR_Right "Mean MD in superior corona radiata  on FA skeleton (right)"


capture drop MD_FOF_Left
gen MD_FOF_Left=n_25147_2_0
label var MD_FOF_Left "Mean MD in fronto-occipital fasciculus on FA skeleton (left)"

capture drop MD_FOF_Right
gen MD_FOF_Right=n_25146_2_0
label var MD_FOF_Right "Mean MD in fronto-occipital fasciculus  on FA skeleton (right)"


capture drop MD_SLF_Left
gen MD_SLF_Left=n_25145_2_0
label var MD_SLF_Left "Mean MD in super longitudinal fasciculus on FA skeleton (left)"

capture drop MD_SLF_Right
gen MD_SLF_Right=n_25144_2_0
label var MD_SLF_Right "Mean MD in super longitudinal fasciculus  on FA skeleton (right)"

capture drop MD_TP_Left
gen MD_TP_Left=n_25151_2_0
label var MD_TP_Left "Mean MD in tapetum on FA skeleton (left)"

capture drop MD_TP_Right
gen MD_TP_Right=n_25150_2_0
label var MD_TP_Right "Mean MD in tapetum  on FA skeleton (right)"

capture drop MD_UNC_Left
gen MD_UNC_Left=n_25149_2_0
label var MD_UNC_Left "Mean MD in unciate fasciculus on FA skeleton (left)"

capture drop MD_UNC_Right
gen MD_UNC_Right=n_25148_2_0
label var MD_UNC_Right "Mean MD in unciate fasciculus on FA skeleton (right)"

save, replace


////////////////STEP 17: AD AND OTHER STANDARD PGS////////////////////////


use SMALLERDATASETUKB,clear

keep n_eid  n_262*_0_0


save UKB_PGS, replace

capture drop AD_PGS
gen AD_PGS=n_26206_0_0

save, replace


//////////////////////////STEP 18: MERGE WITH THE OTHER VARIABLES/////////////


use UKB_PAPER5_RACESESMRI_SUBCORT_DMRI,clear
sort n_eid
capture drop _merge
save, replace

use UKB_HPYLORIPERSIST,clear
sort n_eid
capture drop _merge
save, replace


use UKB_BRAINSMRI,clear
sort n_eid
capture drop _merge
save, replace


use UKB_BRAINDMRI,clear
sort n_eid
capture drop _merge
save, replace


use UKB_PGS,clear
sort n_eid
capture drop _merge
save, replace

use UKB_PAPER5_RACESESMRI_SUBCORT_DMRI,clear
sort n_eid
capture drop _merge
save, replace
merge n_eid using UKB_HPYLORIPERSIST
capture drop _merge
sort n_eid
merge n_eid using UKB_BRAINSMRI
capture drop _merge
sort n_eid
merge n_eid using UKB_BRAINDMRI
capture drop _merge
sort n_eid
merge n_eid using UKB_PGS
capture drop _merge
sort n_eid


save UKB_PAPER5_RACESESMRI_SUBCORT_DMRI, replace

capture log close


capture log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\DATA_MANAGEMENT3.smcl"


use UKB_PAPER5_RACESESMRI_SUBCORT_DMRI,clear


//STEP 19: DETERMINE FINAL ANALYTIC SAMPLE SIZE/////

capture drop FA_mean
egen FA_mean=rmean(FA_*)



capture drop MD_mean
egen MD_mean=rmean(MD_*)




**Sample 1**

capture drop sample1
gen sample1=.
replace sample1=1 if baselineage~=. 
replace sample1=0 if sample1~=1

tab sample1

**Sample 2: Complete data on brain sMRI and dMRI**

capture drop sample2
gen sample2=.
replace sample2=1 if ICV~=. & TOTALBRAIN~=. & WMH~=. & Hippocampus_Left~=. & FA_mean~=. & MD_mean~=. & householdsize~=.
replace sample2=0 if sample2~=1

tab sample2

**Sample 3: Complete data on AD_PGS and POORCOGN
capture drop sample3
gen sample3=.
replace sample3=1 if AD_PGS~=. & POORCOGN~=.
replace sample3=0 if sample3~=1


tab sample3

**Sample 4: final sample: exclude prevalent dementia cases and incident dementia cases within a year**

stset Age_dementia, failure(dem_diag==1) enter(baselineage) id(n_eid) scale(1)

capture drop sample4
gen sample4=.
replace sample4=1 if (sample3==1 & sample2==1 & priordem==0 & _t~=.) | (sample3==1 & sample2==1 & time01_1Ayears>1 & time01_1Ayears~=. & dem_diag==1 & _t~=.)
replace sample4=0 if sample4~=1


capture drop sample_final
gen sample_final=sample4

tab sample_final

save, replace


///////////////////////////GENERATE STANDARDIZED MEASURED WITHIN FINAL ANALYTIC SAMPLE/////////////////////////

capture drop FA_mean
egen FA_mean=rmean(FA_*)



capture drop zFA_*
foreach x2 of varlist FA_* {
	egen z`x2'=std(`x2') if sample_final==1
}


capture drop MD_mean
egen MD_mean=rmean(MD_*)


capture drop zMD_*
foreach x3 of varlist MD_* {
	egen z`x3'=std(`x3') if sample_final==1
}

save, replace

//STEP 20: DETERMINE DIFFERENCES IN FINAL ANALYTIC SAMPLE VS. EXCLUDED SAMPLE BY BASIC SOCIO-DEMOGRAPHICS and generate inverse mills ratio//

capture drop RACE_ETHN
gen RACE_ETHN=ethnicity2

logistic sample_final baselineage sex i.RACE_ETHN 


save, replace


xi:probit sample_final baselineage sex i.RACE_ETHN

capture drop p1MRIINF
predict p1MRIINF, xb

capture drop phiMRIINF
capture drop caphiMRIINF
capture drop invmillsMRIINF

gen phiMRIINF=(1/sqrt(2*_pi))*exp(-(p1MRIINF^2/2))

egen caphiMRIINF=std(p1MRIINF)

capture drop invmillsMRIINF
gen invmillsMRIINF=phiMRIINF/caphiMRIINF


su invmillsMRIINF

save , replace

capture log close


capture log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\IMPUTATION.smcl"



//STEP 21: RUN MULTIPLE IMPUTATIONS///

use UKB_PAPER5_RACESESMRI_SUBCORT_DMRI, clear

capture drop AGE
gen AGE=baselineage

capture drop SEX
gen SEX=sex

save, replace

save finaldata_unimputed, replace

sort n_eid 


keep n_eid AGE SEX RACE_ETHN AD_PGS educationbr  smoking etsmoke townsend householdincome packyearssmoke alcohol HDI_TOTALSCORE  SS_friendsfamily SS_leisuresocial SS_abilityconfide srhbr allostatic METmin vitamind rdw comorbid bmi  FA_* MD_* zFA_* zMD_* sample* *_seropos* hpylori* TOTALBRAIN ICV WM GM WMH* LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right invmillsMRIINF LE8* TIME_V0V2 POORCOGN ad_diag dem_diag Age_dementia Age_AD baselineage householdsize REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC POORCOGN

keep if sample_final==1

save finaldata_unimputedFINAL, replace



set matsize 11000

capture mi set, clear

mi set flong

capture mi stset,clear


mi stset Age_dementia, failure(dem_diag==1) enter(baselineage) id(n_eid) scale(1)

**List of exogenous variables:
**RACE_ETHN AGE SEX centre householdsize NonWhite

**List of mediator variables:

**SES: educationbr townsend householdincome
**SMOKING: smoking etsmoke packyearssmoke
**ALCOHOL: alcohol
**PA: METmin
**DIET: HDI_TOTALSCORE DIET_VARIATION 
**NUTR: vitamind rdw
**SS: SS_friendsfamily SS_leisuresocial SS_abilityconfide
**HEALTH: comorbid bmi srh allostatic


mi unregister educationbr townsend householdincome smoking etsmoke packyearssmoke alcohol METmin HDI_TOTALSCORE vitamind rdw SS_friendsfamily SS_leisuresocial SS_abilityconfide comorbid bmi srh allostatic   

mi register imputed educationbr townsend householdincome smoking etsmoke packyearssmoke alcohol METmin HDI_TOTALSCORE vitamind rdw SS_friendsfamily SS_leisuresocial SS_abilityconfide comorbid bmi srh allostatic 

mi register passive RACE_ETHN AGE SEX sample* POORCOGN


mi impute chained (mlogit) educationbr  smoking  (regress)  etsmoke householdincome  alcohol HDI_TOTALSCORE  SS_friendsfamily SS_leisuresocial SS_abilityconfide srhbr allostatic  packyearssmoke townsend METmin vitamind rdw comorbid bmi = AGE SEX i.RACE_ETHN if  sample_final==1 , force augment noisily  add(5) rseed(1234) savetrace(tracefile, replace) 



save finaldata_imputed, replace



save finaldata_imputedFINAL, replace



***************CHECK IF THE IMPUTED VARIABLES HAVE NEGATIVE VALUES*************************
use finaldata_imputed, clear

save finaldata_imputedFINAL, replace


su educationbr  smoking  etsmoke householdincome  alcohol HDI_TOTALSCORE  SS_friendsfamily SS_leisuresocial SS_abilityconfide srhbr allostatic  packyearssmoke METmin vitamind rdw comorbid bmi  if sample_final==1



foreach x of varlist educationbr  smoking  etsmoke householdincome  alcohol HDI_TOTALSCORE  SS_friendsfamily SS_leisuresocial SS_abilityconfide srhbr allostatic  packyearssmoke METmin vitamind rdw comorbid bmi {
replace `x'=0 if `x'<0 & `x'~=.
}




***************RE-GENERATE MEDIATORS AND RUN PCA**************


**SES: educationbr townsend householdincome**
capture drop SES

capture drop zeducationbr
mi passive: egen zeducationbr=std(educationbr) if  sample_final==1

capture drop ztownsend
mi passive: egen ztownsend=std(townsend) if sample_final==1


capture drop zhouseholdincome
mi passive: egen zhouseholdincome=std(householdincome) if sample_final==1

capture drop ztownsendinv
mi passive: gen ztownsendinv=ztownsend*-1


capture drop SES
mi passive: egen SES=rmean(zeducationbr ztownsendinv zhouseholdincome)

tabstat SES, statistics(count mean sd)
mi estimate:  mean SES if sample_final==1
mi estimate:  reg SES educationbr townsend householdincome if sample_final==1


pca educationbr townsend householdincome if sample_final==1 & _mi_m==0, factors(1)

**SMOKING**

capture drop SMOKING

capture drop zsmoking
mi passive: egen zsmoking=std(smoking) if sample_final==1

capture drop zetsmoke
mi passive: egen zetsmoke=std(etsmoke) if sample_final==1

capture drop zpackyearssmoke
mi passive: egen zpackyearssmoke=std(packyearssmoke) if sample_final==1



capture drop SMOKING
mi passive: egen SMOKING=rmean(zsmoking zetsmoke zpackyearssmoke)


tabstat SMOKING, statistics(count mean sd)
mi estimate:  mean SMOKING if sample_final==1
mi estimate:  reg SMOKING smoking etsmoke packyearssmoke if sample_final==1

pca  smoking etsmoke packyearssmoke if sample_final==1 & _mi_m==0, factors(1)


**ALCOHOL**

capture drop ALCOHOL

capture drop zalcohol
mi passive: egen zalcohol=std(alcohol) if sample_final==1


capture drop ALCOHOL
mi passive: egen ALCOHOL=rmean(zalcohol)


tabstat ALCOHOL, statistics(count mean sd)
mi estimate:  mean ALCOHOL if sample_final==1
mi estimate:  reg ALCOHOL alcohol if sample_final==1


**PA**
capture drop PA

capture drop zMETmin
mi passive: egen zMETmin=std(METmin) if sample_final==1

capture drop PA
mi passive: egen PA=rmean(zMETmin)



tabstat PA, statistics(count mean sd)
mi estimate:  mean PA if sample_final==1
mi estimate:  reg PA METmin if sample_final==1


**DIET**

capture drop DIET

capture drop zHDI_TOTALSCORE 
mi passive: egen zHDI_TOTALSCORE=std(HDI_TOTALSCORE) if sample_final==1


capture drop DIET
mi passive: gen DIET=zHDI_TOTALSCORE
 


tabstat DIET, statistics(count mean sd)
mi estimate:  mean DIET if sample_final==1


**NUTR**

capture drop NUTR

capture drop zvitamind
mi passive: egen zvitamind=std(vitamind) if sample_final==1


capture drop zrdw
mi passive: egen zrdw=std(rdw) if sample_final==1


capture drop zrdwinv
mi passive: gen zrdwinv=zrdw*-1 if sample_final==1


capture drop NUTR
mi passive: egen NUTR=rmean(zvitamind zrdwinv)


tabstat NUTR, statistics(count mean sd)
mi estimate:  mean NUTR if sample_final==1
mi estimate:  reg NUTR vitamind rdw if sample_final==1

pca vitamind rdw if sample_final==1 & _mi_m==0, factors(1)

**SS**

capture drop SS

capture drop zSS_friendsfamily
mi passive: egen zSS_friendsfamily=std(SS_friendsfamily) if sample_final==1


capture drop zSS_leisuresocial
mi passive: egen zSS_leisuresocial=std(SS_leisuresocial) if sample_final==1

capture drop zSS_abilityconfide
mi passive: egen zSS_abilityconfide=std(SS_abilityconfide) if sample_final==1


capture drop SS
mi passive: egen SS=rmean(zSS_friendsfamily zSS_leisuresocial zSS_abilityconfide)


tabstat SS, statistics(count mean sd)
mi estimate:  mean SS if sample_final==1
mi estimate:  reg SS SS_friendsfamily SS_leisuresocial SS_abilityconfide if sample_final==1

pca SS_friendsfamily SS_leisuresocial SS_abilityconfide if sample_final==1 & _mi_m==0, factors(1)

**HEALTH***************


capture drop HEALTH

capture drop zcomorbid
mi passive: egen zcomorbid=std(comorbid) if sample_final==1


capture drop zbmi
mi passive: egen zbmi=std(bmi) if sample_final==1

capture drop zsrh
mi passive: egen zsrh=std(srh) if sample_final==1

capture drop zallostatic
mi passive: egen zallostatic=std(allostatic) if sample_final==1


capture drop HEALTH
mi passive: egen HEALTH=rmean(zcomorbid zbmi zsrh zallostatic)


tabstat HEALTH, statistics(count mean sd)
mi estimate:  mean HEALTH if sample_final==1
mi estimate:  reg HEALTH comorbid bmi srh allostatic if sample_final==1

pca comorbid bmi srh allostatic if sample_final==1 & _mi_m==0, factors(1)


save, replace


**TERTILE OF AD PGS**
capture drop AD_PGStert
xtile AD_PGStert=AD_PGS if sample_final==1, nq(3)


**NONWHITE VS. WHITE**
capture drop NonWhite
gen NonWhite=.
replace NonWhite=RACE_ETHN
recode NonWhite (0=0) (1=1) (2=1) (3=1)

save finaldata_imputedFINAL, replace


**************************************************************MAIN PART OF THE ANALYSIS: *****************************************


capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE1.smcl"

*********************************************TABLE 1: DESCRIPTIVES, OVERALL AND BY RACE/ETHNICITY: NON-WHITE VS. WHITE ******************************************************************

use finaldata_imputedFINAL, clear



**Overall**
foreach x1 of varlist SEX RACE_ETHN educationbr  smoking AD_PGStert {
	mi estimate: prop `x1' if sample_final==1
}



foreach x2 of varlist AGE  townsend householdsize  householdincome srh packyearssmoke etsmoke alcohol HDI_TOTALSCORE SS_friendsfamily SS_leisuresocial SS_abilityconfide allostatic srhbr METmin vitamind rdw comorbid bmi SES SMOKING ALCOHOL DIET SS PA NUTR HEALTH  TIME_V0V2 FA_mean MD_mean  TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right AD_PGS REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC POORCOGN {
	mi estimate: mean `x2' if sample_final==1
}



**WHITE**
foreach x1 of varlist SEX educationbr  smoking AD_PGStert {
	mi estimate: prop `x1' if sample_final==1 & NonWhite==0
}



foreach x2 of varlist AGE  townsend householdsize  householdincome srh packyearssmoke etsmoke alcohol HDI_TOTALSCORE SS_friendsfamily SS_leisuresocial SS_abilityconfide allostatic srhbr METmin vitamind rdw comorbid bmi SES SMOKING ALCOHOL DIET SS PA NUTR HEALTH  TIME_V0V2 FA_mean MD_mean  TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right AD_PGS REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC POORCOGN {
	mi estimate: mean `x2' if sample_final==1 & NonWhite==0
}


**NON-WHITE**
foreach x1 of varlist SEX RACE_ETHN educationbr  smoking AD_PGStert {
	mi estimate: prop `x1' if sample_final==1 & NonWhite==1
}



foreach x2 of varlist AGE  townsend householdsize  householdincome srh packyearssmoke etsmoke alcohol HDI_TOTALSCORE SS_friendsfamily SS_leisuresocial SS_abilityconfide allostatic srhbr METmin vitamind rdw comorbid bmi SES SMOKING ALCOHOL DIET SS PA NUTR HEALTH TIME_V0V2 FA_mean MD_mean  TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right AD_PGS REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC POORCOGN {
	mi estimate: mean `x2' if sample_final==1 & NonWhite==1
}




***************BY RACE/ETHNICITY: NON-WHITE VS. WHITE***********************
foreach x1 of varlist SEX  educationbr  smoking AD_PGStert {
	mi estimate: mlogit `x1' NonWhite if sample_final==1
}



foreach x2 of varlist AGE  townsend householdsize  householdincome srh packyearssmoke etsmoke alcohol HDI_TOTALSCORE SS_friendsfamily SS_leisuresocial SS_abilityconfide allostatic srhbr METmin vitamind rdw comorbid bmi SES SMOKING ALCOHOL DIET SS PA NUTR HEALTH  TIME_V0V2 FA_mean MD_mean TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right AD_PGS REACTION_TIME PAIRSMATCHING_INC PAIRSMATCHING_TTC POORCOGN {
	mi estimate: reg `x2' NonWhite if sample_final==1
}



save, replace


capture log close


log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE2.smcl"

*********************************************TABLE 2: sMRI volumetric outcomes vs. RACE/ETHNICITY: Non-White vs. White: ANALYSIS A *******************

use finaldata_imputedFINAL, clear

**Main exposures: NonWhite 
**Main outcomes: TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right 
**Main covariates: 
**invmillsMRIINF
**AGE SEX  AD_PGS TIME_V0V2 householdsize 
**ICV: for sub-coritcal volumes
**SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN

**Main effect modifier: AD PGS tertile


**************Non-White vs. White************************************

************Overall*******************

**Model 1**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX   AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1
}


**Model 2**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES  invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES invmillsMRIINF if sample_final==1
}

**Model 3**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR  invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR invmillsMRIINF if sample_final==1
}


**Model 4**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1
}

**Model 5**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1
}


************AD PGS LOWEST TERTILE**********************


**Model 1**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX   AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1 & AD_PGStert==1
}


**Model 2**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES  invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

**Model 3**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR  invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR invmillsMRIINF if sample_final==1 & AD_PGStert==1
}


**Model 4**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

**Model 5**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==1
}



************AD PGS MIDDLE TERTILE**********************


**Model 1**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX   AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1 & AD_PGStert==2
}


**Model 2**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES  invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

**Model 3**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR  invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR invmillsMRIINF if sample_final==1 & AD_PGStert==2
}


**Model 4**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

**Model 5**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==2
}




************AD PGS UPPER TERTILE**********************


**Model 1**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX   AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1 & AD_PGStert==3
}


**Model 2**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES  invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

**Model 3**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR  invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR invmillsMRIINF if sample_final==1 & AD_PGStert==3
}


**Model 4**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

**Model 5**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==3
}


//////////////////////DIFFERENCE BY AD PGS TERTILE//////////////////////////////////



**Model 1**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite##c.AD_PGS AGE SEX   AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1
}


**Model 2**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize SES  invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES invmillsMRIINF if sample_final==1
}

**Model 3**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR  invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR invmillsMRIINF if sample_final==1
}


**Model 4**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH invmillsMRIINF if sample_final==1
}

**Model 5**
foreach y1 of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate:reg `y1' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1
}

foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate:reg `y2' NonWhite##c.AD_PGS AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  SES SMOKING ALCOHOL PA DIET SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1
}



capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE2_ANALYSISAPRIME.smcl"

***************************************ANALYSIS A and A': MODEL 1: Q-VALUES*********************************************************

use finaldata_imputedFINAL, clear

mi extract 0

save finaldata_unimputedFINAL, replace

**ANALYSIS A: **


global tflist ""
global modseq=0
foreach X of var NonWhite {
foreach Y of var TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT  {
global modseq=$modseq+1
tempfile tfcur
parmby "regr `Y' `X'  AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1, beta", label command format(estimate min95 max95 %8.2f p %8.1e) idn($modseq) saving(`"`tfcur'"', replace) flist(tflist)
 }
 }
 
drop _all
append using $tflist
sort idnum command `Y' parmseq
describe
by idnum command:list `Y' parm label estimate min95 max95 p, noobs

save Outputdata_overall_A, replace



**Smile plot**
use Outputdata_overall_A, clear

keep if parmseq==1


sort parm
multproc, pval(p) meth(bonferroni) gpunc(uncp) gpcor(corp) rej(signif)
smileplot7, est(estimate) pval(p) punc(uncp) pcor(corp) ptl(command)  t1(" ")
list if signif, nodisp



//q-value: FDR//

capture drop myqvallargervolumes

qqvalue p, method(simes) qvalue(myqvallargervolumes)
sort myqvallargervolumes

list parm myqvallargervolumes command p  estimate stderr if p<0.05

save, replace

**ANALYSIS Aprime**

**foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
**reg `y2' hpylori_seropos2 AGE SEX  RACE_ETHN AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1
**}


**foreach y2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
**reg `y2' c.IB_tert AGE SEX  RACE_ETHN AD_PGS TIME_V0V2 ICV  invmillsMRIINF if sample_final==1
**}


use finaldata_imputedFINAL, clear

mi extract 0

save finaldata_unimputedFINAL, replace


global tflist ""
global modseq=0
foreach X of var NonWhite {
foreach Y of var Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right  {
global modseq=$modseq+1
tempfile tfcur
parmby "regr `Y' `X'  AGE SEX  AD_PGS TIME_V0V2 householdsize ICV  invmillsMRIINF if sample_final==1, beta", label command format(estimate min95 max95 %8.2f p %8.1e) idn($modseq) saving(`"`tfcur'"', replace) flist(tflist)
 }
 }
 
drop _all
append using $tflist
sort idnum command `Y' parmseq
describe
by idnum command:list `Y' parm label estimate min95 max95 p, noobs

save Outputdata_overall_Aprime, replace



**Smile plot**
use Outputdata_overall_Aprime, clear

keep if parmseq==1


sort parm
multproc, pval(p) meth(bonferroni) gpunc(uncp) gpcor(corp) rej(signif)
smileplot7, est(estimate) pval(p) punc(uncp) pcor(corp) ptl(command)  t1(" ")
list if signif, nodisp



//q-value: FDR//

capture drop myqvallargervolumes

qqvalue p, method(simes) qvalue(myqvallargervolumes)
sort myqvallargervolumes

list parm myqvallargervolumes command p  estimate stderr if p<0.05

save, replace


capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE3.smcl"


*********************************************TABLE 3: NON-WHITE VS. WHITE, VS. FA/MD, OVERALL AND BY AD PGS SCORE TERTILE***********************************************************************

use finaldata_imputedFINAL, clear


**Main exposures: NonWhite 
**Main outcomes: FA_mean MD_mean 
**Main covariates: 
**invmillsMRIINF
**AGE SEX  AD_PGS TIME_V0V2 householdsize 
**ICV: for sub-coritcal volumes
**SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN

**Main effect modifier: AD PGS



************Overall*******************

**Model 1**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1
}


**Model 2**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES invmillsMRIINF if sample_final==1
}


**Model 3**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR invmillsMRIINF if sample_final==1
}


**Model 4**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH invmillsMRIINF if sample_final==1
}

**Model 5**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1
}


************LOWEST AD PGS TERTILE**********************


**Model 1**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1 & AD_PGStert==1
}


**Model 2**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES invmillsMRIINF if sample_final==1 & AD_PGStert==1
}


**Model 3**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR invmillsMRIINF if sample_final==1 & AD_PGStert==1
}


**Model 4**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==1
}

**Model 5**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==1
}


************MIDDLE AD PGS TERTILE*********************************

**Model 1**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1 & AD_PGStert==2
}


**Model 2**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES invmillsMRIINF if sample_final==1 & AD_PGStert==2
}


**Model 3**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR invmillsMRIINF if sample_final==1 & AD_PGStert==2
}


**Model 4**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==2
}

**Model 5**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==2
}


************HIGHEST AD PGS TERTILE*********************************

**Model 1**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1 & AD_PGStert==3
}


**Model 2**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES invmillsMRIINF if sample_final==1 & AD_PGStert==3
}


**Model 3**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR invmillsMRIINF if sample_final==1 & AD_PGStert==3
}


**Model 4**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH invmillsMRIINF if sample_final==1 & AD_PGStert==3
}

**Model 5**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1 & AD_PGStert==3
}




//////////////////////DIFFERENCE BY AD PGS TERTILE//////////////////////////////////

**Model 1**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite##c.AD_PGStert AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1
}


**Model 2**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite##c.AD_PGStert AGE SEX  AD_PGS TIME_V0V2 householdsize SES invmillsMRIINF if sample_final==1
}


**Model 3**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite##c.AD_PGStert AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR invmillsMRIINF if sample_final==1
}


**Model 4**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite##c.AD_PGStert AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH invmillsMRIINF if sample_final==1
}

**Model 5**
foreach y1 of varlist FA_mean MD_mean {
mi estimate:reg `y1' NonWhite##c.AD_PGStert AGE SEX  AD_PGS TIME_V0V2 householdsize SES SMOKING ALCOHOL DIET PA SS NUTR HEALTH POORCOGN invmillsMRIINF if sample_final==1
}



capture log close

log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE3_ANALYSISB.smcl"



**ANALYSIS B: **

use finaldata_imputedFINAL, clear

mi extract 0

save finaldata_unimputedFINAL, replace

**foreach y1 of varlist FA_mean MD_mean {
**reg `y1' infectionburden AGE SEX  RACE_ETHN AD_PGS TIME_V0V2  invmillsMRIINF if sample_final==1
**}


global tflist ""
global modseq=0
foreach X of var NonWhite {
foreach Y of var FA_mean MD_mean  {
global modseq=$modseq+1
tempfile tfcur
parmby "regr `Y' `X'  AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1, beta", label command format(estimate min95 max95 %8.2f p %8.1e) idn($modseq) saving(`"`tfcur'"', replace) flist(tflist)
 }
 }
 
drop _all
append using $tflist
sort idnum command `Y' parmseq
describe
by idnum command:list `Y' parm label estimate min95 max95 p, noobs

save Outputdata_overall_B, replace



**Smile plot**
use Outputdata_overall_B, clear

keep if parmseq==1


sort parm
multproc, pval(p) meth(bonferroni) gpunc(uncp) gpcor(corp) rej(signif)
smileplot7, est(estimate) pval(p) punc(uncp) pcor(corp) ptl(command)  t1(" ")
list if signif, nodisp



//q-value: FDR//

capture drop myqvallargervolumes

qqvalue p, method(simes) qvalue(myqvallargervolumes)
sort myqvallargervolumes

list parm myqvallargervolumes command p  estimate stderr if p<0.05


save, replace

capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLES6_ANALYSISBprime.smcl"



**ANALYSIS Bprime: **

use finaldata_imputedFINAL, clear

mi extract 0

save finaldata_unimputedFINAL, replace

**foreach y1 of varlist zFA_* zMD_* {
**reg `y1' infectionburdentert AGE SEX  RACE_ETHN AD_PGS TIME_V0V2  invmillsMRIINF if sample_final==1
**}



global tflist ""
global modseq=0
foreach X of var NonWhite {
foreach Y of var zFA_*   {
global modseq=$modseq+1
tempfile tfcur
parmby "regr `Y' `X'  AGE SEX  AD_PGS TIME_V0V2 householdsize  invmillsMRIINF if sample_final==1, beta", label command format(estimate min95 max95 %8.2f p %8.1e) idn($modseq) saving(`"`tfcur'"', replace) flist(tflist)
 }
 }
 
drop _all
append using $tflist
sort idnum command `Y' parmseq
describe
by idnum command:list `Y' parm label estimate min95 max95 p, noobs

save Outputdata_overall_Bprime, replace

**Smile plot**
use Outputdata_overall_Bprime, clear

keep if parmseq==1


sort parm
multproc, pval(p) meth(bonferroni) gpunc(uncp) gpcor(corp) rej(signif)
smileplot7, est(estimate) pval(p) punc(uncp) pcor(corp) ptl(command)  t1(" ")
list if signif, nodisp



//q-value: FDR//

capture drop myqvallargervolumes

qqvalue p, method(simes) qvalue(myqvallargervolumes)
sort myqvallargervolumes

list parm myqvallargervolumes command p  estimate stderr if p<0.05

save, replace



capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE4.smcl"

*********************************************TABLE 4-6, FIGURES 2-4: SEM MODELS: NONWHITE-->SES-->LIFESTYLE-->HEALTH-->COGN-->SMRI/DMRI OUTCOMES***********************
use finaldata_imputedFINAL, clear

**TOTAL EFFECT OF NON-WHITE VS. WHITE FOR EACH OUTCOME***********


//No MEDIATOR: ONLY EXOGENOUS VARIABLES//

foreach MRI1  of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate, cmdok: sem (TIME_V0V2 -> `MRI1')  (NonWhite -> `MRI1') (AGE -> `MRI1')  (SEX -> `MRI1') (AD_PGS -> `MRI1')  (householdsize -> `MRI1')   if sample_final==1, nocapslatent  method(ml) 
  
}




foreach MRI2 of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
	
mi estimate, cmdok: sem (TIME_V0V2 -> `MRI2')  (NonWhite -> `MRI2') (AGE -> `MRI2')  (SEX -> `MRI2')  (householdsize -> `MRI2') (AD_PGS -> `MRI1') (ICV -> `MRI2')   if sample_final==1, nocapslatent  method(ml) 

}


foreach MRI3 of varlist  FA_mean MD_mean {
	
mi estimate, cmdok: sem (TIME_V0V2 -> `MRI3')  (NonWhite -> `MRI3') (AGE -> `MRI3')  (SEX -> `MRI3')  (householdsize -> `MRI3') (AD_PGS -> `MRI1')   if sample_final==1, nocapslatent  method(ml) 

}



capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE5.smcl"

use finaldata_imputedFINAL, clear

***FULL SEM MODEL FOR EACH OUTCOME***********************

foreach MRI1  of varlist TOTALBRAIN ICV WM GM LnWMHpctICV FRONTAL_GM_LEFT FRONTAL_GM_RIGHT {
mi estimate, cmdok: sem (TIME_V0V2 -> `MRI1', nocapslatent)  (NonWhite -> `MRI1') (AGE -> `MRI1')  (SEX -> `MRI1')  (householdsize -> `MRI1') (AD_PGS -> `MRI1') (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> `MRI1') (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> `MRI1')  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> `MRI1')  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> `MRI1')  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> `MRI1')  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> `MRI1')   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> `MRI1')   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> `MRI1')   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> `MRI1') if sample_final==1, nocapslatent method(ml)
}


****************************TOTALBRAIN**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> TOTALBRAIN, nocapslatent)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> TOTALBRAIN, nocapslatent)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> TOTALBRAIN, nocapslatent)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> TOTALBRAIN, nocapslatent)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> TOTALBRAIN, nocapslatent)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************ICV**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> ICV, nocapslatent)  (NonWhite -> ICV) (AGE -> ICV)  (SEX -> ICV)  (householdsize -> ICV) (AD_PGS -> ICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> ICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> ICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> ICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> ICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> ICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> ICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> ICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> ICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> ICV) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> ICV, nocapslatent)  (NonWhite -> ICV) (AGE -> ICV)  (SEX -> ICV)  (householdsize -> ICV) (AD_PGS -> ICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> ICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> ICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> ICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> ICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> ICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> ICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> ICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> ICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> ICV) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> ICV, nocapslatent)  (NonWhite -> ICV) (AGE -> ICV)  (SEX -> ICV)  (householdsize -> ICV) (AD_PGS -> ICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> ICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> ICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> ICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> ICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> ICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> ICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> ICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> ICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> ICV) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> ICV, nocapslatent)  (NonWhite -> ICV) (AGE -> ICV)  (SEX -> ICV)  (householdsize -> ICV) (AD_PGS -> ICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> ICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> ICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> ICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> ICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> ICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> ICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> ICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> ICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> ICV) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> ICV, nocapslatent)  (NonWhite -> ICV) (AGE -> ICV)  (SEX -> ICV)  (householdsize -> ICV) (AD_PGS -> ICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> ICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> ICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> ICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> ICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> ICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> ICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> ICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> ICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> ICV) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects


****************************WM**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> WM, nocapslatent)  (NonWhite -> WM) (AGE -> WM)  (SEX -> WM)  (householdsize -> WM) (AD_PGS -> WM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> WM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> WM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> WM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> WM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> WM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> WM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> WM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> WM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> WM) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> WM, nocapslatent)  (NonWhite -> WM) (AGE -> WM)  (SEX -> WM)  (householdsize -> WM) (AD_PGS -> WM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> WM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> WM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> WM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> WM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> WM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> WM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> WM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> WM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> WM) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> WM, nocapslatent)  (NonWhite -> WM) (AGE -> WM)  (SEX -> WM)  (householdsize -> WM) (AD_PGS -> WM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> WM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> WM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> WM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> WM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> WM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> WM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> WM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> WM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> WM) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> WM, nocapslatent)  (NonWhite -> WM) (AGE -> WM)  (SEX -> WM)  (householdsize -> WM) (AD_PGS -> WM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> WM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> WM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> WM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> WM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> WM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> WM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> WM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> WM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> WM) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> WM, nocapslatent)  (NonWhite -> WM) (AGE -> WM)  (SEX -> WM)  (householdsize -> WM) (AD_PGS -> WM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> WM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> WM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> WM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> WM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> WM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> WM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> WM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> WM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> WM) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************GM**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> GM, nocapslatent)  (NonWhite -> GM) (AGE -> GM)  (SEX -> GM)  (householdsize -> GM) (AD_PGS -> GM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> GM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> GM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> GM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> GM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> GM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> GM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> GM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> GM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> GM) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> GM, nocapslatent)  (NonWhite -> GM) (AGE -> GM)  (SEX -> GM)  (householdsize -> GM) (AD_PGS -> GM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> GM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> GM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> GM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> GM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> GM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> GM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> GM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> GM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> GM) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> GM, nocapslatent)  (NonWhite -> GM) (AGE -> GM)  (SEX -> GM)  (householdsize -> GM) (AD_PGS -> GM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> GM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> GM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> GM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> GM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> GM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> GM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> GM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> GM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> GM) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> GM, nocapslatent)  (NonWhite -> GM) (AGE -> GM)  (SEX -> GM)  (householdsize -> GM) (AD_PGS -> GM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> GM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> GM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> GM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> GM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> GM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> GM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> GM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> GM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> GM) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> GM, nocapslatent)  (NonWhite -> GM) (AGE -> GM)  (SEX -> GM)  (householdsize -> GM) (AD_PGS -> GM) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> GM) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> GM)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> GM)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> GM)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> GM)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> GM)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> GM)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> GM)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> GM) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects



****************************LnWMHpctICV**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> LnWMHpctICV, nocapslatent)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> LnWMHpctICV, nocapslatent)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> LnWMHpctICV, nocapslatent)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> LnWMHpctICV, nocapslatent)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> LnWMHpctICV, nocapslatent)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************FRONTAL_GM_LEFT**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> FRONTAL_GM_LEFT, nocapslatent)  (NonWhite -> FRONTAL_GM_LEFT) (AGE -> FRONTAL_GM_LEFT)  (SEX -> FRONTAL_GM_LEFT)  (householdsize -> FRONTAL_GM_LEFT) (AD_PGS -> FRONTAL_GM_LEFT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_LEFT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_LEFT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_LEFT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_LEFT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_LEFT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_LEFT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_LEFT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_LEFT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_LEFT) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> FRONTAL_GM_LEFT, nocapslatent)  (NonWhite -> FRONTAL_GM_LEFT) (AGE -> FRONTAL_GM_LEFT)  (SEX -> FRONTAL_GM_LEFT)  (householdsize -> FRONTAL_GM_LEFT) (AD_PGS -> FRONTAL_GM_LEFT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_LEFT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_LEFT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_LEFT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_LEFT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_LEFT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_LEFT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_LEFT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_LEFT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_LEFT) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> FRONTAL_GM_LEFT, nocapslatent)  (NonWhite -> FRONTAL_GM_LEFT) (AGE -> FRONTAL_GM_LEFT)  (SEX -> FRONTAL_GM_LEFT)  (householdsize -> FRONTAL_GM_LEFT) (AD_PGS -> FRONTAL_GM_LEFT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_LEFT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_LEFT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_LEFT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_LEFT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_LEFT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_LEFT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_LEFT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_LEFT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_LEFT) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> FRONTAL_GM_LEFT, nocapslatent)  (NonWhite -> FRONTAL_GM_LEFT) (AGE -> FRONTAL_GM_LEFT)  (SEX -> FRONTAL_GM_LEFT)  (householdsize -> FRONTAL_GM_LEFT) (AD_PGS -> FRONTAL_GM_LEFT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_LEFT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_LEFT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_LEFT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_LEFT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_LEFT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_LEFT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_LEFT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_LEFT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_LEFT) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> FRONTAL_GM_LEFT, nocapslatent)  (NonWhite -> FRONTAL_GM_LEFT) (AGE -> FRONTAL_GM_LEFT)  (SEX -> FRONTAL_GM_LEFT)  (householdsize -> FRONTAL_GM_LEFT) (AD_PGS -> FRONTAL_GM_LEFT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_LEFT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_LEFT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_LEFT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_LEFT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_LEFT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_LEFT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_LEFT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_LEFT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_LEFT) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************FRONTAL_GM_RIGHT**********************************
**first imputation**

mi xeq 1: sem (TIME_V0V2 -> FRONTAL_GM_RIGHT, nocapslatent)  (NonWhite -> FRONTAL_GM_RIGHT) (AGE -> FRONTAL_GM_RIGHT)  (SEX -> FRONTAL_GM_RIGHT)  (householdsize -> FRONTAL_GM_RIGHT) (AD_PGS -> FRONTAL_GM_RIGHT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_RIGHT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_RIGHT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_RIGHT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_RIGHT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_RIGHT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_RIGHT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_RIGHT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_RIGHT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_RIGHT) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (TIME_V0V2 -> FRONTAL_GM_RIGHT, nocapslatent)  (NonWhite -> FRONTAL_GM_RIGHT) (AGE -> FRONTAL_GM_RIGHT)  (SEX -> FRONTAL_GM_RIGHT)  (householdsize -> FRONTAL_GM_RIGHT) (AD_PGS -> FRONTAL_GM_RIGHT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_RIGHT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_RIGHT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_RIGHT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_RIGHT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_RIGHT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_RIGHT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_RIGHT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_RIGHT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_RIGHT) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> FRONTAL_GM_RIGHT, nocapslatent)  (NonWhite -> FRONTAL_GM_RIGHT) (AGE -> FRONTAL_GM_RIGHT)  (SEX -> FRONTAL_GM_RIGHT)  (householdsize -> FRONTAL_GM_RIGHT) (AD_PGS -> FRONTAL_GM_RIGHT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_RIGHT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_RIGHT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_RIGHT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_RIGHT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_RIGHT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_RIGHT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_RIGHT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_RIGHT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_RIGHT) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> FRONTAL_GM_RIGHT, nocapslatent)  (NonWhite -> FRONTAL_GM_RIGHT) (AGE -> FRONTAL_GM_RIGHT)  (SEX -> FRONTAL_GM_RIGHT)  (householdsize -> FRONTAL_GM_RIGHT) (AD_PGS -> FRONTAL_GM_RIGHT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_RIGHT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_RIGHT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_RIGHT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_RIGHT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_RIGHT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_RIGHT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_RIGHT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_RIGHT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_RIGHT) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> FRONTAL_GM_RIGHT, nocapslatent)  (NonWhite -> FRONTAL_GM_RIGHT) (AGE -> FRONTAL_GM_RIGHT)  (SEX -> FRONTAL_GM_RIGHT)  (householdsize -> FRONTAL_GM_RIGHT) (AD_PGS -> FRONTAL_GM_RIGHT) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> FRONTAL_GM_RIGHT) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> FRONTAL_GM_RIGHT)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> FRONTAL_GM_RIGHT)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> FRONTAL_GM_RIGHT)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> FRONTAL_GM_RIGHT)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> FRONTAL_GM_RIGHT)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> FRONTAL_GM_RIGHT)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> FRONTAL_GM_RIGHT)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> FRONTAL_GM_RIGHT) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects



capture log close
capture log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE6.smcl"

use finaldata_imputedFINAL, clear


*******************************************SUB-CORTICAL STRUCTURES*************************************************************

foreach MRI2  of varlist Accumbens_Left Accumbens_Right Amygdala_Left Amygdala_Right Caudate_Left Caudate_Right Hippocampus_Left Hippocampus_Right Pallidum_Left Pallidum_Right Putamen_Left Putamen_Right Thalamus_Left Thalamus_Right {
mi estimate, cmdok: sem (ICV -> `MRI2')(TIME_V0V2 -> `MRI2')  (NonWhite -> `MRI2') (AGE -> `MRI2')  (SEX -> `MRI2')  (householdsize -> `MRI2') (AD_PGS -> `MRI2') (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> `MRI2') (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> `MRI2')  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> `MRI2')  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> `MRI2')  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> `MRI2')  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> `MRI2')   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> `MRI2')   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> `MRI2')   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> `MRI2') if sample_final==1, nocapslatent method(ml)
}

****************************Accumbens_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Accumbens_Left)(TIME_V0V2 -> Accumbens_Left)  (NonWhite -> Accumbens_Left) (AGE -> Accumbens_Left)  (SEX -> Accumbens_Left)  (householdsize -> Accumbens_Left) (AD_PGS -> Accumbens_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Accumbens_Left)(TIME_V0V2 -> Accumbens_Left)  (NonWhite -> Accumbens_Left) (AGE -> Accumbens_Left)  (SEX -> Accumbens_Left)  (householdsize -> Accumbens_Left) (AD_PGS -> Accumbens_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Accumbens_Left)(TIME_V0V2 -> Accumbens_Left)  (NonWhite -> Accumbens_Left) (AGE -> Accumbens_Left)  (SEX -> Accumbens_Left)  (householdsize -> Accumbens_Left) (AD_PGS -> Accumbens_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Accumbens_Left)(TIME_V0V2 -> Accumbens_Left)  (NonWhite -> Accumbens_Left) (AGE -> Accumbens_Left)  (SEX -> Accumbens_Left)  (householdsize -> Accumbens_Left) (AD_PGS -> Accumbens_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Accumbens_Left)(TIME_V0V2 -> Accumbens_Left)  (NonWhite -> Accumbens_Left) (AGE -> Accumbens_Left)  (SEX -> Accumbens_Left)  (householdsize -> Accumbens_Left) (AD_PGS -> Accumbens_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Accumbens_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Accumbens_Right)(TIME_V0V2 -> Accumbens_Right)  (NonWhite -> Accumbens_Right) (AGE -> Accumbens_Right)  (SEX -> Accumbens_Right)  (householdsize -> Accumbens_Right) (AD_PGS -> Accumbens_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Accumbens_Right)(TIME_V0V2 -> Accumbens_Right)  (NonWhite -> Accumbens_Right) (AGE -> Accumbens_Right)  (SEX -> Accumbens_Right)  (householdsize -> Accumbens_Right) (AD_PGS -> Accumbens_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Accumbens_Right)(TIME_V0V2 -> Accumbens_Right)  (NonWhite -> Accumbens_Right) (AGE -> Accumbens_Right)  (SEX -> Accumbens_Right)  (householdsize -> Accumbens_Right) (AD_PGS -> Accumbens_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Accumbens_Right)(TIME_V0V2 -> Accumbens_Right)  (NonWhite -> Accumbens_Right) (AGE -> Accumbens_Right)  (SEX -> Accumbens_Right)  (householdsize -> Accumbens_Right) (AD_PGS -> Accumbens_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Accumbens_Right)(TIME_V0V2 -> Accumbens_Right)  (NonWhite -> Accumbens_Right) (AGE -> Accumbens_Right)  (SEX -> Accumbens_Right)  (householdsize -> Accumbens_Right) (AD_PGS -> Accumbens_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Accumbens_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Accumbens_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Accumbens_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Accumbens_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Accumbens_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Accumbens_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Accumbens_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Accumbens_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Accumbens_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects



****************************Amygdala_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Amygdala_Left)(TIME_V0V2 -> Amygdala_Left)  (NonWhite -> Amygdala_Left) (AGE -> Amygdala_Left)  (SEX -> Amygdala_Left)  (householdsize -> Amygdala_Left) (AD_PGS -> Amygdala_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Amygdala_Left)(TIME_V0V2 -> Amygdala_Left)  (NonWhite -> Amygdala_Left) (AGE -> Amygdala_Left)  (SEX -> Amygdala_Left)  (householdsize -> Amygdala_Left) (AD_PGS -> Amygdala_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Amygdala_Left)(TIME_V0V2 -> Amygdala_Left)  (NonWhite -> Amygdala_Left) (AGE -> Amygdala_Left)  (SEX -> Amygdala_Left)  (householdsize -> Amygdala_Left) (AD_PGS -> Amygdala_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Amygdala_Left)(TIME_V0V2 -> Amygdala_Left)  (NonWhite -> Amygdala_Left) (AGE -> Amygdala_Left)  (SEX -> Amygdala_Left)  (householdsize -> Amygdala_Left) (AD_PGS -> Amygdala_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Amygdala_Left)(TIME_V0V2 -> Amygdala_Left)  (NonWhite -> Amygdala_Left) (AGE -> Amygdala_Left)  (SEX -> Amygdala_Left)  (householdsize -> Amygdala_Left) (AD_PGS -> Amygdala_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Amygdala_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Amygdala_Right)(TIME_V0V2 -> Amygdala_Right)  (NonWhite -> Amygdala_Right) (AGE -> Amygdala_Right)  (SEX -> Amygdala_Right)  (householdsize -> Amygdala_Right) (AD_PGS -> Amygdala_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Amygdala_Right)(TIME_V0V2 -> Amygdala_Right)  (NonWhite -> Amygdala_Right) (AGE -> Amygdala_Right)  (SEX -> Amygdala_Right)  (householdsize -> Amygdala_Right) (AD_PGS -> Amygdala_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Amygdala_Right)(TIME_V0V2 -> Amygdala_Right)  (NonWhite -> Amygdala_Right) (AGE -> Amygdala_Right)  (SEX -> Amygdala_Right)  (householdsize -> Amygdala_Right) (AD_PGS -> Amygdala_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Amygdala_Right)(TIME_V0V2 -> Amygdala_Right)  (NonWhite -> Amygdala_Right) (AGE -> Amygdala_Right)  (SEX -> Amygdala_Right)  (householdsize -> Amygdala_Right) (AD_PGS -> Amygdala_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Amygdala_Right)(TIME_V0V2 -> Amygdala_Right)  (NonWhite -> Amygdala_Right) (AGE -> Amygdala_Right)  (SEX -> Amygdala_Right)  (householdsize -> Amygdala_Right) (AD_PGS -> Amygdala_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Amygdala_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Amygdala_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Amygdala_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Amygdala_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Amygdala_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Amygdala_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Amygdala_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Amygdala_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Amygdala_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects


****************************Caudate_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Caudate_Left)(TIME_V0V2 -> Caudate_Left)  (NonWhite -> Caudate_Left) (AGE -> Caudate_Left)  (SEX -> Caudate_Left)  (householdsize -> Caudate_Left) (AD_PGS -> Caudate_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Caudate_Left)(TIME_V0V2 -> Caudate_Left)  (NonWhite -> Caudate_Left) (AGE -> Caudate_Left)  (SEX -> Caudate_Left)  (householdsize -> Caudate_Left) (AD_PGS -> Caudate_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Caudate_Left)(TIME_V0V2 -> Caudate_Left)  (NonWhite -> Caudate_Left) (AGE -> Caudate_Left)  (SEX -> Caudate_Left)  (householdsize -> Caudate_Left) (AD_PGS -> Caudate_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Caudate_Left)(TIME_V0V2 -> Caudate_Left)  (NonWhite -> Caudate_Left) (AGE -> Caudate_Left)  (SEX -> Caudate_Left)  (householdsize -> Caudate_Left) (AD_PGS -> Caudate_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Caudate_Left)(TIME_V0V2 -> Caudate_Left)  (NonWhite -> Caudate_Left) (AGE -> Caudate_Left)  (SEX -> Caudate_Left)  (householdsize -> Caudate_Left) (AD_PGS -> Caudate_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Caudate_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Caudate_Right)(TIME_V0V2 -> Caudate_Right)  (NonWhite -> Caudate_Right) (AGE -> Caudate_Right)  (SEX -> Caudate_Right)  (householdsize -> Caudate_Right) (AD_PGS -> Caudate_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Caudate_Right)(TIME_V0V2 -> Caudate_Right)  (NonWhite -> Caudate_Right) (AGE -> Caudate_Right)  (SEX -> Caudate_Right)  (householdsize -> Caudate_Right) (AD_PGS -> Caudate_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Caudate_Right)(TIME_V0V2 -> Caudate_Right)  (NonWhite -> Caudate_Right) (AGE -> Caudate_Right)  (SEX -> Caudate_Right)  (householdsize -> Caudate_Right) (AD_PGS -> Caudate_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Caudate_Right)(TIME_V0V2 -> Caudate_Right)  (NonWhite -> Caudate_Right) (AGE -> Caudate_Right)  (SEX -> Caudate_Right)  (householdsize -> Caudate_Right) (AD_PGS -> Caudate_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Caudate_Right)(TIME_V0V2 -> Caudate_Right)  (NonWhite -> Caudate_Right) (AGE -> Caudate_Right)  (SEX -> Caudate_Right)  (householdsize -> Caudate_Right) (AD_PGS -> Caudate_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Caudate_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Caudate_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Caudate_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Caudate_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Caudate_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Caudate_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Caudate_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Caudate_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Caudate_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects




****************************Hippocampus_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Hippocampus_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects





****************************Pallidum_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Pallidum_Left)(TIME_V0V2 -> Pallidum_Left)  (NonWhite -> Pallidum_Left) (AGE -> Pallidum_Left)  (SEX -> Pallidum_Left)  (householdsize -> Pallidum_Left) (AD_PGS -> Pallidum_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Pallidum_Left)(TIME_V0V2 -> Pallidum_Left)  (NonWhite -> Pallidum_Left) (AGE -> Pallidum_Left)  (SEX -> Pallidum_Left)  (householdsize -> Pallidum_Left) (AD_PGS -> Pallidum_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Pallidum_Left)(TIME_V0V2 -> Pallidum_Left)  (NonWhite -> Pallidum_Left) (AGE -> Pallidum_Left)  (SEX -> Pallidum_Left)  (householdsize -> Pallidum_Left) (AD_PGS -> Pallidum_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Pallidum_Left)(TIME_V0V2 -> Pallidum_Left)  (NonWhite -> Pallidum_Left) (AGE -> Pallidum_Left)  (SEX -> Pallidum_Left)  (householdsize -> Pallidum_Left) (AD_PGS -> Pallidum_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Pallidum_Left)(TIME_V0V2 -> Pallidum_Left)  (NonWhite -> Pallidum_Left) (AGE -> Pallidum_Left)  (SEX -> Pallidum_Left)  (householdsize -> Pallidum_Left) (AD_PGS -> Pallidum_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Pallidum_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Pallidum_Right)(TIME_V0V2 -> Pallidum_Right)  (NonWhite -> Pallidum_Right) (AGE -> Pallidum_Right)  (SEX -> Pallidum_Right)  (householdsize -> Pallidum_Right) (AD_PGS -> Pallidum_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Pallidum_Right)(TIME_V0V2 -> Pallidum_Right)  (NonWhite -> Pallidum_Right) (AGE -> Pallidum_Right)  (SEX -> Pallidum_Right)  (householdsize -> Pallidum_Right) (AD_PGS -> Pallidum_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Pallidum_Right)(TIME_V0V2 -> Pallidum_Right)  (NonWhite -> Pallidum_Right) (AGE -> Pallidum_Right)  (SEX -> Pallidum_Right)  (householdsize -> Pallidum_Right) (AD_PGS -> Pallidum_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Pallidum_Right)(TIME_V0V2 -> Pallidum_Right)  (NonWhite -> Pallidum_Right) (AGE -> Pallidum_Right)  (SEX -> Pallidum_Right)  (householdsize -> Pallidum_Right) (AD_PGS -> Pallidum_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Pallidum_Right)(TIME_V0V2 -> Pallidum_Right)  (NonWhite -> Pallidum_Right) (AGE -> Pallidum_Right)  (SEX -> Pallidum_Right)  (householdsize -> Pallidum_Right) (AD_PGS -> Pallidum_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Pallidum_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Pallidum_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Pallidum_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Pallidum_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Pallidum_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Pallidum_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Pallidum_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Pallidum_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Pallidum_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects




****************************Putamen_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Putamen_Left)(TIME_V0V2 -> Putamen_Left)  (NonWhite -> Putamen_Left) (AGE -> Putamen_Left)  (SEX -> Putamen_Left)  (householdsize -> Putamen_Left) (AD_PGS -> Putamen_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Putamen_Left)(TIME_V0V2 -> Putamen_Left)  (NonWhite -> Putamen_Left) (AGE -> Putamen_Left)  (SEX -> Putamen_Left)  (householdsize -> Putamen_Left) (AD_PGS -> Putamen_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Putamen_Left)(TIME_V0V2 -> Putamen_Left)  (NonWhite -> Putamen_Left) (AGE -> Putamen_Left)  (SEX -> Putamen_Left)  (householdsize -> Putamen_Left) (AD_PGS -> Putamen_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Putamen_Left)(TIME_V0V2 -> Putamen_Left)  (NonWhite -> Putamen_Left) (AGE -> Putamen_Left)  (SEX -> Putamen_Left)  (householdsize -> Putamen_Left) (AD_PGS -> Putamen_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Putamen_Left)(TIME_V0V2 -> Putamen_Left)  (NonWhite -> Putamen_Left) (AGE -> Putamen_Left)  (SEX -> Putamen_Left)  (householdsize -> Putamen_Left) (AD_PGS -> Putamen_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Putamen_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Putamen_Right)(TIME_V0V2 -> Putamen_Right)  (NonWhite -> Putamen_Right) (AGE -> Putamen_Right)  (SEX -> Putamen_Right)  (householdsize -> Putamen_Right) (AD_PGS -> Putamen_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Putamen_Right)(TIME_V0V2 -> Putamen_Right)  (NonWhite -> Putamen_Right) (AGE -> Putamen_Right)  (SEX -> Putamen_Right)  (householdsize -> Putamen_Right) (AD_PGS -> Putamen_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Putamen_Right)(TIME_V0V2 -> Putamen_Right)  (NonWhite -> Putamen_Right) (AGE -> Putamen_Right)  (SEX -> Putamen_Right)  (householdsize -> Putamen_Right) (AD_PGS -> Putamen_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Putamen_Right)(TIME_V0V2 -> Putamen_Right)  (NonWhite -> Putamen_Right) (AGE -> Putamen_Right)  (SEX -> Putamen_Right)  (householdsize -> Putamen_Right) (AD_PGS -> Putamen_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Putamen_Right)(TIME_V0V2 -> Putamen_Right)  (NonWhite -> Putamen_Right) (AGE -> Putamen_Right)  (SEX -> Putamen_Right)  (householdsize -> Putamen_Right) (AD_PGS -> Putamen_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Putamen_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Putamen_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Putamen_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Putamen_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Putamen_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Putamen_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Putamen_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Putamen_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Putamen_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects





****************************Thalamus_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Thalamus_Left)(TIME_V0V2 -> Thalamus_Left)  (NonWhite -> Thalamus_Left) (AGE -> Thalamus_Left)  (SEX -> Thalamus_Left)  (householdsize -> Thalamus_Left) (AD_PGS -> Thalamus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Thalamus_Left)(TIME_V0V2 -> Thalamus_Left)  (NonWhite -> Thalamus_Left) (AGE -> Thalamus_Left)  (SEX -> Thalamus_Left)  (householdsize -> Thalamus_Left) (AD_PGS -> Thalamus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Thalamus_Left)(TIME_V0V2 -> Thalamus_Left)  (NonWhite -> Thalamus_Left) (AGE -> Thalamus_Left)  (SEX -> Thalamus_Left)  (householdsize -> Thalamus_Left) (AD_PGS -> Thalamus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Thalamus_Left)(TIME_V0V2 -> Thalamus_Left)  (NonWhite -> Thalamus_Left) (AGE -> Thalamus_Left)  (SEX -> Thalamus_Left)  (householdsize -> Thalamus_Left) (AD_PGS -> Thalamus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Thalamus_Left)(TIME_V0V2 -> Thalamus_Left)  (NonWhite -> Thalamus_Left) (AGE -> Thalamus_Left)  (SEX -> Thalamus_Left)  (householdsize -> Thalamus_Left) (AD_PGS -> Thalamus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

****************************Thalamus_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Thalamus_Right)(TIME_V0V2 -> Thalamus_Right)  (NonWhite -> Thalamus_Right) (AGE -> Thalamus_Right)  (SEX -> Thalamus_Right)  (householdsize -> Thalamus_Right) (AD_PGS -> Thalamus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 1: estat teffects


**second imputation**

mi xeq 2: sem (ICV -> Thalamus_Right)(TIME_V0V2 -> Thalamus_Right)  (NonWhite -> Thalamus_Right) (AGE -> Thalamus_Right)  (SEX -> Thalamus_Right)  (householdsize -> Thalamus_Right) (AD_PGS -> Thalamus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 2: estat teffects



**third imputation**

mi xeq 3: sem (ICV -> Thalamus_Right)(TIME_V0V2 -> Thalamus_Right)  (NonWhite -> Thalamus_Right) (AGE -> Thalamus_Right)  (SEX -> Thalamus_Right)  (householdsize -> Thalamus_Right) (AD_PGS -> Thalamus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects



**Fourth imputation**

mi xeq 4: sem (ICV -> Thalamus_Right)(TIME_V0V2 -> Thalamus_Right)  (NonWhite -> Thalamus_Right) (AGE -> Thalamus_Right)  (SEX -> Thalamus_Right)  (householdsize -> Thalamus_Right) (AD_PGS -> Thalamus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects



**Fifth imputation**

mi xeq 5: sem (ICV -> Thalamus_Right)(TIME_V0V2 -> Thalamus_Right)  (NonWhite -> Thalamus_Right) (AGE -> Thalamus_Right)  (SEX -> Thalamus_Right)  (householdsize -> Thalamus_Right) (AD_PGS -> Thalamus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Thalamus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Thalamus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Thalamus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Thalamus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Thalamus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Thalamus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Thalamus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Thalamus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Thalamus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects


capture log close


*********GENERATE PATTERNS OF TEFFECTS FOR EACH GROUP OF OUTCOMES AND FOR RACE/ETHNICITY AND SES: FSLEYES******************************


***************************************SUBCORTICAL VOLUMES****************************************************


********Group 1: Non-significant total effect, direct and indirect effects.

*1.1. For race: None
*1.2. for SES: None 

 
********Group 2: Non-significant total effect, significant indirect effect, non-significant direct effect. 


*2.1. For race: Left amygdala, Left caudate, Right caudate, Left Hippocampus, Right Hippocampus.
*2.2. for SES: None. 



********Group 3: Significant total effect, significant indirect effect, non-significant direct effect. 


*3.1. For race: Left accumbens, Right amygdala, Left putamen, 
*3.2. for SES: None.


*********Group 4: Significant total effect, non-significant indirect effect, significant direct effects.
*4.1 for race: none. 
*4.2. for SES: none. 


********Group 5: Significant total effect, significant indirect effect, significant direct effect. 

 
*5.1. For race:  Right accumbens, Right pallidum, Right putamen, Left Thalamus, Right Thalamus
*5.2. for SES: Left accumbens, Right accumbens, Left caudate, Right caudate, Left pallidum,  Right pallidum, Left Hippocampus, Right hippocampus, Left putamen, Right putamen, Left Thalamus, Right Thalamus.  

**********Group 6: Non-significant total effect, significant indirect effect, significant direct effect. 
*6.1 for race: Left pallidum
*6.2. for SES: None. 


***************************************FA*************************************************************


********Group 1: Non-significant total effect, direct and indirect effects.

*1.1. For race: 
*1.2. for SES:  

 
********Group 2: Non-significant total effect, significant indirect effect, non-significant direct effect. 


*2.1. For race: 
*2.2. for SES:  



********Group 3: Significant total effect, significant indirect effect, non-significant direct effect. 


*3.1. For race:  
*3.2. for SES: 


*********Group 4: Significant total effect, non-significant indirect effect, significant direct effects.
*4.1 for race:  
*4.2. for SES:  


********Group 5: Significant total effect, significant indirect effect, significant direct effect. 

 
*5.1. For race:  
*5.2. for SES:   



*****************PLOT SEM MODEL FOR REGIONS WHERE TOTAL EFFECT IS SIGNIFICANT AND INDIRECT EFFECT IS SIGNIFICANT FOR RACE ***************************

******and test 6 indirect effects***********************



capture log close
log using "E:UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLE9.smcl"



************************************************************SMRI ANALYSIS: SIX INDIRECT EFFECTS**********************************************

****************************INDIRECT EFFECTS FOR SELECTED ROIs******************************************************


use finaldata_imputedFINAL, clear



**********************************************************************TOTALBRAIN**********************************


**first imputation**

mi xeq 1: sem (TIME_V0V2 -> TOTALBRAIN)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)




**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 1: nlcom _b[TOTALBRAIN:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 1:nlcom _b[TOTALBRAIN:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]





**second imputation**

mi xeq 2: sem (TIME_V0V2 -> TOTALBRAIN)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 2: nlcom _b[TOTALBRAIN:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 2:nlcom _b[TOTALBRAIN:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> TOTALBRAIN)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 3: nlcom _b[TOTALBRAIN:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 3:nlcom _b[TOTALBRAIN:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]


**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> TOTALBRAIN)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 4: nlcom _b[TOTALBRAIN:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 4:nlcom _b[TOTALBRAIN:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> TOTALBRAIN)  (NonWhite -> TOTALBRAIN) (AGE -> TOTALBRAIN)  (SEX -> TOTALBRAIN)  (householdsize -> TOTALBRAIN) (AD_PGS -> TOTALBRAIN) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> TOTALBRAIN) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> TOTALBRAIN)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> TOTALBRAIN)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> TOTALBRAIN)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> TOTALBRAIN)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> TOTALBRAIN)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> TOTALBRAIN)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> TOTALBRAIN)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> TOTALBRAIN) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 5: nlcom _b[TOTALBRAIN:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[TOTALBRAIN:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 5:nlcom _b[TOTALBRAIN:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**********************************************************************LnWMHpctICV**********************************


**first imputation**

mi xeq 1: sem (TIME_V0V2 -> LnWMHpctICV)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)




**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 1: nlcom _b[LnWMHpctICV:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 1:nlcom _b[LnWMHpctICV:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]





**second imputation**

mi xeq 2: sem (TIME_V0V2 -> LnWMHpctICV)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 2: nlcom _b[LnWMHpctICV:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 2:nlcom _b[LnWMHpctICV:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]



**third imputation**

mi xeq 3: sem (TIME_V0V2 -> LnWMHpctICV)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 3: nlcom _b[LnWMHpctICV:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 3:nlcom _b[LnWMHpctICV:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]


**Fourth imputation**

mi xeq 4: sem (TIME_V0V2 -> LnWMHpctICV)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 4: nlcom _b[LnWMHpctICV:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 4:nlcom _b[LnWMHpctICV:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**Fifth imputation**

mi xeq 5: sem (TIME_V0V2 -> LnWMHpctICV)  (NonWhite -> LnWMHpctICV) (AGE -> LnWMHpctICV)  (SEX -> LnWMHpctICV)  (householdsize -> LnWMHpctICV) (AD_PGS -> LnWMHpctICV) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> LnWMHpctICV) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> LnWMHpctICV)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> LnWMHpctICV)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> LnWMHpctICV)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> LnWMHpctICV)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> LnWMHpctICV)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> LnWMHpctICV)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> LnWMHpctICV)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> LnWMHpctICV) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 5: nlcom _b[LnWMHpctICV:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[LnWMHpctICV:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 5:nlcom _b[LnWMHpctICV:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**********************************************************************Hippocampus_Left**********************************


**first imputation**

mi xeq 1: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)




**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 1: nlcom _b[Hippocampus_Left:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 1:nlcom _b[Hippocampus_Left:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]





**second imputation**

mi xeq 2: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 2: nlcom _b[Hippocampus_Left:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 2:nlcom _b[Hippocampus_Left:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]



**third imputation**

mi xeq 3: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 3: nlcom _b[Hippocampus_Left:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 3:nlcom _b[Hippocampus_Left:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]


**Fourth imputation**

mi xeq 4: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 4: nlcom _b[Hippocampus_Left:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 4:nlcom _b[Hippocampus_Left:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**Fifth imputation**

mi xeq 5: sem (ICV -> Hippocampus_Left)(TIME_V0V2 -> Hippocampus_Left)  (NonWhite -> Hippocampus_Left) (AGE -> Hippocampus_Left)  (SEX -> Hippocampus_Left)  (householdsize -> Hippocampus_Left) (AD_PGS -> Hippocampus_Left) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Left) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Left)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Left)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Left)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Left)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Left)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Left)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Left)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Left) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 5: nlcom _b[Hippocampus_Left:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Left:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 5:nlcom _b[Hippocampus_Left:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**********************************************************************Hippocampus_Right**********************************


**first imputation**

mi xeq 1: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)




**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 1: nlcom _b[Hippocampus_Right:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 1: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 1:nlcom _b[Hippocampus_Right:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]





**second imputation**

mi xeq 2: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 2: nlcom _b[Hippocampus_Right:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 2: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 2:nlcom _b[Hippocampus_Right:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]



**third imputation**

mi xeq 3: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 3: estat teffects


**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 3: nlcom _b[Hippocampus_Right:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 3: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 3:nlcom _b[Hippocampus_Right:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]


**Fourth imputation**

mi xeq 4: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 4: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 4: nlcom _b[Hippocampus_Right:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 4: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 4:nlcom _b[Hippocampus_Right:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]




**Fifth imputation**

mi xeq 5: sem (ICV -> Hippocampus_Right)(TIME_V0V2 -> Hippocampus_Right)  (NonWhite -> Hippocampus_Right) (AGE -> Hippocampus_Right)  (SEX -> Hippocampus_Right)  (householdsize -> Hippocampus_Right) (AD_PGS -> Hippocampus_Right) (NonWhite -> SES) (AGE -> SES)  (SEX -> SES)   (householdsize -> SES) (SES -> ALCOHOL) (SES -> SMOKING) (SES -> DIET) (SES -> NUTR) (SES -> PA) (SES -> SS) (SES -> HEALTH) (SES -> POORCOGN) (SES -> Hippocampus_Right) (NonWhite -> ALCOHOL) (AGE -> ALCOHOL)  (SEX -> ALCOHOL)  (householdsize -> ALCOHOL) (ALCOHOL -> HEALTH) (ALCOHOL -> POORCOGN) (ALCOHOL -> Hippocampus_Right)  (NonWhite -> SMOKING) (AGE -> SMOKING)  (SEX -> SMOKING)   (householdsize -> SMOKING)  (SMOKING -> HEALTH) (SMOKING -> POORCOGN) (SMOKING -> Hippocampus_Right)  (NonWhite -> DIET) (AGE -> DIET)  (SEX -> DIET)  (householdsize -> DIET)  (DIET -> HEALTH) (DIET -> POORCOGN) (DIET -> Hippocampus_Right)  (NonWhite -> NUTR) (AGE -> NUTR)  (SEX -> NUTR)  (householdsize -> NUTR) (NUTR -> HEALTH) (NUTR -> POORCOGN) (NUTR -> Hippocampus_Right)  (NonWhite -> PA) (AGE -> PA) (SEX -> PA)  (householdsize -> PA)  (PA -> HEALTH) (PA -> POORCOGN) (PA -> Hippocampus_Right)   (NonWhite -> SS) (AGE -> SS)  (SEX -> SS)  (householdsize -> SS)  (SS -> HEALTH) (SS -> POORCOGN) (SS -> Hippocampus_Right)   (NonWhite -> HEALTH) (AGE -> HEALTH)  (SEX -> HEALTH)  (householdsize -> HEALTH)  (HEALTH -> POORCOGN) (HEALTH -> Hippocampus_Right)   (NonWhite -> POORCOGN) (AGE -> POORCOGN)  (SEX -> POORCOGN)  (householdsize -> POORCOGN)  (POORCOGN -> Hippocampus_Right) if sample_final==1, nocapslatent method(ml)
mi xeq 5: estat teffects

**INDIRECT EFFECT 1: Indirect effect THROUGH SES ONLY*****

mi xeq 5: nlcom _b[Hippocampus_Right:SES]*_b[SES:NonWhite]



**INDIRECT EFFECT 2:Indirect effect THROUGH SES AND LIFESTYLE FACTORS******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:PA])



**INDIRECT EFFECT 3:Indirect effect THROUGH SES, LIFESTYLE FACTORS AND HEALTH-RELATED FACTORS*****

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:HEALTH]*_b[HEALTH:PA])  

 


**INDIRECT EFFECT 4: Indirect effect THROUGH SES, LIFESTYLE FACTORS, HEALTH AND COGNITIVE PERFORMANCE*****


mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:HEALTH]*_b[HEALTH:PA])


**INDIRECT EFFECT 5: Indirect effect THROUGH SES, LIFESTYLE FACTORS,  AND COGNITIVE PERFORMANCE*******

mi xeq 5: nlcom (_b[SES:NonWhite]*_b[SMOKING:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SMOKING])+(_b[SES:NonWhite]*_b[ALCOHOL:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:ALCOHOL])+(_b[SES:NonWhite]*_b[DIET:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:DIET])+(_b[SES:NonWhite]*_b[NUTR:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:NUTR])+(_b[SES:NonWhite]*_b[SS:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:SS])+(_b[SES:NonWhite]*_b[PA:SES]*_b[Hippocampus_Right:POORCOGN]*_b[POORCOGN:PA])


**INDIRECT EFFECT 6:Indirect effect THROUGH SES,  AND COGNITIVE PERFORMANCE*****

mi xeq 5:nlcom _b[Hippocampus_Right:POORCOGN]*_b[SES:NonWhite]*_b[POORCOGN:SES]

capture log close




