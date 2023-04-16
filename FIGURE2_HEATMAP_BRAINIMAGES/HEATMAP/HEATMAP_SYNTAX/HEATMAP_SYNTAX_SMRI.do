cd "E:\16GBBACKUPUSB\BACKUP_USB_SEPTEMBER2014\May Baydoun_folder\UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\HEATMAP_DATA"
use HEATMAP_DATA_SMRI, clear

**COLLAPSE MEAN ESTIMATES AND VARIANCES**
collapse (mean) de_nonwhite_est ie_nonwhite_est te_nonwhite_est percent_mediated_nonwhite var_de_nonwhite var_ie_nonwhite var_te_nonwhite z_de_nonwhite z_ie_nonwhite z_te_nonwhite p_de_nonwhite p_ie_nonwhite p_te_nonwhite sd_roi te_nonwhite_st ie_nonwhite_st de_nonwhite_st de_ses_est ie_ses_est te_ses_est percent_mediated_ses var_de_ses var_ie_ses var_te_ses z_de_ses z_ie_ses z_te_ses p_de_ses p_ie_ses p_te_ses te_ses_st ie_ses_st de_ses_st, by(id)

save HEATMAP_DATA_SMRI_collapsed, replace


**GET ROI LABELS AND MERGE THEM WITH COLLAPSED FILE**

use HEATMAP_DATA_SMRI, clear
keep  if imputation==1
keep id roi
save ROI_LABELS_ID, replace
sort id
save, replace

use HEATMAP_DATA_SMRI_collapsed
sort id
save, replace

use  ROI_LABELS_ID,clear
merge id using HEATMAP_DATA_SMRI_collapsed
save HEATMAP_DATA_SMRI_collapsedfin, replace

****GENERATE STANDARD ERRORS FROM VARIANCES****
su var_de_nonwhite var_ie_nonwhite var_te_nonwhite var_de_ses var_ie_ses var_te_ses

foreach x of varlist var_de_nonwhite var_ie_nonwhite var_te_nonwhite var_de_ses var_ie_ses var_te_ses {
	gen se`x'=sqrt(`x')
}

capture rename sevar* se*

save, replace

****GENERATE Z-SCORE*****
**z_de_nonwhite z_ie_nonwhite z_te_nonwhite z_de_ses z_ie_ses z_te_ses


replace z_de_nonwhite=de_nonwhite_est/se_de_nonwhite
replace z_ie_nonwhite=ie_nonwhite_est/se_ie_nonwhite
replace z_te_nonwhite=te_nonwhite_est/se_te_nonwhite

replace z_de_ses=de_ses_est/se_de_ses
replace z_ie_ses=ie_ses_est/se_ie_ses
replace z_te_ses=te_ses_est/se_te_ses

************GENERATE P-VALUES*******************

replace p_de_nonwhite=2*normal(-abs(z_de_nonwhite))
replace p_ie_nonwhite=2*normal(-abs(z_ie_nonwhite))
replace p_te_nonwhite=2*normal(-abs(z_te_nonwhite))

replace p_de_ses=2*normal(-abs(z_de_ses))
replace p_ie_ses=2*normal(-abs(z_ie_ses))
replace p_te_ses=2*normal(-abs(z_te_ses))

save, replace


capture log close


***************************HEATMAP*******************************

tab1 roi te_nonwhite_st p_te_nonwhite ie_nonwhite_st p_ie_nonwhite de_nonwhite_st p_de_nonwhite percent_mediated_nonwhite
tab1 roi te_ses_st p_te_ses ie_ses_st p_ie_ses de_ses_st p_de_ses percent_mediated_ses

sort id

capture log using "E:\16GBBACKUPUSB\BACKUP_USB_SEPTEMBER2014\May Baydoun_folder\UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\HEATMAP.smcl"

list roi te_nonwhite_st p_te_nonwhite ie_nonwhite_st p_ie_nonwhite de_nonwhite_st p_de_nonwhite percent_mediated_nonwhite
list roi te_ses_st p_te_ses ie_ses_st p_ie_ses de_ses_st p_de_ses percent_mediated_ses

capture log close

capture log using "E:\16GBBACKUPUSB\BACKUP_USB_SEPTEMBER2014\May Baydoun_folder\UK_BIOBANK_PROJECT\UKB_PAPER5_RACESESMRI_SUBCORT_DMRI\OUTPUT\TABLES3_4.smcl"

use HEATMAP_DATA_SMRI_collapsedfin,clear

list roi sd_roi de_nonwhite_est ie_nonwhite_est te_nonwhite_est se_de_nonwhite se_ie_nonwhite se_te_nonwhite p_de_nonwhite p_ie_nonwhite p_te_nonwhite percent_mediated_nonwhite
list roi sd_roi de_ses_est ie_ses_est te_ses_est se_de_ses se_ie_ses se_te_ses p_de_ses p_ie_ses p_te_ses percent_mediated_ses


     
