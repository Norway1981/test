# Program Name:ad_analyze_prog
# Date        :2018.02.07

###### Program's Outline ######

#< Goal >
# (1) Visualize Online-Offline ad effectiveness
# (2) Budget planing to maximize ad effectiveness
#
#< Image of visualization of Online-Offline ad effectiveness>
#
# In addition to CPA, making Cognitive-based ad effectiveness
#
#< Image of Budget planing to maximize ad effectiveness>
#
# Show budget planning to maximize ad effectiveness
#
#< Approach of Ad analysis>
#
# Arrange time series data with Path analysis that apply multi-stage regression
# and derive relationship between multiple ad-trials for evaluating Online-Offline ad.
#

###### Set Locale #######
Sys.setlocale("LC_ALL","en_US.UTF-8")

###### Read Library #######
rm(list = ls(all.names = TRUE))
library(MASS)

###### Args (Set File Name) ######
# Rscript CalcAdPerformance.R $filename_adv$ $file_out_regre$ $file_out_roi$
# e.g ) Rscript CalcAdPerformance.R "input_regression_20180208.csv" "output_web_regre.csv" "output_web_tar_no_adv.csv" "201704" "201708"
# e.g ) Rscript CalcAdPerformance.R input_regression_20180208.csv output_web_regre_`date +%Y%m%d_%H%M%S`.csv output_web_tar_no_adv_`date +%Y%m%d_%H%M%S`.csv "201704" "201708"

filename_adv         = sprintf("%s", commandArgs(trailingOnly=TRUE)[1])
file_out_regre       = sprintf("%s", commandArgs(trailingOnly=TRUE)[2])
file_out_tar_no_adv  = sprintf("%s", commandArgs(trailingOnly=TRUE)[3])
date_start           = sprintf("%s", commandArgs(trailingOnly=TRUE)[4])
date_end             = sprintf("%s", commandArgs(trailingOnly=TRUE)[5])
#flag_media_fix      = as.integer(sprintf("%s", commandArgs(trailingOnly=TRUE)[6]))
#media_info          = sprintf("%s", commandArgs(trailingOnly=TRUE)[7])

#dir_calc = setwd(choose.dir(default=getwd()))

#filename_adv        = "input_regression_20180130_1.csv"
#file_out_regre      = "output_web_regre.csv"
#file_out_tar_no_adv = "output_result_total.csv"
#date_start          = "201705"
#date_end            = "201707"

## Output File Path output
print(filename_adv)
print(file_out_regre)
print(file_out_tar_no_adv)

###### Set Variable for Calculation #######

col_target_val = 2   ## Position of target variable

###### Initialize Variables #######

name_explana_1st = matrix(1,1,1)
name_col         = matrix(c("data","coeff","connect"),1,3)

####  Condition for adopting explanatory variable

threshold_expla = 0.00001 #If scale of coeff < threshold_expla*mean(target), deleted in stepwise.

####  Impact ratio of Direct factor

ratio_impact_dr = 0.2 #Indirect factor has larger impact on target

####  Read and Divide Ad-data into:
#Meta:Information about data
#Target:Target Variables
#Click(1st Explanatory):
#recog(2nd Explanatory):Analog Ad + Imp
#Budget

data_ad         = read.table(filename_adv,skip=6,header=F,sep=",",fileEncoding="Shift-JIS")
data_ad_cat_tmp = read.table(filename_adv,nrow=6,header=F,sep=",",fileEncoding="Shift-JIS")
len_ad          = length(data_ad)

data_ad_cat     = matrix(0,4,len_ad)
data_ad_cat_med = matrix("",1,len_ad)
data_ad_cat_kpi = matrix("",1,len_ad)
for (i in 1:len_ad){
	data_ad_cat[1,i]     = as.character(data_ad_cat_tmp[1,i])
	data_ad_cat[2,i]     = as.character(data_ad_cat_tmp[2,i])
	data_ad_cat[3,i]     = as.character(data_ad_cat_tmp[3,i])
	data_ad_cat[4,i]     = as.character(data_ad_cat_tmp[4,i])
	data_ad_cat_med[1,i] = as.character(data_ad_cat_tmp[5,i])
	data_ad_cat_kpi[1,i] = as.character(data_ad_cat_tmp[6,i])
}

data_ad_meta_tmp           = data_ad[,data_ad_cat[1,]==1]	#Meta data (Ex. Date)

####  Locate_date_start:

if(nchar(date_start) == 6){
	locate_date_start　= min(grep(date_start,data_ad_meta_tmp))
}else{
	locate_date_start = match(date_start,data_ad_meta_tmp)
}

if(nchar(date_end) == 6){
	locate_date_end  　= max(grep(date_end,data_ad_meta_tmp))
}else{
	locate_date_end   = match(date_end,data_ad_meta_tmp)
}

num_media     = max(as.integer(data_ad_cat[3,2:ncol(data_ad_cat)]))

data_ad_targ_tmp        = data_ad[locate_date_start:locate_date_end,data_ad_cat[1,]==2]	#Target variable
data_ad_expl            = data_ad[locate_date_start:locate_date_end,data_ad_cat[1,]==3]	#Explanatory variable
data_ad_TV              = data_ad[locate_date_start:locate_date_end,data_ad_cat[2,]==1]	#TV category
data_ad_Offline         = data_ad[locate_date_start:locate_date_end,data_ad_cat[2,]==2]	#Offline category
data_ad_Movie           = data_ad[locate_date_start:locate_date_end,data_ad_cat[2,]==3]	#Movie category
data_ad_Internet        = data_ad[locate_date_start:locate_date_end,data_ad_cat[2,]==4]	#Internet category
data_ad_cost            = data_ad[locate_date_start:locate_date_end,data_ad_cat[4,]==1]	#Cost
data_ad_recog           = data_ad[locate_date_start:locate_date_end,data_ad_cat[4,]==2]	#Recongnize
data_ad_und             = data_ad[locate_date_start:locate_date_end,data_ad_cat[4,]==3]	#Understanding
name_ad_cost            = data_ad_cat_med[1,data_ad_cat[4,]==1]  	#List of media name (Cost)
name_ad_recog           = data_ad_cat_med[1,data_ad_cat[4,]==2]  	#List of media name (Recognize)
name_ad_und             = data_ad_cat_med[1,data_ad_cat[4,]==3] 	#List of media name (Understand)
id_ad_cost              = as.integer(data_ad_cat[3,data_ad_cat[4,]==1])    #Name of name_ad_cost  (Cost)
id_ad_recog             = as.integer(data_ad_cat[3,data_ad_cat[4,]==2])    #Name of name_ad_recog (Recognize)
id_ad_und               = as.integer(data_ad_cat[3,data_ad_cat[4,]==3])    #Name of name_ad_und   (Understand)
names(name_ad_cost)     = id_ad_cost
names(name_ad_recog)    = id_ad_recog
names(name_ad_und)      = id_ad_und
names(data_ad_cost)     = name_ad_cost
names(data_ad_recog)    = name_ad_recog
names(data_ad_und)      = name_ad_und
kpi_ad_recog            = data_ad_cat_kpi[1,data_ad_cat[4,]==2]  	#List of media name (Recognize)
kpi_ad_und              = data_ad_cat_kpi[1,data_ad_cat[4,]==3]  	#List of media name (Recognize)
names(kpi_ad_recog)     = id_ad_recog
names(kpi_ad_und)       = id_ad_und
cost_per_recog          = colSums(data_ad_cost[id_ad_recog])/colSums(data_ad_recog)
cost_per_und            = colSums(data_ad_cost[id_ad_und])/colSums(data_ad_und)

data_ad_tar      = data_ad_targ_tmp[,col_target_val]
tar_with_adv     = sum(data_ad_tar)

####  Apply Stepwise regression (Target - Understand)

reg_data_1st  = stepAIC(lm(data_ad_tar~.,data_ad_und,direction = "both"))

threshold_1st = mean(data_ad_tar)*threshold_expla
coeff_1st     = coefficients(reg_data_1st)

tar_no_adv = coeff_1st[1] * (locate_date_end - locate_date_start)
names(tar_no_adv) = "Intercept"

tar_by_adv  = tar_with_adv - tar_no_adv

ls_explana_1st   = attributes(coeff_1st)
explana_1st_temp = unlist(ls_explana_1st)

k = 1
explana_1st      = matrix(1,length(explana_1st_temp),5)
coeff_1st_def    = matrix(1,length(explana_1st_temp),1)

for (i in 1:length(explana_1st_temp)){
	if((coeff_1st[i]>threshold_1st) && (names(coeff_1st[i]) != "(Intercept)")){
		coeff_1st_def[k]     = coeff_1st[i]
		explana_1st[k,1]     = names(coeff_1st[i])
		num_ad_und         = grep(names(reg_data_1st$coeff[i]),name_ad_und)
		explana_1st[k,2]     = coeff_1st[i]/cost_per_und[num_ad_und]
		explana_1st[k,3]     = names(name_ad_und[num_ad_und])
		explana_1st[k,4]     = 0
		explana_1st[k,5]     = sum(data_ad_cost[,as.integer(explana_1st[k,3])])
		if (k == 1){
			data_explana_1st = data_ad_und[,num_ad_und]
		}else{
			data_explana_1st = cbind(data_explana_1st,data_ad_und[,num_ad_und])
		}
		k = k + 1
	}
}

explana_1st                = head(explana_1st,k-1)
coeff_1st_def              = head(coeff_1st_def,k-1)
size_exp_1st               = nrow(explana_1st)

####  Apply Regression by adopted variables
# This part would be necessary to improve quality in the future (2018/1/13)
# colnames(data_explana_1st) = explana_1st[,1]
# lm(data_ad_tar~.,as.data.frame(data_explana_1st))
#
###########################################

####  Adjustment of ROI scale ####

impact_by_adv_1st = sum(as.numeric(explana_1st[,2])*as.numeric(explana_1st[,5]))
ratio_real_calc   = tar_by_adv/impact_by_adv_1st * ratio_impact_dr

explana_1st[,2]  = as.numeric(explana_1st[,2]) * ratio_real_calc

# Set elements in explana_1st
explana_2nd=matrix(1,1,2)

####  Apply Stepwise regression (Understand - Recognize)

for (i in 1:size_exp_1st){

	reg_data_2nd = stepAIC(lm(data_ad_und[,explana_1st[i,1]]~.,data_ad_recog,direction = "both"))

	threshold_2nd = mean(data_ad_und[,explana_1st[i,1]])*threshold_expla

	### Adopt explanatory variable meeting certain condition
	k = 1
	names_list = matrix(1, nrow=1, ncol=1)
	coeff_tmp = coefficients(reg_data_2nd)
	size_coeff_tmp = length(coeff_tmp)
	explana_2nd_tmp = matrix(1,size_coeff_tmp,5)

	for (j in 1:size_coeff_tmp){
		if((coeff_tmp[j]>threshold_2nd)&&(names(coeff_tmp[j]) != "(Intercept)")){
			explana_2nd_tmp[k,1]  = names(coeff_tmp[j])
			num_ad_und	    = grep(names(reg_data_2nd$coeff[j]),name_ad_recog)
			explana_2nd_tmp[k,2]  = coeff_1st_def[i]/coeff_tmp[j]/cost_per_recog[num_ad_und]
			explana_2nd_tmp[k,3]  = names(name_ad_recog[num_ad_und])
			explana_2nd_tmp[k,4]  = i
		      explana_2nd_tmp[k,5]  = sum(data_ad_cost[,as.integer(explana_2nd_tmp[k,3])])
			k = k + 1
		}
	}
	explana_2nd_tmp = head(explana_2nd_tmp,k-1)

	if(i == 1){
		explana_2nd = explana_2nd_tmp
	}else{
		explana_2nd = rbind(explana_2nd,explana_2nd_tmp)
	}
}
size_exp_2nd     = nrow(explana_2nd)

####  Adjustment of ROI scale (2nd)####

impact_by_adv_2nd = sum(as.numeric(explana_2nd[,2])*as.numeric(explana_2nd[,5]))
ratio_real_calc   = tar_by_adv/impact_by_adv_2nd * (1 - ratio_impact_dr)

explana_2nd[,2]  = as.numeric(explana_2nd[,2]) * ratio_real_calc

media_exp_tmp_1   = matrix(explana_1st[,1],length(explana_1st[,1]),1)
media_exp_tmp_2   = matrix(explana_2nd[,1],length(explana_2nd[,1]),1)
media_1st_2nd     = rbind(media_exp_tmp_1,media_exp_tmp_2)

idmed_exp_tmp_1   = matrix(explana_1st[,3],length(explana_1st[,3]),1)
idmed_exp_tmp_2   = matrix(explana_2nd[,3],length(explana_2nd[,3]),1)
idmed_1st_2nd     = rbind(idmed_exp_tmp_1,idmed_exp_tmp_2)

kpi_exp_tmp_1    = matrix(as.character(kpi_ad_und[as.integer(explana_1st[,3])]),length(explana_1st[,1]),1)
kpi_exp_tmp_2    = matrix(as.character(kpi_ad_recog[as.integer(explana_2nd[,3])]),length(explana_2nd[,1]),1)
kpi_1st_2nd      = rbind(kpi_exp_tmp_1,kpi_exp_tmp_2)

connect_tmp_1st  = matrix(explana_1st[,4],length(explana_1st[,4]),1)
connect_tmp_2nd  = matrix(explana_2nd[,4],length(explana_2nd[,4]),1)
connect_1st_2nd  = as.integer(rbind(connect_tmp_1st,connect_tmp_2nd))

coeff_tmp_1st    = matrix(explana_1st[,2],length(explana_1st[,2]),1)
coeff_tmp_zero1  = matrix(0,length(explana_2nd[,2]),1)
coeff_1st_dr     = as.numeric(rbind(coeff_tmp_1st,coeff_tmp_zero1))

coeff_tmp_2nd    = matrix(explana_2nd[,2],length(explana_2nd[,2]),1)
coeff_tmp_zero2  = matrix(0,length(explana_1st[,2]),1)
coeff_2nd_id     = as.numeric(rbind(coeff_tmp_zero2,coeff_tmp_2nd))

cost_tmp_1st     = matrix(explana_1st[,5],length(explana_1st[,5]),1)
cost_tmp_2nd     = matrix(explana_2nd[,5],length(explana_2nd[,5]),1)
cost_total       = as.integer(rbind(cost_tmp_1st,cost_tmp_2nd))

Regression_result = data.frame(Media=media_1st_2nd,Media_ID=idmed_1st_2nd,KPI=kpi_1st_2nd,Cost=cost_total,Connection=connect_1st_2nd,ROI_Direct=coeff_1st_dr,ROI_InDirect=coeff_2nd_id)

#### Prepare CSV for Output

write.csv(Regression_result,file_out_regre,row.names=F)
write.csv(tar_no_adv,file_out_tar_no_adv,row.names=T)
