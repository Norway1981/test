# Program Name:ad_analyze_prog
# Date        :2018.01.03

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

###### Read Library #######
rm(list = ls(all.names = TRUE))
library(MASS)

###### Args (Set File Name) ######
# Rscript CalcAdPerformance.R $filename_adv$ $file_out_regre$ $file_out_roi$
# e.g ) Rscript CalcAdPerformance.R "input_regression_20171215_3.csv" "output_web_regre.csv" "output_web_tar_no_adv.csv"
# e.g ) Rscript CalcAdPerformance.R input_regression_20171215_3.csv output_web_regre_`date +%Y%m%d_%H%M%S`.csv output_web_tar_no_adv_`date +%Y%m%d_%H%M%S`.csv

filename_adv         = sprintf("%s", commandArgs(trailingOnly=TRUE)[1])
file_out_regre       = sprintf("%s", commandArgs(trailingOnly=TRUE)[2])
file_out_tar_no_adv  = sprintf("%s", commandArgs(trailingOnly=TRUE)[3])

## Output File Path output
print(filename_adv)
print(file_out_regre)
print(file_out_tar_no_adv)

###### Set Variable for Calculation ####### 

col_target_val = 2   ## Position of target variable
row_data       = 150 ## Row that the data contained

###### Initialize Variables #######

name_explana_1st = matrix(1,1,1)
name_col         = matrix(c("data","coeff","connect"),1,3)

####  Condition for adopting explanatory variable

threshold_expla = 0.0001 #If scale of coeff < threshold_expla*mean(target), deleted in stepwise.

####  Read and Divide Ad-data into:
#Meta:Information about data
#Target:Target Variables
#Click(1st Explanatory):
#IMP(2nd Explanatory):Analog Ad + Imp
#Budget

data_ad     = read.table(filename_adv,skip=4,header=T,sep=",",fileEncoding="Shift-JIS")
data_ad_cat = read.table(filename_adv,nrow=3,header=F,sep=",")
len_ad      = length(data_ad)
num_media   = max(data_ad_cat[2,])
name_attri  = colnames(data_ad)

data_ad_meta     = data_ad[1:row_data,data_ad_cat[1,]==1]	#Meta data (Ex. Date)
data_ad_targ_tmp = data_ad[1:row_data,data_ad_cat[1,]==2]	#Target variable
data_ad_expl     = data_ad[1:row_data,data_ad_cat[1,]==3]	#Explanatory variable
data_ad_cost     = data_ad[1:row_data,data_ad_cat[3,]==1]	#Cost
data_ad_IMP      = data_ad[1:row_data,data_ad_cat[3,]==2]	#IMP
data_ad_click    = data_ad[1:row_data,data_ad_cat[3,]==3]	#Click
name_ad_cost     = names(data_ad[1,data_ad_cat[3,]==1]) #List of media name (Cost)
name_ad_IMP      = names(data_ad[1,data_ad_cat[3,]==2]) #List of media name (IMP)
name_ad_click    = names(data_ad[1,data_ad_cat[3,]==3]) #List of media name (Click)

data_ad_tar      = data_ad_targ_tmp[1:row_data,col_target_val]

####  CPM for each advertizement

IMP_per_cost     = matrix(0,num_media)
cost_media       = apply(data_ad_cost,2,sum)

for (i in 1:num_media){
	IMP_per_cost[i] = sum(data_ad_IMP[,i]) / cost_media[i]
}

####  Apply Stepwise regression (Target - Click) 

reg_data_1st = stepAIC(lm(data_ad_tar~.,data_ad_click,direction = "both"))

threshold_1st = mean(data_ad_tar)*threshold_expla
coeff_1st    = coefficients(reg_data_1st)

tar_no_adv = coeff_1st[1]
names(tar_no_adv) = "Intercept"

ls_explana_1st  = attributes(coeff_1st)
explana_1st_temp = unlist(ls_explana_1st)

k = 1
explana_1st = matrix(1,length(explana_1st_temp),4)
for (i in 1:length(explana_1st_temp)){
	if((coeff_1st[i]>threshold_1st) && (names(coeff_1st[i]) != "(Intercept)")){
		explana_1st[k,1] = names(coeff_1st[i])
		explana_1st[k,2] = coeff_1st[i]
		explana_1st[k,3] = grep(names(reg_data_1st$coeff[i]),name_ad_click)
		explana_1st[k,4] = 0
		k = k + 1
	}
}

explana_1st = head(explana_1st,k-1)
size_exp_1st = nrow(explana_1st)

# Set elements in explana_1st
explana_2nd=matrix(1,1,2)

####  Apply Stepwise regression (Click - IMP1)

for (i in 1:size_exp_1st){
	
	reg_data_2nd = stepAIC(lm(data_ad_click[,explana_1st[i,1]]~.,data_ad_IMP,direction = "both"))
	
	threshold_2nd = mean(data_ad_click[,explana_1st[i,1]])*threshold_expla�@

	### Adopt explanatory variable meeting certain condition 
	k = 1
	names_list = matrix(1, nrow=1, ncol=1)
	coeff_tmp = coefficients(reg_data_2nd)
	size_coeff_tmp = length(coeff_tmp)
	explana_2nd_tmp = matrix(1,size_coeff_tmp,4)

	for (j in 1:size_coeff_tmp){
		if((coeff_tmp[j]>threshold_2nd)&&(names(coeff_tmp[j]) != "(Intercept)")){
			explana_2nd_tmp[k,1]  = names(coeff_tmp[j])
			explana_2nd_tmp[k,2]  = coeff_tmp[j]
			explana_2nd_tmp[k,3]  = grep(names(reg_data_2nd$coeff[j]),name_ad_IMP)
			explana_2nd_tmp[k,4]  = i
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

media_exp_tmp_1  = matrix(explana_1st[,1],length(explana_1st[,1]),1)
media_exp_tmp_2  = matrix(explana_2nd[,1],length(explana_2nd[,1]),1)
media_1st_2nd    = rbind(media_exp_tmp_1,media_exp_tmp_2)

coeff_tmp_1st    = matrix(explana_1st[,2],length(explana_1st[,2]),1)
coeff_tmp_zero1  = matrix(0,length(explana_2nd[,2]),1)
coeff_1st_dr     = as.numeric(rbind(coeff_tmp_1st,coeff_tmp_zero1))

coeff_tmp_2nd    = matrix(explana_2nd[,2],length(explana_2nd[,2]),1)
coeff_tmp_zero2  = matrix(0,length(explana_1st[,2]),1)
coeff_2nd_id     = as.numeric(rbind(coeff_tmp_zero2,coeff_tmp_2nd))

connect_tmp_1st  = matrix(explana_1st[,4],length(explana_1st[,4]),1)
connect_tmp_2nd  = matrix(explana_2nd[,4],length(explana_2nd[,4]),1)
connect_1st_2nd  = as.integer(rbind(connect_tmp_1st,connect_tmp_2nd))

Regression_result = data.frame(Media=media_1st_2nd,Connection=connect_1st_2nd,ROI_Direct=coeff_1st_dr,ROI_InDirect=coeff_2nd_id)

#### Prepare CSV for Output

write.csv(Regression_result,file_out_regre,row.names=F)
write.csv(tar_no_adv,file_out_tar_no_adv,row.names=T)
