# Program Name:ad_analyze_prog
# Date        :2017.12.12

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

###### Read Library #######

rm(list = ls(all.names = TRUE))

library("sem")
library(MASS)
library(DiagrammeR)

###### Set File Name ####### 

filename_adv = "sample_easy_2_20171212.txt"
filename_out_1st = "output_regres_1st.csv"

data_ad=read.table(filename_adv,header=TRUE,sep=",")

len_ad = length(data_ad)

###### Set Variables #######

i = 1
j = 1
k = 1
n = 0

count_search = 5

resp_val_num_1  = 4
resp_val_num_2a = 0
resp_val_num_2b = 0

min_exp_val_x21 = 0
min_exp_val_x22 = 0
min_exp_val_x31 = 0
min_exp_val_x32 = 0

name_explana_1st = matrix(1,1,1)
name_attri = colnames(data_ad)

####  Condition for adopting explanatory variable

threshold_expla = 0

####  Divide Ad-data into:
#Meta:Information about data
#Target:sales and SRI
#Click(1st Explanatory):
#IMP(2nd Explanatory):Analog Ad + Imp
#Budget

#data_ad_sd     = data_ad  #Standardization

data_ad_meta   = data_ad[,c(1,2)]	#date,temperature
data_ad_SRI    = data_ad[,c(4)]	#SRI
data_ad_click  = data_ad[,c(9,13,17,21,24,28,32,36,40,43,46,49)]	#Click
data_ad_IMP    = data_ad[,c(6,7,8,12,16,20,23,27,31,35,39,45,48)]	#IMP
data_ad_budget = data_ad[,c(5,7,11,15,19,22,26,30,34,38,41,44,47,50)]	#Cost

name_col       = matrix(c("data","coeff","connect"),1,3)

####  Apply Stepwise regression (SRI - Click) 

reg_data_1st = stepAIC(lm(data_ad_SRI~.,data_ad_click,direction = "both"))

coeff_1st    = coefficients(reg_data_1st)
ls_explana_1st  = attributes(coeff_1st)
explana_1st_temp = unlist(ls_explana_1st)

k = 1
explana_1st = matrix(1,length(explana_1st_temp),3)
for (i in 1:length(explana_1st_temp)){
	if((coeff_1st[i]>0) && (names(coeff_1st[i]) != "(Intercept)")){
		explana_1st[k,1] = names(coeff_1st[i])
		explana_1st[k,2] = coeff_1st[i]
		explana_1st[k,3] = 0
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
	
	### Adopt explanatory variable meeting certain condition 
	k = 1
	names_list = matrix(1, nrow=1, ncol=1)
	coeff_tmp = coefficients(reg_data_2nd)
	size_coeff_tmp = length(coeff_tmp)
	explana_2nd_tmp = matrix(1,size_coeff_tmp,3)

	for (j in 1:size_coeff_tmp){
		if((coeff_tmp[j]>0)&&(names(coeff_tmp[j]) != "(Intercept)")){
			explana_2nd_tmp[k,1]  = names(coeff_tmp[j])
			explana_2nd_tmp[k,2]  = coeff_tmp[j]
			explana_2nd_tmp[k,3]  = i
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

####  Write Regression Coefficient

write.table(name_col,    filename_out_1st, quote=F, sep=",",row.names=F,col.names=F, append=F)
write.table(explana_1st, filename_out_1st, quote=F, sep=",",row.names=F,col.names=F, append=T)
write.table(explana_2nd, filename_out_1st, quote=F, sep=",",row.names=F,col.names=F, append=T)