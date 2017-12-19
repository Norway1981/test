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

###### Read Library #######

rm(list = ls(all.names = TRUE))

library(MASS)
library("tablaxlsx")
library("openxlsx")

###### Set File Name ####### 

dir_calc = setwd(choose.dir(default=getwd())) 

filename_adv = "input_regression_20171215_2.txt"
filename_out_smp = "output_regression.csv"
filename_out_tar = "output_result_total.xlsx"

###### Set Variable for Calculation ####### 

col_target_val = 3   ## Position of target variable
Sales_Ini      = 40000000

###### Initialize Variables #######

name_explana_1st = matrix(1,1,1)
name_col         = matrix(c("data","coeff","connect"),1,3)

####  Condition for adopting explanatory variable

threshold_expla = 0

####  Position in output file

col_start_result = 2
row_start_title  = 2
row_start_sw_result  = 4

####  Read and Divide Ad-data into:
#Meta:Information about data
#Target:Target Variables
#Click(1st Explanatory):
#IMP(2nd Explanatory):Analog Ad + Imp
#Budget

data_ad     = read.table(filename_adv,skip=4,header=T,sep=",")
data_ad_cat = read.table(filename_adv,nrow=3,header=F,sep=",")
len_ad      = length(data_ad)
num_media   = max(data_ad_cat[2,])
name_attri  = colnames(data_ad)

data_ad_meta     = data_ad[,data_ad_cat[1,]==1]	#Meta data (Ex. Date)
data_ad_targ_tmp = data_ad[,data_ad_cat[1,]==2]	#Target variable
data_ad_expl     = data_ad[,data_ad_cat[1,]==3]	#Explanatory variable
data_ad_cost     = data_ad[,data_ad_cat[3,]==1]	#Cost
data_ad_IMP      = data_ad[,data_ad_cat[3,]==2]	#IMP
data_ad_click    = data_ad[,data_ad_cat[3,]==3]	#Click
name_ad_cost     = names(data_ad[1,data_ad_cat[3,]==1]) #List of media name (Cost)
name_ad_IMP      = names(data_ad[1,data_ad_cat[3,]==2]) #List of media name (IMP)
name_ad_click    = names(data_ad[1,data_ad_cat[3,]==3]) #List of media name (Click)

data_ad_tar      = data_ad_targ_tmp[,col_target_val]

####  CPM for each advertizement

IMP_per_cost     = matrix(0,num_media)
cost_media       = apply(data_ad_cost,2,sum)

for (i in 1:num_media){
	IMP_per_cost[i] = sum(data_ad_IMP[,i]) / cost_media[i]
}

####  Apply Stepwise regression (Target - Click) 

reg_data_1st = stepAIC(lm(data_ad_tar~.,data_ad_click,direction = "both"))

coeff_1st    = coefficients(reg_data_1st)
ls_explana_1st  = attributes(coeff_1st)
explana_1st_temp = unlist(ls_explana_1st)

k = 1
explana_1st = matrix(1,length(explana_1st_temp),4)
for (i in 1:length(explana_1st_temp)){
	if((coeff_1st[i]>0) && (names(coeff_1st[i]) != "(Intercept)")){
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
	
	### Adopt explanatory variable meeting certain condition 
	k = 1
	names_list = matrix(1, nrow=1, ncol=1)
	coeff_tmp = coefficients(reg_data_2nd)
	size_coeff_tmp = length(coeff_tmp)
	explana_2nd_tmp = matrix(1,size_coeff_tmp,4)

	for (j in 1:size_coeff_tmp){
		if((coeff_tmp[j]>0)&&(names(coeff_tmp[j]) != "(Intercept)")){
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
coeff_tmp_2nd    = matrix(explana_2nd[,2],length(explana_2nd[,2]),1)
coeff_1st_2nd    = as.numeric(rbind(coeff_tmp_1st,coeff_tmp_2nd))

connect_tmp_1st  = matrix(explana_1st[,4],length(explana_1st[,4]),1)
connect_tmp_2nd  = matrix(explana_2nd[,4],length(explana_2nd[,4]),1)
connect_1st_2nd  = as.integer(rbind(connect_tmp_1st,connect_tmp_2nd))

Regression_result = data.frame(Media=media_1st_2nd,Connection=connect_1st_2nd)

size_exp_1st_2nd = nrow(media_1st_2nd)
row_start_roi_result = row_start_sw_result + size_exp_1st_2nd + 4

####  Allocate Direct Coefficient by Indirect Coefficient Ratio

explana_2nd_alloc = explana_2nd


####  Calculate ROI for each media 

k = 1
size_exp_2nd_ali = 0
explana_2nd_ali_tmp = matrix("",1,2)
explana_2nd_ali     = matrix("",1,2)
for (i in 1:(size_exp_2nd)){
	flag_same   = 0
	pos_same_1  = 0
	pos_same_2  = 0
	if (size_exp_2nd_ali == 0){
		explana_2nd_ali[1,1] = explana_2nd_alloc[i,1]
		explana_2nd_ali[1,2] = explana_2nd_alloc[i,2]
	}else{
		for (j in 1:size_exp_2nd_ali){
			if(explana_2nd[i,1]==explana_2nd_ali[j,1]){
				flag_same   = 1
				pos_same_1  = j
				pos_same_2  = i	
			}
		}

		if(flag_same == 0){
			explana_2nd_ali_tmp[1,1] = explana_2nd_alloc[i,1]
			explana_2nd_ali_tmp[1,2] = explana_2nd_alloc[i,2]
			explana_2nd_ali          = rbind(explana_2nd_ali,explana_2nd_ali_tmp)
			k = k + 1
		}else{
			e2_tmp_1 = as.numeric(explana_2nd_ali[pos_same_1,2])
			e2_tmp_2 = as.numeric(explana_2nd[pos_same_2,2])
			explana_2nd_ali[pos_same_1,2] = as.character(e2_tmp_1 + e2_tmp_2)
		}
	}
	size_exp_2nd_ali = nrow(explana_2nd_ali)
}

####  Summarize Result for Output

media_default   = matrix("intercept",1,1)

num_media_total = size_exp_1st + size_exp_2nd_ali + 1
media_1st_tmp   = matrix(explana_1st[,1],size_exp_1st,1)
media_2nd_tmp   = matrix(explana_2nd_ali[,1],size_exp_2nd_ali,1)
media_imp_total = rbind(media_default,media_1st_tmp,media_2nd_tmp)

cost_default = matrix(0,1,1)
cost_1st_tmp = matrix(0,size_exp_1st,1)
cost_2nd_tmp = matrix(0,size_exp_2nd_ali,1)
for (i in 1:size_exp_1st){
	id_media_1st = grep(media_1st_tmp[i],name_ad_click)
	cost_1st_tmp[i,1] = cost_media[id_media_1st]
}
for (i in 1:size_exp_2nd_ali){
	id_media_2nd = grep(media_2nd_tmp[i],name_ad_IMP)
	cost_2nd_tmp[i,1] = cost_media[id_media_2nd]
}
cost_total      = rbind(cost_default,cost_1st_tmp,cost_2nd_tmp)

roi_default     = matrix(0,1,1)
roi_1st_dr_tmp  = matrix(as.numeric(explana_1st[,2]),size_exp_1st,1)
roi_2nd_dr_tmp  = matrix(0,size_exp_2nd_ali,1)
roi_dr_total    = rbind(roi_default,roi_1st_dr_tmp,roi_2nd_dr_tmp)
roi_1st_id_tmp  = matrix(0,size_exp_1st,1)
roi_2nd_id_tmp  = matrix(as.numeric(explana_2nd_ali[,2]),size_exp_2nd_ali,1)
roi_id_total    = rbind(roi_default,roi_1st_id_tmp,roi_2nd_id_tmp)

media           = media_imp_total
cost            = cost_total
roi_Direct      = roi_dr_total
roi_Indirect    = roi_id_total

roi_Sum         = rep("a",length = num_media_total)
sales_Direct    = rep("b",length = num_media_total)
sales_Indirect  = rep("c",length = num_media_total)
sales_Sum       = rep("d",length = num_media_total)

for (i in 1:num_media_total){
	roi_Sum[i]         = paste("=",letters[col_start_result+2],row_start_roi_result+i,"+",letters[col_start_result+3],row_start_roi_result+i)
	sales_Direct[i]    = paste("=",letters[col_start_result+1],row_start_roi_result+i,"*",letters[col_start_result+2],row_start_roi_result+i)
	sales_Indirect[i]  = paste("=",letters[col_start_result+1],row_start_roi_result+i,"*",letters[col_start_result+3],row_start_roi_result+i)
	sales_Sum[i]       = paste("=",letters[col_start_result+5],row_start_roi_result+i,"+",letters[col_start_result+6],row_start_roi_result+i)
}
sales_Total = paste("=sum(",letters[col_start_result+7],row_start_roi_result+1,":",letters[col_start_result+7],row_start_roi_result+num_media_total,")")

roi_Sum        = gsub(" ","",roi_Sum)
sales_Direct   = gsub(" ","",sales_Direct)
sales_Indirect = gsub(" ","",sales_Indirect)
sales_Sum      = gsub(" ","",sales_Sum)
sales_Total    = gsub(" ","",sales_Total)

ROI_target = data.frame(Media=media,Cost=cost,ROI_DR=roi_Direct,ROI_ID=roi_Indirect,ROI_Sum=roi_Sum,
				Sales_DR=sales_Direct,Sales_ID=sales_Indirect,Sales_Sum=sales_Sum)

#### Prepare Workbook for Output 

wb_output <- createWorkbook()
addWorksheet(wb_output, sheetName = "Result")

#### Write Summarized Result

writeData(wb_output, sheet = "Result", x = "Regression Result", startCol = col_start_result, startRow = row_start_title)

escribirTabla(tabla = Regression_result, wb_output, hoja = "Result", fila = row_start_sw_result, columna = col_start_result,
              limpiarFilas = FALSE, cabecerasFila = FALSE, limpiarColumnas = TRUE,
              bordes = c("TABLA","CABECERA","CABECERASFILA","CABECERASCOLUMNA","DATOS"))

writeData(wb_output, sheet = "Result", x = "ROI", startCol = col_start_result, startRow = row_start_roi_result - 2)

escribirTabla(tabla = ROI_target, wb_output, hoja = "Result", fila = row_start_roi_result, columna = col_start_result,
              limpiarFilas = FALSE, cabecerasFila = FALSE, limpiarColumnas = TRUE,
              bordes = c("TABLA","CABECERA","CABECERASFILA","CABECERASCOLUMNA","DATOS"))

writeFormula(wb_output, sheet = "Result", x = roi_Sum, startCol = col_start_result+4, startRow = row_start_roi_result+1)
writeFormula(wb_output, sheet = "Result", x = sales_Direct, startCol = col_start_result+5, startRow = row_start_roi_result+1)
writeFormula(wb_output, sheet = "Result", x = sales_Indirect, startCol = col_start_result+6, startRow = row_start_roi_result+1)
writeFormula(wb_output, sheet = "Result", x = sales_Sum, startCol = col_start_result+7, startRow = row_start_roi_result+1)
writeFormula(wb_output, sheet = "Result", x = sales_Total, startCol = col_start_result+7, startRow = row_start_roi_result+num_media_total+1)

writeData(wb_output, sheet = "Result", x = "", startCol = col_start_result+1, startRow = row_start_roi_result+1)
writeData(wb_output, sheet = "Result", x = "", startCol = col_start_result+2, startRow = row_start_roi_result+1)
writeData(wb_output, sheet = "Result", x = "", startCol = col_start_result+3, startRow = row_start_roi_result+1)
writeData(wb_output, sheet = "Result", x = "", startCol = col_start_result+4, startRow = row_start_roi_result+1)
writeData(wb_output, sheet = "Result", x = "", startCol = col_start_result+5, startRow = row_start_roi_result+1)
writeData(wb_output, sheet = "Result", x = "", startCol = col_start_result+6, startRow = row_start_roi_result+1)
writeData(wb_output, sheet = "Result", x = Sales_Ini, startCol = col_start_result+7, startRow = row_start_roi_result+1)

setColWidths(wb_output, "Result", cols = c(1,2), widths = c(1,30))

saveWorkbook(wb_output, file = filename_out_tar, overwrite = TRUE) 
