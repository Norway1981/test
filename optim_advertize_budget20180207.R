# Program Name:optim_advertize_budget
# Date        :2018.02.07

###### Program's Outline ######
#< Goal >
# Optimize Advertizing budget by linear programming 
# 
#< Manual >
# (1) Prepare parameter file for optimization
# (2) Set budget limitation
# (3) Apply optimization (Linear Programming)
# 
#< Input >
#
# filename_intercept  : Intercept
# filename_ROI_cost   : ROI of Advertizement, Cost
# calc_param          : 1: Max Total Cost, 2: Sales Target
# percent_diff        : Optimization range of each channel's cost
# limit_target        : Limitation of Cost or Sales 
# 
#< Output>
#
# output_optim.csv : Result of Optimization of Advertizement budget
#

#Read Library

library(lpSolve) #Library for Linear Programming

##Variable Setting

#filename_intercept = "output_result_total.csv"
#filename_ROI_cost  = "output_web_regre.csv"
#filename_output    = "output_optim.csv"
#flag_calc          = 2
#flag_media_fix     = 2
#info_media_cost    = "12:1500000,1:39000000"
#percent_diff_low   = 50
#percent_diff_high  = 50
#limit_target       = 8810000000000000

filename_intercept         = sprintf("%s", commandArgs(trailingOnly=TRUE)[1])
filename_ROI_cost          = sprintf("%s", commandArgs(trailingOnly=TRUE)[2])
filename_output            = sprintf("%s", commandArgs(trailingOnly=TRUE)[3])
flag_calc                  = sprintf("%s", commandArgs(trailingOnly=TRUE)[4])
flag_media_fix             = sprintf("%s", commandArgs(trailingOnly=TRUE)[5])
info_media_cost            = sprintf("%s", commandArgs(trailingOnly=TRUE)[6])
percent_diff_low           = as.integer(sprintf("%s", commandArgs(trailingOnly=TRUE)[7]))
percent_diff_high          = as.integer(sprintf("%s", commandArgs(trailingOnly=TRUE)[8]))
limit_target               = as.numeric(sprintf("%s", commandArgs(trailingOnly=TRUE)[9]))

#print(paste("Limit Target:",limit_target))

if(flag_calc==1){
	# In case of flag_calc = 1
	cost_max_total = limit_target  # Cost Limitation
}else if(flag_calc==2){
	# In case of flag_calc = 2
	sales_target   = limit_target  # Sales Target
}

if(flag_media_fix==2){
	# In case of fixing media+cost
	text_media_cost_tmp = strsplit(info_media_cost, ",")
	text_media_cost     = as.character(text_media_cost_tmp[[1]])
	num_media_fix 	  = length(text_media_cost)
	data_media_fix      = matrix(0,num_media_fix,2)
	for(i in 1:num_media_fix){
		locate_separate     = as.integer(regexpr(":",text_media_cost[i]))
		length_char         = nchar(text_media_cost[i])
		data_media_fix[i,1] = as.integer(substr(text_media_cost[i],1,locate_separate-1))
		data_media_fix[i,2] = as.integer(substr(text_media_cost[i],locate_separate+1,length_char))
	}
}

##Read Input File
data_intercept       = read.csv(file(filename_intercept))
data_ROI_cost        = read.csv(file(filename_ROI_cost))

num_param = length(unique(data_ROI_cost[,1]))

data_input_media     = matrix("",num_param)
data_input_idmed     = matrix(0,num_param)
data_input_kpi       = matrix("",num_param)
data_input_ROI 	   = matrix(0,num_param)
data_input_cost	   = matrix(0,num_param)
data_input_sales     = matrix(0,num_param)
k = 1

for(i in 1:nrow(data_ROI_cost)){
	if(i == 1){
		data_input_media[k] = as.character(data_ROI_cost[i,1])
		data_input_idmed[k] = as.character(data_ROI_cost[i,2])
		data_input_kpi[k]   = as.character(data_ROI_cost[i,3])
		data_input_ROI[k]   = data_ROI_cost[i,6]+data_ROI_cost[i,7] 
		data_input_cost[k]  = data_ROI_cost[i,4]
		data_input_sales[k] = data_input_ROI[k] * data_ROI_cost[i,4]
		k = k + 1
	}else{
		id_param_same = grep(as.character(data_ROI_cost[i,1]),data_input_media)
		if(length(id_param_same) == 0){
			data_input_media[k] = as.character(data_ROI_cost[i,1])
			data_input_idmed[k] = as.character(data_ROI_cost[i,2])
		      data_input_kpi[k]   = as.character(data_ROI_cost[i,3])
			data_input_ROI[k]   = data_ROI_cost[i,6]+data_ROI_cost[i,7]
			data_input_cost[k]  = data_ROI_cost[i,4]
			data_input_sales[k] = data_input_ROI[k] * data_ROI_cost[i,4]
			k = k + 1
		}else{
			data_input_ROI[id_param_same]   = data_input_ROI[id_param_same]+data_ROI_cost[i,6]+data_ROI_cost[i,7]	
			data_input_sales[id_param_same] = data_input_ROI[id_param_same]  * data_ROI_cost[i,4]
		}
	}			
}

data_input = data.frame(Media=data_input_media,Media_ID=data_input_idmed,KPI=data_input_kpi,ROI=data_input_ROI,Cost=data_input_cost,Sales=data_input_sales)
intcpt_ini = data.frame(Media="Intercept",Media_ID="-",KPI="-",ROI=0,Cost=0,Sales=as.integer(data_intercept[1,2]))

## Parameter Setting for Optimization

data_input_calc = data_input

channel    = data_input_calc[,1]
ROI        = data_input_calc[,4]
param_ini  = data_input_calc[,5]
param_max  = data_input_calc[,5] * (1 + percent_diff_high / 100)
param_min  = data_input_calc[,5] * (1 - percent_diff_low  / 100)

if(flag_calc == 1){

	## Optimization with fixing total budget

	# Limitation for each channel
	con_opt_each = matrix(0,num_param*2,num_param)

	for(i in 1:num_param){
		con_opt_each[i,i] = 1
		con_opt_each[i+num_param,i] = 1
	}
	if(flag_media_fix == 2){
		row_fix_media = match(data_media_fix[,1],data_input_idmed)
		con_opt_each = con_opt_each[-(num_param+row_fix_media),]
	}
	# Limitation for total cost
	con_opt_total = matrix(1,1,num_param)
	con_opt = rbind(con_opt_each,con_opt_total)

	sign_opt_max_each  = matrix("<=",num_param,1)
	sign_opt_min_each  = matrix(">=",num_param,1)
	if(flag_media_fix == 2){
		row_fix_media = match(data_media_fix[,1],data_input_idmed)
		sign_opt_max_each[row_fix_media] = "=="
		sign_opt_min_each = sign_opt_min_each[-row_fix_media]
		sign_opt_min_each = matrix(sign_opt_min_each,length(sign_opt_min_each),1)
	}
	sign_opt_max_total = matrix("<=",1)

	rhs_opt_max_each  = matrix(param_max,nrow=num_param,byrow=TRUE)
	rhs_opt_min_each  = matrix(param_min,nrow=num_param,byrow=TRUE)
	if(flag_media_fix == 2){
		row_fix_media = match(data_media_fix[,1],data_input_idmed)
		rhs_opt_max_each[row_fix_media] = data_media_fix[,2]
		rhs_opt_min_each = rhs_opt_min_each[-row_fix_media]
		rhs_opt_min_each = matrix(rhs_opt_min_each,length(rhs_opt_min_each),1)
	}
	rhs_opt_min_total = matrix(cost_max_total,nrow=1,byrow=TRUE)

	sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_max_total)
	rhs_opt      = rbind(rhs_opt_max_each,rhs_opt_min_each,rhs_opt_min_total)

	lp_opt = lp("max", ROI, con_opt, sign_opt, rhs_opt)

	# In case of higher optimized cost than cost limitation
	if(lp_opt$status > 0){
		sign_opt_les_target = matrix(">",1)
		sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_les_target)
		lp_opt = lp("min", ROI, con_opt, sign_opt, rhs_opt)
	}

}else{

	## Optimization with fixing target sales
	sales_target_chg = sales_target - as.integer(intcpt_ini[6])
#	print(paste("Sales Target Change:",sales_target_chg))
#	print(paste("Mode:",mode(sales_target_chg)))
	param_cost = matrix(1,1,num_param)

	# Limitation for each channel
	con_opt_each = matrix(0,num_param*2,num_param)

	for(i in 1:num_param){
		con_opt_each[i,i] = 1
		con_opt_each[i+num_param,i] = 1
	}
	if(flag_media_fix == 2){
		row_fix_media = match(data_media_fix[,1],data_input_idmed)
		con_opt_each = con_opt_each[-(num_param+row_fix_media),]
	}
	# Limitation for total cost
	con_opt_total = matrix(ROI,1,num_param)
	con_opt = rbind(con_opt_each,con_opt_total)

	sign_opt_max_each   = matrix("<=",num_param)
	sign_opt_min_each   = matrix(">=",num_param)
	if(flag_media_fix == 2){
		row_fix_media = match(data_media_fix[,1],data_input_idmed)
		sign_opt_max_each[row_fix_media] = "=="
		sign_opt_min_each = sign_opt_min_each[-row_fix_media]
		sign_opt_min_each = matrix(sign_opt_min_each,length(sign_opt_min_each),1)
	}
	sign_opt_eql_target = matrix(">=",1)

	sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_eql_target)

	rhs_opt_max_each   = matrix(param_max,nrow=num_param,byrow=TRUE)
	rhs_opt_min_each   = matrix(param_min,nrow=num_param,byrow=TRUE)
	if(flag_media_fix == 2){
		row_fix_media = match(data_media_fix[,1],data_input_idmed)
		rhs_opt_max_each[row_fix_media] = data_media_fix[,2]
		rhs_opt_min_each = rhs_opt_min_each[-row_fix_media]
		rhs_opt_min_each = matrix(rhs_opt_min_each,length(rhs_opt_min_each),1)
	}
	rhs_opt_eql_target = matrix(sales_target_chg,nrow=1,byrow=TRUE)
	rhs_opt      = rbind(rhs_opt_max_each,rhs_opt_min_each,rhs_opt_eql_target)
#	print(paste("Param:",param_cost))
#	print(paste("Con:",con_opt))
#	print(paste("Sign:",sign_opt))
#	print(paste("Rhs:",rhs_opt))

	lp_opt = lp("min", param_cost, con_opt, sign_opt, rhs_opt)
	# In case of lower optimized sales than target
	if(lp_opt$status > 0){
		sign_opt_les_target = matrix("<",1)
		sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_les_target)
		lp_opt = lp("max", param_cost, con_opt, sign_opt, rhs_opt)
	}
#	print(paste("Optimized Cost2:",lp_opt$solution))
	
}

## Optimization & Output
result_def_cost_tmp = lp_opt$solution
result_def_cost     = as.integer(round(result_def_cost_tmp))
if(flag_calc==1){
	total_cost_def    = sum(result_def_cost)
	diff_cost = total_cost_def - cost_max_total
	if(abs(diff_cost) <= 10){
		for(i in 1:length(result_def_cost)){
			mod_10_cost = result_def_cost[i] %% 10
			if(mod_10_cost != 0){
				result_def_cost[i] = result_def_cost[i] - diff_cost
				break
			}
		}
	} 
}
result_optim_cost = result_def_cost
result_def_sale = round(result_def_cost_tmp * ROI)
if(flag_calc==2){
	total_sale_def    = as.integer(sum(result_def_sale) + intcpt_ini[6])
	diff_sale = total_sale_def - sales_target
	if(abs(diff_sale) <= 10){
		for(i in 1:length(result_def_sale)){
			mod_10_sale = result_def_sale[i] %% 10
			if(mod_10_sale != 0){
				result_def_sale[i] = result_def_sale[i] - diff_sale
				break
			}
		}
	} 
}
result_optim_sale = as.integer(result_def_sale)
total_cost_opt    = sum(result_optim_cost)
total_sales_opt   = sum(result_optim_sale)+intcpt_ini[6]
result_optim = cbind(data_input_calc,result_optim_cost,result_optim_sale)
#dimnames(result_optim) = list(channel,c("ROI","Initial Cost","Initial Sales","Optimized Cost","Optimized Sales"))
	
result_title = matrix(c("Media","Media_ID","KPI","ROI","Initial Cost","Initial Sales","Optimized Cost","Optimized Sales"),1,8)
intcpt_opt   = cbind(intcpt_ini,intcpt_ini[5],intcpt_ini[6])

## Standard Output
print(paste("Optimized Total Cost:",total_cost_opt))
print(paste("Simulated Sales:",total_sales_opt))

## Make Output (Optimization)
write.table(result_title, filename_output, quote=F, sep=",",row.names=F,col.names=F, append=F)
write.table(intcpt_opt  , filename_output, quote=F, sep=",",row.names=F,col.names=F, append=T)
write.table(result_optim, filename_output, quote=F, sep=",",row.names=F,col.names=F, append=T)
