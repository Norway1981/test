# Program Name:optim_advertize_budget
# Date        :2018.01.14

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

filename_intercept         = sprintf("%s", commandArgs(trailingOnly=TRUE)[1])
filename_ROI_cost          = sprintf("%s", commandArgs(trailingOnly=TRUE)[2])
filename_output            = sprintf("%s", commandArgs(trailingOnly=TRUE)[3])
calc_param                 = sprintf("%s", commandArgs(trailingOnly=TRUE)[4])
percent_diff               = as.integer(sprintf("%s", commandArgs(trailingOnly=TRUE)[5]))
limit_target               = as.integer(sprintf("%s", commandArgs(trailingOnly=TRUE)[6]))

if(calc_param==1){
	# In case of calc_param = 1
	cost_max_total = limit_target  # Cost Limitation
}else if(calc_param==2){
	# In case of calc_param = 2
	sales_target   = limit_target  # Sales Target
}

##Read Input File
data_intercept       = read.csv(file(filename_intercept))
data_ROI_cost        = read.csv(file(filename_ROI_cost))

num_param = length(unique(data_ROI_cost[,1]))

data_input_media     = matrix("",num_param)
data_input_ROI 	   = matrix(0,num_param)
data_input_cost	   = matrix(0,num_param)
data_input_sales     = matrix(0,num_param)
k = 1

for(i in 1:nrow(data_ROI_cost)){
	i
	if(i == 1){
		data_input_media[k] = as.character(data_ROI_cost[i,1])
		data_input_ROI[k]   = data_ROI_cost[i,4]+data_ROI_cost[i,5] 
		data_input_cost[k]  = data_ROI_cost[i,2]
		data_input_sales[k] = data_input_ROI[k] * data_ROI_cost[i,2]
		k = k + 1
	}else{
		id_param_same = grep(as.character(data_ROI_cost[i,1]),data_input_media)
		if(length(id_param_same) == 0){
			data_input_media[k] = as.character(data_ROI_cost[i,1])
			data_input_ROI[k]   = data_ROI_cost[i,4]+data_ROI_cost[i,5]
			data_input_cost[k]  = data_ROI_cost[i,2]
			data_input_sales[k] = data_input_ROI[k] * data_ROI_cost[i,2]
			k = k + 1
		}else{
			data_input_ROI[id_param_same]   = data_input_ROI[id_param_same]+data_ROI_cost[i,4]+data_ROI_cost[i,5]	
			data_input_sales[id_param_same] = data_input_ROI[id_param_same]  * data_ROI_cost[i,2]
		}
	}			
}

data_input = data.frame(Media=data_input_media,ROI=data_input_ROI,Cost=data_input_cost,Sales=data_input_sales)

intcpt_ini   = data.frame(Media="Intercept",ROI=0,Cost=0,Sales=data_intercept[1,2])

## Parameter Setting for Optimization

data_input_calc = data_input

channel    = data_input_calc[,1]
ROI        = data_input_calc[,2]
param_ini  = data_input_calc[,3]
param_max  = data_input_calc[,3] * (1 + percent_diff / 100)
param_min  = data_input_calc[,3] * (1 - percent_diff / 100)

if(calc_param == 1){

	## Optimization with fixing total budget

	# Limitation for each channel
	con_opt_each = matrix(0,num_param*2,num_param)

	for(i in 1:num_param){
		con_opt_each[i,i] = 1
		con_opt_each[i+num_param,i] = 1
	}
	# Limitation for total cost
	con_opt_total = matrix(1,1,num_param)
	con_opt = rbind(con_opt_each,con_opt_total)

	sign_opt_max_each  = matrix("<=",num_param)
	sign_opt_min_each  = matrix(">=",num_param)
	sign_opt_max_total = matrix("<=",1)

	sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_max_total)

	rhs_opt_max_each  = matrix(param_max,nrow=num_param,byrow=TRUE)
	rhs_opt_min_each  = matrix(param_min,nrow=num_param,byrow=TRUE)
	rhs_opt_min_total = matrix(cost_max_total,nrow=1,byrow=TRUE)

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

	sales_target_chg = sales_target - intcpt_ini[4]
	param_cost = matrix(1,1,num_param)

	# Limitation for each channel
	con_opt_each = matrix(0,num_param*2,num_param)

	for(i in 1:num_param){
		con_opt_each[i,i] = 1
		con_opt_each[i+num_param,i] = 1
	}
	# Limitation for total cost
	con_opt_total = matrix(ROI,1,num_param)
	con_opt = rbind(con_opt_each,con_opt_total)

	sign_opt_max_each   = matrix("<=",num_param)
	sign_opt_min_each   = matrix(">=",num_param)
	sign_opt_eql_target = matrix(">=",1)

	sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_eql_target)

	rhs_opt_max_each   = matrix(param_max,nrow=num_param,byrow=TRUE)
	rhs_opt_min_each   = matrix(param_min,nrow=num_param,byrow=TRUE)
	rhs_opt_eql_target = matrix(sales_target_chg,nrow=1,byrow=TRUE)

	rhs_opt      = rbind(rhs_opt_max_each,rhs_opt_min_each,rhs_opt_eql_target)

	lp_opt = lp("min", param_cost, con_opt, sign_opt, rhs_opt)

	# In case of lower optimized sales than target
	if(lp_opt$status > 0){
		sign_opt_les_target = matrix("<",1)
		sign_opt     = rbind(sign_opt_max_each,sign_opt_min_each,sign_opt_les_target)
		lp_opt = lp("max", param_cost, con_opt, sign_opt, rhs_opt)
	}
}

## Optimization & Output

if(lp_opt$status == 0){
	result_optim_cost = as.integer(round(lp_opt$solution))
	result_optim_sale = as.integer(result_optim_cost * ROI)
	total_cost_opt    = sum(result_optim_cost)
	total_sales_opt   = sum(result_optim_sale)+intcpt_ini[4]
	result_optim = cbind(data_input_calc,result_optim_cost,result_optim_sale)
	#dimnames(result_optim) = list(channel,c("ROI","Initial Cost","Initial Sales","Optimized Cost","Optimized Sales"))
	
	result_title = matrix(c("Media","ROI","Initial Cost","Initial Sales","Optimized Cost","Optimized Sales"),1,6)
	intcpt_opt   = cbind(intcpt_ini,intcpt_ini[3],intcpt_ini[4])

	## Standard Output
	print(paste("Optimized Total Cost:",total_cost_opt))
	print(paste("Simulated Sales:",total_sales_opt))

	## Make Output (Optimization)
	write.table(result_title, filename_output, quote=F, sep=",",row.names=F,col.names=F, append=F)
	write.table(intcpt_opt  , filename_output, quote=F, sep=",",row.names=F,col.names=F, append=T)
	write.table(result_optim, filename_output, quote=F, sep=",",row.names=F,col.names=F, append=T)
}else{
	## result_optim = "Optimized Total Cost exceeds cost limitation"
	## warning(result_optim)
	## Make Output (Optimization)  Comment Out 12/6
	## write.table(result_optim, filename_output, quote=F, sep=",",row.names=F,col.names=F, append=F)
		
}
