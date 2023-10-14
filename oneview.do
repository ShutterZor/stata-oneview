*===================================================================================*
* Ado-file: 	OneView Version 1
* Author: 		Shutter Zor(左祥太)
* Affiliation: 	Accounting Department,
*			   	School of Management,
*              	Xiamen University
* E-mail: 		Shutter_Z@outlook.com 
* Date: 		2023/9/15
* Description: 	Rapid generation of local web pages documenting the statistical 
*				characteristics of variables                                       
*===================================================================================*


*- define function for get basic data information
capture program drop oneviewConfirm
program define oneviewConfirm
	version 16
	syntax varlist
	local stringVar  = 0
	local stringVarName = ""
	local numericVar = 0
	local numericVarName = ""
	foreach Var in `varlist'{
		capture confirm numeric variable `Var'
		if !_rc{
			local numericVar = `numericVar' + 1
			local numericVarName = "`numericVarName'" + " `Var'"
		}
		else{
			local stringVar = `stringVar' + 1
			local stringVarName = "`stringVarName'" + " `Var'"
		}		
	}
	display as text ""
	display as text  _dup(80) "_"
	display as text "Data Set Shape = " _c
	display as result "`c(N)'×`c(k)' (sample × variable)"
	display as text  _dup(80) "-"
	display as text "Number of String Variables = " _c
	display as result "`stringVar'"
	display as result " `stringVarName'"
	display as text "Number of Numeric Variables = " _c
	display as result "`numericVar'"
	display as result " `numericVarName'"
	display as text  _dup(80) "_"
end

*- define function for get missing values and outliers
capture program drop oneviewFind
program define oneviewFind
	version 16
	syntax varlist
	*- generate new matrix
	local varNum = wordcount("`varlist'")
	matrix define missingMatrix = J(`varNum', 4, 0)
	local Observations : display _N
	*- find string variable
	local n = 0
	foreach Var in `varlist'{
		local n = `n' + 1
		matrix missingMatrix[`n', 1] = `Observations'
		capture confirm numeric variable `Var'
		if !_rc{
			quietly summarize `Var', detail
			local varObservations : display r(N)
			local varIQR : display r(p75)-r(p25)
			local varOutliers1 : display r(p25)-1.5*`varIQR'
			local varOutliers2 : display r(p75)+1.5*`varIQR'
			local varMissingRatio : display %4.2f (_N-r(N))/_N*100
			quietly summarize `Var' if (`Var'<`varOutliers1') | (`Var'>`varOutliers2')
			local varOutliersRatio : display %4.2f (r(N))/_N*100
			matrix missingMatrix[`n', 2] = `varObservations'
			matrix missingMatrix[`n', 3] = `varMissingRatio'
			matrix missingMatrix[`n', 4] = `varOutliersRatio'	
		}
		else{
			quietly count if `Var' == ""
			local varObservations : display _N-r(N)
			local varMissingRatio : display %4.2f r(N)/(_N)*100
			matrix missingMatrix[`n', 2] = `varObservations'
			matrix missingMatrix[`n', 3] = `varMissingRatio'
			matrix missingMatrix[`n', 4] = .
		}
	}
	*- rename and display matrix
	matrix rownames missingMatrix = `varlist'
	matrix colnames missingMatrix = "N" "Obs" "Missing%" "Outlier%"
	display as text  _dup(80) "_"
	display as text "Missing values and outliers of variables"
	display as text  _dup(80) "-"
	matrix list missingMatrix, noheader
	display as text  _dup(80) "_"
end

*- define function for classify the variables
capture program drop oneviewClassify
program define oneviewClassify
	version 16
	syntax varlist
	local categoryVarName = ""
	foreach Var in `varlist'{
		capture confirm string variable `Var'
		if !_rc{
			local categoryVarName = "`categoryVarName'" + " `Var'"
		}
		else{
			tempvar countNum
			bysort `Var': gen `countNum' = _n
			quietly count if `countNum' == 1
			if r(N) <= 0.1*`=_N'{
				local categoryVarName = "`categoryVarName'" + " `Var'"
			}
		}
	}
	local nonCategoryName = ""
	foreach Var of local varlist{
		if strpos("`categoryVarName'", "`Var'")==0{
			local nonCategoryName = "`nonCategoryName'" + " `Var'"
		}
	}
	local categoryVarNum = wordcount("`categoryVarName'")
	local nonCategoryVarNum = wordcount("`nonCategoryName'")
	display as text ""
	display as text  _dup(80) "_"
	display as text "Variable Type (continuous or categorical), 10% threshold"
	display as text  _dup(80) "-"
	display as text "Number of Categorical Variables = `categoryVarNum'"
	display as result " `categoryVarName'"
	display as text "Number of Continuous Variables = `nonCategoryVarNum'"
	display as result " `nonCategoryName'"
	display as text  _dup(80) "_"
	global categoryVars = "`categoryVarName'"		// store categorical variables
	global nonCategoryVars = "`nonCategoryName'"	// store numeric variables 
end

*- define function for analysis categorical variables
capture program drop oneviewCategoryVarsTable
program define oneviewCategoryVarsTable
	version 16
	foreach categoryVar in $categoryVars{
		tempvar countNum
		bysort `categoryVar': gen `countNum' = _n
		quietly count if `countNum' == 1
		if r(N) <= 0.1*`=_N'{
			display as text ""
			display as text  _dup(80) "_"
			display as text "Distribution of categorical variable" _c
			display as result " `categoryVar'"
			display as text  _dup(80) "-"
			tabulate `categoryVar'
			display as text  _dup(80) "_"
		}
		else{
			display as text ""
			display as text "The categorization results for variable" _c
			display as result " # `categoryVar' #" _c
			display as text " are not shown because there is too much variety."
		}
	}
end

*- define function for analysis continuous variables
capture program drop oneviewNonCategoryVarsTable
program define oneviewNonCategoryVarsTable
	version 16
	display as text ""
	display as text  _dup(80) "_"
	display as text "Distribution of continuous variables"
	display as text  _dup(80) "-"
	estpost summarize $nonCategoryVars, detail
	display as text  _dup(80) "_"
end 

*- define function for write a HTML file
capture program drop oneviewHTML
program define oneviewHTML
	version 16
	syntax varlist
	*- define handle and file
	quietly file open oneviewAnalysis using oneviewResult.do, write replace
	file write oneviewAnalysis "<<dd_version: 2>>" _n
	file write oneviewAnalysis "<<dd_include: ../header.txt >>" _n
	file write oneviewAnalysis _n
	*- title
	local dateFormat = subinstr("`c(current_date)'"," ","",3)
	file write oneviewAnalysis "Command oneview at `dateFormat' `c(current_time)'" _n
	file write oneviewAnalysis _dup(62) "=" _n
	file write oneviewAnalysis "" _n
	*- basic information
	file write oneviewAnalysis "<b>User Information</b>"
	file write oneviewAnalysis "<ul style='list-style: none; margin: 0px; padding: 0px;'><li>User name: `c(username)'</li> <li>Stata version: Stata `c(stata_version)'</li> <li>Operating system: `c(os)' `c(machine_type)'</li></ul>" _n
	file write oneviewAnalysis "" _n
	*- data information
	*- section 1
	file write oneviewAnalysis "## 1 Your Dataset" _n
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "~~~" _n
	file write oneviewAnalysis "<<dd_do:nocommands>>" _n
	file write oneviewAnalysis "oneviewConfirm `varlist'" _n
	file write oneviewAnalysis "oneviewClassify `varlist'" _n
	file write oneviewAnalysis "<</dd_do>>" _n
	file write oneviewAnalysis "~~~" _n	
	file write oneviewAnalysis "" _n
	*- define main content
	*- section 2
	file write oneviewAnalysis "## 2 Analysis of variables" _n
	*- section 2.1
	file write oneviewAnalysis "### 2.1 Overview of missing values and outliers" _n
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "~~~" _n
	file write oneviewAnalysis "<<dd_do:nocommands>>" _n
	file write oneviewAnalysis "oneviewFind `varlist'" _n
	file write oneviewAnalysis "<</dd_do>>" _n
	file write oneviewAnalysis "~~~" _n
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "As some variables are string type (listed in previous <b>Your Dataset</b> section), so the proportion of missing values in the table is actually the proportion of null values, they are generated by command count." _n
	file write oneviewAnalysis "" _n
	*- section 2.2
	file write oneviewAnalysis "### 2.2 Categorical variables" _n
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "~~~" _n
	file write oneviewAnalysis "<<dd_do:nocommands>>" _n
	file write oneviewAnalysis "oneviewCategoryVarsTable" _n
	file write oneviewAnalysis "<</dd_do>>" _n
	file write oneviewAnalysis "~~~" _n		
	file write oneviewAnalysis "" _n
	*- section 2.3
	file write oneviewAnalysis "### 2.3 Continuous variables" _n
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "~~~" _n
	file write oneviewAnalysis "<<dd_do:nocommands>>" _n
	file write oneviewAnalysis "oneviewNonCategoryVarsTable" _n
	file write oneviewAnalysis "<</dd_do>>" _n
	file write oneviewAnalysis "~~~" _n		
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "Notes: This table generated by command estpost" _n
	file write oneviewAnalysis "<ul><li>e(count) means number of observations</li> <li>e(sum_w) means sum of the weights</li> <li>e(mean) means average</li> <li>e(Var) means variance</li> <li>e(sd) means standard deviation</li> <li>e(skewn~) means skewness</li> <li>e(kurto~) means kurtosis</li> <li>e(sum) means sum of variables</li> <li>e(min) means minimum</li> <li>e(max) means mean</li> <li>e(pn) means nth percentile</li></ul>" _n
	*- acknowledgements
	file write oneviewAnalysis "## Contact me" _n
	file write oneviewAnalysis "" _n
	file write oneviewAnalysis "<div class='multipleColumn'><p>Thank you for using command <b>oneview</b> in <b>Stata</b>, and feel free to contact me if you have any problems using it, or have better suggestions.<br /> &#9993 <a href='mailto:shutter_z@outlook.com'>Shutter_Z@outlook.com</a></p> <p><ul style='list-style: none; margin: 0px; padding: 0px;'><li>&copy; 2023 <a href='https://shutterzor.cn'>Shutter Zor</a></li> <li><a href='https://sm.xmu.edu.cn/xygk/xszx/hjxx/bxjj.htm'>Accounting Department,<br />School of Management,<br /> Xiamen University</a></li> <li>Fujian, Xiamen, P.R.China</li></p></div>" _n
	file write oneviewAnalysis "" _n
	file close oneviewAnalysis
end

*- define main function
capture program drop oneview
program define oneview
	version 16
	syntax varlist
	oneviewHTML `varlist'
	dyndoc oneviewResult.do, replace
	erase oneviewResult.do
end


