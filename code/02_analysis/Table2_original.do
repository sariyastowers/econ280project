**# Bookmark #1
// ************************************************* //
clear all
set more off
set seed 12345
version 13.1
	
// ****************** GENERATE TABLES ****************** //
	
/// table 2: intent-to-treat effects in a regression framework
		
	/// load j-pal data wide
		use "/Users/sariya/Documents/documents_local/econ280project/data/02_analysis/ms_blel_jpal_wide.dta", clear
		
	///	relabel vars
	
		lab var m_theta_mle1	"Baseline score"
		lab var h_theta_mle1	"Baseline score"
		
	/// run regressions
		
		reg m_theta_mle2 treat m_theta_mle1, robust
		outreg2 using "/Users/sariya/Documents/documents_local/econ280project/results/table2.xls", label less(1) replace noaster

		reg h_theta_mle2 treat h_theta_mle1, robust
		outreg2 using "/Users/sariya/Documents/documents_local/econ280project/results/table2.xls", label less(1) append noaster
		
		xtreg m_theta_mle2 treat m_theta_mle1, robust  i(strata) fe
		outreg2 using "/Users/sariya/Documents/documents_local/econ280project/results/table2.xls", label less(1) append noaster
		
		xtreg h_theta_mle2 treat h_theta_mle1, robust i(strata) fe
		outreg2 using "/Users/sariya/Documents/documents_local/econ280project/results/table2.xls", label less(1) append noaster
		
		
		
		
