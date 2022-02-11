# dann v 0.2.6 (02-11-2022)
  Fixing hard coded sphere argument in sub_dann and graph_eigenvalues.
  
# dann v 0.2.5 (02-10-2022)
  Adding formula functions (_df) and associated tests.
  Vignettes use df functions, not matrix functions.

# dann v 0.2.4 (02-10-2022)
	Minor optimizations.
	  One small loop removed for vectorized R code.
	  Summing a logical vector instead of sub setting and calculating length.
	
# dann v 0.2.3 (06-06-2021)
	Fixing code coverage to work with github actions.

# dann v 0.2.2 (02-10-2021)
	If the most common class fails to break ties in 
	distance, sort by Y so that predictions are consistent
	in all cases.
	
	Moving from travisCI to github actions.
	
# dann v 0.2.1 (10-08-2020)
	Ties in distance are broken by most common class.
	Updating vignettes.
	
# dann v 0.2.0 (03-14-2020)
	Moving to RcppArmadillo for performance improvements.

# dann v 0.1.0 (12-10-2019)
	Initial release.
