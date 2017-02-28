all: fig-1 fig-2 fig-3

clean:
#    rm --force figures/fig-2/plot_mvn_2vars.png

.PHONY: all clean

# Driver script for creating figures
plots-report:	plots_report.R

# Recipe to create Figure 1
fig-1: figures/fig-1/plot_design_illustration.png
fig-1-scripts:	plots-report/plot_design_illustration.R
figures/fig-1/plot_design_illustration.png: plots-report fig-1-scripts
	./mkfigdir.sh fig-1
	Rscript plots_report.R -f 1 -o figures/fig-1/plot_design_illustration.png

# Recipe to create Figure 2
fig-2: figures/fig-2/plot_bivariate.png
fig-2-scripts:	plots-report/plot_bivariate.R
figures/fig-2/plot_bivariate.png: plots-report fig-2-scripts
	./mkfigdir.sh fig-2
	Rscript plots_report.R -f 2 -o figures/fig-2/plot_bivariate.png

# Recipe to create Figure 3
fig-3: figures/fig-3/plot_bivariate_samples.png
fig-3-scripts:	plots-report/plot_bivariate_samples.R
figures/fig-3/plot_bivariate_samples.png: plots-report fig-3-scripts
	./mkfigdir.sh fig-3
	Rscript plots_report.R -f 3 -o figures/fig-3/plot_bivariate_samples.png

# Recipe to create Figure 4
fig-4: figures/fig-4/plot_mvn_2vars.png
fig-4-scripts:	plots-report/plot_mvn_2vars.R \
				plots-report/Rscripts/calcMuStar.R \
				plots-report/Rscripts/calcStdDevStar.R
figures/fig-4/plot_mvn_2vars.png: plots-report fig-4-scripts
	./mkfigdir.sh fig-4
	Rscript plots_report.R -f 4 -o figures/fig-4/plot_mvn_2vars.png

# Recipe to create Figure 5
fig-5: figures/fig-5/plot_mvn_15vars.png
fig-5-scripts:	plots-report/plot_mvn_15vars.R \
				plots-report/Rscripts/calcMuStar.R \
				plots-report/Rscripts/calcStdDevStar.R
figures/fig-5/plot_mvn_15vars.png: plots-report fig-5-scripts
	./mkfigdir.sh fig-5
	Rscript plots_report.R -f 5 -o figures/fig-5/plot_mvn_15vars.png

# Recipe to create Figure 6
fig-6: figures/fig-6/plot_random_function.png
fig-6-scripts:	plots-report/plot_random_function.R
figures/fig-6/plot_random_function.png: plots-report fig-6-scripts
	./mkfigdir.sh fig-6
	Rscript plots_report.R -f 6 -o figures/fig-6/plot_random_function.png

# Recipe to create Figure 7
fig-7: figures/fig-7/plot_corrfun_gauss.png
fig-7-scripts:	plots-report/plot_corrfun_gauss.R
figures/fig-7/plot_corrfun_gauss.png: plots-report fig-7-scripts
	./mkfigdir.sh fig-7
	Rscript plots_report.R -f 7 -o figures/fig-7/plot_corrfun_gauss.png

# Recipe to create Figure 8
fig-8: figures/fig-8/plot_corrfun_gauss_realization.png
fig-8-scripts:	plots-report/plot_corrfun_gauss_realization.R
figures/fig-8/plot_corrfun_gauss_realization.png: plots-report fig-8-scripts
	./mkfigdir.sh fig-8
	Rscript plots_report.R -f 8 -o figures/fig-8/plot_corrfun_gauss.png

# Recipe to create Figure 9
fig-9: figures/fig-9/plot_corrfun_exp.png
fig-9-scripts:	plots-report/plot_corrfun_exp.R
figures/fig-9/plot_corrfun_exp.png: plots-report fig-9-scripts
	./mkfigdir.sh fig-9
	Rscript plots_report.R -f 9 -o figures/fig-9/plot_corrfun_exp.png

# Recipe to create Figure 10
fig-10: figures/fig-10/plot_corrfun_exp_realization.png
fig-10-scripts:	plots-report/plot_corrfun_exp_realization.R
figures/fig-10/plot_corrfun_exp_realization.png: plots-report fig-10-scripts
	./mkfigdir.sh fig-10
	Rscript plots_report.R -f 10 -o figures/fig-10/plot_corrfun_exp_realization.png

# Recipe to create Figure 11
fig-11: figures/fig-11/plot_corrfun_powexp.png
fig-11-scripts:	plots-report/plot_corrfun_powexp.R
figures/fig-11/plot_corrfun_powexp.png: plots-report fig-11-scripts
	./mkfigdir.sh fig-11
	Rscript plots_report.R -f 11 -o figures/fig-11/plot_corrfun_powexp.png

# Recipe to create Figure 12
fig-12: figures/fig-12/plot_corrfun_powexp_realization.png
fig-12-scripts:	plots-report/plot_corrfun_powexp_realization.R
figures/fig-12/plot_corrfun_powexp_realization.png: plots-report fig-12-scripts
	./mkfigdir.sh fig-12
	Rscript plots_report.R -f 12 -o figures/fig-12/plot_corrfun_powexp_realization.png

# Recipe to create Figure 13
fig-13: figures/fig-13/plot_corrfun_matern.png
fig-13-scripts:	plots-report/plot_corrfun_matern.R
figures/fig-13/plot_corrfun_matern.png: plots-report fig-13-scripts
	./mkfigdir.sh fig-13
	Rscript plots_report.R -f 13 -o figures/fig-13/plot_corrfun_matern.png

# Recipe to create Figure 14
fig-14: figures/fig-14/plot_corrfun_matern_realization.png
fig-14-scripts:	plots-report/plot_corrfun_matern_realization.R
figures/fig-14/plot_corrfun_matern_realization.png: plots-report fig-14-scripts
	./mkfigdir.sh fig-14
	Rscript plots_report.R -f 14 -o figures/fig-14/plot_corrfun_matern_realization.png

# Recipe to create Figure 15
fig-15: figures/fig-15/plot_random_surface.png
fig-15-scripts:	plots-report/plot_random_surface.R
figures/fig-15/plot_random_surface.png: plots-report fig-15-scripts
	./mkfigdir.sh fig-15
	Rscript plots_report.R -f 15 -o figures/fig-15/plot_random_surface.png

# Recipe to create Figure 16
fig-16: figures/fig-16/plot_process_variance.png
fig-16-scripts:	plots-report/plot_process_variance.R
figures/fig-16/plot_process_variance.png: plots-report fig-16-scripts
	./mkfigdir.sh fig-16
	Rscript plots_report.R -f 16 -o figures/fig-16/plot_process_variance.png

# Recipe to create Figure 17
fig-17: figures/fig-17/plot_mean_function_unconditional.png
fig-17-scripts:	plots-report/plot_mean_function_unconditional.R
figures/fig-17/plot_mean_function_unconditional.png: plots-report fig-17-scripts
	./mkfigdir.sh fig-17
	Rscript plots_report.R -f 17 -o figures/fig-17/plot_mean_function_unconditional.png

# Recipe to create Figure 18
fig-18: figures/fig-18/plot_mean_function_conditional.png
fig-18-scripts:	plots-report/plot_mean_function_conditional.R
figures/fig-18/plot_mean_function_conditional.png: plots-report fig-18-scripts
	./mkfigdir.sh fig-18
	Rscript plots_report.R -f 18 -o figures/fig-18/plot_mean_function_conditional.png

# Recipe to create Figure 19
fig-19: figures/fig-19/plot_bayesian_perspective.png
fig-19-scripts:	plots-report/plot_bayesian_perspective.R
figures/fig-19/plot_bayesian_perspective.png: plots-report fig-19-scripts
	./mkfigdir.sh fig-19
	Rscript plots_report.R -f 19 -o figures/fig-19/plot_bayesian_perspective.png

# Recipe to create 3D surface plot of Franke's function
franke3d: figures/plot_franke_3d.png
franke3d-scripts:	plots-report/plot_franke_3d.R
figures/plot_franke_3d.png: plots-report franke3d-scripts
	Rscript plots_report.R -f franke_3d -o figures/plot_franke_3d.png

# Recipe to create i/o plots of Franke's function
franke_io: figures/plot_franke_io.png
franke_io-scripts:	plots-report/plot_franke_io.R
figures/plot_franke_io.png: plots-report franke_io-scripts
	Rscript plots_report.R -f franke_io -o figures/plot_franke_io.png

# Recipe to create i/o plots of the piston performance model
piston_output: figures/plot_piston_output.png
piston_output-scripts: plots-report/plot_piston_output.R plots-report/Rscripts/commonDataPiston.R
figures/plot_piston_output.png: plots-report piston_output-scripts
	Rscript plots_report.R -f piston_output -o figures/plot_piston_output.png

# Recipe to create i/o plots of the piston performance model
piston_io: figures/plot_piston_io.png
piston_io-scripts: plots-report/plot_piston_io.R plots-report/Rscripts/commonDataPiston.R
figures/plot_piston_io.png: plots-report piston_io-scripts
	Rscript plots_report.R -f piston_io -o figures/plot_piston_io.png