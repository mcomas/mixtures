.IPHONY: seminari-girona

seminari-girona : pdf/seminari-girona.pdf

pdf/seminari-girona.pdf : tex/seminari-girona.main.tex tex/seminari-girona.fig.observation-posteriori-diagram.tex \
                          tex/seminari-girona.introduction.tex  \
                          tex/seminari-girona.mixture-models.tex \
                          tex/seminari-girona.further-work.tex
	pdflatex -output-directory pdf -jobname seminari-girona tex/seminari-girona.main.tex
	pdflatex -output-directory pdf -jobname seminari-girona tex/seminari-girona.main.tex
	rm pdf/seminari-girona.aux pdf/seminari-girona.log pdf/seminari-girona.nav pdf/seminari-girona.out pdf/seminari-girona.snm pdf/seminari-girona.toc


#DGEN = data/sim-mo_AA-BB-CC.RData 

#OVERLAPS = $(subst AA,01,$(DGEN)) $(subst AA,02,$(DGEN)) $(subst AA,04,$(DGEN)) \
#           $(subst AA,08,$(DGEN)) $(subst AA,16,$(DGEN)) $(subst AA,32,$(DGEN))

#SIMS = $(subst BB,100,$(OVERLAPS)) $(subst BB,1000,$(OVERLAPS))

#M_DGEN = $(subst CC,spherical,$(SIMS)) $(subst CC,nonrestricted,$(SIMS))

#HP_DGEN = $(subst sim-mo,hp_sim-mo,$(M_DGEN))

##all: tex/combining_mixtures.pdf $(M_DGEN) $(HP_DGEN)


#tex/combining_mixtures.pdf : tex/combining_mixtures.tex \
#                             tex/fig/experiment01.pdf tex/fig/experiment02.pdf tex/fig/experiment03.pdf \
#                             tex/fig/experiment04.pdf tex/fig/experiment04b.pdf \
#                             tex/table_experiment_04.tab

#	cd tex/; pdflatex combining_mixtures.tex; cd -

### EXPERIMENT 01
#tex/fig/experiment01.pdf : R/combining_experiment_01_plot.R data/sim-01.RData
#	Rscript -e 'source("R/combining_experiment_01_plot.R")'

#data/sim-01.RData : R/combining_experiment_01.R
#	Rscript -e 'source("R/combining_experiment_01.R")'

### EXPERIMENT 02
#tex/fig/experiment02.pdf : R/combining_experiment_02_plot.R data/sim-02.RData
#	Rscript -e 'source("R/combining_experiment_02_plot.R")'

#data/sim-02.RData : R/combining_experiment_02.R
#	Rscript -e 'source("R/combining_experiment_02.R")'

### EXPERIMENT 03
#data/sim-03.RData : R/combining_experiment_03.R
#	Rscript -e 'source("R/combining_experiment_03.R")'

#tex/fig/experiment03.pdf : R/combining_experiment_03_plot.R data/sim-03.RData
#	Rscript -e 'source("R/combining_experiment_03_plot.R")'

### EXPERIMENT 04
#data/sim-mo_%-spherical.RData: R/spherical-posterioris.R
#	Rscript -e 'v=scan(text="$*", sep="-"); MAXOVERLAP=v[1]/100; NSIM=v[2]; OFILE="$@"; source("R/spherical-posterioris.R")'

#data/sim-mo_%-nonrestricted.RData: R/nonrestricted-posterioris.R
#	Rscript -e 'v=scan(text="$*", sep="-"); MAXOVERLAP=v[1]/100; NSIM=v[2]; OFILE="$@"; source("R/nonrestricted-posterioris.R")'

#data/hp_% : data/% R/hpartition_fitting.R
#	Rscript -e 'IFILE="data/$*"; OFILE="$@"; source("R/hpartition_fitting.R")'

#tex/fig/experiment04.pdf : $(HP_DGEN) R/combining_experiment_04_plot.R
#	Rscript -e 'source("R/combining_experiment_04_plot.R")'
#tex/fig/experiment04b.pdf : $(HP_DGEN) R/combining_experiment_04b_plot.R
#	Rscript -e 'source("R/combining_experiment_04b_plot.R")'

#tex/table_experiment_04.tab : $(HP_DGEN) R/combining_experiment_04b_xtable.R
#	Rscript -e 'source("R/combining_experiment_04b_xtable.R")'
