SIM30 = 1 2 3 4 5 6 7 8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 \
        31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 \
        61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 \
        91 92 93 94 95 96 97 98 99 100

DATA = data/selected-glass-data.RData data/dirichlet_mixture.RData data/coda_gaussian_mixture.RData \
       data/coda_skew_mixture.RData data/coda_skew_t_mixture.RData data/coda_skew_t_mixture_b.RData \
       data/coda_skew_t_mixture_c.RData  data/dirichlet_mixture_parameters.RData

FIGURES = figures/main_df.pdf figures/elim_component_real.pdf figures/elim_component_ternary.pdf \
          figures/elim_component_all.pdf figures/dirichlet_mixture.pdf figures/coda_gaussian_mixture.pdf \
          figures/coda_skew_mixture.pdf \
          figures/coda_skew_t_mixture.pdf figures/coda_skew_t_mixture_b.pdf  figures/coda_skew_t_mixture_c.pdf \
          figures/subcomposition_coherenceA.pdf figures/subcomposition_coherenceB.pdf \
          figures/example_ternary.pdf figures/example_2component.pdf \
          figures/example_ilr2.pdf figures/example_ilr3.pdf figures/ilr_coordinates.pdf

TEX = tex/example-coda3.tex tex/example-glasses-A.tex tex/example-glasses-B.tex tex/pars1_component_elimination.tex \
      tex/pars2_component_elimination.tex tex/pars3_component_elimination.tex tex/pars_dirichlet_mixture.tex \
      tex/pars_coda_gaussian_mixture.tex

SEED30=$(foreach v,$(SIM30),$(shell printf 'data/fit-seed%03d.RData' $(v)))

.PHONY: article seeding

article: pdf/compositional_mixtures.pdf

seeding: $(SEED30)

data/fit-seed%.RData: R/fit-model.R
	Rscript -e 'SEED=$*; source("R/fit-model.R")'

pdf/compositional_mixtures.pdf : tex/compositional_mixtures.tex tex/compositional_mixtures.bbl $(FIGURES) $(DATA) $(TEX)
	pdflatex -output-directory tex tex/compositional_mixtures.tex
	pdflatex -output-directory tex tex/compositional_mixtures.tex
	mv tex/compositional_mixtures.pdf pdf/compositional_mixtures.pdf

tex/compositional_mixtures.bbl : tex/compositional_mixtures.bib
	pdflatex -output-directory tex -jobname compositional_mixtures tex/compositional_mixtures.tex
	bibtex tex/compositional_mixtures

tex/pars3_component_elimination.tex : tex/pars2_component_elimination.tex
tex/pars2_component_elimination.tex : tex/pars1_component_elimination.tex
tex/pars1_component_elimination.tex : R/parameters_component_elimination.R data/component_elimination.RData
	Rscript R/parameters_component_elimination.R

tex/pars_coda_gaussian_mixture.tex : R/parameters_coda_gaussian_mixture.R data/coda_gaussian_mixture.RData
	Rscript R/parameters_coda_gaussian_mixture.R

data/dirichlet_mixture_parameters.RData : R/fitting_dirichlet_mixture.R
	Rscript R/fitting_dirichlet_mixture.R

tex/pars_dirichlet_mixture.tex : R/parameters_dirichlet_mixture.R data/dirichlet_mixture_parameters.RData
	Rscript R/parameters_dirichlet_mixture.R


tex/example-glasses.tex : data/selected-glass-data.RData
data/selected-glass-data.RData : R/glass_example.R
	Rscript R/glass_example.R

figures/ilr_coordinates.pdf : R/coda.gaussian.shapes.R
	Rscript R/coda.gaussian.shapes.R

figures/main_df.pdf : R/plot_glass_data.R data/selected-glass-data.RData
	Rscript R/plot_glass_data.R

data/component_elimination.RData : R/component_elimination.R data/selected-glass-data.RData
	Rscript R/component_elimination.R
figures/elim_component_real.pdf : data/component_elimination.RData R/plot_component_elimination2.R data/selected-glass-data.RData
	Rscript R/plot_component_elimination2.R
figures/elim_component_ternary.pdf : data/component_elimination.RData R/plot_component_elimination.R data/selected-glass-data.RData
	Rscript R/plot_component_elimination.R
figures/elim_component_all.pdf : data/component_elimination.RData R/plot_component_elimination_new.R data/selected-glass-data.RData
	Rscript R/plot_component_elimination_new.R

figures/subcomposition_coherenceB.pdf : figures/subcomposition_coherenceA.pdf
	touch figures/subcomposition_coherenceB.pdf

figures/subcomposition_coherenceA.pdf : R/subcompositional_coherence.R
	Rscript R/subcompositional_coherence.R

## Exemple
figures/example_ilr2.pdf : figures/example_ilr3.pdf

figures/example_ilr3.pdf : figures/example_2component.pdf

figures/example_2component.pdf : tex/example-coda3.tex

tex/example-coda3.tex : figures/example_ternary.pdf

figures/example_ternary.pdf : R/example.elim_component.R
	Rscript R/example.elim_component.R


## Generació dels gràfics de les distribucions
data/%.RData : R/data.%.R data/selected-glass-data.RData data/selected-glass-data.RData data/dirichlet_mixture_parameters.RData
	Rscript R/data.$*.R
figures/%.pdf : data/%.RData R/plot.%.R
	Rscript R/plot.$*.R
