.PHONY : article

article : pdf/compositional_mixtures.pdf

DATA = data/dirichlet_mixture.RData data/coda_gaussian_mixture.RData \
       data/coda_skew_mixture.RData data/coda_skew_t_mixture.RData

FIGURES = figures/main_df.pdf figures/elim_component_real.pdf figures/elim_component_ternary.pdf \
          figures/dirichlet_mixture.pdf figures/coda_gaussian_mixture.pdf \
          figures/coda_skew_mixture.pdf figures/coda_skew_t_mixture.pdf \
          figures/subcomposition_coherenceA.pdf figures/subcomposition_coherenceB.pdf

pdf/compositional_mixtures.pdf : tex/compositional_mixtures.tex $(FIGURES) $(DATA)
	pdflatex -output-directory pdf tex/compositional_mixtures.tex
	pdflatex -output-directory pdf tex/compositional_mixtures.tex
	rm -f pdf/compositional_mixtures.aux pdf/compositional_mixtures.log pdf/compositional_mixtures.spl

figures/main_df.pdf : R/plot_glass_data.R
	Rscript R/plot_glass_data.R

data/component_elimination.RData : R/component_elimination.R
	Rscript R/component_elimination.R
figures/elim_component_real.pdf : data/component_elimination.RData R/plot_component_elimination2.R
	Rscript R/plot_component_elimination2.R
figures/elim_component_ternary.pdf : data/component_elimination.RData R/plot_component_elimination.R
	Rscript R/plot_component_elimination.R

figures/subcomposition_coherenceB.pdf : figures/subcomposition_coherenceA.pdf
	touch figures/subcomposition_coherenceB.pdf

figures/subcomposition_coherenceA.pdf : R/subcompositional_coherence.R
	Rscript R/subcompositional_coherence.R



## Generació dels gràfics de les distribucions
data/%.RData : R/data.%.R
	Rscript R/data.$*.R
figures/%.pdf : data/%.RData R/plot.%.R
	Rscript R/plot.$*.R
