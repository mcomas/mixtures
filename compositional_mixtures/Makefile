.PHONY : article

article : pdf/compositional_mixtures.pdf 

DATA = data/selected-glass-data.RData data/dirichlet_mixture.RData data/coda_gaussian_mixture.RData \
       data/coda_skew_mixture.RData data/coda_skew_t_mixture.RData

FIGURES = figures/main_df.pdf figures/elim_component_real.pdf figures/elim_component_ternary.pdf \
          figures/dirichlet_mixture.pdf figures/coda_gaussian_mixture.pdf \
          figures/coda_skew_mixture.pdf figures/coda_skew_t_mixture.pdf \
          figures/subcomposition_coherenceA.pdf figures/subcomposition_coherenceB.pdf \
          figures/example_ternary.pdf figures/example_2component.pdf \
          figures/example_ilr2.pdf figures/example_ilr3.pdf

TEX = tex/example-coda3.tex tex/example-glasses.tex tex/pars1_component_elimination.tex \
      tex/pars2_component_elimination.tex tex/pars3_component_elimination.tex

pdf/compositional_mixtures.pdf : tex/compositional_mixtures.tex $(FIGURES) $(DATA) $(TEX)
	pdflatex -output-directory pdf tex/compositional_mixtures.tex
	pdflatex -output-directory pdf tex/compositional_mixtures.tex
	rm -f pdf/compositional_mixtures.aux pdf/compositional_mixtures.log pdf/compositional_mixtures.spl

tex/pars3_component_elimination.tex : tex/pars2_component_elimination.tex
tex/pars2_component_elimination.tex : tex/pars1_component_elimination.tex
tex/pars1_component_elimination.tex : R/parameters_component_elimination.R data/component_elimination.RData
	Rscript R/parameters_component_elimination.R

tex/example-glasses.tex : data/selected-glass-data.RData
data/selected-glass-data.RData : R/glass_example.R
	Rscript R/glass_example.R

figures/main_df.pdf : R/plot_glass_data.R data/selected-glass-data.RData
	Rscript R/plot_glass_data.R

data/component_elimination.RData : R/component_elimination.R data/selected-glass-data.RData
	Rscript R/component_elimination.R
figures/elim_component_real.pdf : data/component_elimination.RData R/plot_component_elimination2.R data/selected-glass-data.RData
	Rscript R/plot_component_elimination2.R
figures/elim_component_ternary.pdf : data/component_elimination.RData R/plot_component_elimination.R data/selected-glass-data.RData
	Rscript R/plot_component_elimination.R

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
data/%.RData : R/data.%.R data/selected-glass-data.RData data/selected-glass-data.RData
	Rscript R/data.$*.R
figures/%.pdf : data/%.RData R/plot.%.R
	Rscript R/plot.$*.R
