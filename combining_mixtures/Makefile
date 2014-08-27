.IPHONY: seminari-girona article

article : pdf/combining_mixtures.pdf

tex/combining_mixtures.bbl : tex/combining_mixtures.bib
	pdflatex -output-directory tex -jobname combining_mixtures tex/combining_mixtures.tex
	bibtex tex/combining_mixtures
	rm -f tex/combining_mixtures.blg tex/combining_mixtures.aux tex/combining_mixtures.nav tex/combining_mixtures.out tex/combining_mixtures.snm tex/combining_mixtures.toc tex/*.log

pdf/combining_mixtures.pdf : tex/combining_mixtures.tex tex/combining_mixtures.bbl tex/combining_mixtures.bbl
	pdflatex -output-directory tex -jobname combining_mixtures tex/combining_mixtures.tex
	mv tex/combining_mixtures.pdf pdf/combining_mixtures.pdf
	rm -f tex/combining_mixtures.log tex/combining_mixtures.aux tex/combining_mixtures.nav tex/combining_mixtures.out tex/combining_mixtures.snm tex/combining_mixtures.toc tex/*.log



###################################################################################
#### SEMINARI GIRONA

seminari-girona : pdf/seminari-girona.pdf

seminari-static-figures = static_figures/baudry_ex4_1_all_distributions_2c.pdf \
                          static_figures/baudry_ex4_1_all_distributions_2.pdf \
                          static_figures/baudry_ex4_1_all_distributions_3c.pdf \
                          static_figures/baudry_ex4_1_all_distributions_3.pdf \
                          static_figures/baudry_ex4_1_all_distributions_4c_b.pdf \
                          static_figures/baudry_ex4_1_all_distributions_4c.pdf \
                          static_figures/baudry_ex4_1_all_distributions_5c.pdf \
                          static_figures/baudry_ex4_1_all_distributions_one.pdf \
                          static_figures/baudry_ex4_1_all_distributions_one_tau.pdf \
                          static_figures/baudry_ex4_1_all_distributions.pdf \
                          static_figures/baudry_ex4_1_contour6_no_sample.pdf \
                          static_figures/baudry_ex4_1_contour6.pdf \
                          static_figures/baudry_ex4_1.pdf \
                          static_figures/experiment_01_sim_dat.pdf \
                          tern.png

pdf/seminari-girona.pdf : tex/seminari-girona.main.tex \
                          tex/seminari-girona.intro-combining-mixtures.tex \
                          tex/seminari-girona.combining-components.tex \
                          tex/seminari-girona.combining-general-approach.tex \
                          tex/seminari-girona.further-work.tex \
                          tex/seminari-girona.aliments-example.tex \
                          $(seminari-static-figures)
	pdflatex -output-directory pdf -jobname seminari-girona tex/seminari-girona.main.tex
	pdflatex -output-directory pdf -jobname seminari-girona tex/seminari-girona.main.tex
	rm -f pdf/seminari-girona.aux pdf/seminari-girona.log pdf/seminari-girona.nav pdf/seminari-girona.out pdf/seminari-girona.snm pdf/seminari-girona.toc tex/*.log


