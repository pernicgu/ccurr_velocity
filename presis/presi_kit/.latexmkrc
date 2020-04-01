#`-file-line-error` is similar to `--interaction nonstopmode`, but shows the concrete line number
#Remove it, it you want pdflatex to stop on errors
$pdflatex = 'pdflatex -shell-escape -file-line-error %O %S';

#automatically call pdflatex
$pdf_mode = 1;
#$pdf_mode = 4; # would be lualatex

#use bibtex and remove .bbl files whith -C
$bibtex_use = 2;

#remove more files than in the default configuration
@generated_exts = qw(acn acr alg aux code ist fls glg glo gls idx ind lof lot out thm toc tpt nav run.xml snm vrb);

ensure_path('TEXINPUTS', './themes//', './pics//');

#building diagrams from svg
add_cus_dep("svg", "pdf", 0, svg2pdf);
sub svg2pdf {
  system("inkscape --export-text-to-path --export-area-drawing --export-pdf=\"$_[0].pdf\" \"$_[0].svg\"");
}
$clean_ext = "pics/*.pdf"; # this doesn't seem to work though

@default_files = ("presi");
