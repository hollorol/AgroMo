# Creating html documentation from 
PANDOC=pandoc
# PANDOC=C:\'PROGRAM FILES'\rstudio\bin\pandoc\pandoc.exe # IF only has rstudio installed
DEST=inst/www/readme.html
# DEST=inst\www\readme.html

$(DEST): README.md
	$(PANDOC) --self-contained --standalone -f gfm README.md -o $(DEST)
