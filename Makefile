# Parameters 
SRC_DIR = src
OUT_DIR = out
OUT1 = $(OUT_DIR)/01-fb-scraping.html
OUT2 = $(OUT_DIR)/02-fb-topic-modeling.html

# Make Functions
KNIT = Rscript -e "require(rmarkdown); render('$<', output_dir = '$(OUT_DIR)')"

# Project 
all: $(OUT1) $(OUT2)
clean: 
	rm -rf ./out/*
	rm -rf ./figs/*
	rm -rf ./cache/*

# Pipeline
$(OUT_DIR)/%.html:$(SRC_DIR)/%.Rmd
	$(KNIT)
$(OUT2):$(OUT1)

