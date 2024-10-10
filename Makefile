.PHONY: help testall testone document check install 

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

testall: ## tinytest::build_install_test()
	Rscript -e "pkgload::load_all();tinytest::run_test_dir()"

testone: install ## make testone testfile="inst/tinytest/test-aaa-warn_once.R"
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

document: ## altdoc::render_docs()
	Rscript -e "devtools::document()"

check: document ## devtools::check()
	Rscript -e "devtools::check()"

website: website ## altdoc::render_docs()
	Rscript -e "pkgload::load_all();altdoc::render_docs()"

install: document ## devtools::install(dependencies = FALSE)
	R CMD INSTALL .
