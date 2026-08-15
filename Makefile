.PHONY: help testall testone testall-docker testall-ci document check install

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

testall: ## tinytest::build_install_test()
	Rscript -e "pkgload::load_all();tinytest::run_test_dir()"

testall-docker: ## run full test suite in a Linux container (native arch)
	.devcontainer/run-tests.sh

testall-ci: ## as testall-docker, under amd64 to match CI (needed on Apple Silicon; slower)
	PLATFORM=linux/amd64 .devcontainer/run-tests.sh

testone: install ## make testone testfile="inst/tinytest/test-aaa-warn_once.R"
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

document: ## devtools::document()
	Rscript -e "devtools::document()"

check: document ## devtools::check()
	Rscript -e "devtools::check()"

website: ## altdoc::render_docs(parallel, freeze)
	Rscript -e "future::plan(future::multisession);pkgload::load_all();altdoc::render_docs(parallel=TRUE,freeze=TRUE)"

install: document ## devtools::install(dependencies = FALSE)
	R CMD INSTALL .
