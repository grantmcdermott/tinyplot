.DEFAULT_GOAL := help

VENV := .venv

.PHONY: help
help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

.PHONY: install
install:
	R CMD INSTALL --no-multiarch --with-keep.source .

README.md: README.Rmd
	Rscript -e 'devtools::load_all(); rmarkdown::render("README.Rmd")'

.PHONY: docs
docs: README.md docs/docs/reference.md ## Generate docs
	Rscript -e 'devtools::document();pkgload::load_all();altdoc::update_docs()'
	cd docs && python3 -m mkdocs build

.PHONY: mkdocs-install
mkdocs-install: ## Install MkDocs and the Material theme
	pip3 install --upgrade pip mkdocs mkdocs-material

.PHONY: preview
preview: ## Preview docs on local server. Needs `make docs`
	cd docs && python3 -m mkdocs serve

.PHONY: test
test: ## Run fast unittests
	Rscript -e 'pkgload::load_all();tinytest::run_test_dir("inst/tinytest")'

.PHONY: deploy
deploy: ## Check for git changes and if none, generate docs
	@status=$$(git status --porcelain); if test -z "$$status"; then make deploy-dangerous; else echo "There are uncommitted changes. Please commit or stash them before running this command."; exit 1; fi

deploy-dangerous:
	git checkout gh-pages
	git checkout mkdocs -- .
	make docs
	rsync -av --exclude=.git docs/site/ .
	git add .
	git commit -m "Deploy MkDocs website"
	git push
	git checkout mkdocs