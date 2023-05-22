SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

YELLOW := "\e[1;33m"
NC := "\e[0m"
INFO := @bash -c 'printf $(YELLOW); echo "=> $$1"; printf $(NC)' MESSAGE

POETRY_EXEC := $(shell command -v ${HOME}/.local/bin/poetry || command -v poetry)

.PHONY: help
help: ## Show this help
	@ egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-25s\033[0m %s\n", $$1, $$2}'


.PHONY: install-server
install-server: ## install the emAIcs server. Requires python3 executable in path.
ifdef POETRY_EXEC 
	${INFO} "Installing dependencies..."
	${POETRY_EXEC} install --without dev
else
	${INFO} "Poetry not found. Installing..."
	curl -sSL https://install.python-poetry.org | python3 -
	${INFO} "Installing dependencies..."
    ${HOME}/.local/bin/poetry install --without dev
endif




