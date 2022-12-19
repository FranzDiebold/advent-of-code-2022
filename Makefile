.PHONY: help
help:  ## Show this help.
	@egrep '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-22s\033[0m %s\n", $$1, $$2}'

.PHONY: lint
lint:  ## Lint the code.
	sbt "scalafixAll --check"

.PHONY: test
test:  ## Run all tests.
	sbt test

.PHONY: run
run:  ## Run a specific day.
	sbt "runMain $(shell printf "io.diebold.adventofcode.Day%02d" $(day))"
