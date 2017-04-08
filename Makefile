BUILD_DIR := ./build

.PHONY: clean
clean:
	@rm -rf ${BUILD_DIR}

.PHONY: build
build:
	@if [ ! -d .cask ]; then cask init --dev; fi
	@cask package ${BUILD_DIR} --dev


