BUILD_DIR := ./build

.PHONY: all
all: clean build install

.PHONY: clean
clean:
	@rm -rf ${BUILD_DIR}

.PHONY: build
build:
	@cask package ${BUILD_DIR} --dev

.PHONY: install
install:
	@cask install
