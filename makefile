.PHONY: build
build:
	cargo build --release

.PHONY: dev-build
dev-build:
	cargo build

.PHONY: run
run:
	cargo run

.PHONY: clean
clean:
	cargo clean

