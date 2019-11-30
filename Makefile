.PHONY: docker-coverage
docker-coverage:
	docker run \
		--rm \
		--security-opt seccomp=unconfined \
		--volume "$(shell pwd):/volume" \
		xd009642/tarpaulin \
		cargo tarpaulin --all --verbose

.PHONY: docker-test
docker-test:
	docker run \
		--rm \
		--volume "$(shell pwd):/mnt" \
		--workdir '/mnt' \
		rustlang/rust:nightly \
		cargo test --workspace

.PHONY: docker-wasm-test
docker-wasm-test:
	docker run \
		--rm \
		--volume "$(shell pwd):/mnt" \
		--workdir '/mnt' \
		davesque/rust-wasm \
		wasm-pack test --node -- --workspace

.PHONY: clippy
clippy:
	cargo clippy-preview -Z unstable-options --workspace

.PHONY: rustfmt
rustfmt:
	cargo fmt --all -- --check

.PHONY: lint
lint: rustfmt clippy

.PHONY: build-docs
build-docs:
	cargo doc --no-deps --workspace
