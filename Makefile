.PHONY: docker-coverage
docker-coverage:
	./coverage.sh --out Html

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

notes:
	towncrier --yes --version $(version)
	git commit -m "Compile release notes"

release:
	# Ensure release notes where generated before running the release command
	./newsfragments/validate_files.py is-empty
	cargo release $(version) --all
