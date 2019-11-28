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

.PHONY: wasm-test
wasm-test:
	wasm-pack test --node -- --workspace

.PHONY: build-docs
build-docs:
	cargo doc --no-deps --workspace

.PHONY: watch-docs
watch-docs:
	./watch_docs.py
