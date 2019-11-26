docker-coverage:
	docker run \
		--rm \
		--security-opt seccomp=unconfined \
		--volume "$(shell pwd):/volume" \
		xd009642/tarpaulin \
		cargo tarpaulin --all --verbose

docker-test:
	docker run \
		--rm \
		--volume "$(shell pwd):/mnt" \
		--workdir '/mnt' \
		rustlang/rust:nightly \
		cargo test --workspace
