docker-coverage:
	docker run \
		--rm \
		--security-opt seccomp=unconfined \
		--volume "$(shell pwd):/volume" \
		davesque/tarpaulin \
		tarpaulin --all --verbose

docker-test:
	docker run \
		--rm \
		--volume "$(shell pwd):/mnt" \
		--workdir '/mnt' \
		rustlang/rust:nightly \
		cargo test --workspace
