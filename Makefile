.PHONY: all build tag publish

# REPO = quay.io/briends
REPOS = eu.gcr.io/papego-1005
APP = k8s-riak
BOOT_VERSION := $(shell cat k8s-riak-bootstrapper/k8s-riak-bootstrapper.cabal | grep '^version:' | grep -oE '[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
RIAK_VERSION := $(shell cat Dockerfile | grep '^ENV RIAK_VERSION' | grep -oE '[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
VERSION = $(RIAK_VERSION)-$(BOOT_VERSION)
RIAK_IMG = $(REPO)/$(APP)
PROJECT_DIR = $(shell pwd)/k8s-riak-bootstrapper
# LOCAL_STACK = $(shell stack path --local-bin-path)
LOCAL_STACK = .stack-work/docker/_home/.local/bin

default: all

all: build tag publish

appDir:
	@mkdir -p ./.app

build: appDir
	stack build --copy-bins
	docker build --build-arg BINARY_PATH=$(LOCAL_STACK) -t $(APP):latest .

tag:
	@$(foreach repo, $(REPOS), docker tag $(APP):latest $(repo)/$(APP):$(VERSION);)
	@$(foreach repo, $(REPOS), docker tag $(APP):latest $(repo)/$(APP):latest;)

publish: tag
	@gcloud docker -a
	@$(foreach repo, $(REPOS), docker push $(repo)/$(APP);)

#
# all: stop-cluster riak-container start-cluster
#
# build riak-container:
# 	docker cp `stack `
#
# start-cluster:
# 	./bin/start-cluster.sh
#
# test-cluster:
# 	./bin/test-cluster.sh
#
# stop-cluster:
# 	./bin/stop-cluster.sh
