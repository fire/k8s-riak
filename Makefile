.PHONY: all build riak-container start-cluster test-cluster stop-cluster

LTS_VERSION = latest
# REPO = quay.io/briends
REPOS = quay.io/briends eu.gcr.io/papego-1005
APP = k8s-riak
BOOT_VERSION := $(shell cat k8s-riak-bootstrapper/k8s-riak-bootstrapper.cabal | grep '^version:' | grep -oE '[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
RIAK_VERSION := $(shell cat Dockerfile | grep '^ENV RIAK_VERSION' | grep -oE '[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
VERSION = $(RIAK_VERSION)-$(BOOT_VERSION)
RIAK_IMG = $(REPO)/$(APP)
PROJECT_DIR = $(shell pwd)/k8s-riak-bootstrapper
LOCAL_STACK = $(shell stack path --global-stack-root)
STACK_BUILD_VOLUMES = \
	-v $(shell pwd)/.app/:/root/.local/ \
	-v $(LOCAL_STACK):/root/.stack/ \
	-v $(PROJECT_DIR):/usr/src/app/

RUN_STACK_BUILD = docker run --rm -ti $(STACK_BUILD_VOLUMES) -w /usr/src/app/ fpco/stack-build:$(LTS_VERSION)

appDir:
	@mkdir -p ./.app

build: appDir
	$(RUN_STACK_BUILD) stack build --copy-bins
	docker build -t $(APP):latest .
	@$(foreach repo, $(REPOS), docker tag -f $(APP):latest $(repo)/$(APP):$(VERSION);)
	@$(foreach repo, $(REPOS), docker tag -f $(APP):latest $(repo)/$(APP):latest;)


publish:
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
