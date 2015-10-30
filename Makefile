.PHONY: all build riak-container start-cluster test-cluster stop-cluster

LTS_VERSION = lts-3.10
REPO = quay.io/briends
APP = k8s-riak
VERSION := $(shell cat k8s-riak-bootstrapper/k8s-riak-bootstrapper.cabal | grep '^version:' | grep -oE '[[:digit:]]+.[[:digit:]]+.[[:digit:]]+')
RIAK_IMG = $(REPO)/$(APP)
BOOTSTRAPPER_IMG = $(REPO)/$(APP)-bootstrapper
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
	docker build -t $(RIAK_IMG):$(VERSION) -t $(RIAK_IMG):latest .


publish:
	docker push $(RIAK_IMG):$(VERSION)
	docker push $(RIAK_IMG):latest

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
