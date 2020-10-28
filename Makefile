Q ?= @

APP = $(shell basename $$(pwd))
IMAGE =$(APP)_erlang

REBAR3=rebar3

.PHONY: test

docker-image: ci/Dockerfile
	$Q echo "==> Building docker image"
	$Q docker build -f ci/Dockerfile -t $(IMAGE) ci

test-ci: docker-image
	$Q echo "==> Running tests in $(IMAGE)"
	$Q docker run --rm \
	  -w "/$(APP)" \
	  -v "$(PWD)/src:/$(APP)/src" \
	  -v "$(PWD)/test:/$(APP)/test" \
	  -v "$(PWD)/Makefile:/$(APP)/Makefile" \
	  -v "$(PWD)/rebar.config:/$(APP)/rebar.config" \
	  -v "$(PWD)/rebar.lock:/$(APP)/rebar.lock" \
	  -v "$(PWD)/.ci/_build:/$(APP)/_build" -it $(IMAGE) \
	   make test

test: eunit ct cover
	$Q

eunit:
	$Q echo "==> Running eunit tests"
	$Q $(REBAR3) eunit --cover

ct:
	$Q echo "==> Running common tests"
	$Q $(REBAR3) ct --cover

cover:
	$Q echo "==> Creating coverage report"
	$Q $(REBAR3) cover
