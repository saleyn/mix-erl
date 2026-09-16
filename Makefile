all: compile

compile: deps
	mix $@

deps:
	mix deps.get

doc docs:
	mix docs

.PHONY: doc docs
