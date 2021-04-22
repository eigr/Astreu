.PHONY: clean compile build apply example local

BONNY_IMAGE=eigr/astreu-k8s:0.1.0

all: clean compile build apply

compile:
	mix deps.get
	mix compile

build:
	mix bonny.gen.dockerfile
	docker build -t ${BONNY_IMAGE} .
	docker push ${BONNY_IMAGE}:latest

local:
	- rm manifest.yaml
	mix bonny.gen.manifest
	kubectl apply -f ./manifest.yaml
	iex -S mix

apply:
	mix compile
	mix bonny.gen.manifest --image ${BONNY_IMAGE}
	kubectl apply -f ./manifest.yaml
	kubectl get all

example:
	kubectl apply -f ./example.yaml
	kubectl get all

clean:
	- kubectl delete -f ./example.yaml
	sleep 5
	- kubectl delete -f ./manifest.yaml
	- rm manifest.yaml
	- rm -rf mix.lock _build deps
