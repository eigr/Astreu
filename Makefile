image=eigr/astreu:0.1.0

.PHONY: all clean

all: build install

build:

	docker build -t ${image} .

run: 

	docker run --rm --net=host -e MIX_ENV=prod -e RELEASE_NODE=$(RELEASE_NODE) ${image}

install:

	docker push ${image}