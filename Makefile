docker-all:
	make docker-build
	make docker-pdf

all:
	make pdf



# To use Docker container for building and testing

# build docker image locally, needs to be done first
docker-build:
	cd docker && docker build -t latexmetropolisfirasans .


# Alternatively, fetch docker image from docker hub:

dockerhub-pull:
docker pull fangohr/latexmetropolisfirasans:2018


# build pdf through container. Need to start container in
# current directory, so that git can find the root of the
# repository when executed inside the container.
docker-pdf:
	docker run -v `pwd`:/io latexmetropolisfirasans bash -c "cd slides && make slides.pdf"



# ----------------------------------------------------------------------------------

# The following commands are only useful to push an image to dockerhub

dockerhub-login:
	docker login --username=fangohr

dockerhub-tag:
	docker tag latexmetropolisfirasans fangohr/latexmetropolisfirasans:2018

dockerhub-push:
	docker push fangohr/latexmetropolisfirasans:2018
