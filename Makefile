docker-all:
	make docker-build
	make docker-pdf


# Build pdf through container. Need to start container in
# current directory, so that git can find the root of the
# repository when executed inside the container.
docker-pdf:
	docker run --rm -v `pwd`:/io latexmetropolisfirasans bash -c "cd slides && make slides.pdf"

docker-watch:
	docker run --rm -v `pwd`:/io latexmetropolisfirasans bash -c "cd slides && make watch"

docker-bash:
	docker run --rm -ti -v `pwd`:/io latexmetropolisfirasans bash 


# build docker image locally, needs to be done first
docker-build:
	cd docker && docker build -t latexmetropolisfirasans .

# Alternatively, fetch docker image from docker hub:

dockerhub-pull:
	docker pull fangohr/latexmetropolisfirasans:2022




# ----------------------------------------------------------------------------------

# The following commands are only useful to push an image to dockerhub

dockerhub-login:
	docker login --username=fangohr

dockerhub-tag:
	docker tag latexmetropolisfirasans fangohr/latexmetropolisfirasans:2022

dockerhub-push:
	docker push fangohr/latexmetropolisfirasans:2022
