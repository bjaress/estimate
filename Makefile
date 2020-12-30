

# For commands that don't produce any files, we make one ourselves
dir=.stack-work/make
done=mkdir -p $(dir) && touch $@

.PHONY : clean test build package deploy

test : $(dir)/test
$(dir)/test :  test/ app/ src/ $(dir)/stack
	stack test
	$(done)


clean :
	rm -r .stack-work



$(dir)/stack :
	echo Builds require Haskell Stack.
	echo 'https://docs.haskellstack.org/en/stable/README/#how-to-install'
	which stack
	$(done)

$(dir)/docker :
	echo Packaging and deployment require Docker.
	echo https://www.docker.com/get-started
	which docker
	$(done)


build : $(dir)/estimate
$(dir)/estimate: app/ src/ $(dir)/stack
	stack --local-bin-path $(dir) build --copy-bins

package : $(dir)/package
$(dir)/package : $(dir)/estimate $(dir)/docker Dockerfile
	docker build -t `$(dir)/estimate --version` .
	docker tag `$(dir)/estimate --version` `$(dir)/estimate --name`:latest
	docker images -q `$(dir)/estimate --version` > $@


deploy : $(dir)/deploy
$(dir)/deploy : $(dir)/package $(dir)/estimate
	docker push `$(dir)/estimate --version`
	docker push `$(dir)/estimate --name`:latest
	$(done)
