

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


$(dir)/info : $(dir)/estimate
	$^ --info > $@

$(dir)/%.info : $(dir)/info
	awk >$@ 'BEGIN { FS = ": "} ; $$1=="$*" { print $$2 }' $^

$(dir)/current.tag : $(dir)/name.info $(dir)/namespace.info $(dir)/version.info
	echo >$@ \
		`cat $(dir)/namespace.info`/`cat $(dir)/name.info`:`cat $(dir)/version.info`

$(dir)/latest.tag : $(dir)/name.info $(dir)/namespace.info
	echo >$@ \
		`cat $(dir)/namespace.info`/`cat $(dir)/name.info`:latest


package : $(dir)/package

$(dir)/package : $(dir)/estimate $(dir)/docker Dockerfile $(dir)/current.tag $(dir)/latest.tag
	docker build -t `cat $(dir)/current.tag` -t `cat $(dir)/latest.tag` .
	docker images -q `cat $(dir)/current.tag` > $@


deploy : $(dir)/deploy

$(dir)/deploy : $(dir)/package $(dir)/current.tag $(dir)/latest.tag
	docker push `cat $(dir)/current.tag`
	docker push `cat $(dir)/latest.tag`
	$(done)
