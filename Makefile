build:
	stack exec ottawa-brahms-choir

deploy: build
	cp -Rv dist/* ../obc/
	cd ../obc && git add . && git commit -m "deploy" && git push


.PHONY: build deploy
