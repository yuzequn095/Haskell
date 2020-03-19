
#####################################################################################################
COURSE=cs130w
ASGN=02
NAME=random-art
STACK=stack --allow-different-user
BUILD_OPTS=--ghc-options -O0 
#####################################################################################################

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

test: clean
	$(STACK) test $(BUILD_OPTS)

bin:
	$(STACK) build $(BUILD_OPTS)

clean: 
	$(STACK) clean

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c lib/

ghci:
	$(STACK) exec -- ghci

turnin: 
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse130/02-random-art.git

update:
	git pull upstream master
