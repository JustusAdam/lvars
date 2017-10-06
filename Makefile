HS = src/lvish/Data/LVar/PureSet.hs \
     src/lvish/Data/LVar/SLSet.hs

ifeq ($(PLE),true)
	HS += src/lvish/tests/verified/Bit.hs \
	      src/lvish/tests/verified/Sum.hs \
	      src/lvish/tests/vtest.hs
else ifeq ($(PLE),false)
	HS += src/lvish/tests/verified/BitNoPLE.hs \
	      src/lvish/tests/verified/SumNoPLE.hs \
	      src/lvish/tests/vtestNoPLE.hs
else
	HS += src/lvish/tests/verified/Bit.hs \
	      src/lvish/tests/verified/BitNoPLE.hs \
	      src/lvish/tests/verified/Sum.hs \
	      src/lvish/tests/verified/SumNoPLE.hs \
	      src/lvish/tests/vtest.hs \
	      src/lvish/tests/vtestNoPLE.hs
endif

# Dummy target
CHS = $(subst hs,chs,$(HS))

DOCKER ?= true
TIMEIT ?=

ifeq ($(DOCKER),true)
	STACK ?= stack --docker
	ALL = docker build test check
else
	STACK ?= stack
	ALL = build test check
endif

LIQUID ?= $(STACK) exec liquid --

ifeq ($(TIMEIT),true)
	LIQUID := time $(LIQUID) -q >/dev/null
endif

all: $(ALL)

docker:
	docker build -t parfunc/verified-instances:popl18 .

build:
	$(STACK) build

test:   build
	$(STACK) test --no-run-tests
	$(STACK) bench --no-run-benchmarks

check: build $(CHS)

%.chs: %.hs
	$(LIQUID) -i src/lvish -i src/lvish/tests/verified $<

count:
	for file in $(HS); do echo $$file ; ./lh-count.sh $$file ; done

clean:
	find . -type d -name '.liquid' -exec rm -rf {} \+
	$(STACK) clean

.PHONY: docker build test check clean