.PHONY: deps shim test spike-a spike-b spike-c spike-d spike-e bench-metal bench-text bench-null bench-retained retained-counter retained-counter-check clean

# ASDF looks only in this tree (project and vendored/), never in ~/common-lisp/.
SBCL = sbcl --noinform --non-interactive --no-sysinit --no-userinit
REGISTRY = --eval '(require :asdf)' \
	--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))'

# Optional runtime heap size for the spikes, for example HEAP='--dynamic-space-size 4096'.
HEAP =
SBCL_APP = sbcl $(HEAP) --noinform --disable-debugger --no-sysinit --no-userinit

# cl-nextstep is not in vend's registry, and Spike A depends on it, so it is cloned
# first, at the commit the spike was measured with, with the patch that lets its
# Metal view forward input (spikes/a-shim/metal-view-input.patch).
SHIM_COMMIT = 4c9f062901f73cfe66df769427e9ad9cf8710035

vendored/cl-nextstep:
	mkdir -p vendored
	git clone https://github.com/byulparan/cl-nextstep.git $@
	git -C $@ checkout $(SHIM_COMMIT)
	git -C $@ apply ../../spikes/a-shim/metal-view-input.patch

# vend stops on float-features naming a system only DotCL needs (dotcl-float).
# The converter guide's remedy is to take it out of the vendored .asd and go on.
deps: vendored/cl-nextstep
	vend get || { sed -i '' 's/ (:feature :dotcl (:require :dotcl-float))//' vendored/float-features/float-features.asd && vend get; }

test:
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-core")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-variant-null")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-variant-retained")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-7guis")'

# Build cl-nextstep's C library, which only Spike A uses (the dylib is not committed).
shim: vendored/cl-nextstep
	cmake -B vendored/cl-nextstep/C/build -S vendored/cl-nextstep/C
	make -C vendored/cl-nextstep/C/build

# Spike A: a window and an MTKView through cl-nextstep. Run `make shim` first. The
# first run compiles cl-opengl and the rest, which runs out of the default 1 GB heap:
#   make spike-a HEAP='--dynamic-space-size 4096'
# Once compiled, the default heap is enough.
# (cl-nextstep.asd calls sb-posix:setenv, so sb-posix is loaded before ASDF reads it.)
spike-a:
	$(SBCL_APP) $(REGISTRY) --eval '(require :sb-posix)' --load spikes/a-shim/run.lisp

# Spike B: the same window through our own Objective-C bindings. No dylib needed.
spike-b:
	$(SBCL_APP) $(REGISTRY) --load spikes/b-own/run.lisp

# Spike C: rounded rectangles through the Metal renderer, in a window and without one.
spike-c:
	$(SBCL_APP) $(REGISTRY) --load spikes/c-metal/run.lisp

bench-metal:
	$(SBCL_APP) $(REGISTRY) --load spikes/c-metal/bench.lisp

# Spike D: text, in a window and as a benchmark without one.
spike-d:
	$(SBCL_APP) $(REGISTRY) --load spikes/d-text/run.lisp

bench-text:
	$(SBCL_APP) $(REGISTRY) --load spikes/d-text/bench.lisp

# The null variant in a window: the core's own cost. Run it from a terminal of your
# own and leave the window alone while it runs.
bench-null:
	$(SBCL_APP) $(REGISTRY) --load variants/null/run.lisp

# What painting costs the retained variant, with and without the caches in its nodes.
bench-retained:
	$(SBCL) $(REGISTRY) --load variants/cl-retained/run-measure.lisp

# The retained variant's Counter in a window: click the button yourself. Try a press
# that you drag off the button before you let go. The check target has a thread click
# it instead and print what happened, which does not test real mouse events.
retained-counter:
	$(SBCL_APP) $(REGISTRY) --load variants/cl-retained/run-counter.lisp

retained-counter-check:
	RETAINED_CHECK=1 $(SBCL_APP) $(REGISTRY) --load variants/cl-retained/run-counter.lisp

# Spike E: 2000 requested frames of text (about 35 s), then a window left alone for 10 s.
spike-e:
	$(SBCL_APP) $(REGISTRY) --load spikes/e-frame-loop/run.lisp

clean:
	find . -name '*.fasl' -not -path './vendored/*' -delete
