.PHONY: deps shim test spike-a spike-b spike-c spike-d spike-e bench-metal bench-text bench-null bench-retained bench-compare bench-layout image test-cursor bench-cursor snapshots-cursor bench-run bench-overhead bench-app bench-compile bench-report snapshots replay 7guis 7guis-smoke 7guis-repl-smoke clean

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
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-variant-reactive")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-variant-immediate")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-7guis")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-7guis/acceptance")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-stress")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-stress/acceptance")'

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

# What a frame costs in each of the three variants on the same screen.
bench-compare:
	$(SBCL) $(REGISTRY) --load bench/run.lisp

# The benchmark protocol on the example replay scripts: every variant, REPS repetitions (10)
# after WARMUPS (3), a result record for each in results/ (OUT=dir), never edited, only
# superseded. bench-report prints the latest record of each run.
bench-run:
	WHICH=results $(SBCL) $(REGISTRY) --load bench/run.lisp

# What the instrumentation costs: the phases and build timing on and off, on the example scripts
# in each variant and on the null variant through the host. Written to results/ as a record.
bench-overhead:
	WHICH=overhead $(SBCL) $(REGISTRY) --load bench/run.lisp

# Startup, idle CPU and frames, memory and image size of each variant's executable running each
# task in a window (make image first). VARIANTS=retained,immediate TASKS=counter,cells IDLE=10 REPS=5.
bench-app:
	WHICH=app $(SBCL) $(REGISTRY) --load bench/run.lisp

# How long each variant takes to compile from nothing and after a change to one file, in new
# processes with empty fasl caches (REPS=3).
bench-compile:
	WHICH=compile $(SBCL) $(REGISTRY) --load bench/run.lisp

bench-report:
	WHICH=report $(SBCL) $(REGISTRY) --load bench/run.lisp

# The layout spike (H6) in the three variants, with the real text engine.
bench-layout:
	WHICH=layout $(SBCL) $(REGISTRY) --load bench/run.lisp

# The cursor experiment (record 0022) is archived: it is not in make test. These run its tests,
# its sweep and its pictures.
test-cursor:
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-variant-cursor")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-7guis/cursor/tests")'

# The cursor experiment (record 0022): the layout spike and the dialog screen in the cursor
# front end in each mode, against the box tree, with the fake text and with the real engine.
bench-cursor:
	WHICH=cursor $(SBCL) $(REGISTRY) --load bench/run.lisp

# A picture of each task of a variant after a few events: make snapshots VARIANT=immediate OUT=/tmp/shots/
snapshots:
	VARIANT=$(VARIANT) OUT=$(OUT) sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/snapshot-apps.lisp

# Pictures of the cursor experiment (record 0022): make snapshots-cursor OUT=/tmp/shots/
snapshots-cursor:
	OUT=$(OUT) sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/snapshot-cursor.lisp

# An executable of a variant with its apps: make image VARIANT=immediate, then
# TASK=counter build/immediate (MEASURE=1 reports to the process that started it).
image:
	VARIANT=$(VARIANT) sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/build-image.lisp

# A 7GUIs task of a variant in a window: make 7guis VARIANT=retained TASK=counter
# LOG=1 prints the presses, releases and keys that reach it and what is under the
# pointer. RECORD=session.script writes what you do as a replay script, after each press,
# release and key (make replay SCRIPT=session.script plays it back). 7guis-smoke opens it, draws,
# waits and reports, with nobody at the window, and says whether a key typed would reach it (the view
# is the first responder, the window is the key window and the application is active).
# HOT=1 makes the app again, in the same window, when code is compiled into the process or a
# source file is saved (decision records 0030 and 0031); the state the app keeps (kept-let) stays.
7guis:
	VARIANT=$(VARIANT) TASK=$(TASK) LOG=$(LOG) RECORD=$(RECORD) HOT=$(HOT) CURSOR_MODE=$(CURSOR_MODE) $(SBCL_APP) $(REGISTRY) --load apps/7guis/run.lisp

# Play a replay script back with the real text engine and no window, and say what happened:
#   make replay SCRIPT=apps/7guis/replay/counter-clicks.script [VARIANT=reactive] [FREEZE=golden.script]
# VARIANT plays a script in another variant than the one it was made in. FREEZE plays the
# script twice, and if the frames are the same, writes a copy with a checkpoint after each
# event that holds the hash of the frame there, to check later runs against.
replay:
	SCRIPT=$(SCRIPT) VARIANT=$(VARIANT) FREEZE=$(FREEZE) sbcl $(HEAP) --noinform --non-interactive --no-sysinit --no-userinit --eval '(require :asdf)' --eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))' --load scripts/replay.lisp

7guis-smoke:
	SMOKE=1 VARIANT=$(VARIANT) TASK=$(TASK) CURSOR_MODE=$(CURSOR_MODE) $(SBCL_APP) $(REGISTRY) --load apps/7guis/run.lisp

# What an editor's REPL does: open the task in a window and get the REPL back, compile a
# definition and see the app made again, close the window and keep the Lisp, open it again.
# It is judged by what it says, not by its exit code: AppKit's quit ends a process with code 0.
7guis-repl-smoke:
	@out=$$(mktemp); \
	VARIANT=$(VARIANT) TASK=$(TASK) CURSOR_MODE=$(CURSOR_MODE) $(SBCL_APP) $(REGISTRY) --load apps/7guis/repl-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The REPL check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# Spike E: 2000 requested frames of text (about 35 s), then a window left alone for 10 s.
spike-e:
	$(SBCL_APP) $(REGISTRY) --load spikes/e-frame-loop/run.lisp

clean:
	find . -name '*.fasl' -not -path './vendored/*' -delete
