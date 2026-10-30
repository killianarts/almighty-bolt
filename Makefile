.PHONY: deps test bench-null image bench-run bench-overhead bench-app bench-compile bench-report snapshots replay 7guis 7guis-smoke 7guis-repl-smoke 7guis-hot-smoke examples-smoke smoke clean

# ASDF looks only in this tree (project and vendored/), never in ~/common-lisp/.
SBCL = sbcl --noinform --non-interactive --no-sysinit --no-userinit
REGISTRY = --eval '(require :asdf)' \
	--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))'

# Optional runtime heap size, for example HEAP='--dynamic-space-size 4096'.
HEAP =
SBCL_APP = sbcl $(HEAP) --noinform --disable-debugger --no-sysinit --no-userinit

# vend stops on float-features naming a system only DotCL needs (dotcl-float).
# The converter guide's remedy is to take it out of the vendored .asd and go on.
deps:
	vend get || { sed -i '' 's/ (:feature :dotcl (:require :dotcl-float))//' vendored/float-features/float-features.asd && vend get; }

# Integration and end-to-end tests, the only kind for now (the charter's Testing section): the
# real operating system, text engine and GPU, and the apps driven by events and replay scripts. No
# window is opened; make smoke does that.
test:
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-7guis/acceptance")'
	$(SBCL) $(REGISTRY) --eval '(asdf:test-system "almighty-bolt-stress/acceptance")'

# The window checks, which need a window server (a logged-in Mac) and nobody at the keyboard: the
# app opens, draws and rests; the REPL entry; a saved file made into the app again with its state.
smoke:
	$(MAKE) 7guis-smoke VARIANT=immediate TASK=counter
	$(MAKE) 7guis-repl-smoke VARIANT=immediate TASK=crud
	$(MAKE) 7guis-hot-smoke
	$(MAKE) examples-smoke

# The null variant in a window: the core's own cost. Run it from a terminal of your
# own and leave the window alone while it runs.
bench-null:
	$(SBCL_APP) $(REGISTRY) --load bench/null/run.lisp

# The benchmark protocol on the example replay scripts: REPS repetitions (10)
# after WARMUPS (3), a result record for each in results/ (OUT=dir), never edited, only
# superseded. bench-report prints the latest record of each run.
bench-run:
	WHICH=results $(SBCL) $(REGISTRY) --load bench/run.lisp

# What the instrumentation costs: the phases and build timing on and off, on the example scripts
# and on the null variant through the host. Written to results/ as a record.
bench-overhead:
	WHICH=overhead $(SBCL) $(REGISTRY) --load bench/run.lisp

# Startup, idle CPU and frames, memory and image size of the executable running each
# task in a window (make image first). TASKS=counter,cells IDLE=10 REPS=5.
bench-app:
	WHICH=app $(SBCL) $(REGISTRY) --load bench/run.lisp

# How long the library takes to compile from nothing and after a change to one file, in new
# processes with empty fasl caches (REPS=3).
bench-compile:
	WHICH=compile $(SBCL) $(REGISTRY) --load bench/run.lisp

bench-report:
	WHICH=report $(SBCL) $(REGISTRY) --load bench/run.lisp

# A picture of each task of a variant after a few events: make snapshots VARIANT=immediate OUT=/tmp/shots/
snapshots:
	VARIANT=$(VARIANT) OUT=$(OUT) sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/snapshot-apps.lisp

# An executable of a variant with its apps: make image VARIANT=immediate, then
# TASK=counter build/immediate (MEASURE=1 reports to the process that started it).
image:
	VARIANT=$(VARIANT) sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/build-image.lisp

# A 7GUIs task of a variant in a window: make 7guis VARIANT=immediate TASK=counter
# LOG=1 prints the presses, releases and keys that reach it and what is under the
# pointer. RECORD=session.script writes what you do as a replay script, after each press,
# release and key (make replay SCRIPT=session.script plays it back). 7guis-smoke opens it, draws,
# waits and reports, with nobody at the window, and says whether a key typed would reach it (the view
# is the first responder, the window is the key window and the application is active).
# HOT=1 makes the app again, in the same window, when code is compiled into the process or a
# source file is saved (see the devlog, 2026-10-28 and 2026-10-29); the state the app keeps (kept-let) stays.
7guis:
	VARIANT=$(VARIANT) TASK=$(TASK) LOG=$(LOG) RECORD=$(RECORD) HOT=$(HOT) $(SBCL_APP) $(REGISTRY) --load apps/7guis/run.lisp

# Play a replay script back with the real text engine and no window, and say what happened:
#   make replay SCRIPT=apps/7guis/replay/counter-clicks.script [VARIANT=immediate] [FREEZE=golden.script]
# FREEZE plays the script twice, and if the frames are the same, writes a copy with a checkpoint
# after each event that holds the hash of the frame there, to check later runs against.
replay:
	SCRIPT=$(SCRIPT) VARIANT=$(VARIANT) FREEZE=$(FREEZE) sbcl $(HEAP) --noinform --non-interactive --no-sysinit --no-userinit --eval '(require :asdf)' --eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))' --load scripts/replay.lisp

7guis-smoke:
	SMOKE=1 VARIANT=$(VARIANT) TASK=$(TASK) $(SBCL_APP) $(REGISTRY) --load apps/7guis/run.lisp

# What an editor's REPL does: open the task in a window and get the REPL back, compile a
# definition and see the app made again, close the window and keep the Lisp, open it again.
# It is judged by what it says, not by its exit code: AppKit's quit ends a process with code 0.
7guis-repl-smoke:
	@out=$$(mktemp); \
	VARIANT=$(VARIANT) TASK=$(TASK) $(SBCL_APP) $(REGISTRY) --load apps/7guis/repl-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The REPL check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# What saving a file does, in a real window: the CRUD app is used, its file is saved with a change,
# and the app must be made again with its entries, the keyboard and the caret where they were
# (apps/7guis/hot-smoke.lisp). Judged by what it says, as 7guis-repl-smoke is. It writes
# apps/7guis/crud/immediate.lisp and puts it back.
7guis-hot-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load apps/7guis/hot-smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The hot reload check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

# The same for a program that uses only the public package: run-app on the main thread, the example
# used, redefined while it runs, and closed (examples/smoke.lisp). Judged by what it says.
examples-smoke:
	@out=$$(mktemp); \
	$(SBCL_APP) $(REGISTRY) --load examples/smoke.lisp > $$out 2>&1; \
	grep -v -e '^;' -e '^$$' $$out; \
	grep -q "The run-app check passed" $$out && ! grep -q "FAIL" $$out; status=$$?; rm -f $$out; exit $$status

clean:
	find . -name '*.fasl' -not -path './vendored/*' -delete
