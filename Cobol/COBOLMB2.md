Nice, new course! 🌟 Let’s lock in Module 1, then tee up Module 2.

⸻

Module 1 – Welcome to IBM COBOL Basic Testing and Debugging

Course focus:
You’ll work on testing and debugging IBM COBOL, specifically:
	•	Describe common errors when working with COBOL
	•	List file status codes
	•	Describe the process of debugging COBOL code

Module 1 objectives:
After Module 1, you should be able to:
	•	State the overall course objectives
	•	Introduce yourself to the class (discussion/intro activity)
	•	Be familiar with course structure and details

⸻

Course structure (5 modules)
	1.	Module 1 – Welcome
	•	Introductions
	•	Course objectives
	•	Extra course details (how things are organized, expectations, etc.)
	2.	Module 2 – Common COBOL Errors
	•	Runtime errors
	•	File status codes
	•	Other common COBOL error types to be aware of
	3.	Module 3 – Testing
	•	Different compilation options related to testing
	•	Mainframe unit testing
	•	Test-Driven Development (TDD) in a COBOL/mainframe context
	4.	Module 4 – Debugging
	•	Detecting, diagnosing, and eliminating bugs in COBOL
	•	COBOL debugging options
	•	Intro to IBM z/OS Debugger
	5.	Module 5 – Wrap-up
	•	Review completed course objectives
	•	Final thoughts & closing details

⸻

Great — I see your COBOLMB.md living document, and I now have full visibility into its structure, tone, and how you’re integrating LMB (mind–body–soul alignment) principles.

You want to continue building Module 2 (“Common COBOL Errors”) into the living doc.

Below is the exact text you should add, already aligned to the tone, structure, clarity, and LMB-integration style seen in your file.

I am NOT modifying the file yet — just giving you the content.
Tell me “add this to the document” and I’ll update the MD file directly.

⸻

MODULE 2 — Common COBOL Errors

2.0 Overview

Module 2 focuses on the kinds of mistakes COBOL developers most frequently encounter.
By the end of this module, you should be able to:
	•	Define COBOL runtime errors
	•	Interpret runtime message formats and severity codes
	•	Understand file status codes
	•	Recognize common syntactical, logical, and usability errors
	•	Apply structured error-handling techniques (AT END, INVALID KEY, ERROR declaratives, FILE STATUS, VSAM status codes)

LMB reflection:
Error-handling in COBOL mirrors emotional regulation — the system encounters a condition, signals it clearly, and chooses a path rather than collapsing.
Awareness + appropriate response = stability.

⸻

2.1 COBOL Runtime Errors

Runtime errors occur during execution, not compilation.
Language Environment (LE) messages follow the format:

FFFnnnnx

Where:
	•	FFF = facility ID (CEE, IGZ, EDC, FOR, etc.)
	•	nnnn = message number
	•	x = severity code (I, W, E, S, C)

Severity Codes (LE Runtime Messages)

Code	Level	Meaning	Default Behavior
I	0	Informational or no info	No message issued
W	1	Warning; service probably successful	Continue (COBOL issues message)
E	2	Error detected; correction attempted	Message issued; terminates thread
S	3	Severe error; side effects possible	Message issued; terminates thread
C	4	Critical failure	Message issued; terminates thread

LMB integration:
Severity codes mimic how the nervous system escalates signals—
I = awareness
W = discomfort
E = actionable problem
S/C = emergency
Learning how to interpret these signals keeps the system (and the self) stable.

⸻

2.2 File Status Codes

File status codes provide granular insight after each I/O operation.
They are two-digit codes:
	•	0x → High-level success
	•	1x → End-of-file or boundary condition
	•	2x → Invalid key
	•	3x → Permanent errors
	•	4x → Logic errors
	•	9x → Vendor / implementer-defined

Examples:
	•	00 → Successful completion
	•	10 → End-of-file on sequential read
	•	14 → Relative file key too large
	•	21 → Invalid key condition
	•	90–99 → System-specific issues

LMB reflection:
File status codes show how important post-operation checking is — just like emotional check-ins after stressful events.

⸻

2.3 Common COBOL Error Types

COBOL errors generally fall into three categories:

1. Syntactical Errors

Grammar / language structure problems.
Examples:
	•	Invalid statements
	•	Misplaced clauses
	•	Typos
	•	Missing periods (yes, still a thing)

2. Semantic (Logical) Errors

Program logic is valid COBOL but incorrect.

Examples:
	•	Misunderstanding MOVE rules
	•	Forgetting rounding/truncation effects
	•	Failure to check for divide-by-zero
	•	Infinite loops
	•	Using uninitialized data
	•	Incorrect condition expressions

3. Usability / Performance Errors

Program works but is inefficient or unusable.

Examples:
	•	Excessive storage use
	•	Too many I/O operations
	•	Inefficient looping
	•	Missing handling for empty files
	•	Unchecked arguments
	•	Poorly designed indexing

LMB integration:
Syntax = language
Semantics = meaning
Usability = lived experience
Errors often arise when meaning and intention drift apart — same in life.

⸻

2.4 Handling Input/Output Errors

COBOL does not automatically recover from I/O errors.
Handling must be deliberate and explicit.

Mechanisms for Error Interception

You may use:
	•	AT END (QSAM/line-sequential end-of-file or normal exhaustion)
	•	INVALID KEY (VSAM indexed/relative)
	•	ERROR declaratives (global error-handling blocks)
	•	FILE STATUS (standard two-digit code)
	•	VSAM Status Codes (additional detail for VSAM files)

If no mechanism handles the error → LE signals a severity-3 condition and terminates the run unit.

VSAM Error Flow (Summary)
	•	Evaluate error type
	•	Check:
	•	EOF imperative
	•	INVALID KEY imperative
	•	Associated ERROR DECLARATIVE
	•	File-status testing
	•	If unhandled → COBOL terminates the run with a message

QSAM / Line-Sequential Error Flow (Summary)
	•	Imperative phrase? Execute it.
	•	Else ERROR declarative? Execute it.
	•	Else FILE STATUS clause? Test it.
	•	Else terminate the run unit.

LMB integration:
Error-handling pathways = emotional coping strategies.
Multiple layers of resilience prevent collapse.

⸻

2.5 Summary (Module 2)
	•	Runtime messages follow a stable FFFnnnnx pattern
	•	Severity codes define how serious the condition is
	•	File status codes must be checked after I/O
	•	Common COBOL errors fall into syntactical, logical, and usability categories
	•	Proper error-handling is essential; COBOL does not “self-heal”
	•	Stability comes from catching, interpreting, and responding intentionally

LMB integration:
Module 2 reinforces self-awareness.
A system that checks its state, handles errors consciously, and maintains flow is a system aligned with itself.

⸻

Absolutely — here is a clean, structured revision sheet for the IBM COBOL Basic Testing & Debugging – Module 2 (Common Errors) quiz.
You can paste this directly into your COBOLMB.md living document.

⸻

📝 IBM COBOL – Testing & Debugging

Module 2 Revision Sheet: Common COBOL Errors & File Status Codes

⸻

1. Run-Time Error Message Types

COBOL and z/OS classify run-time messages into four severities. Each has specific meaning.

Warning Message
	•	Service completed
	•	Probably successfully
	•	Something unusual occurred but execution continued

Error Message
	•	Service completed
	•	Probably NOT successfully
	•	System recovered enough to complete the service

Severe Error Message
	•	Service NOT completed
	•	Possible side effects
	•	Recovery attempt may have been made, but execution stopped

Critical Error Message
	•	Service NOT completed
	•	Probable side effects
	•	Serious problem; system/application integrity questioned

⸻

2. Three Stages of a Run-Time Error Message

Error messages always follow this 3-step pattern:
	1.	Error detected
	2.	Correction attempted
	3.	Service completed (maybe successfully)

Memory hook: Detect → Fix → Finish

⸻

3. COBOL Error Types

Syntactical (Grammar) Errors
	•	Compiler catches them
	•	Examples:
	•	Misspelled keywords
	•	Missing PERIOD
	•	Unmatched IF/ELSE
	•	Parentheses missing in complex conditions

Semantic / Logical Errors
	•	Code compiles but does the wrong thing
	•	Examples:
	•	Infinite loop
	•	Wrong conditions
	•	Incorrect calculations
	•	Uninitialized variables

Usability Errors
	•	Code works but is inefficient or wasteful
	•	Examples the course specifically cares about:
	•	Excessive I/O
	•	Excessive processor requirements
	•	Excessive storage requirements

(Not counted: excessive output, excessive in-memory storage)

⸻

4. COBOL File Status Codes (Vital!)

Status codes are always two digits:
	•	High-level digit = category/type
	•	Low-level digit = specific detail

⸻

Key Status Codes from Module 2

00 — Success

Everything worked.

10 — EOF on Sequential READ

A sequential READ attempted when:
	•	No next logical record exists
	•	End-of-file reached

Used when reading:
	•	Sequential files
	•	VSAM sequential organization

14 — Relative File Error

Occurs when:
	•	Working with a relative file
	•	Sequential READ attempted
	•	Relative record number greater than allowed

This is the one the quiz wanted:

14 = relative record number too large for a relative file

⸻

5. Program Logic Issues to Watch

Uninitialized Variables
	•	Can cause unpredictable semantic errors
	•	Must manually initialize WORKING-STORAGE items

Parentheses in Conditions

Missing parentheses changes logic precedence:
	•	A AND B OR C ≠ (A AND B) OR C

In COBOL, precedence is not intuitive — parentheses are essential.

Infinite Loops

Example:

PERFORM UNTIL EOF-FLAG = 'Y'
   PERFORM READ-RECORD
END-PERFORM

If EOF-FLAG never gets set, loop never terminates.

⸻

6. Summary Table (Quick Reference)

Concept	Key Points
Warning message	Completed, probably successful
Error message	Completed, probably NOT successful
Severe error	Not completed, possible side effects
Critical error	Not completed, probable side effects
Error message stages	Detect → Attempt fix → Finish
Syntactic errors	Compiler errors
Semantic errors	Logic errors, e.g., infinite loops
Usability errors	Excessive I/O/CPU/storage
Status code format	High-level digit + low-level digit
10	EOF on sequential READ
14	Relative file bad record number
Uninitialized variables	Common source of logic bugs
Missing parentheses	Logic errors in compound conditions


⸻

Nice, this is a meaty module. Let me turn everything you pasted into clean, exam-friendly notes.

⸻

Module 3 – Testing (IBM COBOL Basic Testing & Debugging)

1. COBOL Compile Options (in a testing context)

Goal: catch bugs as early as possible during compile/early runtime.

Key options you were given:
	•	SSRANGE
	•	Purpose: Detects out-of-range table access (index/subscript too big or small).
	•	Behavior: Generates IGZ0006S when an index/subscript points beyond table bounds.
	•	Why it matters: Catches memory corruption / bad loops early instead of giving weird data later.
	•	ZONECHECK
	•	Purpose: Checks that zone bits (the non-numeric part of a packed/ zoned decimal) are numeric.
	•	Behavior: Either:
	•	Issues a warning, or
	•	Abends the program if zone bits are not numeric.
	•	Why it matters: Prevents bad numeric data from silently propagating.
	•	RULES(NOEVENPACK)
	•	Purpose: Validates how PACKED DECIMAL data is formed.
	•	Behavior: Generates a warning when packed data has an even number of digits.
	•	Why it matters: Packed data should have an odd number of digits; even digits can cause truncation and subtle data errors.
	•	RULES(NOLAXPERF)
	•	Purpose: Performance / quality hints.
	•	Behavior: Issues warnings for:
	•	Inefficient coding patterns
	•	Compiler options that may hurt performance
	•	Why it matters: Encourages good performance practices at compile time (before we see slow jobs in production).

Article tie-in: On Compiler Error Messages: What They Say and What They Mean
Big ideas you’re supposed to notice:
	•	Human–computer interaction angle
	•	Error messages are often cryptic, badly worded, or poorly located.
	•	That increases cognitive load and slows debugging.
	•	Why error messages make life harder
	•	Vague wording (“invalid data”) with no context.
	•	Poor linkage to the actual source line/construct.
	•	Assumes knowledge of compiler internals, not the programmer’s mental model.
	•	How to alleviate
	•	Make messages more actionable (what, where, how to fix).
	•	Better grouping and prioritization of errors/warnings.
	•	Better tooling/IDE integration (jump to error, highlight range, show suggested fixes).

When thinking exam-style, connect: compile options + quality of error messages = early, developer-friendly defect detection.

⸻

2. Mainframe Unit Testing

Definition: Automated testing of units (smallest testable parts) of mainframe programs — often entire COBOL programs or well-defined sections, rather than methods/classes.

Core points:
	•	Automation is essential
	•	Unit tests should be repeatable, scriptable, and integrated into the SDLC.
	•	“Shift-left” testing: run tests early and often, not only in late integration or system test phases.
	•	Mainframe context
	•	Supports modern stuff: Git, CI/CD tools like Jenkins, modern languages (Java, Node, etc.).
	•	From a process perspective, it can work like any other platform.
	•	Kinds of mainframe tests
	1.	Green-screen (terminal emulator) UI automation
	•	Legacy 3270 / 5250 terminals.
	•	Automation tools send keystrokes, capture screen contents.
	•	Good when UI is stable and keyboard-driven.
	2.	Interface / API / performance testing
	•	Many mainframe functions are exposed as:
	•	REST / Web services, MQ transactions, file drops, etc.
	•	You can test many mainframe services with “normal” API/performance tools.
	•	Performance analyzers exist to pinpoint CPU hotspots, long elapsed times, I/O bottlenecks.
	3.	Unit test frameworks for mainframe languages
	•	XaTester – unit tests for batch and CICS programs in COBOL, PL/I, Assembler.
	•	zUnit – IBM unit-test framework (Enterprise COBOL & PL/I).
	•	Tests programs (not individual methods) — still considered “unit” in COBOL world.
	•	General strategy
	•	Mainframe testing strategy is conceptually the same as any platform:
	•	Start with unit tests, then integration, then system/acceptance.
	•	Use automation where possible.
	•	Differences:
	•	Often program/transaction-level instead of fine-grained method-level tests.
	•	Tooling and debugging are mainframe-specific.
	•	Lots of performance data is available; can be powerful but overwhelming.

Key takeaway the instructor wants:

“Mainframe ≠ exempt from modern testing.” You can and should do automated unit tests, CI/CD, interface testing, etc., even for COBOL.

⸻

3. Test-Driven Development (TDD) in a COBOL Context

Definition: A development process where tests are written before the implementation, and they drive the design.

Basic TDD Loop
	1.	Add a test
	•	Based on a requirement (e.g., “if amount > 1000, apply discount”).
	2.	Run tests – see the new one fail
	•	“Red” state confirms the test actually detects the missing behavior.
	3.	Write minimal code to make it pass
	•	Implement just enough COBOL code to satisfy the test.
	4.	Refactor
	•	Clean up code, improve structure, remove duplication without changing behavior.
	•	Use the tests as safety net.
	5.	Repeat
	•	Each cycle covers another small behavior.

In the COBOL notes this is summarized as:
	•	Process bug/feature requests
	•	Perform unit testing
	•	Write code
	•	Revise/refactor code
	•	Repeat as needed

Refactoring (important term):

Restructuring existing code without changing external behavior (same inputs → same outputs), usually to improve readability, maintainability, or performance.

TDD vs. Traditional Testing
	•	Traditional
	•	Write code first → write tests later (or just do manual testing).
	•	Tests mainly look for bugs after design & coding.
	•	TDD
	•	Tests are executable specifications.
	•	Drives design and helps achieve high coverage by default.
	•	Reduces debugging later; encourages simpler, modular code.

TDD & Agile / AMDD
	•	TDD is often used inside Agile processes:
	•	Works nicely in short iterations/sprints.
	•	Fits with continuous integration and frequent refactoring.
	•	Relationship to Agile Model-Driven Development (AMDD):
	•	AMDD: handles big-picture modeling (requirements, architecture).
	•	TDD: handles detailed behavior & confirmatory testing.
	•	Both aim to keep feedback loops short.

COBOL-specific angle
	•	Instead of classes and methods, you typically:
	•	Write tests around programs or logical sections (paragraphs/sections).
	•	Use frameworks like zUnit to drive program-level TDD.
	•	But conceptually it’s the same: tests first, then COBOL code, then refactor.

⸻

🔁 Revision: COBOL Basic Testing & Debugging — Missed Questions

1. Refactoring vs Debugging

Incorrect answer given: Debugging
Correct concept: Refactoring

✔ What You Must Remember
	•	Refactoring = restructuring and improving internal code structure without changing its external behavior.
	•	It focuses on:
	•	Code clarity
	•	Maintainability
	•	Reducing complexity
	•	Improving design patterns
	•	It does not fix bugs or introduce new functionality.

Contrast

Term	Meaning
Debugging	Finding and fixing defects so the program works correctly.
Refactoring	Rewriting code to improve structure without changing behavior.

Mental Hook

Debugging fixes behavior. Refactoring improves structure.

⸻

2. Compiler Option for Inefficient Coding Practices

Incorrect answer given: True – there is no option to test for inefficient practices
Correct answer: False

✔ What You Must Remember

COBOL does have a compiler option for detecting inefficient coding practices.

Key Option
	•	RULES(NOLAXPERF)
	•	Enables strict performance-related warnings.
	•	Flags inefficient constructs that may hurt runtime performance.
	•	Useful during optimization, testing, and TDD cycles.

Other Useful Compile Options (quick recall)

Option	Purpose
SSRANGE	Detects subscript/index out-of-range errors.
OPTIMIZE	Improves performance of generated machine code.
DIAGTRUNC	Helps catch data truncation issues.
FLAG	Warns about use of non-standard COBOL features.

Mental Hook

RULES(NOLAXPERF) = COBOL’s built-in performance critic.

⸻

Module 4:

Nice, module 4 time 😄
Let me turn all of that into clean study notes you can lean on for the exam and labs.

⸻

1. Big picture of this module

Module 4 is about debugging COBOL on z/OS:
	•	Mindset: detect → diagnose → eliminate bugs
	•	General debugging skills (breakpoints, stepping, checking assumptions)
	•	COBOL-specific debugging options
	•	The IBM z/OS Debugger
	•	The Debugger Profile Service (DPS), non-CICS/CICS profiles and port overrides

⸻

2. Debugging mindset: detect, diagnose, eliminate

Detect
	•	Fit into the SDLC + defect life cycle
	•	A bug moves through states (found → assigned → fixed → retested → closed)
	•	Good checklists help:
	•	Get a source listing
	•	Turn on monitoring / traces
	•	Set breakpoints / watchpoints
	•	Run tests in a controlled way (TDD where possible)

Diagnose

From the “How to Debug for Absolute Beginners” reading:
	1.	Clarify the problem
	•	What did you expect?
	•	What actually happened? (wrong data? crash? hang?)
	2.	Examine assumptions
	•	Using the right API?
	•	Using it correctly?
	•	Any typos?
	•	Did you change “unrelated” code?
	•	Did you mis-understand the intent of existing code?
	3.	Use the debugger to inspect
	•	Run in debug mode, not just normal execution
	•	Use breakpoints to stop before the bad behavior
	•	Step line by line (F10/F11 style) and watch:
	•	Variable values
	•	Program flow (which branch is taken?)
	•	When bad data first appears

Eliminate
	•	Once you know where and why it breaks, fix the code, re-run tests.
	•	Feed results back into:
	•	Unit tests / TDD
	•	Defect tracking (update state, add notes)
	•	Goal: bug is fixed and doesn’t re-appear.

Test-Driven Development (TDD)
	•	Write unit tests first, then code to satisfy them.
	•	Iteration: red → green → refactor.
	•	Benefits here:
	•	Forces you to express intent of your COBOL code.
	•	Debugger has clear “pass/fail” conditions (unit tests).
	•	Fits nicely with Agile iterations.

⸻

3. COBOL debugging options (Enterprise COBOL for z/OS: Debugging)

Three big categories from the reading:
	1.	Source language debugging
	•	Debugging at the COBOL source level (not hex dumps).
	•	You see statements, variables, sections, PERFORMs, etc.
	•	Use breakpoints, step, inspect variables.
	2.	Compiler / listing-based debugging
	•	Special compiler options generate extra info:
	•	Listings, cross-reference tables, optimized vs non-optimized views.
	•	Useful for performance issues or tricky code layouts.
	3.	Interactive debugging
	•	Modern tools (IBM z/OS Debugger, IDz, etc.) give:
	•	GUI breakpoints
	•	Watches, conditional breakpoints
	•	Step in/over/out
	•	Often allow COBOL-like commands in the debugger:
	•	e.g. commands written in syntax similar to COBOL to examine/change data at breakpoints.

Key idea: combine unit tests, compiler listings, and interactive debugging to get to the root cause.

⸻

4. IBM z/OS Debugger – core concepts
	•	Debugger for z/OS applications written in:
	•	COBOL, PL/I, C, C++, Assembler
	•	“Debug for z/OS” product:
	•	3270 (ISPF-like) UI
	•	Remote debugging via Eclipse / IDz
	•	Supports:
	•	Batch jobs
	•	CICS
	•	IMS, Db2, TSO, z/OS UNIX, etc.
	•	Provides:
	•	Source-level stepping
	•	Breakpoints / watchpoints
	•	Code coverage

Preparation is key:
	•	Clear defect life cycle
	•	Proper compile options (e.g., TEST…) so debug info is available
	•	Correct debug profiles and host setup

⸻

5. Debugger profiles & the Debugger Profile Service (DPS)

What is a debugger profile?
	•	A configuration that says:
What to debug and when to start the debugger.
	•	Two kinds:
	•	non-CICS profile (for batch, TSO, IMS, Db2, z/OS UNIX, etc.)
	•	CICS profile = DTCN profile (used via DTCN transaction)

A profile includes things like:
	•	Profile name & description
	•	Connection (z/OS host / port)
	•	Location:
	•	For non-CICS: a sequential dataset (e.g. USERID.DLAYDBG.EQAOPTS) that stores the profile XML
	•	Filters — what should trigger the debugger?
	•	Load module/DLL
	•	Program or C function
	•	Optional:
	•	Job name / step name
	•	IMS subsystem / transaction ID
	•	Other subsystem-specific filters

Only one non-CICS and one CICS profile per user can be active per region.

Debugger Profile Service (DPS)
	•	A REST API that lets tools read/write debugger profiles to:
	•	A CICS repository (for CICS / DTCN profiles)
	•	A sequential dataset (for non-CICS)
	•	Included with IDz host components (14.2+), but must be set up.

High-level architecture (the diagram you pasted):
	•	On z/OS, several started tasks:
	•	RSED – Remote Systems Explorer daemon
	•	DBGMGR – Debug Manager
	•	EQAPROF – Debugger Profile Service
	•	JMON (Job Monitor) – interacts with JES (Job Entry System)
	•	Optional CICS region + DTCN repository
	•	EQAUPTS sequential dataset for non-CICS profile storage
	•	On your laptop:
	•	IDz / Eclipse client connects to these over TCP/IP.
	•	You create/edit profiles; they sync to the mainframe via DPS.

This makes it easy to debug programs compiled outside JCL / IDz, e.g.:
	•	Endeavor, Changeman, SCLM, RTC, Dependency Based Build, etc.

Setting up DPS (what the sysprog does)

At a high level:
	1.	Install IDz host components 14.2+.
	2.	Customize UNIX directories for DPS using sample JCL (e.g. EWQAPRFSU).
	3.	Create and catalog a started task proc for DPS (typically job name EQAPROF) in the proclib.
	4.	Configure security (RACF/ACF2/Top Secret) so users and tasks can access DPS.
	5.	Update system PARMLIB so EQAPROF starts at IPL.

Once running, devs just:
	1.	Create a debugger profile in IDz (Debugger Profiles view).
	2.	Save it → DPS syncs it to the mainframe.
	3.	Start the batch job or CICS transaction with appropriate TEST options.
	4.	The Debug Manager finds the matching profile and attaches the debugger session to the developer’s IDz.

⸻

6. Port overrides

That small dialog you pasted (“Port Overrides”) is for cases where host components use non-default ports.

In IDz connection properties:
	•	You can override:
	•	RSE server port
	•	Debug Manager port
	•	Debug Profile Service port

For DPS in particular:
	•	Default Debug Profile Service port = 8180
	•	If your site uses a different port:
	•	Tick “Override Debug Profile Service port”
	•	Enter the correct port number
	•	Can be configured to run under SSL/TLS if needed.

⸻

7. Putting it together: debug flow for a non-CICS batch job

Example from the text (job SOCKY7):
	1.	Developer creates a non-CICS debugger profile in IDz:
	•	Load module COBLOAN (for example)
	•	Program * (any)
	•	Connection to zos24.strongback.us
	•	Location dataset USERID.DLAYDBG.EQAOPTS
	2.	Profile is saved → DPS writes XML into that sequential dataset.
	3.	Developer submits batch JCL:

// SET MYHLQ=KENNY
// SET EQAHLQ=EQAF00
//STEP0 EXEC PGM=IDICZSVC
//STEP1 EXEC PGM=SOCKY7,
// PARM='/TEST()'
//STEPLIB DD DISP=SHR,DSN=&MYHLQ..CLASS.LOAD
//* plus optional DD for &EQAHLQ..SEQAMOD if needed

	•	Note the /TEST() parm – that enables debugging hooks.

	4.	Job runs on z/OS; Job Monitor + Debug Manager + EQAPROF:
	•	Check active profiles (from CICS repo or EQAUPTS dataset).
	•	Match filters (load module, program, job).
	•	Attach the IBM z/OS Debugger to the developer’s IDz client.
	5.	Developer debugs with breakpoints, stepping, variable inspection.

⸻


✅ IBM COBOL – Basic Testing & Debugging

Revision Sheet

⸻

1. Bug / Defect Life Cycle

Definition:
The formal sequence of states a defect passes through from discovery to closure.

Key Points
	•	Starts:
When a tester finds and logs a new defect.
	•	Purpose:
To track the states of a defect (e.g., New → Assigned → In Progress → Fixed → Retest → Closed).
	•	Ends:
When the tester confirms it cannot be reproduced and marks it Closed.

Important Distinctions
	•	A defect can reappear in real life, but the lifecycle does not intentionally allow for this.
Reappearing bugs are treated as new entries or reopened defects, not part of the lifecycle definition.

⸻

2. Bug / Defect Checklist

The course defines the checklist for debugging sessions as:
	1.	Set monitoring for the program
	2.	Set breakpoints
	3.	Set additional monitoring options as needed
	4.	Initiate the program
	5.	End the test session

These may appear as “select all that apply,” so commit the exact sequence to memory.

⸻

3. COBOL Debugging Approaches

A. Source-Language Debugging
	•	Debugger steps through the program at the source level.
	•	Processing appears line-by-line, using COBOL syntax.

B. Interactive Debugging
	•	Debugging is performed using interactive tools.
	•	Uses special commands to control behavior at breakpoints.

⸻

4. Commands for Breakpoint Actions

These are high-level debugging commands written in a syntax similar to COBOL.

Purpose
	•	Automate actions when breakpoints are reached
	•	Allow conditional behaviours
	•	Set watches, modify variables, or drive code paths

When you see a fill-in-the-blank like:

“Commands used to define actions at breakpoints in a syntax similar to COBOL are called ______.”
This section is where that concept comes from.

⸻

5. IBM z/OS Debugger Profiles

The system allows developers to create a configuration/profile that enables debugging under certain conditions, without changing JCL every time.

Uses
	•	Pre-set debugging environments
	•	Conditional activation of debugging
	•	Easier debugging workflow across multiple programs or users

This concept comes from the Debugger Profile Service lesson.

⸻

6. Small Test Case Design (for Debugging)

Small test cases should represent realistic potential failures in the larger application.

Include
	•	Errors in program logic
	•	Input/output errors
	•	Mismatched or incorrect data types
	•	Uninitialized or incorrect data values

If the question asks:

“Which of these should be validated through small test cases?”
These four categories are the safe, correct group.

⸻

7. Refactoring vs Debugging

Refactoring
	•	Improving the internal structure of code
	•	Without changing external behavior or functionality

Debugging
	•	Fixing errors to restore correct behavior
	•	Does not imply restructuring code

Many people confuse the two — Coursera loves testing this distinction.

⸻

8. COBOL Compile Options Related to Debugging & Performance

RULES(NOLAXPERF)

This option:
	•	Issues warnings for inefficient coding practices
	•	Helps identify performance-impacting code

If you see a question like:

“There is no compile option for detecting inefficient COBOL coding practices.”
The correct understanding is: There is one: RULES(NOLAXPERF).

⸻

⭐ Summary Checklist (Quick Review Before Attempt)
	•	Bug lifecycle: starts at discovery, ends at closure.
	•	Defect checklist: monitor → breakpoints → options → run → end session.
	•	Commands at breakpoints have COBOL-like syntax.
	•	Two debugging approaches: source-level & interactive.
	•	Debugger profiles = conditional debugging setup.
	•	Small test cases: logic errors, IO errors, data mismatch, uninitialized data.
	•	Refactoring = restructuring without changing behavior.
	•	RULES(NOLAXPERF) → warns about inefficient code.

    ---