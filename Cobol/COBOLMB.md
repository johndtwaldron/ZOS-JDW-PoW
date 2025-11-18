# COBOL.LMB.md

Living document for growing COBOL skills as part of Project LMB  
(aligning mainframe pedigree, modern DevOps, and long-term Bitcoin/finance goals).

---

## 1. Why COBOL (for JDW)

- COBOL is still the backbone of **core banking and payments** – exactly where I’ve already operated via MQ and z/OS.
- I bring 7+ years of **IBM MQ for z/OS** experience: debugging, dumps, SMP/E, RACF, performance, banking clients.
- My edge: **deep z/OS + modern DevOps** (Python, automation, testing mindset), not “COBOL in isolation”.
- Goal: become the **go-to mainframe engineer** who can:
  - Read, debug, and extend legacy COBOL safely.
  - Bridge batch/online COBOL systems with modern services and, over time, Bitcoin/crypto rails.
  - Operate confidently in AVP/VP-level banking roles (Citi, etc.).

---

## 2. COBOL Mental Model (Osmosis Core)

> COBOL ≈ structured data movement + control flow over well-defined records.

**Divisions (in mandatory order):**

1. `IDENTIFICATION DIVISION`
2. `ENVIRONMENT DIVISION`
3. `DATA DIVISION`
4. `PROCEDURE DIVISION`

### 2.1 Data Layouts

- `WORKING-STORAGE SECTION` – internal program state, persistent for the run.
- `LOCAL-STORAGE SECTION` – fresh allocation per invocation (re-entrant-style behaviour).
- `LINKAGE SECTION` – parameters from JCL / calling programs / CICS / other modules.
- `FILE SECTION` – record layouts for files (mapped via JCL DDs).
- Copybooks – shared layouts for records/structures.
- PIC clauses:
  - `X` – alphanumeric
  - `9` – numeric
  - `V` – implied decimal
  - `S` – signed
  - `COMP`, `COMP-3` – binary / packed
  - Editing masks like `Z,ZZ9.99`

### 2.2 Control Flow

- `PERFORM`, `PERFORM UNTIL`, `PERFORM THRU`
- `IF / ELSE`, `EVALUATE` (multi-branch)
- Execution is **sequential** top-to-bottom unless changed via PERFORM, GO TO, etc.
- The **precedence of control statements** determines actual execution flow.

### 2.3 File & DB Access

- Files:
  - Sequential and indexed files (VSAM KSDS, RRDS, ESDS).
  - Verbs: `OPEN`, `READ`, `WRITE`, `REWRITE`, `DELETE`, `CLOSE`.
  - Conditions: `AT END`, `INVALID KEY`, `FILE STATUS`.
- Db2:
  - `EXEC SQL … END-EXEC` blocks for embedded SQL.
  - Host variables (`:WS-FIELD`) bridging COBOL <→ Db2.
  - Cursors, `FETCH` loops, `COMMIT` / `ROLLBACK`.

### 2.4 Error Handling & Returns

- `RETURN-CODE` for program status back to JCL.
- `FILE STATUS` for file I/O outcomes.
- `SQLCODE` / `SQLSTATE` within SQLCA for Db2.
- Level 88 condition names for readable flags.

---

## 3. z/OS + COBOL Reality in Banks

Most real-world COBOL dev work lives at the intersection of:

- **JCL**
  - JOB / EXEC / DD.
  - Procs, overrides, PARM strings.
  - DD statements for files, Db2, MQ, etc.
- **VSAM & datasets**
  - KSDS, ESDS, RRDS, GDGs, catalogs.
- **Batch orchestration**
  - Nightly cycles, dependencies, restart logic, checkpointing, reruns.
- **Online / CICS (if present)**
  - Transactions, maps, COMMAREA/channels, pseudo-conversational programs.
- **Security & infra**
  - RACF, LPARs, regions, SMF, monitoring via SDSF.
- **Change & deployment**
  - SMP/E, promotion pipelines, version control, change management & approvals.

This is where my **MQ / CST z/OS** background is a direct asset: I already understand dumps, SMF, RACF, SDSF, datasets, system behaviour under load, and client impact.

---

## 4. Learning Path – “Low Hanging” First

### 4.1 Existing Foundations

Already completed / in progress:

- IBM z/OS & mainframe:
  - Master the Mainframe (Part 2 & 3).
  - IBM Mainframe Environment Fundamentals.
  - IBM Z System Programming / Assembler basics.
  - MQ on z/OS courses, z/Architecture, SMP/E, TCP/IP, etc.
- Application & DevOps:
  - MSc in Software Development (QUB).
  - Python, Rust, Playwright, automation/testing courses.

These give me:

- Strong mental model of the platform.
- Proven debugging and customer-facing experience.

### 4.2 Short-Term “Low-Hanging” Courses

Focus here first to build momentum and fresh proof-points.

- [ ] **IBM COBOL Basics** (skills badge / course).  
- [ ] **Intro / Foundations of COBOL (Coursera – pick 1–2)**  
      Should:
      - Cover divisions, file handling, typical business programs.
      - Include quizzes / exercises I can screenshot and log as PoW.
- [ ] **Mainframe application programming refreshers**  
      (short z/OS app-programming modules that emphasize JCL + COBOL together).

> Action: identify 2–3 concrete Coursera/IBM courses, list them here by name, and tick them off as they’re done.

### 4.3 Medium-Term Practice

- [ ] Build a small **bank-style batch program**:
  - Read input file of transactions.
  - Update account balances.
  - Produce a summary report.
- [ ] Add **VSAM**: move from flat file to KSDS.
- [ ] Add **error paths**: invalid records, rolled-back transactions, logging.
- [ ] (Stretch) Add a **CICS front-end** or at least design one on paper.

---

## 5. COBOL Fundamentals (Consolidated)

### 5.1 Language Elements

COBOL programs are made of **divisions**, **sections**, **paragraphs**, **sentences**, and **statements**.

- **Character set** – letters, digits, special characters.
- **Words** – reserved words (DISPLAY, MOVE), user-defined identifiers.
- **Literals** – numeric, alphanumeric (`"HELLO"`), figurative constants (`ZERO`, `SPACES`, `HIGH-VALUE`).
- **Comments** – main readability/documentation mechanism.
- **Statements / commands** – DISPLAY, MOVE, COMPUTE, IF, PERFORM, etc.
- **Paragraphs** – named blocks of logic.

**Hierarchy:**

> Division → Section → Paragraph → Sentence → Statement

### 5.2 Four Divisions (with Data-Section Details)

1. **IDENTIFICATION DIVISION**
   - Program metadata.
   - `PROGRAM-ID.` is the only mandatory paragraph.

2. **ENVIRONMENT DIVISION**
   - Describes runtime environment.
   - `INPUT-OUTPUT SECTION` ties internal file names to external DD names.

3. **DATA DIVISION** — _heart of COBOL_

   Sections (if used, must appear in this order):

   - **FILE SECTION**
     - Record layouts for files.
     ```cobol
     FILE SECTION.
     FD  CUSTOMER-FILE.
     01  CUSTOMER-REC.
         05 CUST-ID        PIC X(10).
         05 CUST-NAME      PIC X(30).
         05 CUST-BALANCE   PIC 9(7)V99.
     ```
   - **WORKING-STORAGE SECTION**
     - Static variables that persist for the run.
     - Counters, flags, constants, buffers.
     ```cobol
     WORKING-STORAGE SECTION.
     01  WS-COUNT           PIC 9(4) VALUE ZERO.
     01  WS-FLAG            PIC X    VALUE "N".
     01  WS-DATE            PIC 9(8).
     ```
   - **LOCAL-STORAGE SECTION**
     - Fresh variables per CALL; useful for re-entrancy.
     ```cobol
     LOCAL-STORAGE SECTION.
     01  LS-TEMP            PIC 9(4).
     ```
   - **LINKAGE SECTION**
     - Data from external callers (programs, JCL `PARM`, CICS, etc.).
     ```cobol
     LINKAGE SECTION.
     01  LK-DATA PIC X(80).
     PROCEDURE DIVISION USING LK-DATA.
     ```

4. **PROCEDURE DIVISION**
   - Executable logic.
   - Organised into paragraphs and sections.
   - Uses PERFORM, IF, EVALUATE, READ/WRITE, DISPLAY, RETURN-CODE, etc.

### 5.3 Level Numbers & Special Levels

- `01` – record/group items.
- `02–49` – subordinate fields.
- `77` – standalone items.
- `66` – RENAMES.
- `88` – condition names (flags with semantic meaning).

### 5.4 Core Commands (quick flashcards)

- `DISPLAY` – write to console/JES.
- `MOVE` – assign compatible fields; supports group move.
- `COMPUTE` – arithmetic and expressions.
- `IF / ELSE`, `EVALUATE` – control logic.
- `PERFORM` – structured looping and paragraph/section calls.
- `READ`, `WRITE`, `REWRITE`, `DELETE` – file I/O.
- `OPEN`, `CLOSE` – file lifecycle.
- `STRING`, `UNSTRING`, `INSPECT` – text operations.
- `GOBACK` / `STOP RUN` – exit behaviour.
- `RETURN-CODE` – communicate status to caller/JCL.

---

## 6. Files, Records, and Blocks (Module 4 Osmosis)

### 6.1 Records – Fundamental Unit

- A **record** ≈ row in a table; COBOL layout describes its fields.
- Record organization on z/OS:
  - **Sequential**
  - **Indexed** (VSAM KSDS)
  - **Relative** (VSAM RRDS)
  - ESDS (entry-sequenced) in VSAM

Record size = total bytes of all fields → affects block size and performance.

### 6.2 Blocks – Physical Grouping for I/O Efficiency

- A **block** groups multiple logical records into one physical I/O.
- Larger blocks → fewer I/O operations → faster & cheaper jobs.
- Dataset has a `BLKSIZE` (block size) attribute.

Best practice:

- Let **z/OS choose BLKSIZE** automatically unless there’s a specific legacy/performance reason.

### 6.3 COBOL File Organization & Devices

COBOL supports:

- Sequential files (QSAM / line-sequential).
- Indexed files (VSAM KSDS).
- Relative files (VSAM RRDS).
- ESDS and other specialty cases via JCL/subsystem config.

COBOL interacts with:

- DASD (disks).
- Tape.
- Terminals/printers.
- Subsystems (e.g., Db2, MQ) via DDs.

---

## 7. VSCode, Zowe, CLI, JES & JCL (Tooling Layer)

### 7.1 VSCode + IBM Z Open Editor

Modern mainframe dev stack:

- COBOL/PL/I/HLASM/REXX syntax highlighting.
- Copybook resolution via RSE / z/OSMF profiles.
- Property groups to locate libraries and copybooks.
- Integration with Zowe Explorer to browse datasets, jobs, USS.

### 7.2 Zowe Explorer (VSCode)

Provides GUI-style access to:

- **DATA SETS** (PDS/PS, GDGs).
- **JOBS** (submit, view SPOOL, cancel).
- **USS files**.

Operations:

- Allocate datasets, create members.
- Upload local code into PDS.
- Submit JCL and inspect outputs.

### 7.3 Zowe CLI

Command-line interface for z/OS:

- Submit JCL.
- Issue TSO and console commands.
- Dataset operations (create, delete, upload, download).
- Scriptable, returns JSON → fits DevOps workflows.

Profiles:

- Created via `zowe profiles create ...`.
- Listed via `zowe config profiles`.
- Precedence: workspace → user settings → default RSE/zOSMF.

### 7.4 JES & JCL Fundamentals

- **JES2/JES3**:
  - Handle job submission, queuing, dispatching, SPOOL.
- **JCL minimal structure**:
  - `JOB` – job identity.
  - `EXEC` – program or proc step.
  - `DD` – datasets and devices (incl. files used by COBOL).

---

## 8. Relational Data, Db2, Embedded SQL (Module 5)

### 8.1 From Hierarchical to Relational

- Traditional z/OS datasets are **hierarchical / flat**, rigid but fast.
- Relational data offers:
  - Tables (rows/columns).
  - Flexible relationships (keys).
  - Powerful querying (joins, aggregations, filters).
- On z/OS, relational workloads are usually on **Db2 for z/OS**.

Mental model:

- COBOL record ≈ table row.
- COBOL field ≈ column.

### 8.2 Db2 – Core Concepts

- **Db2 = IBM’s RDBMS**.
- Stores data in tables, enforces ACID.
- Accessed via **SQL**:
  - `SELECT`, `INSERT`, `UPDATE`, `DELETE`.
- COBOL interacts with Db2 primarily via **embedded SQL**.

Key concepts:

- **Optimizer** decides the best access path:
  - Uses indexes, statistics, join methods, filters, ordering.
- **Indexes**:
  - Speed up lookups.
  - Can be simple / composite.
- **Locking & concurrency**:
  - Locks at row/page/table.
  - Isolation levels: RR, RS, CS, UR.
  - Programs must `COMMIT` periodically and avoid long-held locks.
- **Catalog**:
  - Internal tables describing all Db2 objects and stats.
- **Utilities**:
  - `REORG`, `RUNSTATS`, `LOAD`, `UNLOAD`, `COPY` etc. keep Db2 healthy.

### 8.3 Db2 Data Types (at a glance)

- Numeric: `SMALLINT`, `INTEGER`, `BIGINT`, `DECIMAL`, `NUMERIC`.
- Character: `CHAR`, `VARCHAR`.
- Temporal: `DATE`, `TIME`, `TIMESTAMP`.
- Large / special: `BLOB`, `CLOB`, `DBCLOB`, `XML`.

Mapping matters:
- Avoid mismatches between `PIC` clauses and Db2 column types.

### 8.4 Embedded SQL in COBOL

Pattern:

```cobol
EXEC SQL
    SELECT COL1, COL2
      INTO :WS-COL1, :WS-COL2
      FROM MYTABLE
     WHERE KEYCOL = :WS-KEY
END-EXEC.

---

Key elements:
	•	EXEC SQL … END-EXEC – SQL block.
	•	Host variables:
	•	COBOL variables referenced in SQL, prefixed with :.
	•	Defined in WORKING-STORAGE / LINKAGE.
	•	Embedded SQL requires:
	•	Precompilation step (SQL → COBOL calls).
	•	Program setup to support embedded SQL (not automatic).

Important truth:

By default, COBOL programs do not support embedded SQL – you must configure and precompile them.

8.5 SQLCA & SQLCODE (Corrections from Quiz)
	•	SQLCA (SQL Communications Area) is a collection of variables.
	•	It is updated at the end of the execution of every SQL statement.
	•	You typically check:
	•	SQLCODE (numeric status).
	•	SQLSTATE (5-char status).
	•	Pattern:

    EXEC SQL
    INSERT INTO MYTABLE (COL1) VALUES (:WS-COL1)
END-EXEC.

IF SQLCODE NOT = 0
    * handle error
END-IF

8.6 Cursors
	•	Used when an SQL statement can return multiple rows.
	•	Typical flow:
EXEC SQL
    DECLARE C1 CURSOR FOR
        SELECT COL1, COL2
          FROM MYTABLE
         WHERE STATUS = 'A'
END-EXEC.

EXEC SQL
    OPEN C1
END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
    EXEC SQL
        FETCH C1 INTO :WS-COL1, :WS-COL2
    END-EXEC

    IF SQLCODE = 0
        * process row
    END-IF
END-PERFORM.

EXEC SQL
    CLOSE C1
END-EXEC.

---

Key point reinforced by quiz:

When you use a cursor, the program can retrieve each row sequentially from the results table until end-of-data (SQLCODE indicates no more rows).

8.7 Db2 on z/OS – Environment Characteristics (Quiz Fix)

Db2 on z/OS environment is:
	•	Highly secure
	•	High-performance
	•	Scalable and open
	•	Supports a diverse application execution environment

Also:
	•	z/OS is based on 64-bit z/Architecture, not 32-bit – important for memory and performance.

⸻

9. COBOL Data Interfaces – Quick Checklist

Available COBOL data interfaces (from course + quiz):
	•	Embedded SQL
	•	Db2 Application Programming
	•	Host variables
	•	SQLCA
	•	SQL queries
	•	Cursors

⸻

10. Job & Interview Angle (e.g. Citi AVP COBOL)

Narrative anchors:
	•	“I’ve spent years working next to COBOL systems (MQ + z/OS), and now I’m deliberately moving into the code itself.”
	•	“My strength is understanding how the whole mainframe stack behaves under real bank workloads, not just one program.”
	•	“I bring a testing + DevOps mindset into COBOL: regression risk, observability, deployment flow.”
	•	“I want to be the person who can safely touch the scary, business-critical jobs and make them more robust.”

Checklist for AVP-style roles:
	•	Clear story of transition: MQ → focused COBOL dev.
	•	Examples of debugging complex z/OS issues (from CST/MQ days).
	•	Evidence of ongoing COBOL learning (courses, exercises, GitHub snippets).
	•	Comfort describing JCL and batch flows end-to-end.
	•	Awareness of risk, compliance, and change control in banks.
	•	Ability to talk about Db2 basics: tables, indexes, SQL performance, SQLCA.

⸻

11. Immediate Next Actions
	•	Finalise list of “low-hanging” Coursera/IBM courses and start the first one.
	•	Log each completed module here (date + 1–2 key takeaways).
	•	Draft or refine one small COBOL program this week and commit it to GitHub.
	•	Send follow-up email to Citi regarding the COBOL AVP role (with concrete PoW links).
	•	Revisit this file weekly to:
- Add new learnings,
- Capture patterns I notice,
- Track proof-of-work for future CVs and interviews.