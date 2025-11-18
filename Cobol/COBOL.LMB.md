# COBOL.LMB.md

Living document for growing COBOL skills as part of Project LMB  
(aligning mainframe pedigree, modern DevOps, and long-term Bitcoin/finance goals).

---

## 1. Why COBOL (for JDW)

- COBOL is still the backbone of core banking and payments – exactly where I’ve already operated via MQ and z/OS.
- I bring 7+ years of **IBM MQ for z/OS** experience: debugging, dumps, SMP/E, RACF, performance, banking clients.
- My edge: **deep z/OS + modern DevOps** (Python, automation, testing mind-set), not “COBOL in isolation”.
- Goal: become the **go-to mainframe engineer** who can:
  - Read, debug, and extend legacy COBOL safely.
  - Bridge batch/online COBOL systems with modern services and Bitcoin/crypto rails over time.
  - Operate confidently in AVP/VP-level banking roles (Citi, etc.).

---

## 2. COBOL Mental Model (Osmosis Core)

COBOL ≈ structured data movement + control flow over well-defined records.

**Sections:**
- `IDENTIFICATION DIVISION`
- `ENVIRONMENT DIVISION`
- `DATA DIVISION`
- `PROCEDURE DIVISION`

**Key ideas to internalise:**

- **Data layouts**
  - `WORKING-STORAGE SECTION` for state.
  - `LINKAGE SECTION` for parameters from JCL / calling programs.
  - Copybooks for shared record layouts.
  - PIC clauses: `X`, `9`, `V`, `COMP/COMP-3`, editing (`Z,ZZ9.99` etc).

- **Control flow**
  - `PERFORM … END-PERFORM`, `PERFORM UNTIL`, `PERFORM THRU`.
  - `EVALUATE` (multi-branch) vs cascaded `IF`.
  - Structured, predictable, easy to audit.

- **File & DB access**
  - Sequential and indexed files (VSAM KSDS).
  - `OPEN`, `READ`, `WRITE`, `REWRITE`, `DELETE`, `CLOSE`.
  - `AT END`, `INVALID KEY`, `FILE STATUS`.
  - For DB2: cursors, `EXEC SQL … END-EXEC`.

- **Error handling & returns**
  - `RETURN-CODE`, `FILE STATUS`, and `SQLCODE`.
  - `88` level condition names for readability.

---

## 3. z/OS + COBOL Reality in Banks

Most COBOL dev work sits at the intersection of:

- **JCL**
  - Job steps, PROC/EXEC, PARM strings.
  - DD statements for files, DB2, MQ, etc.
- **VSAM & datasets**
  - KSDS, ESDS, GDGs, catalog concepts.
- **Batch orchestration**
  - Nightly cycles, dependencies, reruns, restart logic, check-pointing.
- **Online / CICS (if used)**
  - Transaction programs, maps, COMMAREA / channels.
- **Security & infra**
  - RACF, regions, LPARs, SMF awareness, monitoring via SDSF.
- **Change & deployment**
  - SMP/E, promotion pipelines, version control.

This is where my **MQ / CST z/OS** background is a direct asset: I already understand dumps, SMF, RACF, SDSF, datasets, system behaviour under load, and client impact.

---

## 4. Learning Path – “Low Hanging” First

### 4.1 Existing Foundations

Already completed / in progress (update as needed):

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

- [ ] **IBM COBOL Basics** (skills badge / course) – consolidate syntax & basic patterns.
- [ ] **Intro / Foundations of COBOL (Coursera – TBD)**  
      → Pick 1–2 short courses that:
      - Cover divisions, file handling, and typical business programs.
      - Include quizzes / small exercises I can screenshot and log as PoW.
- [ ] **Mainframe application programming refreshers**  
      (any short z/OS application programming modules that emphasise JCL + COBOL together).

> Action: identify 2–3 concrete Coursera/IBM courses, list them here by name, and tick them off as they’re done.

### 4.3 Medium-Term Practice

- [ ] Build a small **bank-style batch program**:
  - Read input file of transactions.
  - Update account balances.
  - Produce a summary report.
- [ ] Add **VSAM**: move from flat file to KSDS.
- [ ] Add **error paths**: invalid records, rolled-back transactions, logging.
- [ ] If possible, add a **CICS front-end** or at least design it on paper.

---

## 5. Practical Patterns to Revisit Regularly

Use this section like flashcards.

### 5.1 Program Skeleton

- Standard program template with:
  - Clear naming convention.
  - `WORKING-STORAGE` variables grouped logically.
  - Init paragraph, main loop, teardown.

### 5.2 File Processing

- Single-file sequential read.
- Two-file matching / reconciliation.
- `START` + `READ NEXT` for keyed VSAM access.
- `READ … INTO` VS `READ` with explicit buffer.

### 5.3 Data Validation

- Use of `88` levels for flags.
- Range checks, mandatory fields, reason codes.
- Accumulators for counts and totals.

### 5.4 Banking / Finance Patterns

- Ledger entries, debit/credit structure.
- Interest accrual and rounding issues.
- End-of-day / end-of-month cut-off logic.
- Reconciliation jobs that compare different sources and raise exceptions.

---

## 6. Job & Interview Angle (e.g. Citi AVP COBOL)

Narrative anchors for roles like Citi:

- “I’ve spent years working **next to** COBOL systems (MQ + z/OS), now I’m deliberately moving **into** the code itself.”
- “My strength is understanding how the whole mainframe stack behaves under real bank workloads, not just one program.”
- “I bring a **testing + DevOps mindset** into COBOL: I think about regression risk, observability, and how changes move through environments.”
- “I’m hungry for the work: I want to be the person who can safely touch the scary, business-critical jobs and make them more robust.”

Checklist for AVP-style roles:

- [ ] Clear story of transition: IBM MQ → focused COBOL dev.
- [ ] Examples of debugging complex z/OS issues (from CST/MQ days).
- [ ] Evidence of ongoing COBOL learning (courses, exercises, GitHub snippets).
- [ ] Comfort describing JCL and batch flows.
- [ ] Awareness of risk, compliance, and change control in banks.

---

## 7. Immediate Next Actions

- [ ] Finalise list of “low-hanging” Coursera/IBM courses and start the first one.
- [ ] Log each completed module in this file (date + 1–2 key takeaways).
- [ ] Draft or refine one small COBOL program this week and commit it to GitHub.
- [ ] Once today’s updates are complete, send follow-up email to Citi regarding the COBOL AVP role.
- [ ] Revisit this file weekly to:
      - Add new learnings,
      - Capture patterns I notice,
      - Track proof-of-work for future CVs and interviews.