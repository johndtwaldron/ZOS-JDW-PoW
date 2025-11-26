# HCL Talview Interview Prep
## Morning Revision Cheat Sheet (Bullet Points)

### Core Background
- 7 years IBM MQ on z/OS (Hursley Lab)
- Production support, debugging, dumps, traces, SMF/RMF
- Experience across CICS, DB2, IMS, batch chains, TLS, channels
- Left IBM post-COVID restructuring; upskilled + project work (BitVault)
- Ready for full-time mainframe engineering again

### z/OS Fundamentals
- IPL → LOADxx → PARMLIB (IEASYSxx, PROGxx, COMMxx)
- SMP/E → APAR, PTF, GLOBAL/TARGET/DIST zones
- TSO/ISPF/SDSF basics
- JCL: JOB/EXEC/DD, S0C4/S0C7/U abends
- RACF: ADDUSER, PERMIT, RLIST
- USS: HFS/zFS, basic commands

### Networking / TCPIP
- PROFILE.TCPIP
- NETSTAT (ALL, ROUTE, CONN)
- AT-TLS basics
- VTAM / SNA high-level awareness

### Incident Resolution (Process)
- Stay calm
- Gather SDSF logs + dumps
- Identify failing step/component
- Use IPCS, SMF, traces
- Communicate status
- Apply fix safely → document → prevent recurrence

### Key Example (MQ-heavy)
- CHINIT crashing (5C6-00C51002)
- Root cause: runaway buffer usage from deep queues
- Tools: SDSF, IPCS VERBX, SMF 115/116, DISPLAY BUFFERPOOL
- Fix: rebalance bufferpools, adjust batching logic
- Prevention: thresholds + monitoring

### JCL / Batch Triage
- Identify failing step
- Check DD statements / datasets
- Look for S0C7 (bad data), S0C4 (pointer), U abends
- SDSF JESMSGLG + ABEND report
- Validate rerun strategy

### Why HCL?
- Large enterprise estates
- Deep mainframe engineering still valued
- Banking environments align with experience
- Growth + stability

### 60-Second Intro (Bullets)
- Name, 7 years IBM MQ z/OS
- Debugging, dumps, SMF/RMF
- Post-IBM: upskilling + engineering work
- Looking to return to mainframe full-time
**Author:** JDW  
**Role:** Associate (Mainframe) – HCL Technologies  
**Interview Date:** Tue 25 Nov 2025  

This document contains:
- **B:** MCQ cheat-sheet (z/OS Sysprog topics)
- **C:** Written/essay templates  
- **D:** Automated video interview practice set  

---

# B. MCQ CHEAT-SHEET — z/OS SYS PROG QUICK REF

## 1. z/OS Operating System Basics
- **IPL (Initial Program Load):** OS boot process. Loads nucleus and masters.
- **Master Scheduler:** Starts system tasks and JES.
- **JES2 vs JES3:**
  - JES2 = decentralized, spool-centric (most common).
  - JES3 = centralized scheduling (used less in modern shops).
- **PARMLIB:** Runtime configuration member library for subsystems (IEASYSxx, PROGxx, COMMxx).
- **PROCLIB:** Stores JCL procedures (startup procedures, system tasks).
- **LINKLIB:** System program libraries searched for load modules.
- **SYS1.LINKLIB:** Primary system program library.
- **Catalog:** Maps dataset names → VOLSER + location.

## 2. SMP/E (Software Maintenance Program / Enhanced)
- Tool for **installing, updating, maintaining** z/OS & subsystems.
- Tracks **APARs**, **PTFs**, **USERMODs**.
- Key datasets:
  - **GLOBAL** zone
  - **TARGET** zone
  - **DISTRIBUTION** zone

**APAR:** Authorized Program Analysis Report (problem record).  
**PTF:** Program Temporary Fix (solution for an APAR).

## 3. TSO / ISPF / SDSF
- **TSO:** Time Sharing Option — provides command line environment.
- **ISPF:** Menu-driven dev/admin interface.
- **SDSF:** Used for viewing JES2 logs, job output, system messages, operator queues.

Common commands:
- `DA` — view datasets  
- `SDSF LOG` — system log  
- `PREFIX xxxx` — filter jobs  

## 4. JCL Essentials
- `JOB`, `EXEC`, `DD`  
- **COND codes**, `RC=`, `ABEND`  
- **VSAM:** Key-Sequenced Datasets (KSDS), Entry-Sequenced (ESDS), Relative (RRDS).

Common abends:
- **S0C4:** Protection exception (bad pointer).
- **S0C7:** Numeric → non-numeric.
- **U*** — user ABEND.

## 5. z/OS UNIX (USS)
- POSIX environment inside z/OS.
- FS: HFS, zFS.
- Commands: `cd`, `ls`, `oedit`, `chmod`, `ps`, etc.

## 6. RACF (Security)
- Controls: **Users**, **Groups**, **Datasets**, **Resources**.
- Commands: `ADDUSER`, `ALTUSER`, `SEARCH`, `RLIST`, `PERMIT`.

## 7. TCP/IP on z/OS
- Configured via **PROFILE.TCPIP**.
- **NETSTAT**: Display sockets, connections, interfaces.
- **PAGENT:** Policy Agent.
- **AT-TLS:** Encrypted TLS at transport level (no app change required).
- **VTAM:** SNA networking.

## 8. Storage
- **VOLSER:** 6-char volume serial number.
- **UCB:** Unit Control Block, represents device.
- **SMS:** Storage Mgmt Subsystem, handles class policies.

## 9. SMP/E, IPL, and Subsystems Review
- IPL uses **LOADxx** to locate PARMLIB.
- Subsystems started via:  
  - **IEFSSNxx**  
  - JCL PROCs in PROCLIB  
  - Operator commands `S STCNAME`.

---

# C. WRITTEN / ESSAY TEMPLATE ANSWERS

These templates are tuned for HCL’s typical essay prompts.

## 1. “Describe a time you solved a production issue.”
**Template Answer:**

In my previous role supporting IBM MQ on z/OS, I was responsible for diagnosing issues across queue managers, channels, storage, and JES2 batch flows. One notable example involved an MQ channel initiator that repeatedly failed with storage abends. I reviewed the SDSF logs, IPCS dump summary, and SMF/RMF indicators which pointed to runaway buffer usage caused by a misconfigured application. I worked with the application team and rebalanced the bufferpools, then implemented a small configuration change to prevent future recurrence. This stabilised the environment and eliminated the repeated abends. It is an example of how I approach issues methodically: gather evidence, form hypotheses, validate, and implement a corrective change.

## 2. “Why do you want to join HCL?”
I have a strong mainframe background from IBM, and HCL’s work supporting major banks and enterprise z/OS estates aligns with both my experience and my long-term goals. I enjoy production engineering, problem-solving, and being close to the systems that keep critical services running. HCL offers both scale and learning opportunities, including exposure to multiple clients and modern automation approaches. It’s a place where I can contribute from day one while continuing to grow.

## 3. “Describe your experience with mainframe technologies.”
I have 7 years’ experience working with IBM MQ on z/OS, covering administration, debugging, test environment support, IPCS dump analysis, SMF reporting, TLS/AT-TLS, exit behaviour, and cross-subsystem integration across CICS, DB2, and batch. I’m comfortable with TSO/ISPF, SDSF, JCL, SMP/E concepts, USS, RACF basics, and overall problem-solving in a z/OS production environment. My background combines systems thinking with hands-on investigation.

## 4. “How do you handle learning new technologies?”
My approach is structured: I reference manuals, explore the system directly, and learn by replicating small proof-of-concept tasks. I maintain detailed notes and always ensure I can explain what I learn in simple terms. This builds confidence and transferability across systems and clients.

---

# D. AUTOMATED VIDEO INTERVIEW MOCK (TALVIEW SIM)

Below are realistic prompts you may face.  
Practice giving **60–90 second** answers.

---

## **Q1. Tell us about yourself and your mainframe experience.**
**Checklist for answer:**
- 7 years IBM MQ on z/OS  
- Production support + debugging  
- Good with dumps, logs, SMF/RMF  
- Left IBM after COVID restructuring  
- Spent time working in Bitcoin/startup world  
- Returning to mainframe because it’s your technical home  

### Sample Answer (Expanded)
I have over seven years of experience working at IBM Hursley supporting MQ on z/OS for major global banks. My work involved production support, system debugging, and deep technical investigation across queue managers, channels, storage, and batch flows. I developed strong skills in SDSF analysis, IPCS dump reading, SMF/RMF interpretation, and cross-team collaboration. After COVID the organisation changed direction, and I decided to take some time for personal development and to work on a smaller technical project while continuing to upskill in z/OS. I’m now ready to return to a full-time mainframe engineering path, which is where I’ve always done my best work.

---

## **Q2. Why do you want to join HCL Technologies?**
**Checklist:**
- Large client environments  
- Opportunity to apply deep z/OS experience  
- Excited to contribute to reliability & production support  
- Long-term stable career  

### Sample Answer (Expanded)
I want to join HCL because it offers exposure to large enterprise mainframe estates where deep technical engineering still matters. My background with IBM MQ on z/OS has given me strong diagnostic and production support skills, and HCL’s work across banking and financial systems aligns closely with my experience. I’m particularly interested in the variety of environments and tooling HCL engineers work with, as well as the opportunity to grow through real-world challenges. HCL’s global reach and technical culture make it the right place for me to contribute and develop long-term.

---


## **Q3. Describe a difficult technical challenge you solved.**
Use the MQ channel abend example or storage violation example.

Structure:
1. Problem  
2. Steps taken  
3. Tools used (SDSF, IPCS, RMF, SMF)  
4. Fix  
5. Long-term prevention  

### MQ-Heavy Example (CHINIT Storage Abend)

**Example: MQ Channel Initiator Storage Abend (5C6-00C51002)**  
A test environment experienced repeated MQ CHINIT failures every few hours, disrupting application testing. Logs and IPCS analysis showed uncontrolled buffer growth caused by a misconfigured application creating deep queue buildup and runaway storage. Using SDSF, DISPLAY commands, SMF 115/116, and IPCS VERBX diagnostics, I identified the overload pattern, rebalanced bufferpool allocations, and coordinated with the application team to adjust batching logic. The change restored stability, and I implemented monitoring thresholds and trend checks to prevent recurrence.

---

## **Q4. How do you approach incident resolution under pressure?**
Checklist:
- Stay calm  
- Gather logs first  
- Reproduce or isolate  
- Communicate clearly  
- Apply fix safely  
- Document & prevent recurrence  

### Sample Answer (Expanded)
When an incident occurs, my first priority is to stay calm and gather the right evidence. I begin with SDSF logs, system messages, and any abend or dump output. From there I narrow the problem: Is it an application issue, a JCL error, a configuration change, or an environmental fault? I use IPCS, SMF data, and subsystem traces to confirm the root cause. I communicate clearly with stakeholders throughout so everyone knows what is happening. Once the fix is applied, I ensure documentation is updated, and I follow up with a prevention step so the issue does not repeat.

---

## **Q5. What is your experience with JCL and batch problem determination?**
Checklist:
- Comfortable with JCL syntax  
- Understanding of abends (S0C4, S0C7, U codes)  
- Experience triaging failed batch chains  
- Familiar with scheduler output via SDSF  

### Sample Answer (Expanded)
I’m comfortable reading and modifying JCL to diagnose failures. I understand key abends such as S0C4, S0C7, and U-codes, and I’ve handled many cases where bad data, incorrect DD statements, missing datasets, or scheduling issues caused batch failures. In previous roles I used SDSF extensively to review JES logs, job output, and system messages. Where necessary, I traced the flow downstream to CICS, DB2, or MQ to identify data integrity or timing issues. My approach is methodical: identify the failing step, isolate the cause, correct the input or JCL, validate, and document for future runs.

---

# END OF DOCUMENT