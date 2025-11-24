# HCL Talview Interview Prep
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

---

## **Q2. Why do you want to join HCL Technologies?**
**Checklist:**
- Large client environments  
- Opportunity to apply deep z/OS experience  
- Excited to contribute to reliability & production support  
- Long-term stable career  

---

## **Q3. Describe a difficult technical challenge you solved.**
Use the MQ channel abend example or storage violation example.

Structure:
1. Problem  
2. Steps taken  
3. Tools used (SDSF, IPCS, RMF, SMF)  
4. Fix  
5. Long-term prevention  

---

## **Q4. How do you approach incident resolution under pressure?**
Checklist:
- Stay calm  
- Gather logs first  
- Reproduce or isolate  
- Communicate clearly  
- Apply fix safely  
- Document & prevent recurrence  

---

## **Q5. What is your experience with JCL and batch problem determination?**
Checklist:
- Comfortable with JCL syntax  
- Understanding of abends (S0C4, S0C7, U codes)  
- Experience triaging failed batch chains  
- Familiar with scheduler output via SDSF  

---

# END OF DOCUMENT