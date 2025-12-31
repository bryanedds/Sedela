# **Sedela – The Semantic Design Language** [![License](https://img.shields.io/badge/license-MIT-blue.svg)]

Sedela is a **typed, denotational, and partially informal program design language**.  
Its purpose is to let software designers express the **meaning** and **structure** of a system *before* implementation—cleanly, precisely, and without being constrained by the quirks of any particular programming language.

Sedela is inspired by Conal Elliott’s *Denotational Design* and appends that with **propositions**, enabling a flexible blend of formal and informal specification.

Sedela is *not* a programming language.  
It has **no compiler**, **no interpreter**, and **no runtime semantics**.  
Instead, it is a **pure design language** with a parser and type‑checker that ensure structural correctness while leaving implementation concerns behind.

---

## 🌱 Why Sedela?

Modern systems—especially legacy systems—often have architectures that are:

- too complex to fully formalize,
- too important to leave undocumented,
- too brittle to redesign without a clear semantic model.

Sedela provides a way to describe such systems using:

### ✔ **Formal constructs**
- Algebraic data types  
- Typed lambda calculus (System Fω with type families and opt‑in subtyping)  
- Categories (similar to type classes)  
- Witnesses for category membership  

### ✔ **Informal constructs**
- Natural‑language propositions  
- Descriptions of intent  
- High‑level architectural meaning  

This dual approach lets designers choose the right level of precision for each part of a system.

---

## 🧠 What Sedela Is (and Isn’t)

### Sedela *is*:
- A **semantic design language**  
- A way to encode **abstract program structure**  
- A tool for **architecture, domain modeling, and system meaning**  
- A blend of **formal types** and **informal propositions**  
- A language with a **parser** and **type‑checker**  

### Sedela is *not*:
- A programming language  
- A runtime system  
- A compiler or interpreter  
- A verification system with dependent types  
- An alternative to Agda, or Coq

Sedela is about **design**, not execution.

Sedela aims to provide:

- A **formal yet human‑readable language** for describing software systems.
- A **semantic** (meaning‑driven) rather than syntactic (notation‑driven) approach.
- A way to express **design intent**, not just structure.
- A bridge between **architecture**, **domain modeling**, and **implementation guidance**.

This places Sedela in the same conceptual family as:

| Language / Framework | Similarity |
|----------------------|------------|
| UML                  | Structural modeling, diagrams |
| SysML                | Systems engineering semantics |
| TLA+                 | Formal specification, correctness |
| Alloy                | Declarative modeling |
| Domain‑Driven Design | Ubiquitous language, domain semantics |
| Architecture DSLs    | High‑level system design |

Sedela unifies these ideas into a single, coherent semantic language.

---

# 🌟 **Sedela’s Value Proposition**

Most tools fall into one of two buckets:

### **1. Programming languages**  
They force you to express your design in terms of execution, side effects, and implementation constraints.

### **2. Documentation tools (UML, diagrams, prose)**  
They’re informal, untyped, and not machine-checkable.

However, Sedela gives you something that no other tool provides:

> **A way to formally and informally describe the *meaning* of a system’s architecture without being inhibited by implementation details.**

Sedela sits in the middle:

### ✔ **Formal enough to be type-checked**  
### ✔ **Informal enough to describe messy or legacy systems**  
### ✔ **Expressive enough to capture design intent**  
### ✔ **Structured enough to be unambiguous**  
### ✔ **Free from implementation constraints**  

This combination is unique.

---

# 🧩 **Why this matters in the real world**

Most large systems suffer from the same problems:

- The architecture exists only in people’s heads  
- The codebase doesn’t reflect the intended design  
- Documentation is incomplete or outdated  
- Legacy systems can’t be fully formalized  
- Implementation details distort the conceptual model  

Sedela directly addresses these pain points.

It gives you:

### **A single, typed, semantically rich language for expressing the architecture itself.**

Not the code.  
Not the runtime.  
Not the implementation.  
The *architecture*.

---

# 🌱 **Why this is genuinely valuable**

Because in software, the biggest failures aren’t caused by bad code — they’re caused by:

- unclear architecture  
- mismatched mental models  
- undocumented assumptions  
- lost design intent  
- accidental complexity  

Sedela gives you a way to capture the *meaning* of a system so that:

- implementations can vary  
- teams can align  
- intent is preserved  
- structure is explicit  
- complexity is tamed  

That’s a real, practical, and unique value proposition.

---

## 📘 Language Overview

Sedela includes:

### **Propositions**
Natural‑language descriptions of meaning or intent.

```sedela
Proposition "Convert a symbol to a string."
Proposition! "Attach debugger to code called inside the given container."
```

### **Algebraic Data Types**
Products and sums, similar to ML/Haskell.

```sedela
type Maybe<a> =
  | Some of a
  | None
```

### **Functions**
Typed lambda expressions or propositions.

```sedela
let symbolToString (s : Symbol) : String =
  Proposition "Convert a symbol to string."
```

### **Categories (Type Classes)**
With optional constraints and witnesses.

```sedela
category Monad<m; Applicative<m>> =
  bind<a, b> : m<a> -> (a -> m<b>) -> m<b>
```

### **Witnesses**
Provide implementations for category members.

```sedela
witness Monad =
  pure = vsyncReturn
  map = vsyncMap
  apply = vsyncApply
  bind = vsyncBind
```

---

## 🏗 Example: MetaFunctions (Microservice Replacement)

Sedela includes a full semantic design for **MetaFunctions**, a system architecture intended to replace microservices.

```sedela
type MetaFunction =
  Provider -> Intent -> Symbol -> Vsync<Symbol>

let call (mfn : MetaFunction) provider intent args =
  mfn provider intent args
```

This example demonstrates how Sedela can describe complex distributed systems using a mix of formal types and informal propositions.

---

## 🧩 How Sedela Uses the Term *Semantics*

The word **“semantics”** is heavily overloaded in computer science, so it’s important to clarify what it means in the context of Sedela. In programming‑language theory, *semantics* usually refers to the **behavior of a program when it runs**—for example:

- **Operational semantics:** step‑by‑step execution rules  
- **Denotational semantics:** mapping programs to mathematical functions  
- **Axiomatic semantics:** reasoning about correctness  

Sedela does not use the term in this sense.

### **Sedela’s semantics are about design meaning, not execution behavior.**

Sedela is a **pure design language**, not a programming language.  
It has no interpreter, no runtime, and no notion of program execution.  
Instead, Sedela uses “semantics” to refer to the **meaning of a system’s structure**, including:

- the meaning of types and type relationships  
- the meaning of categories (similar to type classes)  
- the meaning of architectural components  
- the meaning of domain concepts  
- the meaning of design intent  

This meaning can be expressed in two complementary ways:

### **1. Formal semantics**  
Using algebraic data types, typed lambda calculus, categories, and witnesses.

### **2. Informal semantics**  
Using natural‑language **propositions** to describe intent, behavior, or constraints that are too complex or unnecessary to formalize.

Together, these form what Sedela calls **Semantic Design**: This approach allows Sedela to describe both rigorously defined components *and* partially specified or legacy systems where full formalization is impractical or undesirable. In short, **Sedela’s semantics describe what a system *means*, not how it *executes*.**

---

## 📦 Repository Structure

```
Sedela/
 ├── Sedela.Specification/   # The language specification (PDF)
 ├── Sedela.Parser/          # Parser implementation
 ├── Sedela.TypeChecker/     # Type-checking logic
 ├── Sedela.Examples/        # Example semantic designs
 └── README.md               # You are here
```

---

## 🚧 Project Status

Sedela is currently:

- **Actively designed**
- **Partially implemented**
- **Open to feedback and contributions**

The parser and type‑checker are under development.  
There will never be a compiler or interpreter.

---

## 🤝 Contributing

Contributions are welcome!  
You can help by:

- Discussing language features  
- Suggesting improvements  
- Reporting issues  
- Contributing examples  
- Helping refine the type system  

Please open an issue or submit a pull request.

---

## 📄 License

Sedela is released under the MIT License.  
See `LICENSE` for details.

---

## 🌐 Related Work

- Conal Elliott – *Denotational Design: From Meanings to Programs*  
- Haskell type classes  
- Category theory for program structure  
- Algebraic specification languages
