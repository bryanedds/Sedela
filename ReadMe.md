# **Sedela – The Semantic Design Language** [![License](https://img.shields.io/badge/license-MIT-blue.svg)]

Sedela is a **typed, denotational, and partially informal program design language**.  
Its purpose is to let software designers express the **meaning** and **structure** of a system *before* implementation—cleanly, precisely, and without being constrained by the quirks of any particular programming language.

Sedela is inspired by Conal Elliott’s *Denotational Design* and extends it with **propositions**, enabling a flexible blend of formal and informal specification.

> **Semantic Design = Denotational Design + Propositions**

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
- A replacement for Haskell, Agda, or Coq  

Sedela is about **design**, not execution.

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
