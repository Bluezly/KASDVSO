# KASDVSO – KAS Language

KASDVSO is an experimental scripting language runtime written in **Rust**.
The main focus of this project is the **KAS language itself**, not packages or tooling.

> ⚠️ IMPORTANT
> This language is under **heavy and active development**.
> Breaking changes are expected.
> The language is **NOT stable**.

---

## What is KAS?

KAS is a simple and minimal scripting language designed to explore:

- Language and runtime design
- Script execution models
- Modular script loading
- Native integration with Rust
- Explicit behavior without hidden magic

KAS is intentionally minimal.
Every feature is added deliberately and step-by-step.

---

## KAS Language Basics

### Variables

```kas
let x = 10
let name = "KAS"
```

### Functions

```kas
fn add(a, b) {
    return a + b
}
```

### Function Calls

```kas
print add(2, 3)
```

### Printing

```kas
print "Hello from KAS"
```

### Sleep

```kas
sleep(2000)   // milliseconds
```

---

## Multi-File Scripts

KAS supports splitting scripts into multiple files using local imports.

Example:

```kas
import "./utils.kas"
```

Each imported file becomes a module, accessible by its filename.

### Example Structure

```
app.kas
utils.kas
```

### app.kas

```kas
import "./utils.kas"

print utils.sum(2, 3)
```

### utils.kas

```kas
fn sum(a, b) {
    return a + b
}
```

---

## Execution Model

- Scripts are executed sequentially
- Imported files are executed once
- Imported modules expose their functions and values
- There is no hidden global state (by design)

---

## Native Integration

KAS is designed to integrate tightly with native Rust code.

Native modules:

- Are written in Rust
- Expose functions directly to KAS
- Allow system-level access when needed

This makes KAS suitable for experimentation with:

- Networking
- I/O
- Runtime extensions

---

## Error Philosophy

Errors are explicit and loud.

Example:

```
[KASDVSO Error] File not found
```

The goal is clarity, not silence.

---

## Project Status

🚧 UNDER HEAVY DEVELOPMENT

- Syntax may change
- APIs may change
- Features may be removed or redesigned
- Documentation is incomplete by design

If you are looking for a stable production language, this is not it (yet).

---

## Contributing

💡 Contributions are very welcome.

This project needs help and feedback.

You can contribute by:

- Improving the language syntax
- Enhancing error messages
- Adding documentation
- Writing tests
- Improving the runtime
- Reviewing code and proposing ideas

### How to help

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Open a Pull Request (PR)

Even small PRs matter.

---

## Contribution Rules

- Keep changes focused
- Avoid large refactors without discussion
- Be explicit in PR descriptions
- Expect feedback (this is a learning project)

---

## Roadmap (Flexible)

Possible future work:

- export / import as
- Module caching
- Circular import detection
- Better error stack traces
- Language specification document
- Expanded standard library
- Improved tooling

Nothing here is guaranteed.

---

## Final Notes

KAS is not finished.
KAS is not stable.
KAS is not pretending to be complete.

If you enjoy building things from the ground up, you are in the right place.

Pull Requests are encouraged.
