A collection of various practical Haskell bits.

## Motivation

It happens to all of us.

Once in a while you stumble upon a problem that seems way too familiar. You remember that you've either solved it before or saw it in a blog post somewhere.

You start browsing through Reddit, old projects, your bookmarks, searching aggressively for that single line of code that you need to copy-paste or at least take a glimpse at to remember what it was about. It's just one "bit" that you need.

Practical Haskell is an initiative to contain as many of these "practical bits" as possible and become the go-to place for patterns, snippets and popular library examples.

## What defines a Practical Haskell bit?

First of all, an important note:

Bits are aimed towards **everyone** in the community, regardless of their level of experience or understanding.

> No example is too trivial!

The Haskell community is often criticized for focusing on things that are way too complicated to outsiders (and frankly, most "intermediate" Haskellers as well) and pushing away beginners. This is true and in order to attract more people and have them become productive, we must fight it.

Not everyone can figure out how to use a library by reading the source code.

With that out of the way.

---

A Practical Haskell bit is a **mini-project** that:

* Looks at a single, well-defined scenario
* Is of production quality
* Contains as little code and dependencies as possible
* Is self-contained
* Is buildable
* Reflects the current (or at least some) best practices
* Is suitable for refreshing your memory or just copy-pasting
* Has a sufficient, but not necessarily detailed explanation
* Aims to use terminology and 

A Practical Haskell bit is **not**:

* [Necessarily] a detailed tutorial
* An incomplete code snippet
* A full blown project example (e.g. [realworld.io](realworld.io))
* A blog post that you need to follow to put the code together

## Examples and non-examples:

Good Practical Haskell bit candidates:

* Streaming `persistent` queries using `conduit`
* Integrating with an external API via servant-client
* Setting up your application monad with `mtl`
* Protecting `servant` routes with a JWT token
* Using smart constructors
* Record updates via `generic-lens`
* Logging to multiple destinations (e.g. file + stdout) via `katip`
* Property testing with QuickCheck

Bad Practical Haskell bits:

* A CRUD app
* A console game
* A snippet containing some part of a solution
* A snippet without sufficient context

## Contributing

Contributions of any kind are welcome.
