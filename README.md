A collection of various practical Haskell bits.

## Motivation

It happens to all of us.

Once in a while you stumble upon a problem that seems way too familiar. You remember that you've either solved it before or saw it in a blog post somewhere.

You start browsing through Reddit, old projects and bookmarks, searching aggressively for that single line of code or function that you just need to copy-paste or at least take a glimpse at to remember what it was about. It's just one "bit" that you need.

Practical Haskell Bits is an initiative to contain as many of these as possible and become the go-to place for real-world patterns, snippets and popular library examples.

Bits should be aimed towards **everyone** in the community, regardless of their level of experience or understanding.

> No example is too trivial!

The Haskell community is often criticized for focusing on things that are way too complicated to outsiders (and frankly, most "intermediate" Haskellers as well) and pushing away beginners. This is true and in order to attract more people and have them become productive, we need to address it.

Not everyone can figure out how to use a library by reading the source code.

Some people will struggle with "basic" things such as parsing JSON or setting up a web server. It's exactly these people that will not have the confidence to ask a question because seemingly everyone is busy looking at [`examples of compiler optimizations changing asymptotic complexity`](https://www.reddit.com/r/haskell/comments/xah8v1/examples_of_compiler_optimizations_changing/).

# What defines a Practical Haskell Bit?

#### A Practical Haskell Bit is a **mini-project** that:


* Looks at a single, well-defined scenario
* Is of production quality
* Contains as little code and dependencies as possible
* Is self-contained and buildable on its own
* Reflects the current (or at least some) best practices
* Is suitable for copy-pasting or just refreshing your memory
* Has a sufficient, but not necessarily detailed explanation
* Aims to use terminology and examples as close to the real world as possible

#### A Practical Haskell Bit is **not**:

* [Necessarily] a detailed tutorial
* An incomplete code snippet
* A full blown project example (e.g. [realworld.io](realworld.io))
* A blog post that you need to follow to put the code together

# Examples and non-examples:

#### Good Practical Haskell Bit candidates:

* Streaming `persistent` queries using `conduit`
* Integrating with an external API via `servant-client`
* Making your `wai` app AWS Lambda compatible
* Setting up your application monad and business logic with `mtl`
* Protecting `servant` routes with a JWT token
* Using smart constructors
* Record update scenarios via `lens`
* Logging to multiple destinations (e.g. file + stdout) via `katip`
* Property testing with `QuickCheck`
* etc.

#### Bad Practical Haskell Bit candidates:

* A CRUD app
* A console game
* A snippet containing some part of a solution
* A snippet without sufficient context
* etc.

## Contributing

Contributions of any kind are welcome.
