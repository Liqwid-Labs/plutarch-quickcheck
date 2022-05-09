## Introduction

We describe a set of standards for code. We also explain our reasoning for these
choices. We want this to act as a living document of our practices for current,
and future, contributors to the project. We also intend this document to evolve
as our needs change, as well as act as a single point of truth for standards.

## Motivation

We define our desired outcomes from our choice of practices.

### Increased consistency

Inconsistency is worse than _any_ standard. It requires us to track a large
amount of case-specific information without any logic to it, complicates
becoming familiar with the codebase for newcomers, works _against_ intuition,
and generally leads to friction, frustration and ultimately, worse results.
Software development is already difficult enough: not only are many of our
problems precise and detail-oriented, we also have to deal with _decades_ of
poor choices foisted upon us, over which we have no control at all.

Thus, we strive to be _automatically consistent_. Similar things should look
similar; different things should look different. As much as possible, we must
pick some rules and _stick to them_; furthermore, said rules must be clear,
explicit and well-motivated. This will ultimately benefit us, both in the short
and long term. The standards described in this document, and indeed, the
document itself, are written with this foremost in mind.

### Limited non-local information

There is a limited amount of space in a developer's skull. Everyone, no matter
how experienced or skilled, has bad days, and we forget things, or make decision
that are not ideal in the short or long term. Therefore, we need to limit our
cognitive load: by giving ourselves less to think about, we reduce the amount of
trouble we can get ourselves into due to the aforementioned skull limitations. 

One of the worst contributors to cognitive load, after inconsistency, is
_non-local information_: that is, the requirement to have some knowledge or
understanding of matters beyond the current scope of work. That scope of work
ranges from a data type, to a module, to a whole project; in all cases, the more
non-local information we require ourselves to hold in our minds, the less space
that leaves to solving the problems, or doing the task, we actually have at
hand, and the more errors and bad choices we introduce in the process.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided, and we should have as much locality of
information as possible, whenever possible. Our work should be broken down into
discrete, minimal and logical units, which can be analyzed, worked on, reviewed
and tested in as much isolation as possible. This also applies to our external
dependencies.

We make many of the choices described in this document around limiting the
amount of non-local information required at all levels of a codebase.
Additionally, we aim to proscribe doing certain things 'just because we can' in
a way that would be difficult for other Haskellers to follow, regardless of
skill level, for similar reasons: a lot of such 'because we can' techniques
require a lot of background to understand, which not every developer necessarily
has, or can recall easily.

### Minimized legacy impact

Haskell is a language older than many of the people writing it, and many parts
of its ecosystem _definitely_ look their age. Age brings legacy, much of which
is based on decisions we now know to be bad or ill-considered in retrospect. We
cannot escape our history, but we can minimize its impact on our current work.

In light of this, we use this document to describe _today's_ good practices. We
also aim to avoid 'sharp edges' by proscribing them away in a principled,
consistent and justifiable manner.

### Reduction of drudgery by automatic means

As developers, we should use our tools to make ourselves as productive as
possible. There is no reason for us to do a task if a machine can do it for us,
specially when this task is boring or repetitive. We love Haskell as a language
not least of all for its capability to abstract, to describe, and to make _fun_
what other languages make _dull_ or _impossible_; likewise, our work must do the
same.

Many of the tool-related proscriptions and requirements of this document are
driven by the desire to remove boring, repetitive tasks that don't need a human
to complete. By removing the need to think about such things, we can focus on
the tasks that _do_ need a human to complete; thus, we get more done, quicker,
with less effort.

## Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC 2119](https://tools.ietf.org/html/rfc2119).

## Tools

### Compiler warning settings

The following warnings MUST be enabled for all builds of any project, in the
`ghc-options` of the Cabal file:

* `-Wall`
* `-Wcompat`
* `-Wincomplete-uni-patterns`
* `-Wincomplete-record-updates`
* `-Wredundant-constraints`
* `-Wmissing-export-lists`
* `-Wmissing-deriving-strategies`
* `-Werror`

Additionally, `-Wredundant-constraints` SHOULD be enabled for all builds of any
project, in the `ghc-options` of the Cabal file. Exceptions are allowed when the
additional constraints are designed to ensure safety, rather than due to
reliance on any method. If this warning is to be disabled, it MUST be disabled
in the narrowest possible scope: this SHOULD be a single module.

#### Justification

Most of these options are suggested by 
[Alexis King](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build)
- the justifications for them can be found at the link. These fit well with 
our motivations, and thus, should be used everywhere. `-Werror` ensures that 
warnings _cannot_ be ignored: this means that problems get fixed sooner. We 
also add `-Wmissing-export-lists` and `-Wmissing-deriving-strategies`: the 
first ensures that we clearly indicate what is, and isn't, part of a module's 
public API, and the second ensures that we have clarity about how everything 
is derived. As we mandate both export lists and deriving strategies in this 
document, these warnings ensure compliance, as well as checking it 
automatically.

### Linting

Every source file MUST be free of warnings as produced by
[HLint](http://hackage.haskell.org/package/hlint). The CI for any project MUST
enforce this.

#### Justification

HLint automates away the detection of many common sources of boilerplate and 
inefficiency. It also describes many useful refactors, which in many cases 
make the code easier to read and understand. As this is fully automatic, it 
saves effort on our part, and ensures consistency across the codebase without 
us having to think about it.


Naming convention for Plutarch functions and types: Prefixed with `p` without affecting the case of the second character.
