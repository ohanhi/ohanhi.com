---
layout: post
title: The Why and When of Choosing Elm
read_this_to:
  - gain a quick understanding of where Elm fits the picture well and where it poses challenges
---

_For a visual version of this content, [see here](why-and-when-of-choosing-elm-visual.html)._

## What is Elm?

- Language (and "framework") for building web frontend applications
- Can be used in place of HTML, CSS and JavaScript
- Compiles into the above


## Why choose Elm?

### The problem with JavaScript

- Basically nobody knows all of JavaScript - in fact it is getting more and more complex each year
- Different people have very different ways of using the language (object-oriented, functional, this-less, ...)
- With JavaScript, anything may suddenly fail in production because of an oversight
- Nothing can save you from that (except maybe boatloads of menial tests: what if this function is called without arguments, with an empty array...)
- It takes a great deal of discipline to keep code readable and adhere to the project's code style
- Huge potential for regressions when refactoring
- Every project has a wildly different build scheme since there is no "official" or even generally settled-upon toolchain
- It is common for builds to break because a library was updated

➡️ JavaScript is unwieldy in big projects

➡️ JavaScript is in fact poorly suited for lean development, because refactoring is risky


### How Elm solves it

- Elm is a simple and compact language
- It does not have the baggage of JavaScript's history
- Past releases have made it simpler and simpler - they removing language features that cause confusion
- It has a great type system that helps model data and application states
    - This can be used to eliminate an entire category of bugs: arriving in impossible states
- If the code compiles, it is correct (can still have logic errors, of course)
- Elm has no `null` or `undefined`
- Elm has no runtime exceptions except for `Debug.crash`
- All potential cases need to be handled - no surprises or unhandled errors, guaranteed!

> Using TypeScript or Flow can give some of the benefits, but many libraries' typings will still non-exhaustive)

- Elm enforces writing code in a single paradigm and style (functional, no mutable state)
- It comes with great tooling out of the box
- Builds can never break because of library updates, because semantic versioning is ensured by the package manager

➡️ Elm is very well suited for lean development, because refactoring Elm is fun and we can do it with confidence!


### But a new language, surely it's an additional cost!

- JavaScript projects use a combination of e.g. React, Redux, redux-loop, etc.
- They might even be using TypeScript or Flow, which are languages with great improvements over plain JS
    - But they build on top of JS, and as said, basically no one knows even all of JavaScript
- There is very likely no other project with the exact same libraries and/or code style
    - New dev in a JS project needs at least a week or two to learn the project
- Elm is a small language
    - It takes a couple of weeks to be productive
    - About a month or two to master all of it
- The Elm compiler is the state of the art when it comes to [friendly error messages](http://elm-lang.org/blog/compilers-as-assistants)
- All Elm projects follow the same general pattern, because the language doesn't allow anything else
- Elm code is easy to read as it communicates intentions well
- New dev can't inadvertently mess up an Elm project


### What if the language development is discontinued in a year

- Even if the language development was discontinued today, it would be fine
- There's no reason why a project couldn't be using an older version of Elm for years to come
- JS as a compile target is solid, because all old JS code will continue to work in browsers also in the future. (One JavaScript (1JS) principle)



## When to choose Elm and when not to

### What is Elm a poor fit for

- Mostly static pages (e.g. news websites)
- Very short projects where you actually want to use ready-made UI components (e.g. MVP admin UIs)
- Lots of integration with terrible third-party JavaScript (advertisements in particular)
- Sites that need server-side rendering

> Also worth remembering, it can't be used to build a backend


### What is Elm great for

- Single page applications
- Bespoke design
- Longer project (> 2 months)
- Especially helpful for bigger frontend teams
