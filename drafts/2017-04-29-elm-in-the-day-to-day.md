---
title: Elm in the day-to-day
subtitle: Software consultancy and Elm, how do they mix?
read_this_to:
  - understand how the functional purity of Elm affects day-to-day work
  - know some common cases when you need to reach out to JavaScript
  - see how the interop works in practice
i_expect_you_to_know:
  - customer work in the field of software
  - some functional programming principles (e.g. React is enough)
---

I believe anyone who has worked as a developer knows how to weigh possible solutions on a few levels. There is elegant code and there are customer demands, there are optimized algorithms and there are budget constraints. Like one of my ex-coworkers so eloquently put it:

> Professional programming is a balancing act between craftsmanship and getshitdonefulness.
>
> <cite>&mdash; An ancient [jevakallio](https://twitter.com/jevakallio) proverb</cite>


This balancing act also affects people evaluating technology choices: they need to feel safe with the choice they are making. The technology needs to have a certain level of "getshitdonefulness" so that if the day comes when a deadline is looming like a dark cloud above your head, you know you can get it done.

Elm has gained a great reputation with its beautiful ideology that enforces purity and statelessness. But this is not a solely positive aspect. When it comes to "quick hacks" or wrapping existing JavaScript libraries, Elm seems not to be very allowing. This is often backed by the fact that Elm libraries on [package.elm-lang.org](http://package.elm-lang.org/) need to be individually whitelisted if they contain any JavaScript code.


## Functional purity

A pure function is one that always returns the same result, if you provide the same arguments. Always, no matter when and where you call it. Furthermore, a pure function has no effect on the outside world. It is nothing more than a calculation.

You might be familiar with the basic idea from Redux or Cycle, but probably not in the extent you see with Elm. No matter which framework you are using, there's always a way to introduce side effects almost anywhere in the code. What if you couldn't get the current time or a JSON file from the server in the middle of your functions? In Elm, you can't. The language doesn't have a syntax for doing things like that. Instead, you make a function that returns a command: "I want the current Unix timestamp." If you feed that command into the `main` function, Elm will do that for you. There is no other way of doing HTTP requests or touching the DOM: commands are literally the only way to do effectful things in Elm, apart from `Debug`.

This might seem a little extreme, but as a matter of fact, it feels liberating. Because I know I can't (accidentally or otherwise) alter the world in any function, I don't need to worry about most of the everyday bugs at all. I also know that if a function says it will return an integer, there's no way it can also cause an analytics event. Seriously, this is a _huge_ time saver in project work. It does take some time to get used to, and it does make some simple-sounding things more of a hurdle, but it certainly is worth it.


## The trickier parts
