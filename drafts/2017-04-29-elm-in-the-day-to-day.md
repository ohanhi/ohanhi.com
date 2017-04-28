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
