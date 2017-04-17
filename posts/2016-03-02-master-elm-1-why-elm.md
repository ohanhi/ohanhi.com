---
layout: post
title: "Master Elm pt. 1 - Why Elm"
i_expect_you_to_know:
  - some JavaScript
read_this_to:
  - know what "Elm" is in layman's terms
  - understand what _confidence_ in programming means
  - get a feel of how Elm makes you confident
---

# The case for Elm

Elm is a language for building web frontend. It compiles to JavaScript so that it runs in the browser. What sets it apart from most other "to-javascript" languages is that it really doesn't replicate or try to mend the intricacies of JavaScript. Elm is a language and ecosystem of its own, and it just happens to compile to JavaScript.

It is very common nowadays to write code that is translated into JavaScript the most used browsers can understand. ES6 and JSX are some of the most popular ones, but there's a multitude of others including CoffeeScript, ClojureScript and TypeScript.

Why should you learn Elm then? There are many reasons, such as learning to really think in the _functional mindset_ and seeing how helpful a compiler can in reality be. If you have any JavaScript experience, you probably recognize the phrases "Uncaught ReferenceError: foo is not defined" and "Uncaught TypeError: bar.quux is not a function". This will never happen with Elm. Because of the language design, there are no runtime exceptions. Below is a list of key benefits in my opinion.


# Benefits of using Elm instead of plain JavaScript

- **Strong static types**<br> Find errors fast with readable compiler messages.
- **No `null` or `undefined`**<br> Never miss a potential problem.
- **Total immutability**<br> Focus on what you are writing right now without worrying about outer state.
- **Purely functional**<br> Leads to decoupled and easily refactorable code.
- **No runtime exceptions**<br> Gives incomparable reliability.
- **Reactive by design**<br> FRP isn't opt-in, it is baked right in the language.

Don't worry if you don't know what all of this means, I'll be covering all of these subjects in later posts.


# Confidence in programming

Confidence is a tricky subject. Talking about it very easily comes off as purely subjective. A state of mind, so to say. What I am trying to convey here though is something objective &mdash; how much of programming is about _remembering_ to do things and handle error cases.

I am a simple man. I can't handle an entire codebase in my head. In JavaScript and many other languages this means I need to write tests for trivial things to make sure future me is covered.

I am talking about this:
<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Me writing Haskell: 30% test coverage. Scala: 60% test coverage. Ruby: 97% test coverage. Guess the order of reliability of code..</p>&mdash; Wille (@wfaler) <a href="https://twitter.com/wfaler/status/704807786660687874">March 1, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Elm is right up there with Haskell in terms of reliability. The strictness of the type system eliminates an enormous amount &mdash; if not _the vast majority of_ &mdash; mundane errors. Furthermore, refactoring is incredibly nice. Daniel Bachler had a relatable talk in BerlinJS not long ago: [Fearless refactoring with Elm](http://slides.com/danielbachler/fearless-refactoring-with-elm#/). The refactoring experience most of the time is:

1. Make changes, any changes.
2. The compiler tells what you missed.
3. Go to 1.

When the code compiles, it will work. There will not be any crashes. The only things that can go wrong are application logic. Application logic is what we should be focused on at all times, but many languages don't let us do that. And since Elm enforces immutability and functional style, it is in fact very hard to cause regressions that you don't know about.

I find that when coding Elm, I get to spend most of my time thinking about the actual problem that I am solving. Whereas when coding JavaScript, I spend my time debugging and only every once in a while get to think about the actual problem.
