---
title: Elm and the unknown
subtitle: Using Elm for project work
read_this_to:
  - understand how the constraints in Elm affect day-to-day work
  - know some common cases when you need to reach out to JavaScript
  - see what kinds of options you have in practice
i_expect_you_to_know:
  - customer work in the field of software
  - some functional programming principles (e.g. React is enough)
---

## Table of contents

- [Foreword](#foreword)
- [Choose the right tool for the job](#choose-the-right-tool-for-the-job)
- [What sort of a risk is using Elm?](#what-sort-of-a-risk-is-using-elm)
- [When do we need interop?](#when-do-we-need-interop)
- [Preface: Getting info from the DOM](#preface-getting-info-from-the-dom)
- [1. Package catalog](#package-catalog)
- [2. Ports](#ports)
- [3. Installing non-published packages](#installing-non-published-packages)
- [4. elm-ffi](#elm-ffi)
- [Conclusion](#conclusion)

## Foreword

I believe anyone who has worked as a developer knows how to weigh possible solutions on a few levels. There is elegant code and there are customer demands, there are optimized algorithms and there are budget constraints. Like one of my ex-coworkers so eloquently put it:

> Professional programming is a balancing act between craftsmanship and getshitdonefulness.
>
> <cite>&mdash; An ancient [jevakallio](https://twitter.com/jevakallio) proverb</cite>



This balancing act also affects people evaluating technology choices: they need to feel safe with the choice they are making. The technology needs to have a certain level of "getshitdonefulness" so that if the day comes when a deadline is looming like a dark cloud above your head, you know you can get it done.

Elm has gained a great reputation with its beautiful ideology that enforces purity and statelessness. But this is not a solely positive aspect. When it comes to "quick hacks" or wrapping existing JavaScript libraries, Elm seems to be a hindrance. In this post, I explain the tools and strategies to use when Elm's restrictions or relatively smaller ecosystem come in the way of meeting a deadline.


## Choose the right tool for the job

After posting this article, we had a brief discussion with a coworker. He had a good point that I had not adequately stressed in this article: some kinds of applications are better for Elm than others.

The tools we use are intrinsically tied to the demands of the products and services we build. Choose the best fit, not the one you currently like best or are most familiar with. Familiarity grows over time, but poor technology choices will slowly but surely take their toll over the course of the project.

[Miro](https://twitter.com/mironieminen) gave his consent to use this in the post.

---

**Miro**: extremely interesting<br>
**Miro**: I'd like to challenge your conclusions a little bit - because I doubt everything and I'm not really yet sold with Elm. for eg the maps example, it seems that it can be done, but would you do an app which needs heavy maps integration with Elm?<br>
**Miro**: and what about the changes to the language, there was mention about 2-day refactor required by it - what is your gut feeling, are there more radical changes like that still to be expected?<br>
**Miro**: actually that previous thread about this is a good one [link within our internal chat]<br>

**ohanhi**: @Miro, If the application is a map app, I wouldn't do it in Elm.<br>
**ohanhi**: There are cases where Elm is a great choice, there are cases where React is a great choice and there are cases where Handlebars and vanilla JS is a great choice<br>
**ohanhi**: I am definitely not saying that Elm is the _End-all And Only Best Solution For Frontend Get It Now for 49.99!_<br>
**ohanhi**: As with all things, you as a consultant need to consider the fitness of the technology to your overall understanding of what the application is supposed to be.<br>

**Miro**: yeah I know - and thank you for the tip; that &amp; the article itself was really good

**ohanhi**: Cool, thanks. I'll add it.

---

I hope this clears things up for the rest of you, too!


## What sort of a risk is using Elm?

Elm is a young language as far as programming languages go. First version was released in 2012, and it is a <abbr title="Benevolent Dictator For Life">BDFL</abbr> sort of a language like Python. This means there is a single person who guides the language design and prepares new releases. For Elm, this is [Evan Czaplicki](https://twitter.com/czaplic) (for Python, it's [Guido van Rossum](https://twitter.com/gvanrossum)). There is a strong but fairly small community around the core development of Elm.

Elm is ready to be used for production. Many companies, such as [Pivotal Tracker](https://www.pivotaltracker.com/blog/Elm-pivotal-tracker/) and [Ableton](https://twitter.com/AbletonDev/status/861580662620508160), are already using it for real customer-facing things. I have used Elm in two big customer projects myself and another customer project by Futurice was just open sourced ([source on GitHub](https://github.com/Tradenomiliitto/tradenomiitti)).

Personally I've never felt blocked by Elm. As I mentioned, I have used Elm in two customer projects, one of which was heavily based on drag and drop and the other one had things like image uploading. For both of these, I used ports and had no trouble at all (except for dealing with the HTML5 Drag and Drop API).

The language is still in flux, and some [big changes](http://elm-lang.org/blog/farewell-to-frp) have shook up the user community. Removing Signals was definitely the biggest and boldest move I've seen in language design, and it happened right in the middle of one of our customer projects. Keeping a cool head, we waited for a few weeks for the dependencies to be updated, and then I took the plunge.

It took me two working days to get the entire codebase to compile again. Once it did, all but one of our end-to-end tests passed. I had figured that while I was already working on the entire codebase, I could also make some refactoring to how our user authentication worked. The test failed because I had forgotten to wire up something related to logging out. Once that was done, it all worked again.

Had this sort of a change happened in a JavaScript codebase, we would most likely have not had the time to upgrade at all. The confidence I have for the compiler is 100% the reason why I took on this endeavor. I knew if I just keep going through the compile errors, I will emerge on the other side with a codebase that is _sound_. There can't be any undefined variables, missed cases or calls to functions with the wrong number of arguments anywhere.

The restrictions in Elm are what make the language incredibly useful for lean projects: no matter how fast you move, you will always have code that is working and sound.

There are times when Elm alone is not enough, so let's move on to the interop story.


## When do we need interop?

There are three major cases where you may be forced to get outside the safety of Elm:

1. Direct access to the DOM
2. Web platform stuff not yet implemented in Elm
3. Large and/or closed-source third party JavaScript like maps, live customer service chats etc.

For anything else, you can stay within Elm, albeit it might take a little more time to code the feature.

### Direct access to the DOM

In short, Elm doesn't have any manner of getting a reference to a DOM node. This may be a problem in some cases, particularly when you need to call a method on the element like `myDiv.getBoundingBox()`. These are usually quite easily handled with ports, but you do need to use e.g. an `id` attribute to target the correct element.


### Web platform stuff

Certain features like the File API and the Web Audio API are not implemented in Elm yet. Again, ports can probably do what you need rather easily.


### Large third party JS

This is a bit trickier. Depending on how invasive and how uncommon the thing is, you may need to figure out how best to integrate it on your own. Maybe it should be a Web Component within your Elm application. Maybe it is best handled outside of the Elm app as a totally separate part of the DOM. Maybe you have to come up with a client-server API to communicate between Elm and the JavaScript library.

In all of the above cases, the Elm Slack ([join here](http://elmlang.herokuapp.com/)) is a great place to ask for help and ideas!

Now that you have an idea of when some sort of interop is often needed, let's go through the different strategies in the order I would personally use them. The priorities may be a little bit opinionated, so feel free to make your own judgment.


## Preface: Getting info from the DOM

If all you need is to get some information from the DOM, there may be a way to do that in pure Elm. Soren Debois has written [a whole article](https://medium.com/@debois/elm-the-dom-8c9883190d20) on the subject, but it really boils down to this: you can decode any data from a DOM event by attaching a custom listener with `Html.Events.on`. I use this technique in my [autoexpanding textarea](http://package.elm-lang.org/ohanhi/autoexpand/latest) ([source code](https://www.github.com/ohanhi/autoexpand/)).


## 1. Package catalog

This should always be your first step:<br>
**Check if a package solving your problem already exists on [package.elm-lang.org](http://package.elm-lang.org/)!**

The Elm ecosystem is growing by the day, so things that didn't exist last month or even last week might exist now.

Here are some examples of things that do exist:

- "routing": [navigation](http://package.elm-lang.org/packages/elm-lang/navigation/latest) and [url-parser](http://package.elm-lang.org/packages/evancz/url-parser/latest)
- DOM support for focus and scroll
- great date and time handling libraries
- date pickers
- autocomplete input
- keyboard combination helpers


## 2. Ports

Ports are Elm's built-in way to send messages to, and receive messages from, JavaScript. They can be used for every sort of need, though I have to admit the ergonomics are best suited for things that can be classified as _message passing_. This is how they look like:

```haskell
-- Elm to JS
port requestUuid : () -> Cmd msg

-- JS to Elm
port receiveUuid : (String -> msg) -> Sub msg
```

You tie the `receiveUuid` port to a specific message:
```haskell
-- add subscription to the port
subscriptions =
    receiveUuid UuidReceived

-- when a value comes in, you handle it just like any other message
update msg model =
    case msg of
        UuidReceived uuid ->
            useTheNewUuid uuid
        -- ...
```


On the JavaScript side, you simply `subscribe` to Elm to JS ports, and can `send` data through JS to Elm ports.

```javascript
myElmApp.ports.requestUuid.subscribe(function() {
  var newUuid = uuid.v4();
  myElmApp.ports.receiveUuid.send(newUuid);
});
```

**Note**: Ports might strike you as an unnecessarily elaborate way of dealing with something "as simple as" calling external functions. However, once I understood how this approach made all communication to the outside completely safe and decoupled from the uncertainty of JavaScript, I came to really appreciate the idea. [Episode 13](https://elmtown.github.io/2017/05/09/history-in-elm-town-ports-episode-13.html) of the Elm Town podcast is entirely about the thought processes and design considerations behind ports, so I highly recommend listening to it in case you are interested.


## 3. Installing non-published packages

By default, packages containing ports or "native" JavaScript code are not accepted on the package catalog. Some packages have gone through an acceptance process and gotten whitelisted, but one can't simply wrap e.g. Moment.js or Google Maps and publish that to package.elm-lang.org.

Obviously people do that sort of stuff anyway, and there is a way to install those too: [elm-install](https://github.com/gdotdesign/elm-github-install). With this tool, you can add any GitHub repository as a dependency to your `elm-package.json` file and install the dependencies just as easily as you normally would.

Just remember you might be introducing runtime exceptions in your applications this way.

```bash
# when you only have published packages in your elm-package.json
$ elm-package install

# when you have non-published packages in your elm-package.json
$ elm-install
```

**Note**: As well as packages containing JavaScript, you can use elm-install for installing "private packages" from any Git repository or even your own hard drive. See the [advanced usage](https://github.com/gdotdesign/elm-github-install#advanced-usage) section on the readme for more info.  


## 4. elm-ffi

If you have a lot of experience with Elm already, and find that something is too much of a hassle to do with ports, there is an alternative: [elm-ffi](https://github.com/eeue56/elm-ffi). You can install it with the elm-install tool.

With elm-ffi you can create functions - both synchronous and asynchronous - that call native JavaScript functions directly.

```haskell
import FFI

log : a -> ()
log thing =
    FFI.sync "console.log(_0);" [ FFI.asIs thing ]
        |> (\_ -> ())
```

### Installation

Put the necessary lines in your elm-package.json
```javascript
{
  // ...
  "native-modules": true,
  "dependencies": {
    "eeue56/elm-ffi": "1.3.0 <= v < 2.0.0"
  },
  "dependency-sources": {
    "eeue56/elm-ffi": "git@github.com:eeue56/elm-ffi"
  },
  // ...
}
```

Then use the tool on the command line:

```bash
$ elm-install
```

I shouldn't even have to point out that using elm-ffi is a very easy way to make your Elm application unsafe. Only use it if you really deem it necessary.


## Conclusion

There are real use cases to reach out to the JavaScript land in almost any larger Elm application. Most of the time ports are nice enough to write and use. There are more than one way to handle interop, however. I hope this article fleshed out the situation nicely and helps you weigh out the options.
