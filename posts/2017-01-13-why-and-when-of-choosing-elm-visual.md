---
layout: post
title: The Why and When of Choosing Elm - the Visual Edition
---

_For a textual version of this content, [see here](why-and-when-of-choosing-elm.html)._


![](img/why-when-elm/why-01.jpg)

Each picture has an expandable text section if you'd like some explanation for the graphics.

<details>
<summary>Click on this to try expanding...</summary>
<p>This is a quick overview of why and when you should consider choosing Elm over JavaScript.</p>
</details>


![](img/why-when-elm/why-02.jpg)

<details>
<summary>What is Elm?</summary>
<ul>
<li>Elm is a language (and "framework") for building web frontend applications</li>
<li>Can be used in place of HTML, CSS and JavaScript, and compiles into them</li>
</ul>
</details>


![](img/why-when-elm/why-03.jpg)

<details>
<summary>JS gets more complex, Elm gets simpler</summary>
<ul>
<li>The amount of features, or the "API surface", of JavaScript grows all the time</li>
<li>Elm's API surface is much much smaller to begin with, and it's actually been shrinking the past few releases</li>
</ul>
</details>


![](img/why-when-elm/why-04.jpg)

<details>
<summary>(Summary of the table)</summary>
<ul>
<li>Elm <strong>does not</strong> have <code>this</code>, <code>null</code>/<code>undefined</code>, runtime exceptions or multiple paradigms</li>
<li>Elm <strong>does</strong> have guarantees of correctness, settled-upon tooling and great ease of refactoring</li>
<li>JavaScript has the opposite</li>
</ul>
</details>


![](img/why-when-elm/why-05.jpg)

<details>
<summary>Lean development is continuous refactoring</summary>
<ul>
<li>The features on the previous picture, especially the fickleness of refactoring and lack of guarantees, leads to the fact that JavaScript is in fact poorly suited for lean development</li>
<li>Elm, on the other hand, is a dream to work with when the requirements change and the software needs to be adapted</li>
</ul>
</details>


![](img/why-when-elm/why-06.jpg)

<details>
<summary>Learning a completely new language may sound like a huge investment, but:</summary>
<ul>
<li>JavaScript is not a very well defined area of expertise. There's TypeScript, Flow, React, Vue.js, Angular, plus the build tools</li>
<li>Essentially every a JavaScript project is a unique combination of libraries and design decisions</li>
<li>Take at least 1-2 weeks for new developers to familiarize themselves with</li>
<li>Elm takes about 2 weeks for new developers to get productive with</li>
</ul>
</details>


![](img/why-when-elm/why-07.jpg)

<details>
<summary>Elm has incredible upsides</summary>
<ul>
<li>Even a newcomer is not likely to inadvertently mess up a codebase, because the language has such strong guarantees</li>
<li>A superb compiler that tells what the problem in my code is, and often suggests how to fix it</li>
<li>The code is very easy to read, and it always follows the same general pattern</li>
</ul>
</details>


![](img/why-when-elm/why-08.jpg)

<details>
<summary>What about the "bus factor", what if Elm's development was dropped?</summary>
<ul>
<li>We'd be fine using the current version of Elm for the foreseeable future</li>
<li>The committee behind JavaScript specifications have the "One JavaScript" principle, which basically means "everything that has been introduced to JavaScript, stays in JavaScript"</li>
<li>There is no reason why you'd **need** the new JavaScript features, and if you want to, you can call that from Elm</li>
</ul>
</details>


![](img/why-when-elm/why-09.jpg)

<details>
<summary>Elm is a poor fit for things like</summary>
<ul>
<li>Mostly static pages (e.g. news websites)</li>
<li>Very short projects where you actually want to use ready-made UI components (e.g. MVP admin UIs)</li>
<li>Lots of integration with terrible third-party JavaScript (advertisements in particular)</li>
<li>Sites that need page-by-page visibility in search engines</li>
</ul>
</details>


![](img/why-when-elm/why-10.jpg)

<details>
<summary>Elm is great for</summary>
<ul>
<li>Single page applications</li>
<li>Bespoke design</li>
<li>Longer project (> 2 months)</li>
<li>Especially helpful for bigger frontend teams</li>
</ul>
</details>
