---
layout: post
title: If you're using React, Redux and TypeScript, you would be so happy with Elm!
read_this_to:
  - see how React, Redux and TypeScript is already quite close to using Elm
  - get to know some things where Elm could help you more than TypeScript
i_expect_you_to_know:
  - how it is to work with React and Redux, and some TypeScript
---

## Introduction

I have used Elm in two client projects spanning about three years total. In my latest project we used React, Redux and TypeScript instead, and that was pretty nice too. This post is meant to be a conversation starter, a thought piece, on "what if we tried Elm for real?" If you're thinking no one is using Elm, let me point out that companies like [Microsoft](https://github.com/Microsoft/elm-json-tree-view), [IBM](https://discourse.elm-lang.org/t/ibm-releases-elm-powered-app/2364) and [Ableton](https://learningmusic.ableton.com) have successfully used Elm in production.

If you've used TypeScript in a project already, good for you! You can probably agree that the static types it provides can be a real help when adding new features. And where it really shines is refactoring. Changing function arguments or removing fields from a configuration object in a JavaScript project can be a very risky thing, but the TypeScript compiler can spot many places where things are going wrong. You can be much more confident that the code will work. Now imagine that was the case for every single change in the code base? And you could be 100% certain there are no places left using the old structure once you're done? That's what Elm can give you! Furthermore, the compiler will help you go through all the steps needed while adding a feature, but let's come back to that a bit later.

Before we begin, I want to emphasize that I am not saying Elm is the best solution in all cases. There are very valid reasons to use React, Redux and TypeScript instead! There are valid reasons even for not using a framework at all. This post just focuses on the lovely things I know from Elm, compared to how the same thing worked in the React project. Do what you love and what feels like the best solution for the problem at hand. 💝

## Overview of similarities

Let's start with comparing the vocabulary. How do React, Redux and TypeScript features relate to Elm in the overall context of building a single page app? You might have heard that Redux is [inspired by](https://github.com/reduxjs/redux#influences) the Elm architecture. This is very helpful, since it means we can draw some rather direct analogies between it and Elm. In React, components without any local state correspond to the way Elm views work. I won't talk about too many TypeScript features since they are not the point of this post.

This table is a simplification for sure, but hopefully a helpful one.

| Concern                     | React+Redux+TS                  | Elm             |
| --------------------------- | ------------------------------- | --------------- |
| views                       | stateless React components      | `view`          |
| data modeling               | TypeScript types                | types           |
| app state                   | Redux store                     | `model`         |
| input/events                | Redux actions                   | `Msg` (message) |
| updating state              | Redux reducer                   | `update`        |
| effects (e.g. HTTP request) | `redux-loop`, `redux-saga`, ... | `Cmd` (command) |

All in all, the two "frameworks" provide comparable functionality and one can follow very similar coding patterns in both. The main differences are that in Elm you can only have one `model` and in Redux you could have several stores, and that there are no stateful views in Elm. Everything that changes the UI simply has to be in the `model`.  These might sound like big restrictions, but in my experience they really cut down on the bikeshedding we all end up doing in bigger projects. You never have to argue whether a slice of state should have its own store or not, or if the input value should go in the Redux store or local state. 

With that, let's move on to covering some points that we knew were especially nice about Elm and we had some trouble with in our React, Redux and TypeScript project!

## Everything is safe

"I call it my billion-dollar mistake" said [Sir Tony Hoare](https://en.wikipedia.org/wiki/Tony_Hoare) at a conference in 2009. He was speaking about the null reference, something he came up with in 1965. "My goal was to ensure that all use of references should be absolutely safe, with checking performed automatically by the compiler."

TypeScript is a superset of JavaScript, so it can never remedy the billion dollar mistake. Elm has done it. The way you deal with potentially non-existing values (like the first element in a list) is that you always have a fallback of some sort. This is incredibly reassuring. Even in a large codebase I have never seen before, I can be certain that changes I make will not cause runtime exceptions somewhere else. Also, no matter what kind of deadlines we've been under, there won't be unexplored paths that lead to crashes. We have much more time to focus on the logic bugs instead!

If this idea seems unfamiliar, here's a concrete example of how this works in Elm. Converting a string to a floating point number is a simple case where things might not work out:

```haskell
showNumber maybeNumber =
    case maybeNumber of
        Just number ->
            -- Great, we have the number so we can format it nicely!
            formatNumberNicely number

        Nothing ->
            -- This is the fallback in case the number isn't there.
            -- The code wouldn't compile without this branch.
            "The conversion didn't work out"


showNumber (String.toFloat "3.14159265") --> "3.14"

showNumber (String.toFloat "3 stars") --> "The conversion didn't work out"
```

So in any case, we will get a string out of the `showNumber` function. In practice the best place to handle missing information is usually on the Html view itself. Content is loading? Show a loading view. Request failed? Show a failure view. 

## Reliable types for all packages

To me, the single most appealing feature of a statically typed language is that as a developer I can rely 100% on things like function names and argument types to be correct when the compiler says "Success". If you've used TypeScript for a while, you have most likely come across packages that either don't provide any TypeScript type information, and you need community-provided typings that are out of sync with the package itself, or worse, the package includes typings that are downright incorrect. I have sadly had this experience several times in the past year.

In Elm, the package manager knows the types. They are an intrinsic feature of all Elm packages and not something you can omit or get wrong -- all types in the code must match the documentation for the package to be publishable. Speaking of, all Elm packages have to have documentation for every single function they expose, and semantic versioning is enforced by the compiler too. What's super nice for the user is that all packages have their documentation in the same place ([package.elm-lang.org](https://package.elm-lang.org/)) formatted the same way.

The package ecosystem in Elm is very different from npm. There are far fewer packages, and I feel on average they are incredibly well designed and documented. In general, you don't need many dependencies at all for building a big project -- the language core provides lodash-like utilities and such by default. Things like [sortable tables][table], [date pickers][date], [charts][chart] and [visualizations][viz] have one or two packages that almost everyone needing them uses. On the other hand, there are things that do not exist in Elm like Google Maps (though there are other map packages). For these, you can either [wrap them in Web Components][wc] or use ports, which allow you to freely but safely communicate with the JS land.

[table]: https://package.elm-lang.org/packages/NoRedInk/elm-sortable-table/latest/
[date]: https://package.elm-lang.org/packages/abradley2/elm-datepicker/latest/
[chart]: https://package.elm-lang.org/packages/terezka/line-charts/latest/
[viz]: https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/
[wc]: https://dev.to/lukewestby/talk-when-and-how-to-use-web-components-with-elm-f85

## Compiler helps you finish new features

There are a good amount of places in the code base to go through when adding a new feature in Redux. You need to create the UI, event handler, action creator, action, and reducer branch. It's a lot to remember! TypeScript does not help me remember what parts of the code I was supposed to touch -- which makes total sense as you can use it for so many other things besides Redux apps.

In Elm, you can start with creating the UI part using a message name that doesn't exist yet, and the compiler will then guide you through all of the rest. I know this sounds silly, so let me demonstrate. Starting with the classic counter example you get when you head to [ellie-app.com/new](https://ellie-app.com/new), let's add a reset feature!

1. Add a button to the UI (as line 38): `, button [ onClick Reset ] [ text "reset" ]`.
    <br>➤ Compile. The message will say "I cannot find a \`Reset\` constructor"
2. Realize you need the new message, add `| Reset` to the Msg type (as line 20).
    <br>➤ Compile. The message will say "This \`case\` does not have branches for all possibilities" and mention the missing Reset branch.
3. Recall you need to add the branch to the `update` (eg. as line 32): `Reset -> initialModel`.
    <br>➤ Compile. The program will compile and have a new feature: _a fully working reset button_ 🎉!

## All of the code has good typings

A lovely feature in modern statically typed languages, like TypeScript, is type inference. This means the compiler can figure out the types in your code on its own. Unfortunately the compiler can get confused sometimes, like in the case of filtering specific types of things from an array.

```javascript
type MaybeMessage =
    | { type: 'has-message', message: string }
    | { type: 'no-message' }

const myArray: MaybeMessage[] = [/*...*/]

myArray
    .filter(item => item.type === 'has-message')
    .map(item => item.message) 

// ERROR: Property 'message' does not exist on type '{ type: "no-message"; }'
```

The error can be resolved in a number of ways, such as casting to `any` or creating a type guard function, but it's always a bit of an awkward feeling when you need to tell the compiler what is really going on.

This might sound weird but in my experience the type inference in Elm is flawless. I've written tens of thousands of lines of Elm and not once have I seen a case where the compiler didn't know what the types really are. I have disagreed on many occasions of course, but the compiler has always had it right and not me. It does not matter if you've written type annotations or not, the type inference will work just as well without any hints. As a matter of fact, the compiler only uses your type annotations to check that your expectations and the actual types match up.

## Validating data is not optional

One of the things I've come to love the most in Elm was something I was most confused about when I was first learning the language: JSON has to be decoded before the data can be used in your app. This might sound cumbersome and I have to admit it is a little. 

However, the positives start to outweigh the negatives as soon as you find a discrepancy between what you expected the data to be and what it really is. If you are validating the data (and handling potential errors) right at the border of your app, there won't be any unexpected crashes even if the backend responds something totally strange. This means you immediately know what part of the codebase you need to be touching.

You can do this in TypeScript too with e.g. [io-ts](https://github.com/gcanti/io-ts). It's worth trying out, I promise!

## Conclusion

If you read this far I want to thank you for your time! There are many many things to love about working with React, Redux and TypeScript. If you feel like you're happy with these technologies, feel free to keep using them! Also remember that there are places where the React-Redux world is ahead of Elm: notably there is no official way to do server side rendering or code-splitting in Elm as of yet. These are planned, but if you do need those now it's probably a good idea to hold off for a while still!

If you did get interested in trying out Elm, I suggest starting with the official [guide](https://guide.elm-lang.org/) and then joining the super friedly and welcoming [Slack](http://elmlang.herokuapp.com/). Don't be afraid to ask any beginner questions either, the community loves to help people!

## Appendix: Code example comparison

This section is here just to show how the TypeScript and Elm languages represent the same ideas. TypeScript is definitely a more general purpose language than Elm, which is mixed blessing. On the one hand, you can write whatever code you could in JavaScript (from React, Angular, and Vue.js to CLI scripts). On the other hand, there are no hints or built-in patterns to guide you how to write your React+Redux application. Elm is a language that only has the React+Redux -like application pattern (the Elm Architecture), which means there is considerably less "boilerplate" to tie things together at the type level. This is particularly true for the event handlers -- see the Full module example.

### 1. Set up a type and initial value for the app state

TypeScript

```javascript
interface State {
  yourName: string;
}

const defaultState: State = {
  yourName: '',
}
```

Elm

```haskell
type alias Model =
    { yourName : String
    }

model : Model
model =
    { yourName = ""
    }
```

### 2. Create a union type to represent all inputs and events in the app

TypeScript

```javascript
type Action = { type: 'NameChanged', payload: { text: string } }

// But we also want an action creator
function nameChanged(text: string): Action {
  return { type: 'NameChanged', payload: { text } }
}
```

Elm

```haskell
type Msg
    = NameChanged String
```

There's no need for an action creator in Elm, `NameChanged` is now a function that takes a string and returns a message.

### 3. Create a view that uses the state and can dispatch an action

TypeScript

```javascript
const MyComponent = (props: Props) => (
  <div>
    <input
      onInput={(event: any) => props.onNameChanged(event.target.value)}
      value={props.yourName}
    />
    <h1>Hello, {props.yourName}!</h1>
  </div>
)
```

With React, people usually use JSX, which has it's own semantics and syntactic peculiarities.

Elm

```haskell
view : Model -> Html Msg
view model =
    div []
        [ input [ onInput NameChanged, value model.yourName ] []
        , h1 [] [ text model.yourName ]
        ]
```

In Elm, people use `elm/html`. It is just a collection of functions like `div` that work the exact same way as all other functions, both syntactically and semantically.

### 4. Full example of a simple UI module

TypeScript (adapted from [this post](https://medium.com/knerd/typescript-tips-series-proper-typing-of-react-redux-connected-components-eda058b6727d))

```javascript
import * as React from 'react'
import * as Redux from 'redux'
// import Action and State from someplace
import * as Actions from './actions.ts'
import { State } from './reducer.ts'

interface DispatchProps {
  onNameChanged: (text: string) => void
}

type Props = State & DispatchProps

const MyComponent = (props: Props) => (
  <div>
    <input
      onInput={(event: any) => props.onNameChanged(event.target.value)}
      value={props.yourName}
    />
    <h1>Hello, {props.yourName}!</h1>
  </div>
)

const mapStateToProps = (state: State): State => state

const mapDispatchToProps = (dispatch: Redux.Dispatch<Actions.Action>): DispatchProps => ({
  onNameChanged: (text: string) => dispatch(Actions.onNameChanged(text)),
})

export default connect<State, DispatchProps>(
  mapStateToProps,
  mapDispatchToProps
)(MyComponent)
```

Elm

```haskell
module View exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
-- import Model and Msg from someplace
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput NameChanged, value model.yourName ] []
        , h1 [] [ text model.yourName ]
        ]
```

These modules really do achieve the same things and have roughly the same amount of type safety. That is:

- We know the shape of the app state (that there is a `yourName`)
- We are certain about the name and type of the action (that there is a `NameChanged` and it expects a single string)
