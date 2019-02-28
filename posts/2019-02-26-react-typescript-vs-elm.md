---
layout: post
title: If you're using React, Redux and TypeScript, you would be so happy with Elm!
read_this_to:
  - learn why React+Redux+TypeScript is already quite close to using Elm
  - see how TypeScript and Elm solve the same problems
i_expect_you_to_know:
  - React, Redux and some TypeScript
---

## Introduction

I have used Elm in two client projects spanning about three years total. In my latest project we used React, Redux and TypeScript instead, and that was pretty nice too. This post is meant to be a conversation starter, a thought piece, on "what if we tried Elm for real?"

If you've used TypeScript in a project already, you probably know that the static types it provides can be a real help when adding new features. But where it really shines is refactoring. Changing an action's payload in a JavaScript project is a very risky thing, but the TypeScript compiler can spot many places where things are going wrong so you can be much more confident that the code will work. What if that was the case for every single change in the code base? And you could be 100% certain there are no places left using the old structure once you're done? That's what Elm can give you! Furthermore, the compiler will help you go through all the steps needed while adding a feature, but let's come back to that a bit later.

You might have heard that Redux is [inspired by](https://github.com/reduxjs/redux#influences) the Elm architecture. This is very helpful, since it means I can draw some rather direct analogies between the two. As for the React features, I will mostly talk about stateless components, because I prefer to think of React as only the rendering (view) layer of the system. I try to be light on the TypeScript features, to cut down on the amount of syntax covered.

## Overview of similarities

Let's start with comparing the vocabulary. How do React, Redux and TypeScript features relate to Elm in the overall context of building a single page app?
This table is a big simplification, but hopefully a helpful one.

| Concern                     | React+Redux+TS                  | Elm             |
| --------------------------- | ------------------------------- | --------------- |
| views                       | stateless React components      | `view`          |
| data modeling               | TypeScript types                | types           |
| app state                   | Redux store                     | `model`         |
| input/events                | Redux actions                   | `Msg` (message) |
| updating state              | Redux reducer                   | `update`        |
| effects (e.g. HTTP request) | `redux-loop`, `redux-saga`, ... | `Cmd` (command) |

All in all, the two "frameworks" provide comparable functionality and one can follow a very similar coding patterns in both. To make this clearer, let's walk through how our React application worked in practice, and how the same would look like in Elm. We were using [redux-loop](https://github.com/redux-loop/redux-loop) for the effects, so that's where `LoopReducer` comes from.

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

```js
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

## Full example of a simple UI module

TypeScript (adapted from [this post](https://medium.com/knerd/typescript-tips-series-proper-typing-of-react-redux-connected-components-eda058b6727d))

```ts
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

This might seem strange, but these modules really do achieve the same things and have roughly the same amount of type safety. That is:

- We know the shape of the app state (that there is a `yourName`)
- We are certain about the name and type of the action (that there is a `NameChanged` and it expects a single string)

## Notes

https://medium.com/@martin_hotell/improved-redux-type-safety-with-typescript-2-8-2c11a8062575
