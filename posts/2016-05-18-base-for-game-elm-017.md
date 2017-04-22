---
layout: post
title: Base for a game in Elm 0.17
subtitle: Revisiting the "hard way"
i_expect_you_to_know:
  - some programming
read_this_to:
  - learn the basics of Elm
  - get acquainted with subscriptions
---

[![](https://img.shields.io/badge/Sponsored%20by-Chilicorn-brightgreen.svg)](http://chilicorn.org/)

Licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Foreword

I first learned Elm about a year ago, in May 2015. I fell in love. I also wrote [an article](https://gist.github.com/ohanhi/0d3d83cf3f0d7bbea9db) describing how to get started with the language by implementing the base for a game. The title of the article, *Learning FP the hard way*, was supposed to be a joke of sorts, as Elm is in fact a very easy language to learn! I'm not sure how many people got that. :)

Since then, I have used Elm in two separate customer projects, and it has definitely [made my work better](http://futurice.com/blog/elm-in-the-real-world)!

The recent update (0.17) meant a [rather large shift](http://elm-lang.org/blog/farewell-to-frp) in the way the language works, so I decided to revisit the original subject. So here it is: the base for a Space Invaders game in Elm 0.17!

You can find all of the code on [GitHub](https://github.com/ohanhi/elm-game-base), along with some setup instructions, but it is also listed at the bottom of this article.

## Elm, what was it again?

Elm is a beautiful little functional language that makes building frontend web applications easy and fun. If you're interested in an overview, I gave a talk about the language at GeeCON 2016, and the annotated slides are here: [Confidence in the frontend with Elm](https://speakerdeck.com/ohanhi/confidence-in-the-frontend-with-elm-1).


## Modeling the problem

First off, let's re-iterate what we want to achieve.

From the player's perspective the program should be like this:

- There is a ship representing the player near the bottom of the screen
- The player can move the ship left and right with corresponding arrow buttons
- The player can shoot with the space bar

And from the ship's perspective the same is:

- Ship has a position on a 1D axis
- Ship can have a velocity (positive or negative)
  - Ship changes position according to its velocity
- Ship can shoot


This gives us a definition of what the `Model` of our little program should look like:

```haskell
type alias Model =
    { position : Float
    , velocity : Float
    , shotsFired : Int
    }
```

This is an example of a data structure called [Record](http://guide.elm-lang.org/core_language.html#records). It is like a strongly typed and immutable cousin of the JavaScript object. Now, we only defined the type of the data so far, so let's create a model to start from:

```haskell
model : Model -- this is a type annotation
model =
    { position = 0
    , velocity = 0
    , shotsFired = 0
    }
```

What we have here is a simple value, or a constant. As everything in Elm is immutable, `model` will always be the same no matter what happens in the app. If we tried to redefine it, the compiler would simply complain that there are multiple definitions for the same name and the code would not compile.

Alright, moving on to moving the ship! I remember from high school that _s = v*dt_, or moved distance is the velocity times timedifference. So that's how we can update the ship. In Elm, that would be something like the following.

```haskell
applyPhysics : Time -> Model -> Model
applyPhysics dt model =
    { model | position = model.position + (model.velocity * dt) }
```

The above is the way to update a record. We start off with the `model` as the base, but update the `position` as per the formula. Note that `{ record | x = newX }` creates a new record, as everything in Elm is immutable. We will never have to worry about affecting anyone else's state by accident. Even better, we can be certain no one else is affecting our state either!

The type annotation on `applyPhysics` says: given a `Float` and a `Model`, I will return a `Model`, but also: given a `Float`, I will return `Model -> Model`. For example, `(applyPhysics 16.7)` would actually return a working function to which we can pass a `Model`, and get the physics-applied ship as the return value. This property is called [Currying](http://en.wikipedia.org/wiki/Currying) and all Elm functions automatically behave this way. Currying is very useful in many cases, but that is a topic for another article.

We can update the other properties in the very same way:

```haskell
updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }

incrementShotsFired : Model -> Model
incrementShotsFired model =
    { model | shotsFired = model.shotsFired + 1 }
```

Using these little functions we can update all of our state, but we're missing something quite necessary:
1. View of the current state, and
2. getting input from the user and turning that into updates.


## Showing the state

Our game wouldn't be much use if it couldn't show the current state in some way. To keep things as simple as possible, let's just print the model as text. We can do it like so:

```haskell
view : Model -> Html msg
view model =
    text (toString model)
```

Here, `toString` turns the `model` record into a readable String representation of it, and `text` from the Html package turns a String into an HTML text node. Pretty handy! Now the strange part here might be the return type of our `view` function: `Html msg`. We don't need to worry about it too much right now, but what the type annotation is saying is in essence: "I am returning some HTML, which may produce messages of the `msg` variety."

This will do for now, so let's move on to the interactive part!


## Subscribing to user input

Elm 0.17 brought a new way of reacting to changes: Subscriptions. What we will do is this: we will subscribe to certain changes in the world, and when they happen, give the changes some names. We want to control the game by keyboard, so let's start by taking a look at the [`Keyboard`](http://package.elm-lang.org/packages/elm-lang/keyboard/1.0.0/Keyboard) package. It seems we want to listen for both pressing down on buttons, and letting go of them. With these, we can determine when the user is pressing down on a certain key. We will need something else aswell: to keep updating the position of our ship, we need to have a somewhat steady rhythm of `applyPhysics` with the time difference! That we can get using the [`AnimationFrame.diffs`](http://package.elm-lang.org/packages/elm-lang/animation-frame/1.0.0/AnimationFrame#diffs). Bundling that up into code looks a little like this.

Defining the messages in our program:

```haskell
type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
```

Here we have a union type. For something to be considered a `Msg` in this module, it will have to be one of the above (`TimeUpdate`, `KeyDown` or `KeyUp`). Furthermore, the contents of e.g. `TimeUpdate` must be something that can be considered `Time`, and so on.

Okay, now let's declare the subscriptions we need, and name them with our newly-defined message types.

```haskell
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
```

This again will just return a subscription, or a `Sub Msg`. It doesn't do anything on its own, but we need it for the actual wiring part of our code:

```haskell
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
```

If you are really paying attention, you might notice that we have `view` and `subscriptions` done by now, but both `init` and `update` are still missing. Luckily we already have all the building blocks, so taking this home shouldn't be too much of a stretch anymore! In fact, `init` is so simple that we should get it out of the way right now.

```haskell
init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )
```

That's all there is to it! Again, we can leave the `Cmd` stuff for later, but just as a primer: commands are the only way to have effects in an Elm program. What are effects? They are anything that can affect the world outside the app (such as posting something to the Internet), or whose value can vary between program runs (such as the current time, or random numbers). Here we don't need to do any commands, so we define it to be `none`.


## Putting it all together: the update

All right, now's the time to make it all work!

Let's begin from the high level. The `update` function takes the incoming message and the old model, and returns the updated model along with possible commands. In this case we won't need any commands, but we still need to fulfill the contract with `none`s.

```haskell
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )
```

Note that each of the possible `Msg` options is handled. If they weren't, the Elm compiler would catch the problem, which is pretty cool and impressive. Anyway, the `TimeUpdate` is nice and easy. We can simply use the `applyPhysics` function to get the updated model. For the keypressing cases, I decided to split the handling into their own functions as well.

When it comes to the packages, Elm 0.17 is still a bit of a work in progress. So to make the keyboard handling a little nicer, I made a tiny helper module. There is a function that can turn a `KeyCode` into a `Key`, which is a simple union type. It only has the keys we need for this exercise now, but could easily be extended.

```haskell
keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        Space ->
            incrementShotsFired model

        ArrowLeft ->
            updateVelocity -1.0 model

        ArrowRight ->
            updateVelocity 1.0 model

        _ ->
            model
```

The above should be pretty clear. Spacebar shoots once as soon as it is pressed down and doesn't do anything else. The arrow keys set the velocity of the ship when pressed down. Notice that we need an "otherwise" case, customarily denoted as `_`. This is because there are many other possible keys on the keyboard besides the ones we've covered.

How about the release part?

```haskell
keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 model

        ArrowRight ->
            updateVelocity 0 model

        _ ->
            model
```

If the released key happened to be one of the movement keys, reset the velocity to 0, otherwise let's just keep the current model. Pretty straightforward, right?


Now it should work!


## The code

Below is the full code, imports and all. You can also find the code on [GitHub](https://github.com/ohanhi/elm-game-base), along with some setup instructions.

**Game.elm**

```haskell
module Game exposing (..)

import Html exposing (Html, text)
import Html.App as Html
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { velocity : Float
    , position : Float
    , shotsFired : Int
    }


model : Model
model =
    { velocity = 0
    , position = 0
    , shotsFired = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        Space ->
            incrementShotsFired model

        ArrowLeft ->
            updateVelocity -1.0 model

        ArrowRight ->
            updateVelocity 1.0 model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 model

        ArrowRight ->
            updateVelocity 0 model

        _ ->
            model


applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    { model | position = model.position + model.velocity * dt }


updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }


incrementShotsFired : Model -> Model
incrementShotsFired model =
    { model | shotsFired = model.shotsFired + 1 }



-- VIEW


view : Model -> Html msg
view model =
    text (toString model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]

```

**Key.elm**

```haskell
module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        _ ->
            Unknown
```

**elm-package.json**

```json
{
    "version": "1.0.0",
    "summary": "Example base for a game in Elm 0.17",
    "repository": "https://github.com/ohanhi/elm-game-base.git",
    "license": "BSD3",
    "source-directories": [
        "."
    ],
    "exposed-modules": [],
    "dependencies": {
        "elm-lang/animation-frame": "1.0.0 <= v < 2.0.0",
        "elm-lang/core": "4.0.0 <= v < 5.0.0",
        "elm-lang/html": "1.0.0 <= v < 2.0.0",
        "elm-lang/keyboard": "1.0.0 <= v < 2.0.0"
    },
    "elm-version": "0.17.0 <= v < 0.18.0"
}
```
