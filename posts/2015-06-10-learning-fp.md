---
layout: post
title: "Learning FP the hard way: Experiences on the Elm language"
---

Originally published as a [public Gist](https://gist.github.com/ohanhi/0d3d83cf3f0d7bbea9db)

with the support of [Futurice](http://futurice.com/).

Licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).


## Foreword

A good friend of mine convinced me about the benefits of [_Reactive Programming_](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754) not that long ago. It almost feels wrong not to write _Functional Reactive Programming_ -- apparently the functional methodology lends itself magnificently to complement reactive programming. How, I did not know. So I decided to learn this stuff.

Knowing myself, I soon realized I would only ever get in the mindset if I had to solve some actual problems using the techniques. Having written JavaScript for the past few years, I could have just gone for RxJS. But again, I knew myself and felt it would give me way too much room for "going against the grain". I needed something that would enforce me to solve everything in a functional manner. This is where Elm comes in.

### Elm? What's that?

[Elm](http://elm-lang.org/) is a programming language that compiles to HTML5: HTML, CSS and JavaScript. Depending on how you display your output, it may be a `<canvas>` with objects inside, or a more traditional web page. Let me repeat. Elm is a language, which compiles into the _three languages_ used to build web apps. What's more, it is a functional reactive programming language with strong types and immutable data structures.

Okay, so you may have gathered I am no expert in this field, but in case you're lost, here are my short explanations on the terminology: [Appendix: Glossary](#appendix-glossary).


## Part I. Restriction proves helpful

I decided to try and make a Space Invaders type of game (or a base for one) with Elm. Let's first consider how it works from the player's perspective.
- There is a ship representing the player near the bottom of the screen
- The player can move the ship left and right with corresponding arrow buttons
- The player can shoot with the up arrow

Okay, let's switch the point of view now and revise from the ship's perspective.
- Ship has a position on a 1D axis
- Ship can have a velocity (left or right)
  - Ship changes position according to its velocity
- Ship can be shooting or not

This basically gave me a definition of what the `Ship` should look like as a data structure, or a [Record](http://elm-lang.org/guide/core-language#records) in Elm terminology. Although optional, I like to define these things as type aliases, which will allow the use of `Ship` in type annotations.

```haskell
type alias Ship =
  { position : Float  -- just 1 degree of freedom (left-right)
  , velocity : Float  -- either 0, 1 or -1
  , shooting : Bool
  }
```

Nice. Now let's create one of those things then.

```haskell
initShip : Ship   -- this is the type annotation
initShip =
  { position = 0      -- the type is Float
  , velocity = 0      -- Float
  , shooting = False  -- Bool
  }
```

So we got to an interesting point already. Take another look at the definition above. Is it a simple statement? Is it a function definition? It doesn't matter! `initShip` can be thought of either as just the literal record defined, or a function that always returns that record. Because the function is pure, and the data structure is immutable, there is no way to distinguish between those. Wow, cool.

> _Sidenote:_ If you're like me, you're now thinking what happens if you try to reassign the `initShip`. Well, a compile-time error happens: "Name Collision: There can only be one definition of 'foo'."

Alright, moving on to moving the ship! I remember from high school that _s = v*dt_, or moved distance is the velocity times timedifference. So that's how I want to update my ship. In Elm, that would be something like the following.

```haskell
applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  { ship | position = ship.position + ship.velocity * dt }
```

The type annotation says: given a `Float` and a `Ship`, I will return a `Ship`, but also: given a `Float`, I will return `Ship -> Ship`. For example, `(applyPhysics 16.7)` would actually return a working function to which we can pass a `Ship`, and get the physics-applied ship as the return value. This property is called [Currying](http://en.wikipedia.org/wiki/Currying) and all Elm functions automatically behave this way.

> _Sidenote:_ So what's the point of all this? Well, say I want to create a cross-reference table of two lists of values of `Item`. I know how to formulate that as "given a list and a simple value, find matching items from the list", or `findMatches : List -> Item -> List`. But I need to map the other list with something that "already knows" the previous list. That's where currying is great: I can just go `crossReference = map (findMatches listA) listB` and be done with it. `(findMatches listA)` is a function of type `Item -> List`, which is exactly what the doctor ordered.

Now getting back to the actual subject, `applyPhysics` created a new record, using the given `Ship` as a base, while setting the `position` to something different. That is what the `{ ship | position = .. }` syntax means. For more on this, see [Updating Records](http://elm-lang.org/guide/core-language#records).

Updating the other two properties of the ship can be done similarly:

```haskell
updateVelocity : Float -> Ship -> Ship
updateVelocity newVelocity ship =
  { ship | velocity = newVelocity }

updateShooting : Bool -> Ship -> Ship
updateShooting isShooting ship =
  { ship | shooting = isShooting }
```

Putting it all together to get the current version of the ship, we could do something like this:

```haskell
-- represents pressing the arrow buttons
-- x and y go from -1 to 1, and stay at 0 if nothing is pressed
type alias Keys = { x : Int, y : Int }

update : Float -> Keys -> Ship -> Ship
update dt keys ship =
  let newVel      = toFloat keys.x  -- `let` defines local variables for `in`
      isShooting  = keys.y > 0
  in  updateVelocity newVel (updateShooting isShooting (applyPhysics dt ship))
```

Now if I could just call `update` 30 times per second, giving it the time difference from last update, the keys pressed and the previous incarnation of `ship`, I'd have a nice little game model going on. Except of course I couldn't see anything since there is no render... but in principle.

So let's just quickly recap what's happened thus far.

- Type aliases define data models
- All data is immutable
- Type annotations clarify the goal of the function
- All of the functions are pure

Actually, there is no way to accidentally change state in the language. Also, there are no loops of any kind.

I've covered quite a lot of ground already with my game. There is a model and all functions that update the model have been defined. Only trouble is that all of the functions operate based on the ship from the previous update. Remember, in Elm you cannot store the state in the shared scope in any way, not even for the current module -- there are no ways to alter anything that's been defined before. So how can anything really change in the program?



## Part II. State is a result of an immutable past

This is the point where some mind-bending realizations need to happen. In the object oriented programming approach, the state of a program is "scattered" among several entities. The `Ship` in this case would be a class and `myShip` would be an instance of that class. At any given time during the program, `myShip` would know the value of its position and other attributes. Not so in functional programming. At any given time during the program, `initShip` will simply be the same thing it was in the beginning. To get the relevant current state of things, I need to look at what has happened in the past. I need to use those happenings as input to the functions I've defined and only then I get the `Ship` as it currently should be. This quite the departure from what I am used to, so I will break down the process.

### The first step

In the beginning there was `initShip` with its dull `0, 0, False` values. There were also functions that could transform a `Ship` into another `Ship`. In particular, there was the `update` function, which would take input and a ship to get an updated ship. I will repeat the function here, so you don't need to scroll.

```haskell
update : Float -> Keys -> Ship -> Ship
update dt keys ship =
  let newVel      = toFloat keys.x
      isShooting  = keys.y > 0
  in  updateVelocity newVel (updateShooting isShooting (applyPhysics dt ship))
```

So if `initShip` is the initial state of the model, I can go one step forward from that, at least. Elm programs define a `main` function, that gets run when the program starts, so let's try showing `initShip` first. I import the `Graphics.Element` package to use the `show` function.

```haskell
import Graphics.Element exposing (..)

-- (other code)
main : Element
main = show initShip
```

This gives us

```haskell
{ position = 0, shooting = False, velocity = 0 }
```

Now if I want to go forward one step, I can apply the `update` function once before showing the ship. Let's try that then. I set the `keys` so that left and up are being pressed to see some effects (`x` is -1 and `y` is 1).

```haskell
dt = 100
keys = { x = -1, y = 1 }
main = show (update dt keys initShip)
```

This gives us

```haskell
{ position = 0, shooting = True, velocity = -1 }
```

All right! So this works! My ship is `shooting` because the up button is pressed, and it has a negative `velocity` to account for the left button being pressed. Notice how the `position` stayed the same, still. This is because I defined the update sequence to first apply physics and then update the other properties. `initShip`'s velocity was 0, so applying physics didn't move it.


### Signals

At this point I'd like you to take your time and read [Signals on Elm-lang](http://elm-lang.org/guide/reactivity#signals), and if you're interested, maybe even watch a video or two about the Elm Signals. I am going to assume you know what Signals are from now on.

But just to recap: a signal is like a stream, where for any given point of time, there is a simple value. So a signal of mouse click count will always hold an integer - in other words, it is of type `Signal Int`. I could make a signal of ships, too, if I wanted: a `Signal Ship`, which would always hold a current `Ship`. But then I'd need to refactor all of my previous functions and take into account that the values are not simple, but in fact signals of that value... So I'll heed the advice from Elm-lang.org:
> A common way to get stuck using signals is to try to use them too much. It is tempting to try to do everything with signals, but it is usually best to write as much code as possible without them.

So my ship can move one step forward, but that's not terribly exciting. I want it to move left when I am pressing left and vice versa. And most importantly, I want it to shoot when I press up!

I have actually constructed my models and logic in a great way, because there happens to be a ready-made signal called [`fps n`](http://package.elm-lang.org/packages/elm-lang/core/2.0.1/Time#fps), which updates `n` times per second. It tells the time difference from the last update. This is the `dt` I need. Furthermore, there is another built-in signal called [`Keyboard.arrows`](http://package.elm-lang.org/packages/elm-lang/core/2.0.1/Keyboard#arrows), which holds the current arrow buttons in exactly the same way I defined `Keys`. This gets updated whenever there is a change.

Okay, in order to get an interesting input signal, I will have to combine these two built-in signals so that "on each update of `fps`, check the status of `Keyboard.arrows` and report both of them".

- "Both of them" sounds like a tuple, `(Float, Keys)`
- "on each update" sounds like [`Signal.sampleOn`](http://package.elm-lang.org/packages/elm-lang/core/2.0.1/Signal#sampleOn)

In code, this should look something like the following.

```haskell
import Time exposing (..)
import Keyboard

-- (other code)
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 30
      -- map the two signals into a tuple signal
      tuples = Signal.map2 (,) delta Keyboard.arrows
  -- and update `inputSignal` whenever `delta` changes
  in  Signal.sampleOn delta tuples
```

Cool, now all I need to do is wire up my `main` so that the input is actually used with the `update` function. For this, I need `Signal.foldp`, or "fold from the past". This is analogous to having a simpler fold like:

```haskell
summed = List.foldl (+) 0 [1,2,3,4,5]
```

Here we start with 0, then sum that up with 1, then sum that up with 2 and so on, until all the numbers are summed up, and we arrive at the return value of 15.

So in short, this should make a lot of sense. `foldp` looks at the value at the "start of time" and folds the whole past of the signal to finally arrive at the present moment -- the entire past of the program reduced to the present state.
Mind.. Blown. Well, for me at least.

Anyway, let's see how that works in code. Now since I will have the `main` function update its results, it should also reflect that in its type, so I'll make it a `Signal Element` instead of just `Element` like before.

```haskell
main : Signal Element
main = Signal.map show (Signal.foldp update initShip inputSignal)
```

A couple of things are happening there:

1. I use `Signal.foldp` to apply `update` with the signal, starting with `initShip`.
2. Folding still results in a signal, since it will continue updating the "folded state".
3. I use `Signal.map` to map the current value of my "folded state" to `show`.

Only this results in a crapton of type errors, the bottom-most of which is the following.

    Type mismatch between the following types on line 49, column 38 to 44:

           Temp9243.Ship -> Temp9243.Ship

           Temp9243.Keys

       It is related to the following expression:

           update

Uhh... Well, at least I know where to look for. My function's type signature looks like this: `update : Float -> Keys -> Ship -> Ship`. However, what I'm passing to it is actually `(Float, Keys)` and `Ship`. So yeah, I'll just change my function signature a little...

```haskell
update : (Float, Keys) -> Ship -> Ship
update (dt, keys) ship =
  -- the same as before
```

... and it works! 🎉

I now have a fully functional model, updates and everything, for my game, in a total of 50 lines of code! The whole thing can be seen here: [game.elm](#gameelm). To see it in action, you can copy-paste the code into the [Try Elm](http://elm-lang.org/try) interactive editor (in case nothing happens, click Compile and then the right-hand side of the screen, then press the arrow buttons).


Again, a quick recap of what happened here:

- A signal is a function of time
  - For each moment in time, there exists a pure value of a signal
- `Signal.foldp` reduces the value in the same sense as `List.foldl`
- Each state of the program is explicitly derived from all the things that have happened in the past


## Part III. The learnings

There was a lot to learn in this endeavor for me. I hope you, too, have learned something while reading this. My personal (subjective) findings were:

- Types are actually pretty nice - and useful
- Immutability and restricting global state didn't feel that strange after all
- Functional programming in Elm is very terse and very readable
- FP makes input and output explicit
- Because of the whole idea of state is so different, it is a bit tricky to grasp, but it does make a lot of sense
- There are no worries about bugs due to weird mixed state as the state is always a direct result of input
- Listening in on updates (reactive), rather than causing things to change (imperative) felt good


One last thing: _if you liked this article, please consider sharing it with your friends. Sharing is caring!_


## Appendix: Glossary

**Immutable data** means that once you assign a value to something, it cannot be changed. Take a JavaScript `Array` for example. If it was immutable, `myArray.push(item)` could not change (mutate) the existing `myArray`, but would instead return a new one with the same items plus the new one.

**Strongly typed** languages look to prevent unexpected behavior by disallowing using a string in place of an integer, for example. Languages like Scala, Haskell and Elm utilize [static type checking](http://en.wikipedia.org/wiki/Strong_and_weak_typing#Static_type-checking) to prevent compiling altogether when there is a type mismatch.

**Pure functions** always return the same value given the same arguments, and have no side-effects. In essence, the function must not depend on anything else besides the arguments, and it must not mutate anything.

**Functional programming** refers to the paradigm where the program is mainly expressed as pure functions.

**Reactive programming** about having something that a component can start listening for, and react to the events as it pleases. In Elm, these listenable things are signals. The component using a signal knows how to utilize it, but the signal has no knowledge of the component(s) that it is affecting.


## game.elm

```haskell
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard

-- MODEL
type alias Ship =
  { position : Float  -- just 1 degree of freedom (left-right)
  , velocity : Float  -- either 0, 1 or -1
  , shooting : Bool
  }

initShip : Ship
initShip =
  { position = 0
  , velocity = 0
  , shooting = False
  }

type alias Keys = { x : Int, y : Int }


-- UPDATE
applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  { ship | position = ship.position + ship.velocity * dt }

updateVelocity : Float -> Ship -> Ship
updateVelocity newVelocity ship =
  { ship | velocity = newVelocity }

updateShooting : Bool -> Ship -> Ship
updateShooting isShooting ship =
  { ship | shooting = isShooting }

update : (Float, Keys) -> Ship -> Ship
update (dt, keys) ship =
  let newVel      = toFloat keys.x
      isShooting  = keys.y > 0
  in  updateVelocity newVel (updateShooting isShooting (applyPhysics dt ship))


-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map show (Signal.foldp update initShip inputSignal)
```
