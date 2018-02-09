---
layout: post
title: Tasks in Modern Elm
read_this_to:
  - learn about chaining effects in Elm 0.18
i_expect_you_to_know:
  - Elm basics
---

## Why tasks matter

The [official guide](https://guide.elm-lang.org/) is great for most things in Elm 0.18, but it still doesn't cover a very important feature of the language: `Task`. With the introduction of Elm 0.17, use cases for tasks shrunk from an everyday occurrence to a "you most likely don't need this" status.

The use case that does remain is tying several side-effects together. A common example of this is when people need a time stamp to go with their HTTP request. `Cmd`s cannot be set to run one after another. One _could_ add a subscription to `Time.every (Time.second)` and have the current time always in the model for example, but a whole bunch of requests can happen in a second, and a millisecond timer is just out of the question. So how do we solve this? Tasks.


## Task basics

Let's first take a look at how tasks work on their own, before we start bundling them up with each other. One of the simplest tasks in `elm-lang/core` just so happens to relate to the example above: `Time.now`. Here's what the [current documentation](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Time#now) says about it:

```haskell
now : Task x Time
```
> Get the `Time` at the moment when this task is run.

If you haven't been dealing with tasks before, you might find it strange that this is not a function that returns a `Cmd msg`. In fact, it is not a function at all but a constant value! What is going on?

I find it helpful to think of tasks as if they were shopping lists. A shopping list contains detailed instructions of what should be fetched from the grocery store, but that doesn't mean the shopping is done. I need to use the list while at the grocery store in order to get an end result. Similarly, the `Time.now` task is an instruction for the Elm runtime to find the current time, but the instruction does not do anything until it is turned into a command and returned in a branch of `update`. Elm is running all the errands with the outside world here, we only need to make sure the command gets returned.

So to use a task, we need to turn it into a command. There are two ways to do this: `Task.perform` and `Task.attempt`. As you might guess from the naming, `perform` simply does the thing, while `attempt` has an expectation of failure involved. In our case, `Time.now` cannot really fail, so let's use perform.

```haskell
import Time exposing (Time)
import Task

type Msg = TimeUpdated Time

getTime : Cmd Msg
getTime =
    Time.now
        |> Task.perform TimeUpdated
```

We can use the `getTime` command just like any other now and when it completes, it results in a `TimeUpdated` message with the current time. Great!

What about tasks that can fail? Let's use `Http.get` as an example. There are plenty of ways for an HTTP request to fail, ranging from network issues to expired authentication tokens. These are all categorized under the `Http.Error` type. If you've made HTTP calls in Elm before, you have probably used `Http.send` to convert the request to a `Cmd Msg`. Instead of doing that, let's take a look at how we can do the same using `Http.toTask`. For simplicity's sake we will use `getString`, which does not need a decoder.

```haskell
import Http
import Task exposing (Task)

-- The message is just like usual, containing a result
type Msg = GotResponse (Result Http.Error String)

-- Here we are defining the task
getResponseTask : Task Http.Error String
getResponseTask =
    Http.getString "https://jsonplaceholder.typicode.com/posts/1"
        |> Http.toTask

-- And here we turn the task into a regular old command
getResponseCmd : Cmd Msg
getResponseCmd =
    getResponseTask
        |> Task.attempt GotResponse
```

We could have put the whole thing in a single pipeline, of course:

```
Http.getString "https://jsonplaceholder.typicode.com/posts/1"
    |> Http.toTask
    |> Task.attempt GotResponse
```

That would be the same as using `Http.send` in the first place, though. As a matter of fact, `Http.send` **is** using `toTask` and `Task.attempt` underneath [(source)](https://github.com/elm-lang/http/blob/1.0.0/src/Http.elm#L85).

A full compiling and working example using an HTTP task can be found [on Ellie](https://ellie-app.com/r9XKVFtjVa1/0).


## Chaining tasks

Now that we've established how we can use single tasks on their own, it is time to use them for their true purpose: effects depending on others. For this we will use a function called `andThen`. Let's see what it looks like.

From the [official documentation](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Task#andThen):

```haskell
andThen : (a -> Task x b) -> Task x a -> Task x b
```
> Chain together a task and a callback. The first task will run, and if it is successful, you give the result to the callback resulting in another task. This task then gets run.

Okay, maybe using the tasks from the previous chapter will make this clearer. Let's imagine we have an API where we can ask for events from the past hour by providing a timestamp of "one hour ago". This example doesn't show the imports, decoders etc. to keep it down to the point.

```haskell
getEventsFromPastHour : Cmd Msg
getEventsFromPastHour =
    Time.now
        |> Task.andThen (\currentTime -> getEventsFrom (currentTime - Time.hour))
        |> Task.attempt GotResult


getEventsFrom : Time -> Task Http.Error (List Event)
getEventsFrom time =
    Http.get (apiUrl ++ "?from=" ++ toString time) eventsListDecoder
        |> Http.toTask
```

Let's try to put this in words.

- Find out what time it is
  - And then, ask for events from an hour before the current time
- When done, tell me how it went

And again, in Elm code:

```haskell
Time.now
    |> Task.andThen (\currentTime -> getEventsFrom (currentTime - Time.hour))
    |> Task.attempt GotResult
```

A full compilable example of this is again [on Ellie](https://ellie-app.com/3ZsdCNsjVa1/0). Sadly I couldn't find a suitable open API to showcase this, but you can see the requests in your browser dev tools.
