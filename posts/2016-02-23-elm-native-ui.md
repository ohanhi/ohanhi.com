---
layout: post
title: "Elm Native UI: Writing a React Native app in Elm"
i_expect_you_to_know:
  - what React Native is
  - some Elm
read_this_to:
  - get an inside look into how Elm Native UI was made
---

> **Edited Feb 24, 2016**<br>
> Update on port-related parts: new simpler API and a note on future developments.

We were at the [Reactive 2015](https://reactive2015.com/) conference in Bratislava in November. A lot of people I talked with were interested in Elm, and many asked whether it could be used with a) Node and b) React Native. I knew there was progress on the Node front, but at least [Richard Feldman](https://twitter.com/rtfeldman) hadn't heard anything about React Native being experimented with.

Me and [André](http://staltz.com/) wanted to prove it was possible to write a React Native app using Elm. A weekend of hacking lead to [Elm Native UI](https://github.com/elm-native-ui/elm-native-ui) being born.

[![](/img/elm-native-dribbble.png)](https://github.com/elm-native-ui/elm-native-ui)
<small class="caption">Elm Native UI [logo](https://dribbble.com/shots/2383347-Elm-Native-logo) by [Paul Young](http://paulyoung.me/)</small>


## What we wanted to achieve

We wanted to start with something that would be simple enough, but would have at least some of the characteristics of a real application. From the Elm application, we need to output a tree representing React Native components (`View` and `Text`). From the React Native side, we need to send events that the Elm app wants to listen to.

In the end, we opted for the traditional example of increment and decrement buttons and a counter to display the current value. This would demonstrate that both of the vital parts, the output and the input, work as intended.

![](/img/rn-elm-2.png)

This diagram shows how the whole thing works. Ideally, the Elm application is the only thing a developer needs to work on. React Native works as a "backend" of sorts, relaying messages to and from the native app side to the Elm side.


## Overcoming the Elm border control

Elm's `main` function is special in that it only allows a handful of types, namely `Element`, `Html`, or a signal of either. This means we can't easily have the Elm application produce a component tree (like the virtual DOM) for React Native to pick up as the rendered component tree. Unless we use [ports](http://elm-lang.org/guide/interop#ports), of course.

If you've worked with ports before, you may know that they have a predefined set of [allowed types](http://elm-lang.org/guide/interop#customs-and-border-protection) themselves. The listing mentions Records as one of the allowed types, so my first intuition was to create a self-referential type for the VTree. A `Node` can have a list of `Node`s for children. Sadly, this is forbidden in Elm. Records need to have finite boundaries, and a tree structure is obviously not finite. So instead, I created a tree structure as a union type:

```haskell
type VTree
  = VNode String (List RnStyle.Style) (List VTree)
  | VText (List RnStyle.Style) (String, EventHandlerRef) String
```

To pass the thing to the React Native side, we decided to go with plain JSON values, since they exhibit all the qualities we needed and are allowed through ports:

```haskell
-- "main"
port viewTree : Signal Json.Encode.Value
port viewTree =
  NativeApp.start { model = model, view = view, update = update, init = init }
```

> **Note:** I updated the code sample above to reflect current `master` branch.
>
> Also, Evan Czaplicki, the creator of Elm, contacted me after this post was first published. He said "Supporting a new platform is something that can get special help from the language", so I am hopeful this will become even nicer in the future!


## Ping back

The second big challenge was user interaction. An application ain't much if it cannot react to any user input, after all. In elm-html you set up bindings in the view like so:

```haskell
Html.button
  [ someAttribute
  , Html.Events.onClick address
  ]
  [ Html.text "Click me!" ]
```

We figured we should try and reproduce a somewhat similar API, but couldn't quite get there. We have:

```haskell
RN.text
  [ someAttribute ]
  (RN.onPress address action)
  "Press me!"
```

The reason why the event handler is separate from other attributes is that the elm-html `Attribute` is a Virtual DOM JavaScript property under the hood. Our attributes are (for now) simply styles, as you could see from the type definition:

```haskell
type VTree
  = VNode String (List RnStyle.Style) (List VTree)
  | VText (List RnStyle.Style) (String, EventHandlerRef) String
```

Also, since the React Native `Text` component only allows a single text child, we reflect that in the API.

To bridge the gap between the Elm generated VTree and React Native, there are two things at play:

1) a React Native [app](https://github.com/elm-native-ui/elm-native-ui/blob/master/index.ios.js) that initializes the Elm app with `Elm.worker` and subscribes to its `vtreeOutput` port, and<br>
2) an "Elm Native" [JS module](https://github.com/elm-native-ui/elm-native-ui/blob/master/ReactNative/Native/ReactNative.js) that sets up the listeners for the Elm runtime.

Now the "Elm Native" is something you would generally try to avoid when coding Elm, since there are no guarantees of safety. Also, the way they work is supposed to change in the very near future. But just to clear it up, from the Elm point of view, _native_ refers to JavaScript, which is what the Elm runtime runs on. From the React Native point of view however, _native_ is the Objective-C/Java code that runs on a mobile device. This naming thing is also the reason the project is called **Elm Native UI** and not simply Elm Native.

How the event listeners work under the hood is a bit funky at the moment. The Elm Native module utilizes handler IDs to keep track of them and pass events to the Elm runtime. That is why the `on` function returns an `EventHandlerRef`, which is essentially an integer.

```haskell
on : Json.Decode.Decoder a -> (a -> Signal.Message) -> EventHandlerRef
on decoder toMessage =
    Native.ReactNative.on decoder toMessage
```

We are looking to improve this in the future.


## Did it work out?

Yes it did. There are still big things to overcome, such as the [Navigator](https://facebook.github.io/react-native/docs/navigator.html#content) (though that should change soon) and event handler thing. But all in all it feels like Elm Native UI could actually be a thing somewhere down the line.

A resounding thank you goes to all the [contributors](https://github.com/elm-native-ui/elm-native-ui/graphs/contributors)!

Here's an internetsy representation of what we've accomplished thus far: a GIF.

![](/img/elm-native-ui-capture.gif)

The repository is open source on GitHub: [Elm Native UI](https://github.com/elm-native-ui/elm-native-ui/)


## What's next

I will write another post later on about the future of Elm Native UI. There are some interesting developments, as with [React Native 0.21](https://github.com/facebook/react-native/releases/tag/v0.21.0-rc) the much more reactive NavigationExperimental is becoming a reality.


**Closes [#2](https://github.com/elm-native-ui/elm-native-ui/issues/2)**
