---
layout: post
title: Elm Decoders and Secret Spy Messages
read_this_to:
  - get a novel point of view into what Elm's JSON decoders are
i_expect_you_to_know:
  - what JSON is
  - some Elm
---

This isn't a post that explains how to do decoding in detail, but introduces a way of thinking about decoding in Elm.

## Background

One thing pretty much everybody learning Elm finds a little hard to understand are JSON decoders. The reason we need make such contraptions in the first place is simple: Elm is statically typed and doesn't have runtime exceptions. To get there from unreliable string data, we have to be explicit about how to read it in.

Let's look at a simple Decoder in two different styles:

```haskell
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias Dog =
    { name : String
    , weight : Float
    }

-- Using the core's Json.Decode
dogDecoder : Decoder Dog
dogDecoder =
    map2 Dog
        (field "name" string)
        (field "weight" float)


-- Using NoRedInk/elm-decode-pipeline
dogDecoder2 : Decoder Dog
dogDecoder2 =
    decode Dog
        |> required "name" string
        |> required "weight" float
```

I really recommend the Pipeline approach. You can handle objects of any size with the same syntax, and never have to switch between e.g. `map2` and `map3`, which is great!

Either way we do it though, we end up with something that's a _constant_: a `Decoder Dog`. This is not a function, so it doesn't do anything. To actually decode something, we need to apply this Decoder to a value, which can be done for example like so:

```haskell
myDog =
    Json.Decode.decodeString
        dogDecoder
        """{ "name": "Lassie", "weight": 31.4 }"""

-- myDog == { name = "Lassie", weight = 31.4 }
```

Here, `decodeString` is what applies our Decoder to the value. In most cases in the real world, this happens somewhere outside of our own code: we provide a Decoder as an argument when making an HTTP request and it will get applied automatically if the request succeeds. The same way of thinking still applies.


## Decoding Secret Spy Messages

When I was a kid I loved all kinds of "spy stuff". My parents would buy me these secret agent kits for presents at times, and I had so much fun taking covert pictures of the neighbours' dogs with my spy camera, listening to strangers' conversations with the incredible paraboloid microphone and so on. Some of the kits also came with secret messages that could only be read with a special flashlight, or by using a code breaker of some sort.

One of these secret messages was like this:

- The message was on a card with seemingly arbitrary characters
- The code breaker was a plastic film with some transparent holes in a mostly opaque block
- Put together, they revealed the secret message hidden within the "arbitrary" characters
  - But only if the code breaker is correct for the message card

---

Here is a mouse-draggable demo of how it felt like:

<div id="secretmsg"></div>
<script src="/js/secretmsg.js"></script>
<script>
Elm.SecretMessage.embed(document.getElementById('secretmsg'));
</script>

If you're using a phone or a tablet, [see this as an animated GIF instead](/img/secretmsg.gif).


## Umm... So how does this relate to decoding?

This is exactly how JSON decoders in Elm work!

- The card with arbitrary data is the JSON
  - We don't know what useful information it might contain without decoding it
  - It can contain a bunch of things we're not interested in, and this won't affect the end result
- The plastic code breaker is the Decoder
  - We need to have it ready before-hand
  - It doesn't "do" anything on its own
- Put together, they reveal the the secret meaning hidden within the data
  - But only if the holes in the decoder and the data structure match up!


Elm decoders are a bit more advanced than a piece of transparent plastic, of course: they also check the format of the data, so a number can't sneak into the place of a string and vice versa. What's even cooler, when a Decoder doesn't match up with the data we get nice readable errors like `Expecting a Float at _.weight but instead got: "whoops"` instead of just random garble.


I hope this helps you think about Decoders in the future!

---

**PS.** The source code for the draggy-droppy thingie is [open source on GitHub](https://github.com/ohanhi/secret-message).


**PPS.** You may have noticed the order of things in Decoders matter.

This is why:


Saying

```
map2 Dog
    field1Decoder
    field2Decoder
```

is the same as saying

```
map2 (\name weight -> Dog name weight)
    field1Decoder
    field2Decoder
```

or even

```
map2 (\name weight -> { name = name, weight = weight })
    field1Decoder
    field2Decoder
```

In essence, `Dog` is being used as a _function to construct the record_. All type aliases in Elm can be used this way, in any context!
