---
title: "Master Elm pt. 2 - Introduction to data structures"
read_this_to:
  - get an overview on all of the basic data structures in Elm
i_expect_you_to_know:
  - nothing in particular
---

Most people come to Elm from JavaScript, and traditionally JavaScript has really only had two data structures, object and array. Today I will go through all of the basic data structures in Elm, telling when to use them and comparing them to JavaScript as applicable.

_Note: the double dash `--` starts a comment in Elm for the rest of the line, like `//` in JavaScript._

I suggest you to read these in order, but here's a quick navigation just in case:

- [Record](#record)
- [Tuple](#tuple)
- [List](#list)
- [Dict](#dict)
- [Set](#set)
- [Array](#array)


## Record

Records are the closest thing to the object in JavaScript, but they are statically typed:

```haskell
dog =
    { name = "Fluffy"
    , mother = "Muffin"
    }
```

The type of `dog` here is now defined, even though I didn't specify anything. This record has two `String` fields: `name` and `mother`. There's no way of changing it anymore. The order of the fields is not relevant to the type.

Usually we write out the type definition, too. This might be done like so:

```haskell
type alias Dog =
    { mother : String
    , name : String
    }
```

Note that the record was using `=`, and the type alias is using `:`. In Elm, the colon `:` reads as "has the type". Another thing to note is that `type alias` is literally an alias for a type. These are always interchangeable:

```haskell
-- type annotation says:
-- "dogA has the type Dog"
dogA : Dog
dogA =
    { name = "Fluffy"
    , mother = "Muffin"
    }

-- type annotation says:
-- "dogB has the type (record with two String fields, mother and name)"
dogB : { mother : String, name : String }
dogB =
    { name = "Fluffy"
    , mother = "Muffin"
    }

sameDogs =
    dog1 == dog2 -- True
```

The two parts of syntax related to records are getting and setting:

```haskell
-- Getting
dogA.name -- "Fluffy"
-- or
.name dogA -- "Fluffy"

-- Setting (always returns a new record)
{ dogA | name = "Fluuuffy" } -- { name = "Fluuuffy", mother = "Muffin" }
```

Remember that you can also pattern match on records!

```haskell
-- type annotation says:
-- dogToString is a function from Dog to String
dogToString : Dog -> String
dogToString ({ name } as dog) =
    "The name of this dog is "
        ++ name
        ++ " and all of the data is: "
        ++ toString dog
```


## Tuple

Tuples are basically records without field names. Like records, the fields can be of varying types: one piece of a tuple can be a number while the other is a string. They are quite common in Elm, since they are very handy to create on the fly.

Tuples can look a little confusing at first, since they are constructed with parentheses `()` and the items are separated with commas `,`.

```haskell
-- info has the type "tuple of Int, String and String"
info : ( Int, String, String )
info =
    ( 42, "Life", "everything" )
```

You can pattern match on tuples too:

```haskell
getComment : ( Bool, String )
getComment pair =
    case pair of
        ( True, word ) ->
            "Oh yes, " ++ word ++ " it is!"

        ( False, word ) ->
            "Nope, " ++ word ++ " isn't right."
```

Note: If you see the empty tuple `()` somewhere, it's because `()` holds no information whatsoever, and therefore serves as a type (and value) for certain cases where the contents would be ignored anyway.


## List

`List`s are the only collection data structure that has any special syntax in Elm. They are quite similar to the JavaScript arrays. However, Elm lists don't have indices for the items. They are simply sequences of items of a single type.

```haskell
-- dogs has the type "a list containing Dogs"
dogs : List Dog
dogs =
    [ fluffy
    , scruffy
    ]
```

Lists are written with brackets `[]` and items within are separated with commas `,` just like JavaScript arrays.

Adding an item in front of a list is very efficient, because it doesn't affect the rest of the list in any way! For the CS folks, Elm lists are immutable linked lists.
In the drawing below, the blue one is the existing list with items always linking to the next one. The yellow is the new list. As you can see, only the new item `"E"` had to be added, linking into the previous first item `"D"`.

![](/img/data-structures/list.jpg)


You can combine (concatenate) two lists with `++`, and add a single item to the front of a list with `::` (pronounced cons).

```haskell
moreDogs =
    sparky :: [ lassie, fifi ] ++ [ muffin ] -- [ sparky, lassie, fifi, muffin ]
```

Lists are the most used collection in Elm. They are very versatile, but if you need to access, and especially update certain items often, you should look at `Array` and `Dict`.

Oh and also, lists are the only collection you can pattern match on! This is very useful for some algorithms.

```haskell
getFirstTwo : List Int -> Maybe ( Int, Int )
getFirstTwo list =
    case list of
        -- cons (::) can be used to "deconstruct" a list when pattern matching
        first :: second :: rest ->
            Just ( first, second )

        _ ->
            Nothing -- there weren't enough items in the list
```


## Dict

`Dict` is short for dictionary: the data structure has keywords and values. You can only find a value by its keyword, not the other way around. The keywords also need to be unique within the Dict. This data structure is incredibly useful for storing things like translations, but also collections of records that are referenced by their IDs.

Here's a depiction of how data is organized in a Dict:

![](/img/data-structures/dict.jpg)


This is what it looks like in code:

```haskell
type alias Id = Int

authors : Dict Id Author
authors =
    Dict.fromList
        [ ( 0, { name = "Margaret Atwood" } )
        , ( 1, { name = "John Irving" } )
        ]
```

Note that getting a value from a Dict is always an unknown, and because Elm guarantees no runtime exceptions, `Dict.get` returns a `Maybe` value.

Note: The core Dict can only have `comparable`s as keys: numbers and strings, really. If you need other kinds of keys, check out [`eeue56/elm-all-dict`](http://package.elm-lang.org/packages/eeue56/elm-all-dict/latest)!


## Array

First off, if you are looking to use `Array`s, use [`Skinney/elm-array-exploration`](http://package.elm-lang.org/packages/Skinney/elm-array-exploration/latest), which will soon replace the current core implementation.

Array has a fixed size, which makes adding items inefficient. But unlike List, you can efficiently retrieve or update e.g. the 50th item in an Array.

This is how arrays look like in my mind:

![](/img/data-structures/array.jpg)

And this is how it looks in code:

```haskell
words : Array String
words =
    Array.fromList [ "Hello", "world" ]


newWords : Array String
newWords =
    Array.set 1 "you!" words -- == Array.fromList [ "Hello", "you!" ]
```
