# Boggle Solver in Haskell

This is a short example program I wrote in Haskell as a potential example in an introductory talk.

It shows (immutable) arrays, the list/nondeterminism monad, and contains a compact recursive Trie implementation which I think highlights how compactly Haskell can express various things.

## Development Velocity

I wrote this solution, including the trie implementation, in less than 45 minutes. Even more dramatic, it worked flawlessly on the first actual run. The type system is so robust that there was very little room for error once the types were written down.

That isn't to say I didn't make mistakes, it took a couple of passes through the compiler before I had the mistakes in the `mkTrie` implementation worked out, but the type system caught them all.

## Boggle

The Boggle part itself shows the list monad and the use of an immutable array for fast indexing. It shows a compact way of enumerating the neighbours of a position in the array by scanning all the indices of the array for those which are sufficiently close.

The List monad is displayed to good effect, using `guard` to eliminate bad cases and nondeterminism to handle one adjacent value at a time.

## Trie

The trie implementation shows several interesting things, though it's probably too in-depth for this 15-minute lightning talk. It uses `Maybe` for good effect, shows how easy it is to write two related but not quite identical functions using a helper for the complex logic, and shows an easy use of pattern matching to handle whether paths in the trie are words or not in `mkTrie'`.

# License

This code is released under the BSD3 license; if you have a use for it in teaching or learning or anything else, feel free to make use of it.

(c) 2013 Braden Shepherdson
