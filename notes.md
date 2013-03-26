# Boggle Solver Talk

This code solves a Google interview question. The question is: given a 4x4 grid of letters and a dictionary with isWord (and anything else you'd like to add), find all the words that can be made in the grid by starting anywhere, and moving to adjacent letters, without reusing the same instance of a letter twice in a single word.

## Exploring the code

### `Board` type

We're using an immutable array to hold the grid of letters. `type` defines a "type synonym", which gives a more concise name (`Board`) to a type (`Array (Int, Int) Char`). This is an array whose indices are pairs of integers, and whose values are characters.

Note that indexing with pairs or triples is much more common in Haskell than nesting one-dimensional arrays.

### neighbours

This function enumerates all possible neighbouring points to a given point. It doesn't care whether those points are inside the grid or not, it just lists all 8 neighbours.

It uses a list comprehension -- you may be familiar with these from Python or other languages. It returns a list of points `p`. One such `p` is built for every combination of `i` and `j`, which are taken from the list `[-1, 0, 1]`. We require that `(i,j)` is not `(0,0)`, because a point is not a neighbour of itself. Finally, we build `p`, which is the absolute point of `(x,y)` adjusted by `(i,j)`.


### search

This is the main recursive function. It works using a powerful feature of Haskell called the "list monad". We're not concerned here with the details of how that works, though I do want to note that it's nothing built into Haskell-the-language. Rather, the language gives us sufficiently powerful tools to define this behavior ourselves. The list monad is part of the standard library. In fact list comprehensions like the one in `neighbours` are a special case of the list monad with syntactic sugar.

`search` takes a lot of parameters: the dictionary, the board array, the word built so far, a list of points that have been used while building this word, and the current point. It returns a list of the words found.

In practice, the list monad is great for this kind of recursive search collecting a list. We begin by grabbing a list of all the neighbouring points. Then, and this is vital, we run the remaining code once for each element in the list. So even though there doesn't appear to be a loop here, most of the function will run once for each neighbour.

Each of those iterations returns a list, which will get concatenated into one list and returned.

First we check two conditions: that the neighbour we've found is legal, ie. it's actually inside the grid; and that it hasn't been used before. `guard` is a general function for monads, that exits early.

Then we construct the new string, which is the string so far plus this neighbour's letter. Note that we can't modify the original, so we call the new one str-prime. If str-prime is not a prefix of any words in our dictionary, we can stop exploring this branch.

We let `childWords` be the result of the recursive call: this is all the words found with the current string as a prefix. If the current string is a word, we prepend it to that list. Otherwise we just return it.

### `boggle`

This is a simple wrapper around the `search` recursive function. It calls `search` once for each element of the `board`, starting a search from each spot, and concatenates the resulting lists of words.

This is a complete solution to the interview problem.

### `main`

A basic wrapper to make this actually executable: read in the system's dictionary file, build a trie out of it to be our dictionary, and turn the grid specified as a command-line argument into an array.

Finally, call `boggle` and print the resulting list.

## Trie Implementation

For this project, I needed an implementation of a trie to use as a dictionary, so that I could efficiently support `isPrefix` and `isWord`. There are, of course, trie libraries in the Haskell package repository. However, the main industrial-strength implementation was quite a bit more powerful than we needed here, and would be more confusing as a result.

I don't want to discuss how my quick trie implementation works in detail, but I do want to mention two things about it:

First, observe that it is about 35 lines long. This is a modest implementation, supporting only constructing a trie and querying it, but not updating it.

Second, observe that it would be trivial to change its type so that it didn't specify `Char`, but would instead be polymorphic, and support any type that defines the `Ord` typeclass for ordered types.


## Meta-discussion

Finally, two meta-points about this code.

First, I wrote the whole thing - the boggle solver and the trie implementation, from opening vim to the first successful run - in less than 45 minutes. I'm a proficient Haskell programmer, but hardly a wizard. Bear that in mind when I mention the second point...

Which is that it worked flawlessly on the first real run. It took me several tries to get it to compile - the type system is ruthlessly strict and I had of course made a few mistakes. Note that these weren't type errors of the nagging, useless kind. They were real problems in my code, real type mismatches that showed I had made a mistake, like using a list where I meant to use just one element.

The benefit of the unforgiving type checker is that once you do finally get your code compiling, you can have a much higher degree of confidence that it is truly correct than you can even in so-called "statically typed" languages like Java or C++.

Of course you can still make mistakes in your logic: writing to the wrong file, miscalculating the subtotal, whatever. But I think that the more errors you can catch at compile time, the better.
