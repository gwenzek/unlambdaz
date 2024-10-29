# The Unlambda Programming Language

Unlambda: Your Functional Programming Language Nightmares Come True

## Table of contents

* What's New in Unlambda World?
* Introduction
    * What is Unlambda?
    * What does Unlambda look like?
    * What are the principles of Unlambda?
    * Links and meta-links to other obfuscated programming languages
* Tutorial
    * Functions and application
    * Combinators
    * Abstraction elimination
        * Making abstraction elimination more efficient
    * More Unlambda builtins
        * `v`
        * `.x`
        * `d`
        * `c`
    * HOWTO: various programming techniques
        * How do I write a loop in Unlambda?
        * How can I represent numbers in Unlambda?
        * How can I represent lists (and related data structures) in Unlambda?
        * How do I write tests and booleans in Unlambda?
* A note about the Unlambda Quine Contest
* Implementing Unlambda
    * First-class functions
    * First-class continuations
    * Garbage collection
    * Promises
    * Can Unlambda be compiled?
* Unlambda reference
* Unlambda distribution (download Unlambda here)
* Comprehensive Unlambda Archive Network

## What's New in Unlambda World?

(If you don't know what Unlambda is, skip this section and move directly to the introduction below.)

[2001/08] This page is being revised in preparation of the Unlambda 3 distribution.
Introduction

    “It's disgusting — it's revolting — we love it.” CyberTabloid

    “Unlambda, the language in which every program is an IOUCC.” Encyclopædia Internetica

    “The worst thing to befall us since Intercal.” Computer Languages Today

    “The effect of reading an Unlambda program is like habing your brains smashed out by a Lisp sexp wrapped around an ENIAC. You won't find anything like it west of Alpha Centauri.” The Hitch-Hacker's Guide to Programming

## What is Unlambda?

Unlambda is a programming language. Nothing remarkable there. The originality of Unlambda is that it stands as the unexpected intersection of two marginal families of languages:

* Obfuscated programming languages, of which the canonical representative is Intercal. This means that the language was deliberately built to make programming painful and difficult (i.e. fun and challenging).
* Functional programming languages, of which the canonical representative is Scheme (a Lisp dialect). This means that the basic object manipulated by the language (and indeed the only one as far as Unlambda is concerned) is the function.

Obfuscated programming languages (see below for links) are typically made nasty by either strongly restricting the set of allowed operations in the language, or making them very different from what programmers are used to, or both. (Of course, the goal is to do that while still being Turing-complete.) Unlambda does this (note, however, that the operations permitted were not chosen at random: they have their theoretical importance). But whereas most obfuscated programming languages try to somehow model the Turing Machine paradigm, Unlambda does not use a tape, array or stack. Nor is it binary-oriented; as a matter of fact, it does not manipulate integers in any way. Other remarkable (un)features of Unlambda are the fact that it does not have any variables, data structures or code constructs (such as loops, conditionals and such like).

Rather, Unlambda uses a functional approach to programming: the only form of objects it manipulates are functions. Each function takes a function as argument and returns a function. Apart from a binary “apply” operation, Unlambda provides several builtin functions (the most important ones being the K and S combinators). User-defined functions can be created, but not saved or named, because Unlambda does not have any variables.

Despite all these apparently unsurmountable limitations, Unlambda is fully Turing-equivalent.

Mathematically, the core of the language can be described as an implementation of the lambda-calculus without the lambda operation, relying entirely on the K and S combinators. Hence the name “Unlambda”. It uses head (“eager”, “by value”, “strict”) evaluation. I cannot claim originality there. However, as far as I know, I am the first to have taken this theoretical concept and made it into an actual (deliberately obfuscated) programming language. I added a couple of functions (chosen for their obscurity) to the language so as to make output (and, in version 2, input) possible, or just to make things even more obscure (delay and call/cc are such).

A note on terminology: The phrase “purely functional programming language” is usually applied to languages, like Haskell or Clean, which are lazy and demand explicit sequencing of side effects. I dislike this terminology: for one thing, a “functional” programming language is one in which functions have first-class citizenship, so a “purely functional” one should be one where, as in Unlambda, only functions have first-class citizenship. And what are usually called “purely functional programming languages” should be called, exactly as I just did, lazily evaluating programming languages with explicitly sequenced side effects. All these points are orthogonal: it is quite possible to conceive a lazy programming language which is not functional, or an eager (i.e. non-lazy) functional programming language which still demands explicit sequencing of side effects. In any case, this is to say that I might, on occasion, speak of Unlambda as a “purely functional” programming language, although, with the usual terminology, it is not.

### What does Unlambda look like?

Well, let's discuss an example: the following Unlambda program calculates and prints the Fibonacci numbers (as lines of asterisks)

    ```s``s``sii`ki
      `k.*``s``s`ks
     ``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk
      `k``s`ksk

(All whitespace is optional and arbitrary. Some former versions of this page gave a uselessly complicated and inefficient program.)

You're right: it's not very readable. Writing Unlambda programs isn't really as hard as it might seem; however, reading Unlambda programs is practically impossible. We'll be explaining what all this means later on, but let's just stick to basic observations for the moment.

As you can see, the most common character (essentially, it makes up half of any Unlambda program) is the backquote (ASCII number 96=0x60). The backquote represents Unlambda's apply operation. After that come the S and K combinators (and I, but I can be done away with entirely). Some other characters can occur in Unlambda programs but they are not nearly so common. Besides the backquote and the letters s, k and i, the above program has r and .* as its only other building blocks: these are the Unlambda printing functions (r prints a newline and .* prints an asterisk). The more sophisticated Unlambda functions (v, d, c, e and the input functions) are not used here at all.

### What are the principles of Unlambda?

The number one principle of the Unlambda language is that everything is a function: this is true in the sense that Unlambda is a profile of the pure untyped lambda calculus. (Well, to be honest, the d builtin isn't precisely a function, but we will consider it as such anyway.)

Despite Unlambda being a form of the lambda calculus, it does not have a lambda (abstraction) operation. Rather, this operation must be replaced by the use of the S, K and I combinators — this can be done mechanically using abstraction elimination. Because there is no abstraction, functions are not named in Unlambda (except the builtin ones): there are no variables or such thing. This doesn't mean you can't build up your own functions. Nor does the fact that there are only functions in Unlambda prevent you from coming up with data structures and the like, but you just have to represent them with ad hoc functions. In fact, you can so well build your own structures and such that Unlambda is (and, to work, must be) garbage-collected like any decent high-level language.

So, everything is a function. To start with, you have the builtin functions (i, k, s and the like), and you can do one thing: apply a function F to a function G, the result being denoted `FG. It is from this basic idea that Unlambda is built.

### Links and meta-links to other obfuscated programming languages

* The Random Programming Languages List, by Ben Olmstead (also the inventor of Malbolge, probably the most devilish language in existence), is a quite comprehensive list of evil programming languages. It mentions Unlambda.
* The Turing Tarpit, by Brian Connors (named after an entry in the Jargon File), is a similar list of Bad Languages and other cyberlinguistic horrors. It also mentions Unlambda.
* Ryan Kusnery's list of Weird Programming Languages is also quite good, despite its not mentioning Unlambda.
* Prfnoff's Obfuscated Languages list mentions two languages he wrote (not usually included in similar lists): Fromage and BAK.
* Eric S. Raymond's famous Retrocomputing Museum lists a few thinks that cause a feeling “between nostalgia and nausea”.
* Intercal remains the archetype of the Obfuscated Programming Language.
* Cats-Eye Technologies (used to be http://www.cats-eye.com/ and has moved to http://www.catseye.mb.ca/: thanks to Rafael Kaufmann for pointing this out) hosts a lot of items of related interest, including the famous BrainF*** language, whose name quite appropriately describes the point of all these languages. They also have a page on fortune's lesser-known programming languages.
* The “Institute of Applied Iconoclasm” maintains an Esoteric Languages Database, which lists Unlambda. They also seem to have a very high opinion of it, and of myself ;-)
* The Esoteric Programming Languages Ring of which this site is part:
    Previous 5 Sites | Previous | Next | Next 5 Sites | Random Site | List Sites

## Tutorial

Although the very idea of a tutorial for such an obfuscated language as Unlambda is patently absurd, I shall try to give a brief introduction to the concepts before dwelling in the details of the reference section (which is also very short considering how small Unlambda is as a whole).

### Functions and application

As has been mentioned in the introduction, the only objects that the Unlambda programming language manipulates are functions. Every function takes exactly one argument (that is also a function) and returns one value (that is also a function).

The basic building blocks for Unlambda programs are the primitive functions and the application operation. There are seven primitive functions in Unlambda version 1: k, s, i, v, d, c and .x (where x is an arbitrary characters — so actually that makes 6+256 primitive functions, but we shall consider .x as a single function; the r function is but a commodity synonym for .x where x is the newline character). Unlambda version 2 adds the following new primitive functions: e, @, ?x (where x is a character) and |.

Function application is designated with the backquote (ASCII number 96=0x60) character. The notation is prefix, in other words, `FG means F applied to G.

We'll be explaining in detail what application means exactly, but for the moment, we'll just say that it means that F will do something with the value of G, including applying other functions to it, or applying it to other functions. (That's about the only thing it can do, as a matter of fact.) Just how F does this will become clear later on (or it should). We have to note, of course, that both F and G may themselves be obtained by applying various functions to each other.

The fact that every Unlambda function is unary (takes exactly one argument) means that the backquote notation is unambiguous, and we do not need parentheses (or, if you prefer, the backquote plays the role of the open parenthesis of Lisp, but the closed parenthesis is unnecessary). For example, ``FGH means (F applied to G) applied to H whereas `F`GH means F applied to (G applied to H). To check whether an expression is a valid Unlambda expression, there is a simple criterion: start at the left with a counter equal to the number 1, and move from left to right: for every backquote encountered, increment the counter, and for every primitive function encountered, decrement it; the counter must always remain positive except at the very end when it must reach zero.

Since all Unlambda functions take exactly one argument, when we wish to handle a function of several arguments, it is necessary to “curry” that function. That is, read the arguments one after another. For example, if F is a function that should take three variables, it will be applied thus: ```FG1G2G3. The idea being that F will do nothing but read the first argument and return (without side effects) a function that reads the second argument and returns a function that reads the third argument and finally do whatever calculation it is F was supposed to perform. Thus, both ``FG1G2 and `FG1 are legal, but they don't do much except wait for more arguments to come.

The previous discussion is not so theoretical. Of course, when the user is defining his own functions, he may use whatever mechanism he seems fit for reading the functions' arguments (but such a currying is certainly the best because pairs and lists are so horribly difficult to define in Unlambda). But the builtin k and s functions take respectively 2 and 3 arguments, and the several arguments are passed in the manner which we have just described. (As a side note, I remark that it is, if not impossible, at least inconvenient, to construct functions that take zero arguments because preventing evaluation until all arguments have been read is good but when there are no arguments to be read, the situation is not pleasant; in the pure lambda calculus there is no problem because evaluation order is unspecified and irrelevant, but in Unlambda we have a bigger problem. Here the d function might help.)

A note about evaluation order: when Unlambda is evaluating an expression `FG, it evaluates F first, and then G (the exception being when F evaluates to d), and then applies F to G. Evaluation is idempotent: that is, evaluating an already evaluated expression in Unlambda does not have any effect (there is no level-of-quotation concept as in m4 or SIMPLE).

(Perhaps it would be clearer to describe things by distinguishing expressions and functions, where the latter are obtained by evaluating the former. This is what the Java version of the Unlambda interpreter does, for example (whereas the Scheme version does not). It is merely a matter of choice. True, the distinction might help in understanding the d builtin, since it keeps an expression in its unevaluated form.)

We now turn to the description of the Unlambda builtins.

### Combinators

The k and s builtins are the core of the language. Just these two suffice to make Unlambda Turing complete (although .x is also necessary if you want to print anything). The k builtin is easy enough to describe: it takes two arguments (in curried fashion, as explained above) and returns the first. Thus, ``kXY evaluates to X (evaluated). Note that Y is still evaluated in the process. The s builtin is slightly more delicate. It takes three arguments, X, Y and Z, and evaluates as does ``XZ`YZ.

So, let's get things straight: k doesn't do much until it is applied to two arguments, in which case it throws the second one away and returns the first. As for s, it doesn't do much until it is applied to three arguments, at which point it applies the first to the third, and the second to the third, and the result of the former application to the result of the latter.

To take an example, consider ```skss: here s is applied to three arguments, k, s and s, so it performs the evaluation of ``ks`ss. But here we see that the first k is applied to two arguments (s and `ss), so that it returns the first (namely s), and the final result is s.

We also mention immediately the i function: it is simply the identity function In other words, it takes an argument and returns it intact. The i function is not strictly necessary but it is practical. It could be replaced by ``skk. (Indeed, ```skkX evaluates as ``kX`kX because of the s, which in turn evaluates as X because of the k.)

To summarize, the k builtin is a “constant function constructor”. That is, for all X, `kX is the constant function with value X. The s builtin corresponds to “substituted application”: that is, ``sXY is a function that, instead of applying X to Y directly, will apply each of them to Z (the argument) first, and then one to the other. Finally, i is the identity function.

### Abstraction elimination

We will now try to describe the central process of abstraction elimination. This is not necessary to understand how Unlambda works, but it is necessary to understand how you can do anything with it.

The central feature which appears to be missing from Unlambda is that of variables. This is precisely what abstraction elimination enables us to recover. The problem is, given an expression F that contains, apart from ordinary Unlambda symbols, one “variable” symbol which we will write $x, to build a function that, when applied to some X, will return the value of F with X substituted in place of $x. In other words, we want to build a function (which we will write ^xF) which takes a value X for $x and does some operation (specified by F) on it. This is the lambda (or abstraction) operation of the lambda calculus (our notation ^ is supposed to stand for a lambda). Unfortunately, Unlambda, as its name indicates, does not have this lambda operation, and our problem is to eliminate it, in a systematic way, from expressions (hence the name abstraction elimination). For example, ^x$x (the function of $x which simply returns $x) is supposed to give i (or something equivalent) when abstraction elimination is performed.

So, we take an expression F involving $x, and we want to eliminate the starting lambda in ^xF. We do this by induction on the complexity of F; there are three cases which must be taken into account: either F is builtin (or some variable other than $x, if we permit this), or F is $x, or F is an application, say, `GH, with G and H simpler expressions (which, by induction, we know how to reduce). So we consider these three cases separately:

* In the first case, we want to eliminate the lambda from ^xF, where F does not depend on $x. So it is the constant function with value F. But, as we know, this is `kF. So we know how to eliminate abstraction in this case.
* In the second case, we want to eliminate the lambda from ^x$x. But this is precisely the identity function, so it reduces as i.
* In the third case, we want to eliminate the lambda from ^x`GH, assuming we know how to eliminate the lambda from ^xG and from ^xH. But the function we are considering takes an X and applies it to ^xG, then to ^xH, and finally applies the result of one to the result of the other. This is precisely the role of the s builtin. So ^x`GH is no other than ``s^xG^xH (and by eliminating the lambda from the inner expressions we get what we wanted).

Finally, abstraction elimination is described mechanically as follows: consider the expression F and we want to eliminate the lambda from ^xF. Scan the F expression from left to right: for every ` (backquote) encountered, write ``s (by virtue of the third case above); for every $x encountered, write i (by virtue of the second case); and for any builtin or any variable other than $x, write `k followed by the thing in question.

As an example, the function ^x`$xk, which takes a function and applies that function to the function k, transforms as ``si`kk.

If several lambdas need to be eliminated from one expression, the trick is to start with the innermost ones, and to eliminate the outermost lambdas last. As an example, ^x^y`$y$x becomes first ^x``si`k$x (upon eliminating the innermorst lambda) and then ``s``s`ks`ki``s`kki (upon eliminating the outermost).

If several lambdas are present, i.e. if several abstraction eliminations are performed as explained in the previous paragraph, the length of the resulting expression grows exponentially (with factor 3 for each lambda). A single ` becomes ``s after one abstraction elimination, then ``s``s`ks after a second, ``s``s`ks``s``s`ks``s`kk`ks after a third, ``s``s`ks``s``s`ks``s`kk`ks``s``s`ks``s``s`ks``s`kk`ks``s``s`ks``s`kk`kk``s`kk`ks after a fourth. Similar giveaway sequences appear before any builtin.
Making abstraction elimination more efficient

It is sometimes desirable to obtain a shorter result when performing lambda elimination. Shortcuts can be used, but they demand some care.

Consider the first case we described above. We explained that when F is a builtin or a variable other than $x then we can eliminate the lambda from ^xF by simply rewriting it as `kF. This is quite correct. However, this rule applies a bit more widely than we have suggested: this is true as long as F does not involve x, because it is then a constant, and creating constants is precisely what the K builtin is good at. So if F is a lengthy expression not involving $x, instead of going through the tedious process of eliminating in F, we can shortcut the whole thing and rewrite it as `kF.

There is a danger, however, in so doing. While all this works without glitch in the blessed realm of the pure untyped lambda calculus, i.e. in the absence of side effects (the functions we have seen so far do not create any side effects) and as long as we don't express excessive worries about nontermination, there is a slight difficulty involved when evaluating F causes a side effect or might not terminate. Indeed, when we write ^xF, we probably expect the side effect (or nontermination) in question to be delayed until the function is applied (i.e. until $x receives a value, even if that value is ignored); this is indeed the case if we perform abstraction elimination through the canonical (long) way. If, however, we short-cut and rewrite this as `kF, then F is evaluated as soon as this expression is encountered, even if the function is not applied to anything. This might not be what you wanted.

So the shortcut is really this: you can rewrite ^xF as `kF provided (a) F does not involve $x. amd (b) you can prove that evaluating F terminates and performs no side effect. One easy way of making sure of (b) is to check that the only applications found in F are of the form: the K builtin applied to one argument, or the S builtin applied to one or two arguments, or the D builtin applied to any expression whatsoever (see below about this).

If F does not involve x but does involve evaluations which might (or do) cause side effects or nontermination, there is still a way to perform abstraction elimination from `kF without peering in the entrails of F. Namely, to use the D builtin described below, and write `d`kF. A true purist, however, does not rely on the D to make his program work.

Another shortcut in abstraction elimination is to spot expressions such as ^x`F$x and to rewrite them as simply F — provided of course the variable $x does not appear in F. This is pretty benign if F is just a builtin other than D, or a variable other than $x. But if F can produce side effects, this presents the same risks as the other shortcut we just described (and you can also get around them by writing `dF).

Note also that the V builtin can always be abstracted to itself. (That is, `kv is functionally identical to v.)

## More Unlambda builtins
### `v`

The v function is a kind of “black hole”. It takes an argument, ignores it and returns v. It can be used to swallow any number of arguments.

The v function can be implemented using s, k and i (and hence, using s and k only). Indeed, it can be written using the lambda expression `^h^x`$h$h^h^x`$h$h (which evaluates to ^x of the same thing), and abstraction elimination shows that this is ` ``s``s`kskk ``s``s`kskk (here is an example when some incorrect shortcuts in an abstraction elimination can be disastrous, for example if ` ``s`kk``sii ``s`kk``sii were used instead, as obtained by attempting to reduce ^h^x`$h$h as ^h`k`$h$h).

### `.x`

The .x function is the only way to perform output in Unlambda (note that in Unlambda version 1 there is no way to perform input). This function takes an argument and, like the identity function, returns it unchanged. Only contrary to the identity function it has a side effect, namely to print the character x on the standard output (this writing takes place when .x is applied). Note that while this function is written with two characters, it is still one function; on no account should .x be thought of as something applied to x (and, just to insist, there is no such function as . (dot), only .x (dot x)). The r function is just one instance of the .x function, namely when x is the newline character. Thus, the `ri program has the effect of printing a newline (so would `rv or `rr or `r(anything), but r alone doesn't do it, because here the r function isn't applied: here my note about the impossibility of currying functions of zero arguments should become clearer).

### `d`

The d function is an exception to the normal rules of evaluation (hence it should be called a special form rather than a function). When Unlambda is evaluating `FG and F evaluates to d (for example when F is d) then G is not evaluated. The result `dG is a promise to evaluate G: that is, G is kept unevaluated until the promise is itself applied to an expression H. When that happens, G is finally evaluated (after H is), and it is applied to H. This is called forcing the promise.

For example, `d`ri does nothing (and remains unevaluated), and ``d`rii prints a blank line (because we are forcing the promise). Another point to note is that ``dd`ri prints a blank line: indeed, `dd is first evaluated, and since it is not the d function (instead, it is a promise to evaluate d), it does not prevent the `ri expression from being evaluated (to i, with the side effect of printing a newline), so that when finally d is applied, it is already too late to prevent the newline from being printed; to summarize, the d function can delay the d function itself. On the other hand, ``id`ri does not print a blank line (because `id does evaluate to d). Similarly, ```s`kdri is first transformed to ```kdi`ri, in which ``kdi is evaluated to d, which then prevents `ri from being evaluated so no newline gets printed.

Writing `d`kF is another form of promise (perhaps more customary but at the same time less transparent): when it is applied to an arbitrary argument Y, then Y is ignored and F is evaluated and returned. This possibility has already been mentioned in the discussion on shortcuts during abstraction elimination.

### `c`

The c (“call with current continuation”) function is probably the most difficult to explain (if you are familiar with the corresponding function in Scheme, it will help a lot). I suggest you try reading the call/cc page at this point. c called with an argument F will apply F to the current continuation. The current continuation is a special function which, when it is applied to X, has the effect of making c return immediately the value X. In other words, c can return in two ways: if F applied to the continuation evaluates normally, then its return value is that of c; but if F calls the continuation at some point, c will immediately return the value passed to the continuation.

Note that the continuation can even escape from the c call, in which case calling it will have the effect of going “back in time” to that c call and making it return whatever value was passed to the continuation. For a more detailed discussion, see any book on Scheme or the call/cc page mentioned.

Examples of c include ``cir: here, `ci evaluates to the continuation of the c which we shall write <cont>, and we have `<cont>r: here, the continuation is applied, so it makes the c call return r, and we are left with `rr which prints a newline. Another interesting example is `c``s`kr``si`ki: in this expression, the argument ``s`kr``si`ki (which does not evaluate any further) is applied to the continuation of the c, giving ```s`kr``si`ki<cont> (where we have written <cont> for the continuation in question); this gives ` ``kr<cont> ```si`ki<cont> which evaluates to `r``i<cont>``ki<cont>, hence to `r`<cont>i (this was where we wanted to get), and in this expression, the continuation is applied, so that the c in the initial expression immediately returns i, and the remaining calculations are lost (in particular, the r is lost and no newline gets printed).

Expressions including c function calls tend to be hopelessly difficult to track down. This was, of course, the reason for including it in the language in the first place.

As an exercice in using c, you might try constructing an expression that when applied to v returns i, and when applied to i returns v (this is not possible in the absence of c). Answer is in the HOWTO section on booleans.

## HOWTO: various programming techniques

### How do I write a loop in Unlambda?

We'll explain this with a simple example: how to write a loop that prints “Hello, world!” over and over, followed by a certain number of asterisks, each line having one more asterisk than the previous.

The first step is to write the loop using tail-recursion. We want to write a function <loop>: it will take as its argument a function $f that prints a certain number of asterisks, it will print “Hello, world!”, followed by the asterisks, and a newline, and then it will call itself with a new function that prints one more asterisk.

To get things straight, we assume that <msg> is a function that acts like the identity with the side-effect of printing “Hello, world!”, and we assume that similarly, $f acts like the identity with the side effect of printing the asterisks. Moreover, we will write a function <inc> that takes such an $f and returns a new one that prints one more asterisk.

Our first attempt at writing <loop> is the following: ^f``r`$f`<msg><loop>`<inc>$f. The main ideas are: first, to “increase” $f, we simply call the same function again with `<inc>$f as argument (this is the standard use of tail-recursion to avoid imperative constructions like variable change). Second, to make sure we have the side effects of writing the message, the asterisks and the newline, we apply them to the operator (we could have equally well applied them to the operand), which works since they act like the identity.

Only one thing is wrong with our attempt: <loop> cannot be part of its own expansion. Here is how we get around this: we add one more parameter, $h, to <loop>, and we decide that <loop> will be called with this parameter equal to <loop> itself. Now there is no difficulty in writing the loop: it is ^h^f```r`$f`<msg>$h$h`<inc>$f. Eliminating abstraction, we find <loop> equal to ``s``s`ks``s``s`ks``s`k`s`kr``s`k`si``s`kd``s`kk<msg>k`k<inc> and it remains to write the <msg> and <inc> functions. We obtain them by eliminating abstraction respectively from ^x`````````````.H.e.l.l.o.,. .w.o.r.l.d.!$x and ^f^x`$f`.*$x.

Finally, to start our program, we apply to <loop> the function ^h``$h$hi (i.e. ``s``sii`ki), and our final program is

    ```s``sii`ki
     ``s``s`ks
         ``s``s`ks``s`k`s`kr
                   ``s`k`si``s`k`s`k
                                   `d````````````.H.e.l.l.o.,. .w.o.r.l.d.!
                            k
          k
      `k``s``s`ksk`k.*

(Concerning indentation: the idea is that, if we insert a line break between an expression F and an expression G to which F is applied, then we start G on the same column as F; furthermore, we then always insert a line break after G.)

### How can I represent numbers in Unlambda?

There are many ways to do that. In the previous example, we have (implicitly) represented the integer n by the function that acts like the identity but with the side effect of printing n asterisks. Such a representation is fine for adding integers (just compose the functions, i.e. the addition function is ^m^n^x`$m`$n$x), but you won't be able to multiply them for example.

Generalizing a little we can arive at a representation of integers which is quite standard and which allows any kind of manipulation (i.e. any recursive function on the integers can be represented in the pure lambda calculus using this representation), the so-called “Church integers” (named after Alonzo Church, the inventor of the lambda calculus). Our previous representation represented n as the n-th iterate of the function .*: this is not good because we can't do much with .*. Rather to try to find a function which is general enough to permit arbitrary manipulations (it can be found but it is a bit long), we represent n as the n-th iterate of a parameter function $f. So we write

    <0> is ^f^x$x (i.e. `ki).
    <1> is ^f^x`$f$x (i.e. i).
    <2> is ^f^x`$f`$f$x (i.e. ``s``s`kski).
    <3> is ^f^x`$f`$f`$f$x (i.e. ``s``s`ksk``s``s`kski).
    and so on…

Using this representation, the various operations are quite simple to perform on numbers:

    The function <print> which prints a Church integer as a line of asterisks is ^n`r``$n.*i, i.e. ``s`kr``s``si`k.*`ki.
    The function <inc> which increments a Church integer is ^n^f^x`$f``$n$f$x, i.e. `s``s`ksk (this is a very highly optimized abstraction elimination).
    The function <add> which adds two Church integers is ^m`$m<inc>, i.e. ``si`k`s``s`ksk.
    The function <mul> which multiplies two Church integers (by applying them consecutively to the same function) is ^m^n^f`$m`$n$f, i.e. ``s`ksk.
    The function <pow> which raises its second argument to the power of its first, by applying the first argument to the second (thus multiplying it with itself as many times as given by the first) is ^m^n`$m$n, i.e. i (impressive, isn't it?).

As an example of the use of Church integers, we construct an Unlambda program that prints a line of 1729 stars. To do this, we use the fact that 1729 is the sum of 103 and 93 (Srinivasa Ramanujan in memoriam), so we write the program as `<print>`^n``<add>`<3>$n`<3>`<inc>$n`<2><3>, (we have used the fact that 9 is 32 and 10 is its successor), so `<print>```s``s`k<add><3>``s`k<3><inc>`<2><3> and after replacement our program is finally

    ```s`kr``s``si`k.*`ki
     ```s``s`k``si`k`s``s`ksk``s``s`ksk``s``s`kski
       ``s`k``s``s`ksk``s``s`kski`s``s`ksk
      ```s``s`kski``s``s`ksk``s``s`kski

How about comparing the Church integers? Sure enough, that can be done. I suggest the following — however, keep in mind that their performance (both in size and time) is far worse than the previous functions, and they are on the whole far less “polished”.

    The function <test> which returns i if its argument is nonzero, and v otherwise, is ^n``$n`kiv, i.e. ``s``si`k`kiv. A faster (but longer) version of the same uses call/cc to return immediately when the argument has been discovered to be non-zero: ^n`c^q`v``$n$qi, i.e. ``s`kc``s`k`sv``ss`k`ki.
    The function <leq> which returns i if its first argument is less or equal to its second, and v othwerise, is ^m^n```$m<A>^ti``$n<A>^tv where <A> is ^f^g`$g$f (I suggest working this out to understand the modus operandi); so <leq> is ``s``s`ks``s`kk``s``si`k``s`k`sik`k`ki`k``s``si`k``s`k`sikv.

To decrement (and hence to substract) Church integers is by no means impossible. I don't know if it can be done with even moderate efficiency, however. The following scheme will work, but it is hopeless algorithmically:

    Consider ``$ni`ki: it evaluates to `ki for any Church integer $n, and, if $n is `k`kk, then it evaluates to k. Consequently, ^n````$ni`ki`ki`<inc>$n returns `<inc>$n if applied to an argument $n which is a Church integer, and `ki (i.e. the Church integer <0>) if applied to `k`kk. Call this function <_inc>. It is ``s``s``s``si`ki`k`ki`k`ki`s``s`ksk.
    Consequently, the function <dec>, which decrements a Church integer, can be written as ^n``$n<_inc>`k`kk, i.e. ``s``si`k``s``s``s``si`ki`k`ki`k`ki`s``s`ksk`k`k`kk. Note that this function takes <0> (i.e. `ki) to `k`kk) — so you should only use it on Church integers which were <test>ed as non zero.
    The substraction function can be built from the <dec> function. I won't even write it because it's too ugly.

Note that the Church integers are by no means the only way to represent integers in Unlambda. Essentially, there are two paths: you can either use a representation which is particular to your problem at hand (as we did above) and which is not completely general, or you can use a universal representation, i.e. one which can be transformed into the Church integers and vice versa (and which differs only by questions of convenience). An example of a non-universal representation is: representing n by a function that prints n asterisks. An example of a universal representation is: representing n by a list of length n, or representing n by a function which evaluates its argument applied to i n times.


### How can I represent lists (and related data structures) in Unlambda?

We discuss how to create two types: products (i.e. pairs) and unions.

To create products we represent the pair of $u and $v as a function which is capable, on demand, of producing one or the other variable:

    The <cons> function (creates a pair from its two arguments) is ^u^v^f``$f$u$v, i.e. ``s``s`ks``s`kk``s`ks``s`k`sik`kk.
    The <car> function (returns the pair's first element) is ^p`$pk, i.e. ``si`kk.
    The <cdr> function (returns the pair's second element) is ^p`$p`ki, i.e. ``si`k`ki.

A union type is one which is capable of retaining one of two cases, with one datum in each case, and distinguish the two cases. It is dual to a product type:

    The <q1> function (creates a union in the first case) is ^u^f^g`$f$u, i.e. ``s`k`s`kk``s`k`sik.
    The <q2> function (creates a union in the second case) is ^v^f^g`$g$v, i.e. ``s`kk``s`k`sik.
    The <switch> function (takes a union and two functions and applies the first one to the union's data in the first case, the second one in the second case) is ^q^f^g``$q$f$g, i.e. i.

What you can do with these types is up to your imagination. A list might be created as a chain of pairs, for example. There is a little difficulty at the end of the list (to represent the empty list, that is). The obvious way around it is to represent a list by a union type, where the first case represents the empty list and the second case represents a pair of the first element and the rest of the list. This works but it is long. Another solution is use v for the empty list: this is more practical for some operations (essentially, because it is shorter), and less for others (essentially because v is a bit tricky to detect, and requires call/cc for that).

### How do I write tests and booleans in Unlambda?

A pair of boolean values is a pair of Unlambda functions used (arbitrarily) to represent “true” and “false”. The essential thing is that they be universal, i.e. there should exist an <ifthenelse> function which takes three arguments: if the first is the boolean <true> then it returns the second, if the first is the boolean <false> then it returns the third.

Many pairs of functions can be used to represent booleans. Here are a few suggestions:

    i and v: this choice is the “standard” (or “internal”) Unlambda choice of booleans (because it is used by the input functions, and because the functions are already part of the language). They have some advantages, for example then <and> function is very easy to write (it is ^p^q`$p$q, hence i). They are frequently useful in the case where $f is a function which performs some side effects (when passed an argument $x, say): if you want to make the side effects happen according to the value of a boolean $b, just replace `$f$x by ``$b$f$x. However, because v is difficult to test, the <ifthenelse> function is a bit complex: it is ^b`c^q``k`ki``$b$qk, i.e. ``s`kc``s`k`s`k`k`ki``ss`k`kk
    k and `ki: this choice is also very standard. The advantage is that the <ifthenelse> function is extremely simple: it is ^b^x^y``$b$x$y, i.e. i (notice how i plays many roles in Unlambda?). The perspicacious reader will observe that, because of this, we build the <ifthenelse> of the other boolean pairs by converting them to this pair.
    ^x^y`$x$y (i.e. i) and ^y^x`$y$x (i.e. ``s`k`sik). Then the <ifthenelse> is ^b``$b`kk`k`ki, i.e. ``s``si`k`kk`k`k`ki.
    i and `ki: these are the two first Church integers. The <ifthenelse> function is then ^b``$b`kk`ki, i.e. ``s``si`k`kk`k`ki.

## A note about the Unlambda Quine Contest

Recall that a quine is a program that prints its own listing. By the fixed point theorems in logic, such a program exists in any Turing-complete language in which printing an arbitrary string is possible (by a computable program of the string — a technical criterion which is satisfied in all programming languages in existence). Although the fixed point theorem is constructive (and thus actually algorithmically produces a quine), actually writing down the program can be difficult. See my quine page and my personal collection of quines for examples of quines in (ordinary, non obfuscated) programming languages.

From 1999/10/27 to 1999/11/03, I opened the Unlambda Quine Contest: I had written a quine in Unlambda myself, and I invited anyone else to do so. During that week, the quines were kept secret (only their md5 fingerprint was revealed so that it could be later checked), in order that their independence be guaranteed. I offered a copy of the Wizard Book to the first person to produce a quine (retrospecively I find that I should have offered it to the best quine, or to the shortest one, or some such thing, but no matter).

The contest is now over. Olivier Wittenberg (olivier.wittenberg@ens.fr) won the prize with his one megabyte quine that he sent me within a few hours of the contest's opening. Subsequent quines were written by Panu Kalliokoski (Panu.Kalliokoski@nokia.com), Jean Marot (jean.marot@ens.fr), Denis Auroux (denis.auroux@ens.fr) and Jacob Mandelson (jlm@ghs.com).

All these quines are truly gems (and, once again, I congratulate all the authors). The shortest one is only 491 bytes long, and was written by Jean Marot. The most efficient one (in terms of data/code size ratio) was written by Denis Auroux. Jacob Mandelson's quine is also very remarkable in that it minimizes the number of dots (dots are printing functions in Unlambda) to only 60.

The full list of quines can be found in the quine/ directory on the FTP repository of the Comprehensive Unlambda Archive Network.

I don't have any special remarks to make about how quines can be written in Unlambda. My general quine page already contains everything I have to say about quines; as for the particular case of Unlambda, well, for my part, I used a list representation of the data, but many more subtle and compact representations were found later.
Implementing Unlambda

Writing programs in Unlambda is a good exercice in patience, order and method. It is not fundamentally difficult, being mainly a question of applying abstraction elimination to expressions of the untyped lambda calculus (of course, obtaining these expressions in the first place is not necessarily evident); if you're messy it will turn into a nightmare. Debugging or reading Unlambda programs is just about impossible.

Another problem is to write implementations of the Unlambda language (i.e. interpreters, that is, a program that takes an Unlambda program as input, and executes it). This is a task that demands less caution (an Unlambda interpreter is far easier to debug, or to read if someone else wrote it, than an Unlambda program) but more smartness, especially if the targeted language (the language in which the interpreter is written) is a low-level language like C. For the Unlambda programming language combines the difficulties of its two families: of functional languages as far as writing an interpreter goes and of obfuscated languages as far as writing programs goes. At any rate, Unlambda certainly has some nasty features that make it hard to write an interpreter for; and we now discuss some of these nasty features and how to get around them.

Writing an Unlambda interpreter is certainly pedagogically interesting. I think it can be a good introduction to some important concepts in theoretical computer science, because it presents the major difficulties (the “nasty features” as I called them a minute ago) of high-level language interpreters without some of the technical burden of typing or variable bindings (these are interesting also, but it may be good to postpone learning about them to a later lesson). For my part, I certainly learned a good deal by writing various Unlambda interpreters. Unfortunately, I have neither the place nor the knowledge to go in the details of all the important theoretical concepts, but you will learn a lot by trying to write an Unlambda interpreter yourself.

The first interpreter written was the one in Scheme, which forms the Unlambda 1.0.0 distribution. This interpreter is not really interesting, for one thing because Scheme has so many features it makes it almost trivial to interpret Unlambda in. The Unlambda 2.0.0 distribution, however, includes interpreters in several other languages. I suggest beginners to start with the one in Java. It is only of pedagogical interest (being very inefficient), and I have taken care to put a lot of comments in it, and to write it very cleanly (despite the comments themselves' claim to the contrary).

One question which is yet open, however, is how difficult it is to write an Unlambda interpreter in Unlambda. That more or less combines the two classes of difficulty (writing Unlambda programs and writing Unlambda interpreters). I still think that the first class (the difficulty of writing Unlambda programs) would be the dominant one; for one thing, Unlambda (as Scheme) already has all the necessary bells and whistles (such as a call/cc function) making interpreting Unlambda (relatively) easy.

Basically, any functional language interpreter is centered, as explained in the Wizard Book, on the so-called eval/apply cycle, with two functions, eval and apply, calling each other recursively. Eval takes an expression of the language and evaluates it (i.e. executes it), whereas apply takes a function (already evaluated) and some arguments, and applies the former to the latter. Eval calls on apply to evaluate applications (what in Unlambda we write `FG), and apply calls on eval to evaluate the body of a function (in Unlambda, we can argue that functions don't have bodies, but still, when apply is told, for example, to apply s to three arguments X, Y and Z, it calls eval on ``XZ`YZ).

So, the basic Unlambda interpreter (forgetting about d for the moment), written in a sufficently high-level language is very simple: eval evaluates all the builtins to themselves and calls apply to evaluate applications; in turn, apply makes ``kXY into X and ```sXYZ into ``XZ`YZ (which is again fed to eval). Certain features of Unlambda, or unfeatures of the meta language, will complicate the evaluator.


## First-class functions

Unlambda has first-class functions. In my sketchy description above, I've simply omitted the important fact that an Unlambda function can be something like `sX (a “partially applied” s).

If the underlying language (the language in which the interpreter is written, aka the “meta” language) has first-class functions, then the obvious thing is to represent Unlambda functions by functions of the underlying language, in which case the apply function of the interpreter becomes particularly trivial (it is just the apply function of the underlying language). Actually, this is not what has been done in the interpreters that accompany the Unlambda distribution (for one thing, because it was more tempting to make the eval function trivial than the apply function), even in languages where this would have been possible (see the SML/NJ version of the interpreter for a good example of this). But it is something worth thinking on (an Unlambda interpreter in Unlambda would probably use this system).

Rather than using first-class functions of the underlying language to represent first-class functions in Unlambda, we can represent them using data structures: represent `kX as a function k1 with a hidden parameter X: applying k yields k1, and applying k1 yields the hidden parameter. These hidden parameters are what would correspond, if we were interpreting a real high-level (functional) language, to closures (i.e. function environments). It should be noted that these closures can become arbitrarily complex (indeed, they are the only kind of data structures we have in Unlambda), and that they will require some kind of memory management (see below).

Furthermore, if the underlying language (say, CAML) has first-class functions and is tail-recursive, then, even if it does not have first-class continuations, the difficulties we have with implementing the continuations of Unlambda are greatly alleviated. Indeed, we can then rewrite the interpreter in Continuation Passing Style (see below) and represent the (passed) continuations as functions of the underlying language, which get called in a tail-recursive manner.

If the underlying language does not have first-class functions, then they must be emulated by means of data structures (indeed, the only “variable” part in a first-class function is its closure, and that can be represented by a data structure, since the code is always the same). This is more or less clear in the Java version of the Unlambda interpreter (Java does not have first-class functions, so we use classes and methods instead, as we are supposed to).

## First-class continuations

Continuations are the major pain for implementing Unlambda when the underlying language does not have them. I refer to my call/cc page (hoping for it to be finished some day) for a more detailed discussion on first-class continuations.

Essentially, the canonical method is to rewrite the interpreter in Continuation Passing Style. Then eval and apply take one more argument: a continuation, and instead of returning their result, they throw that result to the continuation they were given (the continuation represents the “future of computation” at this point, and it is the continuation which will call the further eval and apply functions as needed).

If the underlying language has first-class continuations, of course, then we do not need CPS, because we can represent the continuations of Unlambda by continuations in the meta language (this is what has been done in the Scheme and SML/NJ versions of the interpreter). If it does not, but at least it has first-class functions and is properly tail-recursive, then we can very easily rewrite the interpreter in CPS, by representing continuations of Unlambda by functions in the meta language (this is what has been done in the Caml version of the interpreter, since Caml has first-class functions but not first-class continuations; it may be instructive to compare the Caml version with the SML/NJ version). In this case, the eval and apply functions each terminate by calling their continuation in tail-recursive manner, so tail-recursion is heavily used.

If the underlying language (or, more precisely, its implementation) is not properly tail-recursive, then we cannot use CPS directly, because CPS calls are tail-recursive, they never terminate (except at the very end of the program), so in a non properly tail-recursive language, this will give a stack overflow (consider, for example, the RCS revision 1.5 of the Unlambda interpreter in Java that is included in the distribution). There are various ways to work around this. I don't know what is “standard”, if anything. One way which I find elegant is to introduce “tasks”: rather than having apply and eval never terminate and finish by calling their continuation, have them return a “task”, which is something like a continuation plus a value about to be thrown to the continuation, and when the task is run, it proceeds with the computation. For details, consider the changes between RCS revisions 1.5 and 1.6 of the interpreter in Java.

[All this discussion does not specifically concern Unlambda. It should be moved to my call/cc page when I find the time.]

If the underlying language has neither first-class continuations nor first-class functions nor proper tail-recursion, as is the case of C, then things are even more messy. All the missing abstraction layers have to be built up from scratch. First-class functions, as noted earlier, have to be replaced by the appropriate data structures, both in the handling of the Unlambda functions themselves, and in the handling of the Unlambda continuations. Yuck.
Garbage collection

As in any language having first-class (higher-order) functions, and, therefore, escaping closures, the lifetime of the various structures is not statically determined in Unlambda, and some kind of automatic memory management (aka “garbage collection”) is necessary. If the underlying language has first-class functions and Unlambda functions are represented by functions of the meta language, then the garbage collection system for the meta language is used in Unlambda as well, and all is transparent. If it does not, data structures must be used to replace the missing functionalities, and these data structures have to be garbage collected. If the underlying language has garbage collection (e.g. Java), then all is for the better, because, there again, we can rest on the existing structures. But if it doesn't, some kind of memory management has to be added to the interpreter. The simplest solution is to use an external garbage collector (for example, the Hans Boehm conservative C/C++ garbage collector, which I used in the C version of the interpreter).

But, as Jacob Mandelson (jlm@ghs.com) pointed out to me (and as he demonstrated in his Unlambda interpreter, full garbage collection is not necessary in Unlambda. Indeed, since the language is without side-effects, and in particular without the possibility of modifying pointers (in the closures) once they are created, new objects can only point to older objects, and cycles cannot be created. Under those circumstances, a simpler memory mangement system will suffice: reference counting (i.e. keeping a count of the number of references to each object, and freeing a pointer when the reference count becomes 0). The interpreter present in the c-refcnt/ directory of the Unlambda distribution uses this reference counting method (and is otherwise identical to the garbage-collected interpreter in the c/ directory).
Promises

Promises (i.e. the use of the d function) make Unlambda a bit more of a mess. Without them, the eval function would be completely straightforward: call eval on the operator, call eval on the operand, and then call apply of the former on the latter. But in fact, the result of the first eval must be checked: if it happens to be d, then the further steps are not performed, the operand is bundled (unevaluated) in a promise, and that promise is returned (e.g. thrown to the continuation) as the result of the computation. Promises are forced in the apply function: when apply receives a promise as operator, it must call eval to force the promise, and to evaluate the part that was left unevaluated, and finally apply it to the operand.

It may, therefore, seem that the apply function will never receive d as operator (it is held back at the level of the eval function). Indeed, if you consider the SML/NJ or Caml versions of the interpreter, that part of the pattern matching is commented out. But there are subtleties: what about something like `cd: the Unlambda specifications clearly state that this evaluate `d<cont>, with <cont> being the appropriate continuation. But instead of constructing the `d<cont> expression and calling eval on it, we may prefer to directly call apply on d and the appropriate continuation, in which case apply will, indeed, receive d as operator. (See the note in the invoke method of the DelContinuation class in the interpreter written in Java.)
Can Unlambda be compiled?

An interesting question, and one whose answer I do not really know, for one thing because I'm not entirely certain as to what “compiling” should mean.

On the one hand, we can certainly write a program (technically, using the snm theorem) that takes an Unlambda program, possibly parses it, and bundles it with an Unlambda interpreter, and call that the “compiled” version. I don't think that qualifies as a compiler: a compiler should turn Unlambda code into code of the target language, not data that will be interpreted by some generic code. Unfortunately, the boundaries between code and data are not as clear as I would like them to be (see also my quine page for more thoughts on the subject). Unlambda can certainly be compiled in Unlambda at least, by the identity function. Jacob Mandelson (jlm@ghs.com) observed that it is more reasonable to try “decompiling” Unlambda than “compiling” it.

If we restrict ourselves to the S, K and I combinators (as well as printing functions), removing the troublesome C and D functions, then Unlambda can be compiled, at least in a high-level functional language (which can then be compiled in low-level imperative languages using standard methods): for example, ```sii``sii (an endless loop) would be trivially compiled, using a lisp-like notation, in (((S I) I) ((S I) I)), where S and I are part of the “Unlambda run-time library”. This may seem like a void assertion, but note the important difference between this and producing (interpret '(((S I) I) ((S I) I))), where only interpret is defined: the former is a true (albeit trivial) compilation, and the latter is merely bundling the program as data with an interpreter to read the data. The c (call/cc) function would not cause considerable trouble either, if the underlying language (the target language for compilation) has first-class continuations, and even if it doesn't, we can emulate them for example by producing CPS code.

Promises are a much bigger problem: I don't think it is possible to compile Unlambda, with the d special form, in a reasonable programming language. Indeed, whereas we could convert “apparent” promises, such as `dX, into promises from the target language, it is not possible to know beforehand whether a piece of code will really be interpreted or merely made into a promise.

On the other hand, promises aren't anything like an eval function (something that canonically can't be compiled — or at any rate, to compile it you need to bundle the program with an entire interpreter or compiler). So maybe it is possible after all, but I'm very uncertain as to the way it should work. I wish I could express myself more clearly.
Unlambda reference

First we must specify that whitespace is ignored in an Unlambda program (wherever it may be, except, naturally, between the period and the character in the .x function name). Comments are also ignored, a comment being anything starting from the # character to the end of the line.

If F and G are two Unlambda expressions, then the expression `FG is also an expression (called the application of F to G). It is evaluated as follows: first, F is evaluated (and its value is a function, since there is no other kind of values in Unlambda); if the value of F is not d, then, G is evaluated, and finally the value of F is applied to the value of G.

To complete the description of Unlambda, we need therefore only specify what happens when F is applied to G, and to do that we consider each possible value of F.

k (“constant generator”)
    The k function takes an argument X and returns the function `kX (see below).
`kX (“constant function”)
    The `kX function (which is not primitive but obtained by applying the primitive function k to some function X) takes an argument, ignores it and returns X.
s (“substitution”)
    The s function takes an argument X and returns the function `sX (see below).
`sX (“substitution first partial”)
    The `sX function (which is not primitive but obtained by applying the primitive function s to some function X) takes an argument Y and returns the function ``sXY (see below).
``sXY (“substituted application”)
    The ``sXY function (which is not primitive but obtained by applying the primitive function s to two functions X and Y successively) takes an argument Z and returns the evaluation of ``XZ`YZ.
i (“identity”)
    The i function takes an argument and returns that argument.
v (“void”)
    The v function takes an argument X and returns v itself.
c (“call with current continuation”)
    The c function takes an argument X and returns either the evaluation of `X<cont> where <cont> is c's current continuation (see below), or else the value passed to <cont> if the latter was applied (with the effect of making c return immediately).
<cont> (a continuation)
    Continuations take an argument and non-locally jump to the point in history when the evaluator was waiting for the corresponding c to return, making that c return that argument.
d (“delay”)
    The d function is never truly applied (it is a special form). It only occurs in the form `dF where F is an Unlambda expression (see below).
`dF (“promise”)
    The `dF function takes an argument Y and evaluates F, giving a function X, and returns the evaluation of `XY.
.x (“print”) and r (“carriage return”)
    The .x function is written using two characters. The first character is a period and the second is any character. Nevertheless, .x is a single function in Unlambda, and x in this expression is merely a character (read during parsing), not a parameter to the function. The r function is exactly equivalent to .(newline). The .x function behaves like the i (identity) function, with the side effect that it prints the character x (to the standard output) when it is applied. The r function also behaves like the identity and prints a newline character.
e (“exit”) only in Unlambda version 2 and greater
    The e function takes an argument X. It exits immediately, pretending (if the interpreter cares) that the result of the evaluation of the program is X.
@ (“read”) only in Unlambda version 2 and greater
    The @ function takes an argument X. It reads one character from the standard input, making it the “current character” and returns the evaluation of `Xi or of `Xv according as one character has been read successfully or not (for example on EOF).
?x (“compare character read”) only in Unlambda version 2 and greater
    The ?x function (where x is a character, as in the .x function) takes an argument X. It returns the evaluation of `Xi or of `Xv according as the current character (the one read by the last application of @) is x or not (if @ has not been applied or if it has encountered an EOF, there is no current character, and x is deemed not to be equal to the current character).
| (“reprint character read”) only in Unlambda version 2 and greater
    The | function takes an argument X. It returns the evaluation of `X.x, where x is the current character (the one read by the last application of @) or of `Xv if there is no current character (i.e. if @ has not yet been applied or if it has encountered an EOF).

Unlambda distribution

Unlambda 2.0.0 is now available. You can download it using FTP or using HTTP, but using FTP is preferred if you have the choice. If you want older versions, they are available in this FTP directory.

Unlambda is distributed under the terms of the GNU General Public License, either version 2 of this license, or, at your option, any later version. Since Unlambda is Free Software, it comes with absolutely no warranty: see the GNU General Public License for more details.

(Note that this concerns the distribution. There is no copyright on the language itself: you do not need to ask for my permission to write an Unlambda interpreter, and you are permitted (though by no means encouraged) to write a non-free interpreter. As a matter of fact, there exists at least one non-free Unlambda interpreter, the one written by Jacob Mandelson (jlm@ghs.com), which is far more efficient than the interpreters in the Unlambda distribution.)

This document is included in the Unlambda distribution. You can also find it on the World Wide Web at http://www.eleves.ens.fr:8080/home/madore/programs/unlambda/.

Please send comments and suggestions about Unlambda and its interpreters to david.madore@ens.fr.

Happy hacking!
Comprehensive Unlambda Archive Network

The goal of the Comprehensive Unlambda Archive Network is to gather all the Unlambda programs that are written (provided their authors agree, of course). Since there are very few programs in Unlambda altogether, it is convenient to centralize everything in one place, it will not take too much disk space, and a copy of the archive is included in the Unlambda distribution.

You can find the archive in the directory /pub/madore/unlambda/CUAN/ on my FTP site. See the MANIFEST file for a list of the programs in the CUAN. Please drop me a note if you have a program you want to add to the archive.

This site is part of the Esoteric Programming Languages Ring:
[ Previous 5 Sites | Previous | Next | Next 5 Sites | Random Site | List Sites ]
David Madore

Last modified: $Date: 2003/08/10 22:24:48 $
