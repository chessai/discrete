# Discrete

This library provides a typeclass replacement for 'GHC.Enum.Enum' called 'Discrete'. The motivation for 'Discrete' is that Enum, to paraphrase <a href="https://github.com">David Feuer</a>, is a typeclass that does too many things, and does them poorly.

The laws of 'Discrete' are simple:

A discrete type is a set A with at least one element x, along with two functions, succ : A -> A, and pred : A -> A, such that the following holds:

succ . pred = pred . succ = id

and, such that for any (x : A), all values in X can be constructed with only x, succ, and pred.

To write a 'Discrete' instance for a type 'A', one must provide the following three things:

<ol>
  <li>a value x : A. This value is arbitrary, but serves as a proof that the type is inhabited.</li>
  <li>A definition of succ : A -> A such that the laws of construction of values in A hold.</li>
  <li>A definition of pred : A -> A such that the laws of construction of values in A hold.</li>
</ol>

This means that 'Int' is a discrete type, because given any x : Int, one can construct any other Int with succ x = x + 1, and pred x = x - 1.

This also means that 'Double' is <i>not</i> a discrete type, because given any x : Double, one cannot construct any and all Doubles with any non-arbitrary and/or meaningful 'succ' and 'pred'^1.

(1): Given (.5 : Double), how would one define 'succ' and 'pred' such that one can "reach" both values (1 : Double) and (π : Double)? 
