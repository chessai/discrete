# Discrete

This library provides a typeclass replacement for 'GHC.Enum.Enum' called 'Discrete'. The motivation for 'Discrete' is that Enum, to paraphrase <a href="https://github.com">David Feuer</a>, is a typeclass that does too many things, and does them poorly.

The laws of 'Discrete' are simple:

A discrete type is a set X with at least one element, along with two functions, succ :: X -> Maybe X, and pred :: X -> Maybe X, such that the following hold:

Succ retracts pred: pred >=> succ >=> pred = pred,<br>
Pred retracts succ: succ >=> pred >=> succ = succ,<br>
For any (x : A), all values in X can be constructed with only x, succ, and pred.<br>

This means that 'Int' is a discrete type, because given any x : Int, one can construct any other Int with succ x = x + 1, and pred x = x - 1.

This also means that 'Double' is <i>not</i> a discrete type, because given any x : Double, one cannot construct any and all Doubles with any non-arbitrary and/or meaningful 'succ' and 'pred'^1.

(1): Given (.5 : Double), how would one define 'succ' and 'pred' such that one can "reach" both values (1 : Double) and (π : Double)? 
