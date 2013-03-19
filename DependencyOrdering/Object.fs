module DependencyOrdering.Object

open System

let eqHack (f: 'a -> 'b) (x: 'a) (yobj: Object) : Boolean =
  match yobj with
  | :? 'a as y -> f x = f y
  | _          -> false

let compHack (f: 'a -> 'b) (x: 'a) (yobj: Object) : Int32 =
  match yobj with
  | :? 'a as y -> compare (f x) (f y)
  | _          -> invalidArg "yobj" "Cannot compare elements of incompatible types"