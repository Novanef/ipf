module type Coord =
sig
  type dot
  type coord
  val distance : coord -> coord -> dot
end

module Coord_R : Coord =
struct
  type dot = int
  type coord = dot * dot
  let distance coord1 coord2 = 1
end

module Coord_E : Coord =
struct
type dot = float
type coord = dot * dot
let distance coord1 coord2 = 1.0
end

module FoncteurTree(X : Coord) =
struct
let distance = X.distance;
end