module Data.HVect

import Decidable.Equality
import public Data.Vect
import Data.Vect.Elem

%default total

||| Heterogeneous vectors where the type index gives, element-wise,
||| the types of the contents.
public export
data HVect : Vect k Type -> Type where
  Nil : HVect []
  (::) : t -> HVect ts -> HVect (t :: ts)

||| Extract an element from an HVect.
|||
||| ```idris example
||| > index 0 (the (HVect _) [1, "string"])
||| 1
||| ```
public export
index : (1 i : Fin k) -> (1 _ : HVect ts) -> index i ts
index FZ (x :: xs) = x
index (FS j) (x :: xs) = index j xs

||| Delete an element from an HVect.
|||
||| ```idris example
||| > deleteAt 0 (the (HVect _) [1, "string"])
||| ["string"]
||| ```
public export
deleteAt : {l : Nat} -> {ts : Vect (S l) Type} -> (1 i : Fin (S l)) -> HVect ts -> HVect (deleteAt i ts)
deleteAt FZ (x :: xs@(_ :: _)) = xs
deleteAt {l = S m} (FS j) (x :: xs@(_ :: _)) = x :: deleteAt j xs -- x :: deleteAt j xs
deleteAt {l = Z} (FS j) (x :: xs) = absurd j
deleteAt _ [] impossible

||| Replace an element in an HVect.
|||
||| ```idris example
||| > replaceAt 0 "firstString" (the (HVect _) [1, "string"])
||| ["firstString", "string"]
||| ```
public export
replaceAt : (1 i : Fin k) -> t -> HVect ts -> HVect (replaceAt i t ts)
replaceAt FZ y (x :: xs) = y :: xs
replaceAt (FS j) y (x :: xs) = x :: replaceAt j y xs

||| Update an element in an HVect.
|||
||| ```idris example
||| > updateAt 0 (const True) (the (HVect _) [1, "string"])
||| [True, "string"]
||| ```
public export
updateAt : (1 i : Fin k) -> (index i ts -> t) -> HVect ts -> HVect (replaceAt i t ts)
updateAt FZ f (x :: xs) = f x :: xs
updateAt (FS j) f (x :: xs) = x :: updateAt j f xs

||| Append two `HVect`s.
|||
||| ```idris example
||| > (the (HVect _) [1]) ++ (the (HVect _) ["string"])
||| [1, "string"]
||| ```
public export
(++) : HVect ts -> HVect us -> HVect (ts ++ us)
(++) [] ys = ys
(++) (x :: xs) ys = x :: (xs ++ ys)

public export
Eq (HVect []) where
  [] == [] = True

public export
(Eq t, Eq (HVect ts)) => Eq (HVect (t :: ts)) where
  (x :: xs) == (y :: ys) = x == y && xs == ys

public export
hvectInjective1 : {xs, ys: HVect ts} -> {x, y: a} -> x :: xs = y :: ys -> x = y
hvectInjective1 Refl = Refl

public export
hvectInjective2 : {xs, ys: HVect ts} -> {x, y: a} -> x :: xs = y :: ys -> xs = ys
hvectInjective2 Refl = Refl

public export
DecEq (HVect []) where
  decEq [] [] = Yes Refl

public export
(DecEq t, DecEq (HVect ts)) => DecEq (HVect (t :: ts)) where
  decEq (x :: xs) (y :: ys) with (decEq x y)
    decEq (z :: xs) (z :: ys) | Yes Refl with (decEq xs ys)
      decEq (z :: zs) (z :: zs) | Yes Refl | Yes Refl = Yes Refl
      decEq (z :: xs) (z :: ys) | Yes Refl | No contra = No (contra . hvectInjective2)
    decEq (x :: xs) (y :: ys) | No contra = No (contra . hvectInjective1)

public export
interface Shows (ts : Vect k Type) where
  shows : HVect ts -> Vect k String

public export
Shows [] where
  shows [] = []

{- TODO figure out error
public export
(Show t, Shows ts) => Shows (t :: ts) where
  shows (x :: xs) = show x :: shows xs
-}

-- TODO https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/HVect.idr#L71-L72

||| Extract an arbitrary element of the correct type.
||| @ t the goal type
public export
get : HVect ts -> {auto p : Elem t ts} -> t
get (x :: xs) {p = Here} = x
get (x :: xs) {p = (There p')} = get {p = p'} xs

||| Replace an element with the correct type.
public export
put : t -> HVect ts -> {auto p : Elem t ts} -> HVect ts
put y (x :: xs) {p = Here} = y :: xs
put y (x :: xs) {p = (There p')} = x :: put {p = p'} y xs

||| Update an element with the correct type.
public export
update : (t -> u) -> HVect ts -> {auto p : Elem t ts} -> HVect (replaceByElem ts p u)
update f (x :: xs) {p = Here} = f x :: xs
update f (x :: xs) {p = (There p')} = x :: update {p = p'} f xs
