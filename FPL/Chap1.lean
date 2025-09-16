#eval String.append "great " (String.append "oak " "tree")

#eval (String.append "A" (String.append "B" "C")).append "a"

#eval if 3 == 4 then "equal" else "not equal"

#eval (1 - 2 : Nat) -- Nat = Peano numbers

#check (1 - 2 : Int) -- Nat = Peano numbers

def foo := "Hey"
def lean : String := "Lean"

#eval (foo.append " ").append lean


-- 1.3.1. Defining Functions

def add1 (n : Nat) : Nat := n + 1
#eval add1 6

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n
#check maximum

-- def spaceBetween (before : String) (after : String) : String :=
-- mesma coisa que:
def spaceBetween (before after : String) : String :=
  String.append before (String.append " " after)
#check spaceBetween -- Assinatura
#check (spaceBetween) -- Tipo

#check (maximum 2) -- omg currying!!!

-- Exercises
def joinStringsWith (s1 s2 s3 : String) : String :=
  String.append s2 (String.append s1 s3)

#eval joinStringsWith ", " "one" "and another"
#check joinStringsWith ": "

def volume (h w d : Nat) : Nat :=
  h * w * d


-- 1.3.2. Defining Types

def Str : Type := String
def minhaStr : Str := "This is a string."

def NaturalNumber : Type := Nat
def thirtyEight : NaturalNumber := 38
-- o `38` digitado literalmente eh polimorfico, entao
-- seria nescessario alguma definicao como:
-- def um : NaturalNumber := Nat.succ Nat.zero
-- def thirtyEight : NaturalNumber := (38 : Nat)
-- Asism, lean tem certeza que o literal eh um `Nat`

#eval Nat.succ Nat.zero

abbrev N : Type := Nat
def thirtyNine : N := 39


-- 1.4. Structures

#check -1.01234567891
#eval 1.1/0.0
#eval -1.1/0.0
#eval 0/0.0

structure Ponto where
  x : Float
  y : Float
deriving Repr

def cima : Ponto := { x := 0.0, y := 1.0 }
#eval cima
#eval cima.x

def addPontos (p1 p2 : Ponto) : Ponto :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

#check { x := 0.0, y := 0.0 }
#check { x := 0.0, y := 0.0 : Ponto}


-- 1.4.1. Updating Structures
