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
#check { x := 0.0, y := 0.0 : Ponto }


-- 1.4.1. Updating Structures

-- Aloca nova estrutura, copia TODOS os valores e define `x := 0`
-- Altera o `x` duas vezes.
def zeroX (p : Ponto) : Ponto :=
  { x := 0, y := p.y }

-- Copia apenas os valores que nao serao alterados ou seja:
-- `A new instance of the structure is created in which every`
-- `field not specifiedis copied from the value that is being updated,`
-- `and the specified fields are replaced with their new values.`
def zeroXIdiomatic (p : Ponto) : Ponto :=
  { p with x := 0 }

def quatroCinco : Ponto :=
  { x := 4, y := 5 }

#eval quatroCinco
#eval zeroX quatroCinco
#eval zeroXIdiomatic quatroCinco


-- 1.4.2. Behind the Scenes

#check Ponto.mk 7 3

structure OutroPonto where
  -- Muda o nome do contrutor padrao
  ponto ::
  x : Float
  y : Float
deriving Repr

#eval OutroPonto.ponto 3 3

-- o `estrutura.x` vira `ContrutorDaEstrutura.x estrutura`,
#eval quatroCinco.x == Ponto.x quatroCinco

-- `aplicaF` definido no namespace de `Ponto`
def Ponto.aplicaF (f : Float → Float) (p : Ponto) : Ponto :=
  { x := f p.x, y := f p.y }
-- Ambos tem a mesma assinatura
def aplicaF (f : Float → Float) (p : Ponto) : Ponto :=
  { x := f p.x, y := f p.y }

def doubleIt (x : Float) : Float :=
  x * x
-- Mesmo o argumento Ponto vindo depois do argumento que eh a funcao, pode user a dot notation
#eval quatroCinco.aplicaF doubleIt
-- Sem usar dot notation:
#eval aplicaF doubleIt quatroCinco


-- 1.4.3. Exercises

structure RectangularPrism where
  h : Float
  w : Float
  d : Float
deriving Repr

def RectangularPrism.volume (p : RectangularPrism) : Float :=
  p.h * p.w * p.d

structure Segment where
  p₁ : Ponto
  p₂ : Ponto
deriving Repr

def Segment.length (s : Segment) : Float :=
  Float.sqrt ((s.p₂.x - s.p₁.x)^2 + (s.p₂.y - s.p₁.y)^2)

-- Which names are introduced by the declaration of RectangularPrism?
-- RectangularPrism.mk
-- RectangularPrism.h
-- RectangularPrism.w
-- RectangularPrism.d

-- Which names are introduced by the following declarations of Hamster and Book? What are their types?
-- Hamster.mk : String → Bool → Hamster
-- Hamster.name : Hamster → String
-- Hamster.fluffy : Hamster → Bool
-- Book.makeBook : String → String → Float → Book
-- Book.title : Book → String
-- Book.author : Book → String
-- Book.price : Book → Float


-- 1.5. Datatypes and Patterns

-- Sum types
inductive Boole where
  | false : Boole
  | true : Boole

inductive Natu where
  | zero : Natu
  | succ (n : Natu) : Natu

-- 1.5.1. Pattern Matching

def isZero (n : Natu) : Bool :=
  match n with
  | Natu.zero => true
  | Natu.succ k => false

def pred (n : Natu) : Natu :=
  match n with
  | Natu.zero => Natu.zero
  | Natu.succ k => k
-- `k` eh o argumento que o construtor `succ` de `Natu.succ` recebe

-- Pattern matching tambem funciona com structures
def primeiro (p : RectangularPrism) : Float :=
  match p with
  | { h := h, w := _, d := _ } => h

-- 1.5.2. Recursive Functions

def even (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (even k)

def evenLoops (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (evenLoops n)
-- Recursao sem 'diminuir'

def adicao (x y : Nat) : Nat :=
  match y with
  | Nat.zero   => x
  | Nat.succ k => Nat.succ (adicao x k)
-- adicao 3 2
--        x y
-- 3  Nat.succ 1
-- 3  Nat.succ Nat.succ Nat.zero
-- Acho o Nat.zero, retorna o `x`
-- Nat.succ Nat.succ 3
-- 5


-- 1.6. Polymorphism

structure PolPonto (α : Type) where
  x : α
  y : α
deriving Repr

def origemNat : (PolPonto Nat) :=
  { x := 0, y := 0 }

def replaceX (α : Type) (point : PolPonto α) (newX : α) : PolPonto α :=
  { point with x := newX }

def PolPonto.replaceXIdiomatic (point : PolPonto α) (newX : α) :=
  { point with x := newX }

#check (replaceX)
#check replaceX Nat

#eval replaceX Nat origemNat 8
#eval PolPonto.replaceXIdiomatic origemNat 1

inductive Sinal where
  | mais
  | menos

def natOuInt (s : Sinal) : Type :=
  match s with
  | Sinal.mais => Nat
  | Sinal.menos => Int

def exemploMais : natOuInt Sinal.mais := (10 : Nat)
def exemploErrado : natOuInt Sinal.mais := (-1 : Int)

#eval exemploMais


-- 1.6.1. Linked Lists

def tamanhoLivro (α : Type) (xs : List α) : Nat :=
  match xs with
  | List.nil => Nat.zero
  | List.cons _ ys => Nat.succ (tamanhoLivro α ys)

def len (α : Type) (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (len α ys)

def List.tamanho (xs : List α) : Nat :=
  match xs with
  | []      => 0
  | _ :: xs => 1 + tamanho xs

#eval [1, 3].tamanho


-- 1.6.2. Implicit Arguments

-- `{}` sao usados para argumentos implicitos
-- {α : Type}


-- 1.6.3. More Built-In Datatypes

/- 1.6.3.1. Option -/

/--
Caso 1:
┌──Option┐
│        │
│  none  │
│        │
└────────┘
ou

Caso 2:
┌────Option┐
│  ┌some┐  │
│  │ α  │  │
│  └────┘  │
└──────────┘
-/
inductive Opt (α : Type) where
  | none : Opt α
  | some (val : α) : Opt α

-- head para listas que NAO sao nulas (precisa de uma prova)
#check List.head

-- head! para para listas que podem ser nulas (panic quando nula)
#check List.head!

-- head? para para listas que podem ser nulas (retorna Option)
#check List.head?

-- headD para para listas que podem ser nulas (retorna um valor default)
#check List.headD

def List.cabeca? (xs : List α) : Opt α :=
  match xs with
  | []     => Opt.none
  | y :: _ => Opt.some y

#eval [].head? (α := Int)

/- 1.6.3.2. Prod -/

structure Produto (α : Type) (β : Type) where
  fst : α
  snd : β

def meuProduto : Bool × Int := (true, 1)
#eval meuProduto

-- Mesma coisa
def sevens : String × Int × Nat := ("VII", 7, 4 + 3)
def sevenss : String × (Int × Nat) := ("VII", (7, 4 + 3))

/- 1.6.3.3. Sum -/
-- Either

-- Mesma coisa
#check Sum String String
#check String ⊕ String

-- Exemplo de numeros positivos e negativos

def LoginResult : Type := String ⊕ Nat
-- String = mensagem de erro (inl)
-- Nat = ID do usuário logado (inr)

def tentativasLogin : List LoginResult :=
  [Sum.inl "Senha incorreta",           -- erro
   Sum.inr 123,                         -- sucesso (user ID 123)
   Sum.inl "Usuário não encontrado",    -- erro
   Sum.inr 456,                         -- sucesso (user ID 456)
   Sum.inl "Conta bloqueada"]           -- erro

-- Evitar Sum generico (nao da pra sber qual eh qual)
def PetName : Type := String ⊕ String
-- Um tipo eh melhor:
inductive PetNamee where
  | dog : String → PetNamee
  | cat : String → PetNamee

/- 1.6.3.4. Unit -/
#check Unit
#check ()

-- Funcoes de zero argumento podem receber Unit.
-- Parecido com void.
-- * Monadic actions that return `Unit` have side effects without computing values *

/- 1.6.3.5. Empty -/
