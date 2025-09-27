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

#check Empty

-- Representa um valor impossivel, pois nao tem contrutor.
-- Ao contrario do `Unit` (`()`) que eh vazio mas possivel.

/- 1.6.3.6. Naming: Sums, Products, and Units -/

-- PRODUCT TYPES (×): Um construtor com multiplos argumentos
-- Bool tem 2 valores, entao Bool × Bool tem 2 * 2 = 4 possiveis valores
-- Unit tem 1 valor, entao Bool × Unit tem 2 * 1 = 2 valores
#check (true, ())
#check (false, ())

#check (true, true)
#check (true, false)
#check (false, true)
#check (false, false)

-- SUM TYPES (⊕): Multiplos construtores (escolhas)
-- Se α tem n valores e β tem k valores, então α ⊕ β tem n + k valores
-- Bool ⊕ Unit tem 2 + 1 = 3 valores
#check Sum.inl true
#check Sum.inl false
#check Sum.inr ()

-- Bool ⊕ Bool tem 2 + 2 = 4 valores
#check Sum.inl true
#check Sum.inr true
#check Sum.inl false
#check Sum.inr false

-- Sum = adição (escolha entre opcoes)
-- Product = multiplicação (combinacao)


/-
RESUMO:
Tipos com multiplos contrutores sao chamados `sum types (α ⊕ β)`,
tipos com um unico contrutor sao chamados `product types (α × β)`.
  assim, (Bool ⊕ Bool) tem 2 + 2 valores distintos, e (Bool × Bool) tem 2 × 2.


PADRAO DO LEAN:

- Product types
Um construtor (`Ponto`) com multiplos campos.

`structure` Ponto where
  x : Float
  y : Float


- Sum types
Varios construtores (opcoes), pode ser recursivo.

`inductive` Bool where
  | true : Bool
  | false : Bool

inductive Nat where
  | zero : Nat
  | succ (n : Nat) : Nat


OUTROS TIPOS:

- `Option` (igual a Maybe), representa ausencia ou presenca de valor

inductive Option (α : Type) where
  | none : Option α
  | some (val : α) : Option α


- `Prod` produto generico, o valor `(a, b)` tem o tipo `α × β`

structure Prod (α : Type) (β : Type) where
  fst : α
  snd : β


- `Sum` (igual a Either) soma generica, `α ⊕ β`

inductive Sum (α : Type) (β : Type) where
  | inl : α → Sum α β
  | inr : β → Sum α β

-/


/- 1.6.4. Messages You May Meet -/
/- mais explicacoes dps... -/

-- Erros de universe level
-- Quando um construtor aceita um Type como argumento do construtor.
inductive MyType : Type where
  | ctor : (α : Type) → α → MyType
-- Recebe um Type e um valor desse Type

inductive MyType (α : Type) : Type where
  | ctor : α → MyType α

-- Non-Positive Occurrence
inductive MyType₁ : Type where
  | ctor : (MyType₁ → Int) → MyType₁

-- Termination Problems com Pattern Matching
-- O Lean nao consegue ver que xs' e ys' sao "menores" quando faz match em tupla
def sameLength (xs : List α) (ys : List β) : Bool :=
  match (xs, ys) with
  | ([], []) => true
  | (x :: xs', y :: ys') => sameLength xs' ys'
  | _ => false

-- Falta o argumento de tipo
inductive MyType₂ (α : Type) : Type where
  | ctor : α → MyType₂
-- MyType₂ aceita um argumento de tipo, que deveria ser dado:
--| ctor : α → MyType₂ α


-- 1.6.5. Exercises

/-- Write a function to find the last entry in a list. It should return an Option. -/
def List.last (xs : List α) : Option α :=
  match xs with
  | [x]      => some x
  | _ :: xs' => last xs'
  | _        => none

#eval [6, 0, 11, 9].last
#eval ([] : List Int).last

/--
Write a function that finds the first entry in a list that satisfies
a given predicate. Start the definition with def List.findFirst?
{α : Type} (xs : List α) (predicate : α → Bool) : Option α := ….
-/

-- Lembrar do `?` para quem retorna Option.
def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  sorry

/--
Write a function Prod.switch that switches the two fields in a pair for each other.
Start the definition with def Prod.switch {α β : Type} (pair : α × β) : β × α := ….
-/

def Prod.switch {α β : Type} (pair : α × β) : β × α :=
  (pair.snd, pair.fst)

#eval (2, true).switch

/--
Rewrite the PetName example to use a custom datatype and
compare it to the version that uses Sum.
  def PetName : Type := String ⊕ String
-/

inductive PetName₁ : Type where
  | gato : String → PetName₁
  | cachorro : String → PetName₁
deriving Repr

def animais : List PetName₁ :=
  [PetName₁.cachorro "Rex",
   PetName₁.gato "Nicolau"]

/--
Write a function zip that combines two lists into a list of pairs. The resulting
list should be as long as the shortest input list. Start the definition with
def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) := ….
-/

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | x :: xs', y :: ys' => (x, y) :: zip xs' ys'
  | _, _               => []

#eval zip [1, 2, 3, 4] [true, false, true]

/--
Write a polymorphic function take that returns the first `n` entries in a list,
where `n` is a Nat. If the list contains fewer than `n` entries, then the resulting
list should be the entire input list. #eval take 3 ["bolete", "oyster"] should yield
["bolete", "oyster"], and #eval take 1 ["bolete", "oyster"] should yield ["bolete"].
-/

def take {α : Type} (n : Nat) (xs : List α) : List α :=
  match n, xs with
  | 0, _                => []
  | _, []               => []
  | Nat.succ k, y :: ys => y :: take k ys

#eval take 3 ["bolete", "oyster"]
#eval take 1 ["bolete", "oyster"]

/--
Using the analogy between types and arithmetic, write a function that distributes
products over sums. In other words, it should have type α × (β ⊕ γ) → (α × β) ⊕ (α × γ).
-/

def productsOverSums {α β γ : Type} (pair : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match pair.snd with
  | Sum.inl b => Sum.inl (pair.fst, b)
  | Sum.inr c => Sum.inr (pair.fst, c)

/--
Using the analogy between types and arithmetic, write a function that turns
multiplication by two into a sum. In other words, it should have type Bool × α → α ⊕ α.
-/

def mulToSum (mul : Bool × α) : α ⊕ α :=
  match mul with
  | (true, v)  => Sum.inl v
  | (false, v) => Sum.inr v


-- 1.7. Additional Conveniences (algumas coisas repetidas)


-- 1.7.1. Automatic Implicit Parameters

-- Geralmente nao precisa citar os parametros implicitos em funcoes polimorficas.
-- `{α : Type}` (ja nao estava usando muito)


-- 1.7.2. Pattern-Matching Definitions

-- Quando uma funcao usa pattern-matching direto, nao precisa dar nome aos argumentos

def tamanho (xs : List α) : Nat :=
  match xs with
  | []      => 0
  | _ :: ys => Nat.succ $ tamanho ys
-- parece que `$` eh igual ao Haskell (ver o macro e `<|` depois)
-- #check Nat.succ (t [1,2])
-- #check Nat.succ $ t [1,2]
-- #check Nat.succ <| t [1,2]

-- Mesma coisa que:
def t : List α → Nat
  | []      => 0
  | _ :: ys => Nat.succ $ tamanho ys

#eval t [1,2,3]

-- Tambem nao precisa nomear em casos com mais de 2 args, a virgula os separa em ordem:
-- Lembrar de tirar o `:=`
def getIvalue : Nat → List α → Option α
  | _, []               => none
  | Nat.zero, x :: _    => some x
  | Nat.succ k, _ :: xs => getIvalue k xs

#eval getIvalue 2 [7, 3, 1, 5]


-- 1.7.3. Local Definitions

-- Usar `let` para definicoes locais, caso essa definicao seja recursiva,
-- `let rec` eh nescessario.

def unzip₁ : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
  (x :: (unzip₁ xys).fst, y :: (unzip₁ xys).snd)
  -- (unzip₁ xys) eh chamado duas vezes, mesmo tendo o resultado igual

def unzip₂ : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped : List α × List β := unzip₂ xys
    (x :: unzipped.fst, y :: unzipped.snd)

-- `let` tambem aceita pattern matching (quando um pattern eh suficiente)
def unzipPattern : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    -- descontroi o Prod
    let (xs, ys) : List α × List β := unzipPattern xys
    (x :: xs, y :: ys)


-- 1.7.4. Type Inference

-- Lean pode inferir tipos automaticamente em muitas situacoes
-- Tipos explicitos podem ser omitidos tanto em `def` quanto em `let`
def unzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped := unzip xys  -- `unzipped` nao precisa de anotacao de tipo
    (x :: unzipped.fst, y :: unzipped.snd)

-- Quando omitir tipos (rule of thumb):
-- ✓ Literais
-- ✓ Aplicacao de funcoes (Lean ja sabe os args e retorno)
-- ⨯ Parametros de funcoes
-- ⨯ Definicoes sem contexto suficiente

def id₁ (x : α) : α := x

def id₂ (x : α) := x

def id₃ x := x

-- Erros com mensagens "failed to infer" ou "metavariables (?m.XYZ)"
-- geralmente sao causados pelo Lean nao advinhar o tipo.


-- 1.7.5. Simultaneous Matching

-- Pattern matching pode inspecionar multiplos valores simultaneamente

def drop (n : Nat) (xs : List α) : List α :=
  match n, xs with
  | Nat.zero, ys => ys
  | _, [] => []
  | Nat.succ n', _ :: ys => drop n' ys

-- Lean nao consegue ver que xs' e ys' sao estruturalmente menores (ja vi isso).
def sameLengthTupla (xs : List α) (ys : List β) : Bool :=
  match (xs, ys) with
  | ([], []) => true
  | (x :: xs', y :: ys') => sameLengthTupla xs' ys'
  | _ => false
-- A tupla "esconde" a relacao entre xs e x :: xs'

-- Matching simultaneo em listas eh aceito.
def sameLengthOk (xs : List α) (ys : List β) : Bool :=
  match xs, ys with
  | [], []             => true
  | _ :: xs', _ :: ys' => sameLength xs' ys'  -- Lean ve que xs' < xs
  | _, _               => false


-- 1.7.6. Natural Number Patterns

-- Versao tradicional com construtores explicitos:
def even₁(n : Nat) : Bool :=
  match n with
  | Nat.zero   => true
  | Nat.succ k => not (even₁ k)
-- ou
def even₂ : Nat → Bool
  | 0     => true
  | n + 1 => not (even₂ n)  -- n + 1 = Nat.succ n

-- Como funciona o pattern `n + k`:
--   Argumento ESQUERDO (n): vira variavel para os Nat.succ restantes
--   Argumento DIREITO (k): numero de Nat.succ que envolvem o "pattern" (n).

-- Ex:
def halveTradicional : Nat → Nat
  | Nat.zero => 0
  | Nat.succ Nat.zero => 0
  | Nat.succ (Nat.succ n) => halveTradicional n + 1
-- ou
def halve : Nat → Nat
  | 0     => 0
  | 1     => 0
  | n + 2 => halve n + 1

-- halve n + 1 = (halve n) + 1, NAO halve (n + 1)

-- O segundo argumento deve ser literal
def halveErrado : Nat → Nat
  | 0     => 0
  | 1     => 0
  | 2 + n => halve n + 1


-- 1.7.7. Anonymous Functions

-- Funcoes λ com `fun` (ou `λ`).

#check fun (x : Int) => x + 1

#check fun {α : Type} (x : α) => x
#check fun x => x


-- Tambem suportam pattern matching:
#check fun
  | 0     => none
  | n + 1 => some n

-- Funcoes com `def` podem ser reescritas como expressoes de funcao:
def double : Nat → Nat := fun
  | 0     => 0
  | k + 1 => double k + 2

-- `\.` para funcoes simples
-- Em parenteses, o `·` representa um parametro


#check (· + 1)      -- funcao que soma 1
#eval (· + 5, 3) 2  -- funcao que retorna par (x + 5, 3)

#check ((· + 5), 3)  -- duas funcoes anonimas (funcao, numero)
#eval ((· + 5) 1, 3)

-- Multiplos dots viram parametros da esquerda para direita:
#eval (· , ·) 1 2
#eval (· + ·) 1 2

-- O `·` cria funcao nos parenteses MAIS PROXIMOS
-- Util para usar em HOFs


-- 1.7.8. Namespaces
