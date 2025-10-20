# FPL - Functional Programming in Lean

Anotações e exercícios do livro **"[Functional Programming in Lean](https://lean-lang.org/functional_programming_in_lean/)"**.

**Arquivos completos:**
- [`FPL/Chap1.lean`](FPL/Chap1.lean) - Getting to Know Lean
- [`FPL/Chap2.lean`](FPL/Chap2.lean) - Hello, World!
- [`FPL/Interlude.lean`](FPL/Interlude.lean) - Propositions, Proofs, and Indexing
- [`FPL/Chap3.lean`](FPL/Chap3.lean) - Overloading and Type Classes


## 1. Getting to Know Lean

**Falando com o Lean:**
- `#eval expr` - avalia a expressão
- `#check expr` - mostra o tipo da expressão  
- `#reduce expr` - β-reduz a expressão

**Definições:**

```haskell
def name : Type := value

def add1 (n : Nat) : Nat := n + 1

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

abbrev N : Type := Nat
abbrev Hash := String
```

**Estruturas (product types):**
```haskell
structure Point where
  x : Float
  y : Float

def cima : Ponto := { x := 0.0, y := 1.0 }
#eval cima.x

def zeroX (p : Ponto) : Ponto :=
  { x := 0, y := p.y }

-- ou
def zeroX (p : Ponto) : Ponto :=
  ⟨0, p.y⟩ -- Mesma coisa que `Ponto.mk 0 p.y`
  
-- ou
def zeroX (p : Ponto) : Ponto :=
  { p with x := 0 }
```

**Tipos indutivos (sum types):**
```haskell
inductive Bool where
  | true | false

inductive Nat where  
  | zero
  | succ (n : Nat)
```

**Pattern matching:**
```haskell
def isZero : Nat → Bool
  | Nat.zero => true
  | Nat.succ _ => false
```

**Polimorfismo:**
```haskell
def List.length (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | _ :: ys => 1 + length ys
  
-- ou
def List.length : List α → Nat
  | [] => 0
  | _ :: xs => 1 + length xs
```

**Tipos built-in:**
- `Option α` - maybe → `some 42`, `none`
- `List α` - listas → `[1, 2, 3]`, `[]`  
- `α × β` - Product (tuplas) → `(1, "hello")`, `⟨42, true⟩`
- `α ⊕ β` - Sum type (either) → `Sum.inl 5`, `Sum.inr "error"`
- `Unit` - tipo unitário → `()`
- `Empty` - tipo vazio (impossível) → nenhum valor

**Convenções:**
- `{}` argumentos implícitos
- `fun x => expr` / `·` `(· + 1)` funções anônimas
- `_` (hole) ignora nome não usado
- Pattern matching direto sem `match`
- Namespaces e dot notation


**Convenções para funções por tipo de retorno:**
- `head` - listas não-nulas (precisa prova) → `List.head [1,2] proof`
- `head!` - panic se nula → `List.head! [1,2]`
- `head?` - retorna Option → `List.head? [] = none`
- `headD` - retorna valor default → `List.headD [] 0 = 0`


## 2. Hello, World!

**Executando programas:**
```haskell
def main : IO Unit := IO.println "Hello, world!"
-- lean --run FPL/Chap2.lean
```

**Tipos de main:**
- `main : IO Unit` - sem args, sem exit code
- `main : IO UInt32` - sem args, com exit code  
- `main : List String → IO UInt32` - com args e exit code

**IO:**
- `IO α` - Monad de efeito colateral ou erro.
- `return/pure value` - ação sem efeitos que retorna `value`
- Separação: **avaliação** (pura) vs **execução** (com efeitos)

**do notation:**
```haskell
def main : IO Unit := do
  let stdin ← IO.getStdin        -- `←` para ações IO
  let name := input.trim         -- `:=` para valores puros
  IO.println s!"Hello, {name}!"  -- ação IO direta
```

**Nested actions (ações aninhadas):**
```haskell
-- Em vez de:
let stdout ← IO.getStdout
stdout.write buf

-- Pode usar:
(← IO.getStdout).write buf
```

**Combinando ações IO:**
```haskell
def twice (action : IO Unit) : IO Unit := do
  action
  action

def nTimes (action : IO Unit) : Nat → IO Unit
  | 0 => pure ()
  | n + 1 => do
    action
    nTimes action n
```

**Comandos:**
- `lake new <nome> [<template>]` - cria projeto novo usando template:
    - std: library and executable; default
    - exe: executable only
    - lib: library only
    - math-lax: library only with a Mathlib dependency
    - math: library with Mathlib standards for linting and workflows
- `lake init <nome> [<template>]` - cria projeto em um pasta
- `lake build` - compila projeto
- `lake exe <nome>` - executa programa
- `lakefile.toml` - configuração do projeto

**Estrutura de projeto:**
Arquivos `.lean` são módulos, as pastas organizam módulos e o projeto é um package
```
projeto/
├── lakefile.toml     -- configuração
├── Main.lean         -- arquivo principal  
├── Projeto.lean      -- biblioteca raiz
└── Projeto/          -- módulos da biblioteca
    ├── Basic.lean
    └── Utils.lean
```

**Módulos vs Namespaces:**
- **Módulos**: unidades de distribuição (`import Projeto.Basic`)
- **Namespaces**: unidades de organização (`open Projeto`)
- São **desacoplados** - arquivo `A/B.lean` não cria namespace `A.B`

**Funções parciais:**
```haskell
partial def infiniteLoop : IO Unit := do
  IO.println "looping..."
  infiniteLoop
```
- `partial` permite recursão potencialmente infinita
- Não são avaliadas durante type checking
- Usar apenas quando necessário (ex: streams infinitas)

**Convenções do notation:**
- Cada ação em linha separada com mesma indentação
- `;` pode substituir quebras de linha


## Interlude: Propositions, Proofs, and Indexing

**Indexação segura:**
```haskell
def critters : List String := ["hedgehog", "deer", "snail"]
#eval critters[1]  -- "deer"
#eval critters[3]  -- erro: precisa da prova 3 < critters.length
```

**Proposições como tipos:**
- `Prop` - tipo das proposições (statements que podem ser true/false)
- **Proposições**: `1 + 1 = 2`, `3 < 5`, `A ∧ B`
- **Provas**: evidências que uma proposição é verdadeira
- **Teoremas**: proposições que foram provadas

**Provas básicas:**
```haskell
def onePlusOneIsTwo : 1 + 1 = 2 := rfl  -- reflexividade

theorem addAndAppend : 1 + 1 = 2 ∧ "Str".append "ing" = "String" := by
  decide  -- decision procedure para valores concretos
```

**Tactics (táticas):** `by` coloca o lean em tactic mode
- `decide`: decision procedure para proposições decidíveis computacionalmente
  - Funciona com: igualdades numéricas, comparações, booleanos, conectivos finitos
  - Ex: `1 + 1 = 2`, `3 < 5`, `true ∧ false`, `n ∈ [1,2,3]`
  - Não funciona com: quantificadores, proposições sobre tipos infinitos

- `simp`: simplifica goal usando reglas de simplificação (simp lemmas)
  - Remove redundâncias: `x + 0 = x`, `true ∧ P = P`, `List.length [] = 0`
  - Desdobra definições marcadas com `@[simp]`
  - Pode resolver goals completamente ou deixá-los mais simples

- `rfl`: reflexividade da igualdade (`a = a`)
  - Funciona quando ambos lados computam para o mesmo valor
  - Ex: `2 + 3 = 1 + 4` (ambos = 5), `"he" ++ "llo" = "hello"`
  - Falha se lados não são definitivamente iguais

- `ring`: normaliza expressões algébricas em anéis comutativos
  - Funciona com: +, *, -, ^ (potências naturais) sobre ℕ, ℤ, ℚ, ℝ
  - Ex: `(x + y)² = x² + 2*x*y + y²`, `a + b + c = c + a + b`
  - Ignora divisão, raízes, funções transcendentais


**Conectivos lógicos:**
```haskell
-- True/False
theorem trueIsTrue : True := by decide
-- A ∧ B (and) 
theorem bothTrue : 1 + 1 = 2 ∧ 3 < 5 := by decide
-- A ∨ B (or)
theorem oneOrOther : 1 + 1 = 2 ∨ 1 + 1 = 5 := by decide
-- A → B (implication) - funções
theorem implies : A ∧ B → A ∨ B := fun ⟨a, b⟩ => Or.inl a
-- ¬A (not)
theorem notFalse : ¬(1 + 1 = 5) := by decide
```

**Evidência como argumentos:**
```haskell
-- Função que requer prova de segurança
def third (xs : List α) (ok : xs.length > 2) : α := xs[2]

#eval third critters (by decide)  -- prova automática
```

**Variações de indexação:**
- `xs[i]` - segura, requer prova que `i < xs.length`
- `xs[i]!` - unsafe, panic em runtime se inválido
- `xs[i]?` - retorna `Option α` (`some` ou `none`)
- `xs[i]'h` - usa prova explícita `h : i < xs.length`

**Tabela de conectivos:**
| Conectivo | Sintaxe | Evidência |
|-----------|---------|-----------|
| True | `True` | `True.intro : True` |
| False | `False` | Sem evidência possível |
| A and B | `A ∧ B` | `And.intro : A → B → A ∧ B` |
| A or B | `A ∨ B` | `Or.inl : A → A ∨ B` ou `Or.inr : B → A ∨ B` |
| A implies B | `A → B` | Função que transforma evidência de A em B |
| not A | `¬A` | Função `A → False` |

**Inhabited vs Empty:**
- Tipos `Inhabited` têm pelo menos um valor → podem usar `xs[i]!`
- Tipos possivelmente vazios (como `α` genérico) → não podem crashar
- Previne "provas falsas" via panic: `def fakeProof : 1 = 5 := panic!`

**Convenções:**
- `theorem` para provas principais, `lemma` para provas auxiliares vs `def` para definições
- `by decide` para valores concretos
- `by simp` para simplificação automática
- `rfl` quando ambos lados computam para o mesmo valor
- Provas importam **que** existe evidência, não **qual** evidência


## 3. Overloading and Type Classes

Type classes permitem overloading de operadores, funções e literais de maneira que funciona bem com polimorfismo. Uma type class descreve operações overloadable, e uma **instance** fornece implementações para um tipo específico.

**Definindo type classes:**
```haskell
class Plus (α : Type) where
  plus : α → α → α

-- Instance para Nat
instance : Plus Nat where
  plus := Nat.add

-- Instance para tipo customizado
instance : Plus Pos where
  plus := Pos.plus
```

**Type classes built-in:**
```haskell
-- Aritmética
HAdd.hAdd  -- x + y (heterogêneo)
Add.add    -- x + y (homogêneo)
HSub.hSub, HMul.hMul, HDiv.hDiv, HMod.hMod, HPow.hPow
Neg.neg    -- -x

-- Bitwise (UInt8, Int8, UInt16, Int16, UInt32, Int32, UInt64, Int64, USize)
HAnd.hAnd    -- x &&& y
HOr.hOr      -- x ||| y
HXor.hXor    -- x ^^^ y
Complement.complement  -- ~~~x
HShiftRight.hShiftRight, HShiftLeft.hShiftLeft

-- Comparação
BEq.beq    -- x == y (boolean equality)
LT.lt      -- x < y
LE.le      -- x ≤ y
Ord.compare  -- retorna Ordering (lt/eq/gt)

-- Outros
ToString.toString
Hashable.hash
Inhabited.default
Append.append  -- x ++ y
Functor.map    -- f <$> xs
```

**Literais numéricos:**
```haskell
class OfNat (α : Type) (n : Nat) where
  ofNat : α

-- Instance para valores específicos
instance : OfNat LT4 0 where
  ofNat := LT4.zero

-- Instance para intervalos (pattern matching)
instance : OfNat Pos (n + 1) where
  ofNat := 
    let rec natPlusOne : Nat → Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

#eval (3 : Pos)  -- válido
#eval (0 : Pos)  -- erro
```

**Polimorfismo com type classes:**
```haskell
-- Constraints entre colchetes []
def List.sum [Add α] [OfNat α 0] : List α → α
  | [] => 0
  | x :: xs => x + xs.sum

-- Busca recursiva de instances
instance [Add α] : Add (PPoint α) where
  add p1 p2 := { x := p1.x + p2.x, y := p1.y + p2.y }
```

**Controlando busca de instances:**
```haskell
-- Output parameters: iniciam busca mesmo com tipo desconhecido
class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hPlus : α → β → γ

-- Default instances: usadas quando inputs não são totalmente conhecidos
@[default_instance]
instance [Add α] : HAdd α α α where
  hAdd := Add.add

-- Prioridades em default instances
@[default_instance 100]  -- maior prioridade
instance : Display Nat := ...
```

**Indexação segura:**
```haskell
class GetElem 
    (coll : Type)           -- tipo da coleção
    (idx : Type)            -- tipo do índice  
    (item : outParam Type)  -- tipo dos elementos
    (inBounds : outParam (coll → idx → Prop)) where  -- evidência
  getElem : (c : coll) → (i : idx) → inBounds c i → item

-- Instance para NonEmptyList
abbrev NonEmptyList.inBounds (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length

instance : GetElem (NonEmptyList α) Nat α NonEmptyList.inBounds where
  getElem := NonEmptyList.get

#eval idahoSpiders[0]   -- ok
#eval idahoSpiders[9]   -- erro: precisa prova
```

**Igualdade:**

1. **Boolean Equality (==)**: função que retorna Bool
    - BEq type class
    - Não funciona com funções
2. **Propositional Equality (=)**: afirmação matemática (tipo Prop)
    - Pode afirmar igualdade de qualquer expressão
    - Algumas são decidíveis (podem ser checadas automaticamente)

```haskell
#eval "hello" == "world"  -- Bool
#check (fun x => x + 1) = Nat.succ  -- Prop (válido)
#eval if 2 < 4 then "yes" else "no"  -- proposição decidível
```

**Functors:**

Um tipo polimórfico é um functor se tem um overload para `map`, que transforma cada elemento contido nele usando uma função, preservando a estrutura.

```haskell
class Functor (f : Type → Type) where
  map : {α β : Type} → (α → β) → f α → f β
  mapConst {α β : Type} (x : α) (coll : f β) : f α :=
    map (fun _ => x) coll

-- Operador infix
#eval (· + 5) <$> [1, 2, 3]  -- [6, 7, 8]

-- Instance para NonEmptyList
instance : Functor NonEmptyList where
  map f xs := { head := f xs.head, tail := f <$> xs.tail }

-- Instance para PPoint
instance : Functor PPoint where
  map f p := { x := f p.x, y := f p.y }
```

Leis dos Functors:
1. Identidade: `id <$> x = x`
2. Composição: `(h ∘ g) <$> v = h <$> (g <$> v)`

Observações importantes:
- Instance definida para o construtor de tipo (`List`), não o tipo completo (`List α`)
- Functor mapeia objetos (tipos via construtor) e morfismos (funções via map)
- Map só desce um nível: em `NonEmptyList (PPoint Nat)`, função mapeia `PPoint Nat`, não `Nat`
- `mapConst`: método default que substitui todos elementos por valor constante
- Exemplos: `List`, `Option`, `NonEmptyList` são functors; `List α` não é

```haskell
#eval Functor.mapConst 42 [1, 2, 3]  -- [42, 42, 42]
#eval toString <$> some 5            -- some "5"
```

**Derivando instances:**
```haskell
-- Durante definição do tipo
inductive Color where
  | red | green | blue
  deriving Repr, BEq, Hashable, Ord

-- Depois da definição
deriving instance BEq, Hashable for Pos

-- Classes deriváveis: Inhabited, BEq, Repr, Hashable, Ord
```

**Coercions:**
```haskell
-- 1. Coe: coerção entre tipos
instance : Coe Pos Nat where
  coe x := x.toNat

-- 2. CoeDep: coerção dependente do valor
instance : CoeDep (List α) (x :: xs) (NonEmptyList α) where
  coe := { head := x, tail := xs }

-- 3. CoeSort: coerção para sorts (Type ou Prop)
instance : CoeSort Monoid Type where
  coe m := m.Carrier

-- 4. CoeFun: coerção para funções
instance : CoeFun Adder (fun _ => Nat → Nat) where
  coe a := (· + a.howMuch)

-- Coerção manual com ↑
def x : Option Nat := ↑(5 : Nat)
```

**Tabela de type classes comuns:**
| Classe | Operador | Descrição |
|--------|----------|-----------|
| `Add α` | `+` | Adição homogênea |
| `HAdd α β γ` | `+` | Adição heterogênea |
| `BEq α` | `==` | Igualdade booleana |
| `Ord α` | `compare` | Ordenação (lt/eq/gt) |
| `Hashable α` | `hash` | Função hash |
| `ToString α` | `toString` | Conversão para string |
| `Append α` | `++` | Concatenação |
| `Functor f` | `<$>` | Map sobre estrutura |
| `GetElem coll idx` | `xs[i]` | Indexação |

**Convenções:**
- Use `@` antes do nome para ver type signature completa: `@IO.println`
- Type classes usam busca de instances, não unificação
- Instance implicits: colchetes `[]` para constraints
- Coercions não funcionam com dot notation (`x.foo`)
- `abbrev` para proposições (tactics enxergam definição)
- `def` para definições (tactics não enxergam)
- Definições de método default: `:=` na definição da classe
- `deriving` pode ser usado durante ou após definição do tipo
- Output parameters permitem busca com metavariables
- Default instances usadas como fallback quando busca normal falha
