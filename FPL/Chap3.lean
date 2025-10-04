-- 3. Overloading and Type Classes

-- Type classes (pioneiradas em Haskell) permitem overloading de operadores,
-- funcoes e literais de maneira que funciona bem com polimorfismo. Uma
-- Type class descreve uma colecao de operacoes overloadable. Para fazer overload
-- dessas operacoes para um novo tipo, uma `instance` eh criada contendo
-- implementacao de cada operacao para o novo tipo.

-- Exemplo: type class `Add` descreve tipos que permitem adicao,
-- e uma instance de `Add` para `Nat` fornece implementacao de adicao para `Nat`.

-- Type classes resolvem o problema de overloading + polimorfismo,
-- permitindo funcoes polimorficas que funcionam para qualquer tipo
-- que implemente as operacoes necessarias.


-- 3.1. Positive Numbers

-- Em algumas aplicacoes, apenas numeros positivos fazem sentido. Compiladores
-- tipicamente usam numeros de linha e coluna baseados em 1 para posicoes no
-- codigo fonte. Em vez de usar numeros naturais e espalhar o codigo com checagens
-- de que o numero nao eh zero, um datatype que representa apenas numeros
-- positivos pode ser util.

inductive Pos : Type where
  | one : Pos
  | succ : Pos → Pos

-- Esse tipo representa o desejado, mas nao eh conveniente de ser usado:
def sevenn : Pos := 7
def seven : Pos :=
  Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ Pos.one)))))

-- Operadores tambem nao sao faceis de usar:

def fourteen : Pos := seven + seven
def fortyNine : Pos := seven * seven

-- As mensagens de erro comecam com "failed to synthesize".
-- Isso indica que o erro eh devido a uma operacao overloaded que nao foi
-- implementada, e descreve a type class que deve ser implementada.


-- 3.1.1. Classes and Instances

-- Uma type class consiste de um nome, alguns parametros e uma colecao de metodos.
-- Os parametros descrevem os tipos para os quais operacoes overloadable estao
-- sendo definidas, e os metodos sao os nomes e assinaturas de tipo das operacoes
-- overloadable.

-- `Plus` eh o nome da type class, `α : Type` eh o unico argumeto e
-- `plus : α → α → α` eh o unico metodo.
class Plus (α : Type) where
  plus : α → α → α

-- Essa declaracao diz que existe uma type class `Plus` que faz overload de
-- operacoes com o argumento `(α : Type)`. Ha uma operacao overloaded chamada
-- plus que recebe dois αs e retorna um α.

-- Type classes sao first class, assim como tipos sao first class.
-- O tipo de Plus eh `Type → Type`, porque recebe um tipo como argumento (α)
-- e resulta em um novo tipo que descreve o overloading da operacao de Plus para α.

-- Para fazer overload de plus para um tipo particular, escreva uma instance:
instance : Plus Nat where
  plus := Nat.add

-- O dois pontos apos `instance` indica que `Plus Nat` eh um tipo.
-- Cada metodo da class Plus deve receber um valor usando `:=`.

-- Por padrao, metodos de type class sao definidos em namespace com o mesmo
-- nome da type class. Pode ser conveniente abrir o namespace:
open Plus (plus)

#eval plus 5 3

-- Definindo adicao para Pos e instance de Plus Pos:
def Pos.plus : Pos → Pos → Pos
  | Pos.one, k => Pos.succ k
  | Pos.succ n, k => Pos.succ (n.plus k)

instance : Plus Pos where
  plus := Pos.plus

def fourteen₂ : Pos := plus seven seven

-- Como nao ha instance de Plus Float, tentar somar floats usando plus falha:
#eval plus 5.2 917.25861

-- 1. Type class: descreve um conjunto de operacoes para os tipos que a instanciam.
--      nome + parametros (geralmente um tipo) + metodos (a operacao).
-- 2. Instance: a implementacao dos metodos para um tipo especifico.
-- 3. "failed to synthesize" = instance nao encontrada

-- Algumas type classes definem "propiedades" ao inves de operacoes:

#check Inhabited

#eval Inhabited.default (α := Nat)
#eval Inhabited.default (α := String)
#eval (default : Bool)
#eval (default : List Nat)


-- 3.1.2. Overloaded Addition

-- O operador de adicao built-in do Lean eh syntactic sugar para uma type class
-- chamada `HAdd`, que permite flexivelmente que os argumentos da adicao tenham
-- tipos diferentes. HAdd eh abreviacao de "heterogeneous addition".
-- Uma instance HAdd pode ser escrita para permitir que um Nat seja somado a um
-- Float, resultando em um novo Float. `x + y` eh interpretado como `HAdd.hAdd x y`.

-- Embora entender a generalidade completa de HAdd dependa de features discutidas
-- em outra secao (3.3.), ha uma type class mais simples chamada `Add` que nao
-- permite que os tipos dos argumentos sejam misturados. As bibliotecas do Lean
-- sao configuradas para que uma instance de Add seja encontrada ao buscar por
-- uma instance de HAdd onde ambos os argumentos tem o mesmo tipo.

-- Definir uma instance de Add Pos permite que valores Pos usem sintaxe
-- de adicao comum:
instance : Add Pos where
  add := Pos.plus

def fourteen₃ : Pos := seven + seven

-- 1. HAdd: adicao heterogenea (tipos diferentes) - mais geral
-- 2. Add: adicao homogenea (mesmo tipo) - mais simples
-- 3. x + y eh syntactic sugar para HAdd.hAdd x y


-- 3.1.3. Conversion to Strings

-- Outra type class built-in util eh a `ToString`. Instances dela fornecem uma
-- maneira padrao de converter valores de um dado tipo em strings.

-- Uma maneira de converter um `Pos` em `String` eh revelar sua estrutura interna.
-- A funcao posToString recebe um Bool que determina se deve parentesizar usos
-- de Pos.succ, que deve ser true na chamada inicial e false em todas as
-- chamadas recursivas.

def posToString (atTop : Bool) (p : Pos) : String :=
  let paren s := if atTop then s else "(" ++ s ++ ")"
  match p with
  | Pos.one => "Pos.one"
  | Pos.succ n => paren s!"Pos.succ {posToString false n}"

-- Usando essa funcao para uma instance ToString:
instance : ToString Pos where
  toString := posToString true

#eval s!"There are {seven}"

-- Por outro lado, todo numero positivo tem um Nat correspondente. Converter
-- para Nat e entao usar a instance ToString Nat eh uma maneira rapida de
-- gerar saida muito mais curta:

def Pos.toNat : Pos → Nat
  | Pos.one => 1
  | Pos.succ n => n.toNat + 1

instance : ToString Pos where
  toString x := toString (x.toNat)

#eval s!"There are {seven}"

-- Quando mais de uma instance eh definida, a mais recente tem precedencia.
-- Se um tipo tem uma instance ToString, eh possivel exibir o resultado de #eval
-- mesmo se o tipo nao foi definido com `deriving Repr`.


-- 3.1.4. Overloaded Multiplication
