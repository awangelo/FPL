import Lean.Data.Json

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

/-
  * Type class: descreve um conjunto de operacoes para os tipos que a instanciam
      nome + parametros (geralmente um tipo) + metodos (a operacao).
  * Instance: a implementacao dos metodos para um tipo especifico.
  * "failed to synthesize" = instance nao encontrada
-/
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

/-
  * HAdd: adicao heterogenea (tipos diferentes) - mais geral.
  * Add: adicao homogenea (mesmo tipo) - mais simples.
  * x + y eh syntactic sugar para HAdd.hAdd x y.
-/

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

-- Assim como na adicao, para multiplicacao existe o `HMul` (HMul.hMul x y) e
-- `Mul`. Com uma instancia de `Mul` para os Positvos, a multiplicacao funciona
-- como esperado.
def Pos.mul : Pos → Pos → Pos
  | Pos.one, k => k
  | Pos.succ n, k => n.mul k + k

instance : Mul Pos where
  mul := Pos.mul

#eval [seven * Pos.one, seven * seven, Pos.succ Pos.one * seven]


-- 3.1.5. Literal Numbers

-- Eh muito inconveniente escrever uma sequencia de construtores para numeros
-- positivos. Uma solucao seria fornecer uma funcao para converter Nat em Pos.
-- Porem, essa abordagem tem desvantagens:
-- 1. Como Pos nao pode representar 0, a funcao resultaria em um numero maior
--    ou retornaria Option Pos.
-- 2. A necessidade de chamar a funcao explicitamente tornaria programas que
--    usam numeros positivos muito menos convenientes que programas que usam Nat.
-- Ter um trade-off entre tipos precisos e APIs convenientes faz com que os
-- tipos precisos se tornem menos uteis.

-- Ha tres type classes usadas para fazer overload de literais numericos:
-- Zero, One e OfNat.

-- 1. ZERO: Muitos tipos tem valores naturalmente escritos como `0`.
#check Zero
-- Como 0 nao eh um numero positivo, nao deve haver instance de `Zero Pos`.

-- 2. ONE: Muitos tipos tem valores naturalmente escritos como `1`
#check One
-- Uma instance de One Pos faz sentido:
instance : One Pos where
  one := Pos.one

#eval (1 : Pos)

-- 3. OFNAT: Literais de numeros naturais sao interpretados usando OfNat
#check OfNat

-- Esta type class recebe dois argumentos:
-- - `α`: o tipo para o qual um numero natural eh overloaded
-- - `_`: Nat: o literal numerico real encontrado no programa
-- O metodo ofNat eh usado como valor do literal numerico.

-- Como a classe contem o argumento Nat, torna-se possivel definir apenas
-- instances para valores onde o numero faz sentido.

-- OfNat demonstra que argumentos para type classes NAO precisam ser tipos.
-- Essa flexibilidade permite que operacoes overloaded sejam fornecidas para
-- valores particulares alem de tipos particulares, assim, podendo criar uma
-- typeclass menos flexivel.

-- A biblioteca padrao do Lean organiza para que haja uma instance `Zero α`
-- sempre que houver uma instance `OfNat α 0`, e vice-versa. Similarmente, uma
-- instance de `One α` implica uma instance de `OfNat α 1`, assim como uma
-- instance de `OfNat α 1` implica uma instance de `One α`.

/-
  * Zero/One/OfNat: type classes para literais numericos.
  * OfNat permite overload para numeros especificos.
  * Type classes podem ter argumentos nao-tipo (menos flexiveis).
-/

-- Um Sum Type que pode representar numeros naturais menos que 4:
inductive LT4 where
  | zero
  | one
  | two
  | three
deriving Repr

-- Embora nao faca sentido permitir qualquer literal numerico para este tipo,
-- numeros menores que quatro claramente fazem sentido:

instance : OfNat LT4 0 where
  ofNat := LT4.zero

instance : OfNat LT4 1 where
  ofNat := LT4.one

instance : OfNat LT4 2 where
  ofNat := LT4.two

instance : OfNat LT4 3 where
  ofNat := LT4.three

#eval (3 : LT4)
#eval (0 : LT4)

-- Literais fora dos limites ainda nao sao permitidos:
#eval (4 : LT4)

-- Para Pos, a instance OfNat deve funcionar para qualquer Nat exceto Nat.zero.
-- Outra maneira de expressar isso eh dizer que para todos os numeros naturais n,
-- a instance deve funcionar para n + 1. Assim como nomes como `α` automaticamente
-- se tornam argumentos implicitos para funcoes que o Lean preenche sozinho,
-- instances podem receber argumentos implicitos automaticos.

-- Nesta instance, o argumento n representa qualquer Nat, e a instance eh
-- definida para um Nat que eh um maior:

instance : OfNat Pos (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat → Pos
      | 0 => Pos.one
      | k + 1 => Pos.succ (natPlusOne k)
    natPlusOne n

-- Como `n` representa um Nat a menos do que o usuario escreveu, a funcao
-- helper natPlusOne retorna um Pos que eh um maior que seu argumento. Isso
-- torna possivel usar literais de numeros naturais para numeros positivos,
-- mas nao para zero:

def eight : Pos := 8
def zero : Pos := 0

/-
  * OfNat permite instances para valores especificos (OfNat LT4 3).
  * OfNat pode usar padroes (OfNat Pos (n + 1)) para intervalos.
  * Argumentos implicitos em instances funcionam como em funcoes.
  * Isso permite literais numericos apenas onde fazem sentido logicamente.
-/


-- 3.1.6. Exercises


/- 3.1.6.1. Another Representation -/

-- An alternative way to represent a positive number is as the successor of some
-- Nat. Replace the definition of Pos with a structure whose constructor is named
-- succ that contains a Nat:

-- Define instances of Add, Mul, ToString, and OfNat that allow this version of
-- Pos to be used conveniently.

-- Representacao muito ruim (LEMBRAR SEMPRE QUE FALTA ADCIONAR O CASO BASE)
structure Posi where
  succ ::
  pred : Nat

instance : Mul Posi where
  mul a b := Posi.succ (a.pred * b.pred + a.pred + b.pred)

instance : Add Posi where
  add a b := Posi.succ (a.pred + b.pred + 1)

instance : ToString Posi where
  toString a := toString (a.pred + 1)

instance : OfNat Posi (n + 1) where
  ofNat := Posi.succ n

#eval (Posi.succ 2) + (Posi.succ 3)
#eval (Posi.succ 2) * (Posi.succ 3)
#eval s!"{(2 : Posi)}"


/- 3.1.6.2. Even Numbers -/

-- Define a datatype that represents only even numbers. Define instances of
-- Add, Mul, and ToString that allow it to be used conveniently. OfNat requires
-- a feature that is introduced in the next section.

inductive Even : Type where
  | zero : Even
  | succ : Even → Even

-- Parece que o Lean nao consegue otimizar para usar a melhor ordem, mesmo se
-- as em operacoes forem comutativas.
-- (deve ter alguma anotacao ou macro pra isso)
def Even.add : Even → Even → Even
  | Even.zero, k   => k
  | k, Even.zero   => k
  | Even.succ j, k => Even.succ (Even.succ (Even.add j k))

def Even.mul : Even → Even → Even
  | Even.zero, _   => Even.zero
  | _, Even.zero   => Even.zero
  | Even.succ j, k => Even.add k (Even.add k (Even.mul j k))
--  (j+2) * k = k + k + (j * k) = 2k + jk
--  6 * 2 = 2 + 2 + 4 * 2 = 2*4 + 2*4

def Even.toNat : Even → Nat
  | Even.zero => 0
  | Even.succ n => 2 + n.toNat

instance : Add Even where
  add := Even.add

instance : Mul Even where
  mul := Even.mul

instance : ToString Even where
  toString x := toString (x.toNat)

#eval Even.add Even.zero Even.zero
#eval Even.add Even.zero (Even.succ Even.zero)
#eval Even.add (Even.succ Even.zero) (Even.succ Even.zero)
#eval toString (Even.add (Even.succ Even.zero) (Even.succ Even.zero))


/- 3.1.6.3. HTTP Requests -/

-- An HTTP request begins with an identification of a HTTP method, such as GET
-- or POST, along with a URI and an HTTP version. Define an inductive type that
-- represents an interesting subset of the HTTP methods, and a structure that
-- represents HTTP responses. Responses should have a ToString instance that
-- makes it possible to debug them. Use a type class to associate different IO
-- actions with each HTTP method, and write a test harness as an IO action that
-- calls each method and prints the result.

namespace Http

inductive Method where
  | GET
  | POST
  | PUT
  | DELETE
  deriving Repr

structure Response where
  statusCode : Nat
  statusText : String

namespace Response

def toString (r : Response) : String :=
  s!"Status Code: {r.statusCode}, Status Text: {r.statusText}"

def ok : Response :=
  { statusCode := 200, statusText := "OK"}

def badRequest : Response :=
  { statusCode := 400, statusText := "Bad Request"}

def unauthorized : Response :=
  { statusCode := 401, statusText := "Unauthorized"}

def forbidden : Response :=
  { statusCode := 403, statusText := "Forbidden"}

def notFound : Response :=
  { statusCode := 404, statusText := "Not Found"}

def internalServerError : Response :=
  { statusCode := 500, statusText := "Internal Server Error"}

instance : ToString Response where
  toString := toString

end Response

class HttpMethod (m : Method) where
  execute : IO Response

instance : HttpMethod Method.GET where
  execute := pure Response.badRequest

instance : HttpMethod Method.POST where
  execute := pure Response.forbidden

instance : HttpMethod Method.PUT where
  execute := pure Response.ok

instance : HttpMethod Method.DELETE where
  execute := pure Response.notFound

def testHarness : IO Unit := do
  IO.println "Testing HTTP Methods:"
  let getResp ← HttpMethod.execute (m := Method.GET)
  IO.println s!"GET: {getResp}"
  let postResp ← HttpMethod.execute (m := Method.POST)
  IO.println s!"POST: {postResp}"
  let putResp ← HttpMethod.execute (m := Method.PUT)
  IO.println s!"PUT: {putResp}"
  let deleteResp ← HttpMethod.execute (m := Method.DELETE)
  IO.println s!"DELETE: {deleteResp}"

end Http

#eval Http.Response.ok
#eval Http.Response.badRequest
#eval Http.Response.notFound

#eval Http.testHarness


-- 3.2. Type Classes and Polymorphism

-- Colchetes em assinaturas de tipo indicam constraints de type class.
-- `IO.println : {α : Type} → [ToString α] → α → IO Unit`
-- funciona para qualquer tipo α que tenha instance ToString


-- 3.2.1. Checking Polymorphic Functions' Types

-- Verificar o tipo de uma funcao que recebe argumentos implicitos ou usa type
-- classes requer sintaxe adicional.

-- Simplesmente escrever:
#check (IO.println)
-- retorna um tipo com metavariables.

-- Isso acontece porque o Lean faz o melhor para descobrir argumentos implicitos,
-- e a presenca de metavariables indica que ainda nao descobriu informacao de
-- tipo suficiente para fazer isso.

-- Para entender a assinatura de uma funcao, esse comportamento pode ser
-- suprimido com um arroba (@) antes do nome da funcao:
#check @IO.println


-- 3.2.2. Defining Polymorphic Functions with Instance Implicits

-- Uma funcao que soma todo o conteudo de uma lista precisa de duas contraints
-- `Add` para poder soma-las `OfNat` para representar o caso base.

def List.sumAllContents [Add α] [OfNat α 0] : List α → α
  | []      => 0
  | x :: xs => x + xs.sumAllContents

-- Tambem pode ser definida com `Zero α`, que traz o mesmo efeito.
#check Zero

#eval [1, 2, 6].sumAllContents

def fourPos : List Pos := [1, 2, 3, 4]
#eval fourPos.sumAllContents


-- Instance implicits (colchetes []) fazem o Lean buscar valores em uma tabela
-- de instances, diferente de argumentos implicitos normais que usam unificacao.

-- Type classes sao estruturas nos bastidores - instances sao valores dessa
-- estrutura com implementacoes dos metodos.

-- Para adcionar dois `PPoint` eh nescessario que `α` tambem seja instancia de
-- `Add`.
structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

-- Instances podem receber outros instance implicits como argumentos.
-- Ex: Add (PPoint α) requer Add α
instance [Add α] : Add (PPoint α) where
  add p1 p2 := { x := p1.x + p2.x, y := p1.y + p2.y }

-- Isso cria busca recursiva: Lean encontra instance Add (PPoint Nat),
-- que referencia instance Add Nat encontrada automaticamente.

-- Type classes oferecem mais poder que simples overloading:
--   Biblioteca de instances polimorficas = blocos de codigo.
--   Compilador monta automaticamente dado apenas o tipo desejado.
--   Clientes da API nao precisam conectar partes manualmente.


-- 3.2.3. Methods and Implicit Arguments

#check @OfNat.ofNat
-- mas na declaracao do metodo, ofNat tem apenas tipo α.

-- Declarar uma type class cria:
-- 1. Estrutura contendo implementacoes das operacoes.
-- 2. Namespace com mesmo nome da classe.
-- 3. Funcao para cada metodo que busca implementacao na instance.

-- Similar a structures que criam accessors, mas metodos de type class
-- recebem instance como argumento implicito (encontrado automaticamente).

-- Para Lean encontrar instance, parametros devem estar disponveis:
-- cada parametro da type class deve aparecer no metodo antes da instance.

#check Add.add
-- α pode ser implicito pois argumentos fornecem informacao do tipo

-- OfNat.ofNat: literal Nat nao aparece em outros parametros
-- Lean nao teria info para descobrir n implicito → API inconveniente
-- Portanto usa parametro explicito nesses casos


-- 3.2.4. Exercises


/- 3.2.4.1. Even Number Literals -/

-- Write an instance of OfNat for the even number datatype from the previous
-- section's exercises that uses recursive instance search.

/-
  inductive Even : Type where
    | zero : Even
    | succ : Even → Even
-/

instance : OfNat Even 0 where
  ofNat := Even.zero

instance [OfNat Even n] : OfNat Even (n + 2) where
  ofNat := Even.succ (OfNat.ofNat n)

#eval (0 : Even)
#eval (2 : Even)
#eval (4 : Even)


/- 3.2.4.2. Recursive Instance Search Depth -/

-- There is a limit to how many times the Lean compiler will attempt a recursive
-- instance search. This places a limit on the size of even number literals
-- defined in the previous exercise. Experimentally determine what the limit is.

#eval (2222 : Even)
#eval (22222 : Even)
#eval (256 : Even) -- limite


-- 3.3. Controlling Instance Search

-- Add eh suficiente para definir adicao entre numeros de apenas um tipo.
-- Em varios casos pode ser util definir overloading de operadores heterogenos.

def addNatPos : Nat → Pos → Pos
  | 0, p => p
  | n + 1, p => Pos.succ (addNatPos n p)

def addPosNat : Pos → Nat → Pos
  | p, 0 => p
  | p, n + 1 => Pos.succ (addPosNat p n)

-- Funcoes como essas nao podem ser usadas na instancia `Add`, ja que `add` eh
-- do tipo `α → α → α`.


-- 3.3.1. Heterogeneous Overloadings

#check HAdd

instance : HAdd Nat Pos Pos where
  hAdd := addNatPos

instance : HAdd Pos Nat Pos where
  hAdd := addPosNat

#eval (3 : Pos) + (5 : Nat)
#eval (3 : Nat) + (5 : Pos)

-- A type class `HAdd` eh parecida com a seguinte:
class HPlus₀ (α : Type) (β : Type) (γ : Type) where
  hPlus₀ : α → β → γ

instance : HPlus₀ Nat Pos Pos where
  hPlus₀ := addNatPos

instance : HPlus₀ Pos Nat Pos where
  hPlus₀ := addPosNat

#eval toString (HPlus₀.hPlus₀ (3 : Pos) (5 : Nat))
#eval (HPlus₀.hPlus₀ (3 : Pos) (5 : Nat))
-- O erro ocorre porque o Lean nao conhece o tipo de `γ`, que deve ser o
-- tipo resultante da adicao, funciona apenas se o tipo for especificado.

#eval (HPlus₀.hPlus₀ (3 : Pos) (5 : Nat) : Pos)


-- 3.3.2. Output Parameters

-- Esse problema, pode ser resolvido declarando o `γ` como parametro de saida,
-- a maioria dos parametros para uma type class sao inputs para o algoritmo
-- de busca.

-- Em casos como esse eh util iniciar o processo de busca mesmo quando alguns
-- parametros de tipo ainda sao desconhecidos, isso pode ser feito utilizando
-- o modificador `outParam`:

class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hPlus : α → β → γ

instance : HPlus Nat Pos Pos where
  hPlus := addNatPos

instance : HPlus Pos Nat Pos where
  hPlus := addPosNat

#eval HPlus.hPlus (3 : Pos) (5 : Nat)


-- 3.3.3. Default Instances

-- Decidir se um parametro eh input ou output controla as circunstancias que
-- o Lean inicia a type class search, a pesquisa nao ocorre ate que todos os
-- parametros de input sejam conhecidos. Em alguns casos parametros de output
-- nao sao suficientes, e a instance search tambem deveria ocorrer quando
-- alguns inputs sao desconhecidos. (Eh parecido com valores default para
-- parametros opcionais em python, mas nesse caso sao tipos).

-- "Default instances" sao instancias que estao disponiveis para a busca
-- _ate quando nao todos os inputs sao conhecidos_. Quando uma dessas
-- instancias pode ser usada, ela sera. Isso permite que programas passem no
-- type checker ao inves de falhar com erros sobre tipos desconhecidos e
-- metavariables.

-- Exemplo de uma default instance: HPlus pode ser derivado de uma instancia
-- `Add`. Adicao normal eh um caso especial da adicao heterogenea onde todos
-- os tres tipos sao iguais:
instance [Add α] : HPlus α α α where
  hPlus := Add.add

#eval HPlus.hPlus (3 : Nat) (5 : Nat)
#check HPlus.hPlus (3 : Nat) (5 : Nat)

-- hPlus funciona quando os dois tipos sao conhecidos, mas com apenas um
-- argumento, gera metavariables:
#check HPlus.hPlus (3 : Nat)

-- Na maioria dos casos, quando alguem fornece um argumento para adicao,
-- o outro argumento tera o mesmo tipo. Para tornar esta instance em uma
-- default instance, aplica-se o atributo `default_instance`:
@[default_instance]
instance [Add α] : HPlus α α α where
  hPlus := Add.add

#check HPlus.hPlus (3 : Nat)

-- Cada operador que existe em versoes heterogeneas e homogeneas segue esse
-- padrao: o operador infix eh substituido por uma chamada da versao heterogenea,
-- e a instance homogenea default eh selecionada quando possivel.

-- Similarmente, escrever `5` da um Nat ao inves de uma metavariable esperando
-- mais informacao para selecionar uma instance OfNat, porque a instance
-- OfNat para Nat eh uma default instance.

-- Default instances tambem podem receber prioridades que afetam qual sera
-- escolhida em situacoes onde mais de uma pode se aplicar.

#check OfNat


-- 3.3.4. Exercises

/-
  Define an instance of `HMul (PPoint α) α (PPoint α)` that multiplies both
  projections by the scalar. It should work for any type α for which there
  is a Mul α instance. For example,

  `#eval {x := 2.5, y := 3.7 : PPoint Float} * 2.0`

  should yield

  `{ x := 5.000000, y := 7.400000 }`
-/

#check PPoint
#check HMul
#check Mul

instance [Mul α] : HMul (PPoint α) α (PPoint α) where
  hMul p scalar := ⟨p.x * scalar, p.y * scalar⟩

#eval {x := 2.5, y := 3.7 : PPoint Float} * 2.0


-- 3.4. Arrays and Indexing

-- O interlude mostra como usar indexing para pegar entradas de uma lista
-- pela posicao, essa sintaxe tambem eh governada por uma type class.


-- 3.4.1. Arrays

-- Em Lean, `Array α` eh um array dinamicamente redimensionavel contendo
-- valores do tipo α, diferente de `List` onde cada cons tem um pointer.

#check List   -- Linked List
#check Array  -- Slice
#check Vector -- Array com tamanho n

-- Em linguagens funcionais puras como Lean, nao eh possivel mutar uma posicao
-- em uma estrutura de dados. Ao inves disso, uma copia eh feita com as
-- modificacoes desejadas. Porem, copiar nem sempre eh necessario: o compilador
-- e runtime do Lean contem uma otimizacao que permite modificacoes serem
-- implementadas como mutacoes nos bastidores quando ha apenas uma unica
-- referencia ao array.

-- Arrays sao parecidos com listas, mas com um # na frente:
def northernTrees : Array String :=
  #["sloe", "birch", "elm", "oak"]

#eval northernTrees.size

#eval northernTrees[2]
#eval northernTrees[8]


-- 3.4.2. Non-Empty Lists

-- Um datatype que representa listas nao-vazias pode ser definido como uma
-- estrutura com um campo para a cabeca da lista e um campo para a cauda,
-- que eh uma lista comum potencialmente vazia:

structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α

def idahoSpiders : NonEmptyList String := {
  head := "Banded Garden Spider",
  tail := [
    "Long-legged Sac Spider",
    "Wolf Spider",
    "Hobo Spider",
    "Cat-faced Spider"
  ]
}

-- Lookup por indice em uma lista nao-vazia considera tres possibilidades:
-- 1. Indice 0: retorna head
-- 2. Indice n+1, tail vazia: indice fora dos limites
-- 3. Indice n+1, tail nao-vazia → chamada recursiva em tail e n

def NonEmptyList.get? : NonEmptyList α → Nat → Option α
  | xs, 0 => some xs.head
  | {head := _, tail := []}, _ + 1 => none
  | {head := _, tail := h :: t}, n + 1 => get? {head := h, tail := t} n

-- Versao simples:
def NonEmptyList.get?₁ : NonEmptyList α → Nat → Option α
  | xs, 0              => some xs.head
  | ⟨_, []⟩, _ + 1     => none
  | ⟨_, h :: t⟩, n + 1 => get? ⟨h, t⟩ n

-- Versao alternativa usando lookup de lista `xs.tail[n]?`:
def NonEmptyList.get?' : NonEmptyList α → Nat → Option α
  | xs, 0 => some xs.head
  | xs, n + 1 => xs.tail[n]?

-- Indices validos para uma lista nao-vazia sao numeros naturais estritamente
-- menores que o comprimento da lista, ou seja, menor ou igual ao comprimento
-- da cauda.

-- Definicao de indice dentro dos limites usando `abbrev` porque tactics sabem
-- resolver desigualdades numericas, mas nao conhecem nomes customizados:
abbrev NonEmptyList.inBounds (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length

-- Esta funcao retorna uma proposicao que pode ser verdadeira ou falsa:
theorem atLeastThreeSpiders : idahoSpiders.inBounds 2 := by decide

theorem notSixSpiders : ¬idahoSpiders.inBounds 5 := by decide
-- Operador de negacao logica `¬` tem precedencia muito baixa:
-- `¬idahoSpiders.inBounds 5` eh equivalente a `¬(idahoSpiders.inBounds 5)`

-- Funcao de lookup que requer evidencia de que o indice eh valido,
-- portanto nao precisa retornar Option:
def NonEmptyList.get (xs : NonEmptyList α)
    (i : Nat) (ok : xs.inBounds i) : α :=
  match i with
  | 0 => xs.head
  | n + 1 => xs.tail[n]

-- Esta funcao delega para a versao de List que checa evidencia em compile time.
-- Eh possivel escrever esta funcao para usar a evidencia diretamente, mas
-- requer tecnicas para trabalhar com provas e proposicoes (descritas mais tarde).

-- Com def: tactics nao enxergam automaticamente
def BadInBounds (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length

theorem bad : BadInBounds idahoSpiders 2 := by
  -- Decide falharia aqui, precisa fazer `unfold BadInBounds`.
  unfold BadInBounds
  decide

-- Com abbrev: tactics enxergam direto
abbrev GoodInBounds (xs : NonEmptyList α) (i : Nat) : Prop :=
  i ≤ xs.tail.length

theorem good : GoodInBounds idahoSpiders 2 := by
  decide


-- 3.4.3. Overloading Indexing

-- Notacao de indexacao para um tipo de colecao (`xs[i]`, `xs[i]?`, `xs[i]!`,
--`xs[i]'p`) pode ser overloaded definindo uma instance da type class `GetElem`.

class GetElem'
    (coll : Type)
    (idx : Type)
    (item : outParam Type)
    (inBounds : outParam (coll → idx → Prop)) where
  getElem : (c : coll) → (i : idx) → inBounds c i → item

-- GetElem tem quatro parametros:
-- 1. coll: tipo da colecao
-- 2. idx: tipo do indice
-- 3. item: tipo dos elementos extraidos (outParam)
-- 4. inBounds: funcao que determina evidencia de indice valido (outParam)

-- Metodo `getElem` que recebe:
-- - valor da colecao
-- - valor do indice
-- - evidencia de que o indice esta dentro dos limites
-- e retorna um elemento.

-- Para NonEmptyList α, os parametros sao:
-- - colecao: NonEmptyList α
-- - indices: Nat
-- - elementos: α
-- - inBounds: indice ≤ comprimento da cauda

-- A instance pode delegar diretamente para NonEmptyList.get:
instance : GetElem (NonEmptyList α) Nat α NonEmptyList.inBounds where
  getElem := NonEmptyList.get

#eval idahoSpiders[0]
#eval idahoSpiders[2]
#eval idahoSpiders[9]

-- Como tipo de colecao e tipo de indice sao parametros de INPUT, novos tipos
-- podem ser usados para indexar colecoes existentes. Ex:

-- Pos como indice para List (nao pode apontar para primeira entrada):
instance : GetElem (List α) Pos α (fun list n => list.length > n.toNat) where
  getElem (xs : List α) (i : Pos) _ := xs[i.toNat]

-- Bool como indice para PPoint (false = x, true = y):
instance : GetElem (PPoint α) Bool α (fun _ _ => True) where
  getElem (p : PPoint α) (i : Bool) _ :=
    if not i then p.x else p.y

#eval ({x := 10, y := 20} : PPoint Nat)[false]
#eval ({x := 10, y := 20} : PPoint Nat)[true]

-- Neste caso, ambos os Booleans sao indices validos. Como todo Bool possivel (2)
-- esta in bounds (PPoint tem 2 elementos), a evidencia eh simplesmente a proposicao
-- verdadeira `True`.


-- 3.5. Standard Classes

-- Alguns operadores e funcoes que podem ser overloaded usando type classes.
-- Cada operador ou funcao corresponde a um metodo de uma type class. Fazer overload
-- para novos tipos nao eh feito usando o operador em si, mas sim o nome.


-- 3.5.1. Arithmetic

-- A maioria dos operadores aritmeticos esta disponivel em forma heterogenea,
-- (argumentos podem ter tipos diferentes) e um parametro de saida decide o tipo
-- da expressao resultante. Para cada operador heterogeneo, existe uma versao
-- homogenea (sem "h") correspondente.

#check HAdd.hAdd  -- (x + y)
#check HSub.hSub  -- (x - y)
#check HMul.hMul  -- (x * y)
#check HDiv.hDiv  -- (x / y)
#check HMod.hMod  -- (x % y)
#check HPow.hPow  -- (x ^ y)
#check Neg.neg    -- (- x  )


-- 3.5.2. Bitwise Operators

-- Ha instances para tipos de largura fixa como:
-- - UInt8,  Int8
-- - UInt16, Int16
-- - UInt32, Int32
-- - UInt64, Int64
-- - USize : word size na plataforma atual (32 ou 64)

#check HAnd.hAnd  -- (x &&& y)
#check HOr.hOr    -- (x ||| y)
#check HXor.hXor  -- (x ^^^ y)`
#check Complement.complement    -- (~~~x   )
#check HShiftRight.hShiftRight  -- (x >>> y)
#check HShiftLeft.hShiftLeft    -- (x <<< y)

-- Como os nomes `And` e `Or` ja sao usados pelos operadores logicos, as versoes
-- homogeneas do `HAnd` e `HOr` se chamam: `AndOp` e `OrOp`.


-- 3.5.3. Equality and Ordering

-- Testar igualdade de dois valores tipicamente usa a classe `BEq` (Boolean Equality).
-- Devido ao uso do Lean como provador de teoremas, existem dois tipos de
-- operadores de igualdade em Lean:

-- 1. BOOLEAN EQUALITY (==):

-- Eh o mesmo tipo de igualdade encontrado em outras linguagens de programacao.
-- Eh uma funcao que recebe dois valores e retorna um Bool.
-- A expressao `x == y` eh igual a `BEq.beq x y`.
#check BEq

-- Como Lean eh uma linguagem funcional pura, nao ha nocao separada de igualdade
-- de referencia vs valor - ponteiros nao podem ser observados diretamente.

#eval "Octopus" == "Cuttlefish"
#eval "Octopodes" == "Octo".append "podes"

-- Alguns valores, como funcoes, NAO podem ser checados por igualdade:
#check (fun (x : Nat) => 1 + x) == (Nat.succ ·)

-- 2. PROPOSITIONAL EQUALITY (=):

-- Igualdade proposicional eh uma afirmacao matematica ao inves de uma invocacao
-- de um programa. Como proposicoes sao como tipos que descrevem evidencia para
-- alguma afirmacao, igualdade proposicional tem mais em comum com tipos como
-- `String` e `Nat → List Int` do que com igualdade booleana.

-- Isso significa que ela nao pode ser automaticamente checada. No entanto, a
-- igualdade de quaisquer duas expressoes pode ser afirmada em Lean, desde que
-- tenham o mesmo tipo.

#check (fun (x : Nat) => 1 + x) = (Nat.succ ·)
-- Essa eh uma afirmacao perfeitamente razoavel. Do ponto de vista da matematica,
-- duas funcoes sao iguais se mapeiam entradas iguais para saidas iguais, entao
-- da pra perceber que essa afirmacao eh verdadeira, mesmo que o Lean precise de
-- uma prova de uma linha para ser convencido.

-- Algumas proposicoes sao _decidable_, o que significa que podem ser checadas
-- assim como uma funcao booleana. A funcao que checa se a proposicao eh verdadeira
-- ou falsa eh chamada de decision procedure (procedimento de decisao), e retorna
-- evidencia da verdade ou falsidade da proposicao.

-- Exemplos de proposicoes decidiveis incluem:
-- - Igualdade e desigualdade de numeros naturais
-- - Igualdade de strings
-- - "ands" e "ors" de proposicoes que sao elas mesmas decidiveis

-- Em Lean, `if` funciona com proposicoes decidiveis. Por exemplo, `2 < 4`
-- eh uma proposicao:

#check 2 < 4

#eval if 2 < 4 then 1 else 2

-- _Nao_ todas as proposicoes sao decidiveis. Se fossem, entao computadores
-- poderiam provar qualquer proposicao verdadeira apenas executando o procedimento
-- de decisao, e matematicos estariam sem emprego.

-- Mais especificamente, proposicoes decidiveis tem uma instance da type class
-- `Decidable`, que contem o procedimento de decisao. Tentar usar uma proposicao
-- que nao eh decidivel como se fosse um Bool resulta em falha ao encontrar a
-- instance Decidable.

#check if (fun (x : Nat) => 1 + x) = (Nat.succ ·) then "yes" else "no"

-- As seguintes proposicoes, que geralmente sao decidiveis, sao overloaded
-- com type classes:

-- | Expression | Desugaring      | Class Name |
-- |------------|-----------------|------------|
-- | x < y      | LT.lt x y       | LT         |
-- | x ≤ y      | LE.le x y       | LE         |
-- | x > y      | LT.lt y x       | LT         |
-- | x ≥ y      | LE.le y x       | LE         |

#check LT
#check LE

-- Como definir novas proposicoes ainda nao foi demonstrado, pode ser dificil
-- definir instances completamente novas de LT e LE. Mas, elas podem ser
-- definidas usando instances existentes.

instance : LT Pos where
  lt x y := LT.lt x.toNat y.toNat

instance : LE Pos where
  le x y := LE.le x.toNat y.toNat

-- Essas proposicoes _nao_ sao decidiveis por padrao porque Lean nao expande as
-- definicoes de proposicoes ao sintetizar uma instance. Isso pode ser resolvido
-- usando o operador `inferInstanceAs`, que encontra uma instance para uma dada
-- classe se ela existir:

instance {x : Pos} {y : Pos} : Decidable (x < y) :=
  inferInstanceAs (Decidable (x.toNat < y.toNat))

instance {x : Pos} {y : Pos} : Decidable (x ≤ y) :=
  inferInstanceAs (Decidable (x.toNat ≤ y.toNat))

-- Comparar valores usando `<`, `==` e `>` pode ser ineficiente. Checar primeiro
-- se um valor eh menor que outro, e depois se sao iguais, pode requerer duas
-- travessias sobre grandes estruturas de dados.

-- Para resolver esse problema, Java e C# tem metodos padrao `compareTo` e
-- `CompareTo` (respectivamente) que podem ser overridden por uma classe para
-- implementar todas as tres operacoes ao mesmo tempo. Esses metodos retornam
-- um inteiro negativo se o receptor eh menor que o argumento, zero se sao
-- iguais, e um inteiro positivo se o receptor eh maior que o argumento.

-- Ao inves de fazer overload do significado de inteiros, Lean tem um tipo
-- indutivo built-in que descreve essas tres possibilidades:

inductive Ordering' where
  | lt
  | eq
  | gt

#check Ordering

-- A type class `Ord` pode ser overloadada para produzir essas comparacoes.

def Pos.comp : Pos → Pos → Ordering
  | Pos.one, Pos.one => Ordering.eq
  | Pos.one, Pos.succ _ => Ordering.lt
  | Pos.succ _, Pos.one => Ordering.gt
  | Pos.succ n, Pos.succ k => comp n k

instance : Ord Pos where
  compare := Pos.comp

#eval Ord.compare (3 : Pos) (5 : Pos)
#eval Ord.compare (7 : Pos) (5 : Pos)
#eval Ord.compare (5 : Pos) (5 : Pos)


-- 3.5.4. Hashing

-- Java e C# tem metodos `hashCode` e `GetHashCode`, respectivamente, que
-- computam um hash de um valor para uso em estruturas de dados como hash tables.
-- O equivalente em Lean eh uma type class chamada `Hashable`:

class Hashable' (α : Type) where
  hash : α → UInt64

#check Hashable

-- Se dois valores sao considerados iguais de acordo com uma instance `BEq`
-- para seu tipo, entao eles _devem_ ter os mesmos hashes.

-- A stdlib contem uma funcao `mixHash` com tipo `UInt64 → UInt64 → UInt64`
-- que pode ser usada para combinar hashes de diferentes campos de um construtor.

#check mixHash

-- Uma funcao de hash razoavel para um datatype indutivo pode ser escrita
-- atribuindo um numero unico para cada construtor, e entao misturando esse
-- numero com os hashes de cada campo.

def hashPos : Pos → UInt64
  | Pos.one => 0
  | Pos.succ n => mixHash 1 (hashPos n)

instance : Hashable Pos where
  hash := hashPos

#eval hash (7 : Pos)
#eval hash (42 : Pos)

-- Instances de Hashable para tipos polimorficos podem usar busca recursiva
-- de instances. Fazer hash de um `NonEmptyList α` so eh possivel quando
-- `α` pode ser hasheado:

instance [Hashable α] : Hashable (NonEmptyList α) where
  hash xs := mixHash (hash xs.head) (hash xs.tail)

-- Arvores binarias usam tanto recursao quanto busca recursiva de instances
-- nas implementacoes de BEq e Hashable:

inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α → α → BinTree α → BinTree α

-- BEq para BinTree: compara estrutura recursivamente
def eqBinTree [BEq α] : BinTree α → BinTree α → Bool
  | BinTree.leaf, BinTree.leaf =>
    true
  | BinTree.branch l x r, BinTree.branch l2 x2 r2 =>
    x == x2 && eqBinTree l l2 && eqBinTree r r2
  | _, _ =>
    false

instance [BEq α] : BEq (BinTree α) where
  beq := eqBinTree

def hashBinTree [Hashable α] : BinTree α → UInt64
  | BinTree.leaf =>
    0
  | BinTree.branch left x right =>
    mixHash 1
            (mixHash (hashBinTree left)
                     (mixHash (hash x)
                              (hashBinTree right)))

instance [Hashable α] : Hashable (BinTree α) where
  hash := hashBinTree

def exampleTree₁ : BinTree Nat :=
  BinTree.branch
    (BinTree.branch BinTree.leaf 1 BinTree.leaf)
    2
    (BinTree.branch BinTree.leaf 3 BinTree.leaf)

def exampleTree₂ : BinTree Nat :=
  BinTree.branch
    (BinTree.branch BinTree.leaf 1 BinTree.leaf)
    2
    (BinTree.branch BinTree.leaf 4 BinTree.leaf)  -- diferente aqui

#eval hash exampleTree₁
#eval hash exampleTree₂
#eval hash exampleTree₁ == hash exampleTree₂


-- 3.5.5. Deriving Standard Classes

-- Instances de classes como `BEq` e `Hashable` sao frequentemente muito
-- tediosas de implementar manualmente. Lean inclui uma feature chamada
-- "instance deriving" que permite ao compilador construir automaticamente
-- instances de muitas type classes. De fato, a frase `deriving Repr` na
-- definicao de Firewood na primeira secao sobre polimorfismo eh um exempl
-- de instance deriving.

-- Instances podem ser derivadas de duas formas:
-- 1. Quando definindo uma sctructure ou inductive type, adicionando
--    `deriving` no final da declaracao do tipo seguido pelos nomes das
--    classes que serao derivadas:
inductive Color where
  | red
  | green
  | blue
  deriving Repr, BEq, Hashable

#eval Color.red
#eval Color.red == Color.blue
#eval hash Color.green

-- 2. Para um tipo ja definido, escreva `deriving instance C1, C2 for T`
--    para derivar instances de C1 e C2 para o tipo T:
deriving instance BEq, Hashable for Pos
deriving instance BEq, Hashable for NonEmptyList

#eval (5 : Pos) == (7 : Pos)
def list1 : NonEmptyList Nat := {head := 1, tail := [2, 3]}
#eval hash list1

-- Instances podem ser derivadas para pelo menos as seguintes classes:
-- * Inhabited : valores default
-- * BEq       : igualdade booleana
-- * Repr      : representacao
-- * Hashable  : funcao hash
-- * Ord       : ordenacao (comparacao)

structure Example where
  field1 : Nat
  field2 : String
  deriving Inhabited, BEq, Repr, Hashable, Ord

#eval (default : Example)  -- Inhabited
#eval Example.mk 1 "a" == Example.mk 1 "a"  -- BEq
#eval repr (Example.mk 1 "a")  -- Repr
#eval hash (Example.mk 1 "a")  -- Hashable
#eval compare (Example.mk 1 "a") (Example.mk 2 "b")  -- Ord

-- Em alguns casos, a instance Ord derivada pode nao produzir precisamente
-- a ordenacao desejada em uma aplicacao. Quando esse eh o caso, eh aceitavel
-- escrever uma instance Ord manualmente.

structure Person where
  name : String
  age : Nat
  deriving BEq, Repr, Hashable

-- Ord customizado (por idade ao inves de ordem lexicografica):
instance : Ord Person where
  compare p1 p2 := compare p1.age p2.age

def alice : Person := {name := "Alice", age := 30}
def bob : Person := {name := "Bob", age := 25}

#eval compare alice bob

-- Derivar de instancias tambem facilita a manutencao do codigo, pois as
-- instancias sao atualizadas a medida que as definicoes dos tipos evoluem.
-- Ao revisar as alteracoes no codigo, as modificacoes que envolvem atualizacoes
-- nos tipos de dados sao muito mais faceis de ler, sem linhas e mais linhas de
-- modificacoes nas implementacoes das instances.


-- 3.5.6. Appending

-- Datatypes que fazem appending implementam a typeclass `HAppend`, que eh uma
-- operacao heterogenea. Implementala habilita o uso do `++`.

#check HAppend

-- Para casos casos homogeneos a implementacao geralmente segue o padrao:
instance : Append (NonEmptyList α) where
  append xs ys :=
    { head := xs.head, tail := xs.tail ++ ys.head :: ys.tail }

#eval idahoSpiders ++ idahoSpiders

-- Essa instance de HAppend permite append de listas nao vazias com listas
-- normais (o que sempre gera uma lista nao vazia):

instance : HAppend (NonEmptyList α) (List α) (NonEmptyList α) where
  hAppend xs ys :=
    { head := xs.head, tail := xs.tail ++ ys }

#eval idahoSpiders ++ ["Trapdoor Spider"]


-- 3.5.7. Functors

-- Um tipo polimorfico eh um functor se tem um overload para uma funcao
-- chamada `map` que transforma cada elemento contido nele por uma funcao.

#check Functor

-- Exemplos de functors e como suas instances Functor fazem overload de map:
-- - Mapear sobre lista constroi nova lista com cada entrada transformada
-- - Mapear sobre Option: `none` inalterado, `some x` vira `some (f x)`

#eval Functor.map (· + 5) [1, 2, 3]
#eval Functor.map toString (some (List.cons 5 List.nil))
#eval Functor.map List.reverse [[1, 2, 3], [4, 5, 6]]

-- Como `Functor.map` eh um nome longo para uma operacao comum, Lean fornece
-- um operador infix para mapear uma funcao: `<$>`

#eval (· + 5) <$> [1, 2, 3]
#eval toString <$> (some (List.cons 5 List.nil))
#eval List.reverse <$> [[1, 2, 3], [4, 5, 6]]

-- Instance de Functor para NonEmptyList:
instance : Functor NonEmptyList where
  map f xs := { head := f xs.head, tail := f <$> xs.tail }

-- Aqui, map usa a instance Functor para List para mapear a funcao sobre a cauda.
-- A instance eh definida para `NonEmptyList` ao inves de `NonEmptyList α` porque
-- um functor eh uma "funcao" com tipo: `Type u → Type v` (nesse caso o
-- construtor `NonEmptyList`), assim como o map tem tipo `(α → β) → f α → f β`.

-- Aqui, map usa a instance Functor para List para mapear a funcao sobre a cauda.
-- A instance eh definida para `NonEmptyList` ao inves de `NonEmptyList α` porque
-- um functor eh uma "funcao de tipo para tipo" (type constructor): `Type → Type`.
--   `NonEmptyList`: construtor de tipo,
--   `(NonEmptyList α)`: tipo completo.

#check @NonEmptyList

-- Se α fosse um parametro da classe, seria possivel fazer versoes de Functor
-- que so funcionassem para NonEmptyList Nat, mas parte de ser um functor eh
-- que map funciona para qualquer tipo de entrada.

instance : Functor PPoint where
  map f p := { x := f p.x, y := f p.y }

-- Neste caso, f foi aplicado tanto em x quanto em y.

-- Mesmo quando o tipo contido em um functor eh ele mesmo um functor, mapear
-- uma funcao so desce um nivel. Ao usar map em `NonEmptyList (PPoint Nat)`,
-- a funcao sendo mapeada deve receber PPoint Nat como argumento, nao Nat.

-- A definicao da classe Functor usa mais uma feature de linguagem que ainda
-- nao foi discutida: _definicoes de metodo default_. Normalmente, uma classe
-- especifica algum conjunto minimo de metodos overloadable, e entao usa funcoes
-- polimorficas com argumentos de instance implicitos (nesse caso o concat)
-- para fornecer uma gama maior de features.

-- A funcao concat pode concatenar qualquer lista nao-vazia cujas entradas
-- sao appendable:
def concat [Append α] (xs : NonEmptyList α) : α :=
  let rec catList (start : α) : List α → α
    | [] => start
    | (z :: zs) => catList (start ++ z) zs
  catList xs.head xs.tail

-- Porem, para algumas classes, ha operacoes que podem ser implementadas mais
-- eficientemente com conhecimento dos internals de um datatype.

-- Nesses casos, uma definicao de metodo default pode ser fornecida. Uma
-- definicao de metodo default fornece uma implementacao default de um metodo
-- em termos dos outros metodos. Porem, implementadores de instance podem
-- escolher fazer override desse default com algo mais eficiente.

-- _Definicoes de metodo default contem `:=` na definicao da classe._

-- No caso de Functor, alguns tipos tem uma maneira mais eficiente de implementar
-- map quando a funcao sendo mapeada ignora seu argumento. Funcoes que ignoram
-- seus argumentos sao chamadas funcoes constantes porque sempre retornam o
-- mesmo valor.

-- Aqui esta a definicao de Functor, onde `mapConst` tem implementacao default:
class Functor' (f : Type → Type) where
  map : {α β : Type} → (α → β) → f α → f β

  mapConst {α β : Type} (x : α) (coll : f β) : f α :=
    map (fun _ => x) coll

-- Especificamente, instances Functor devem seguir duas regras:

-- 1. Mapear a funcao identidade deve resultar no argumento original.
--      _`id <$> x` deve ser igual a `x`_

#eval id <$> [1,2,0] = [1,2,0]

-- 2. Mapear duas funcoes compostas deve ter o mesmo efeito que compor seu mapping.
--      _For all functions `h : β → γ` and `g : α → β`, `(h ∘ g) <$> v = h <$> g <$> v`_

#eval ((· * 2) ∘ (· + 2)) <$> [1,2,3] =
      (· * 2) <$> (· + 2) <$> [1,2,3]

-- Essas regras previnem implementacoes de map que movem os dados ou deletam
-- alguns deles.

/-
  * _Functor pode ser entendido como a dupla do contrutor de tipo e map, de forma que_
    _o primeiro mapeia objetos (tipos), e o segundo mapeia morfismos (funcoes)._
  * A operacao que define um functor eh um "mapeamento" (transformacao) de um
    objeto X dentro de uma estrutura C para um objeto F(X) dentro de uma estrutura D.
  * Contrutores de tipos polimorficos sao functors caso implementem `map` seguindo
    as duas leis dos functors.
  * Ex:
      - `List` eh um functor.
      - A funcao `NonEmptyList` (construtor do tipo `NonEmptyList`) eh um functor.
      - `List α` _nao_ eh um functor.
-/


-- 3.5.8. Messages You May Meet

-- Lean pode nao ser capaz de derivar algumas instancias para certas classes:

deriving instance ToString for NonEmptyList

-- Chamar `deriving instance` faz o Lean consultar uma tabela interna que faz code
-- generation para instancias de typeclass, essa mensagem significa que nenhum code
-- generator foi encontrado para `ToString`.


-- 3.5.9. Exercises

-- Write an instance of HAppend (List α) (NonEmptyList α) (NonEmptyList α) and test it.

instance : HAppend (List α) (NonEmptyList α) (NonEmptyList α) where
  hAppend xs ys :=
    match xs with
    | [] => ys
    | x' :: xs' => { head := x', tail := xs' ++ ys.head :: ys.tail }
    -- Mesma coisa que `xs' ++ (ys.head :: ys.tail)`, precedencia de `::` eh maior.

-- Implement a Functor instance for the binary tree datatype.

instance : Functor BinTree where
  map f tree :=
    let rec fmap
      | BinTree.leaf => BinTree.leaf
      | BinTree.branch l x r => BinTree.branch (fmap l) (f x) (fmap r)
      -- ⟨(fmap l), (f x), (fmap r)⟩ nao pode ser usado para um tipo indutivo com
      --   mais de um contrutor.
    fmap tree

#eval (· + 1) <$> exampleTree₁


-- 3.6. Coercions

-- Em matematica eh comum utilizar o mesmo simbolo para diferentes aspectos de
-- algum objeto em contextos diferentes. Por exemplo, se um ring eh referenciado
-- em um contexto onde um conjunto eh esperado, entende-se que o conjunto
-- subjacente do ring eh o que deve ser usado.

-- Em linguagens de programacao, eh comum ter regras para traduzir automaticamente
-- valores de um tipo para valores de outro tipo. Java permite que um byte seja
-- automaticamente promovido para um int, e Kotlin permite que um tipo nao-nulo
-- seja usado em um contexto que espera a versao nulavel do tipo.

-- Em Lean, ambos os propositos sao servidos por um mecanismo chamado _coercions_.
-- Quando Lean encontra uma expressao de um tipo em um contexto que espera um
-- tipo diferente, ele tentara fazer coercion da expressao antes de reportar um
-- erro de tipo.


-- 3.6.1. Strings and Paths

-- No codigo fonte do feline, uma String era convertida para FilePath usando
-- sintaxe de construtor anonimo (`⟨fileName⟩`). Mas isso nao era necessario:
-- Lean define uma coercion de String para FilePath, entao uma string pode ser
-- usada em uma posicao onde um path eh esperado.

-- Mesmo que a funcao IO.FS.readFile tenha tipo `System.FilePath → IO String`,
-- o seguinte codigo eh aceito pelo Lean:

def fileDumper : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  stdout.putStr "Which file? "
  stdout.flush
  let f := (← stdin.getLine).trim
  stdout.putStrLn s!"'The file {f}' contains:"
  stdout.putStrLn (← IO.FS.readFile f)

-- String.trim remove espacos em branco no inicio e fim de uma string.
-- Na ultima linha de fileDumper, a coercion de String para FilePath converte
-- automaticamente `f`, entao nao eh necessario escrever `IO.FS.readFile ⟨f⟩`.


-- 3.6.2. Positive Numbers

-- Todo numero positivo corresponde a um numero natural. A funcao Pos.toNat
-- que foi definida anteriormente converte um Pos para o Nat correspondente:

#check Pos.toNat

-- A funcao List.drop, remove um prefixo de uma lista. Aplicar List.drop a um
-- Pos, porem, leva a um erro de tipo:

#check [1, 2, 3, 4].drop (2 : Pos)

-- Como o autor de List.drop nao a tornou um metodo de uma type class, ela
-- nao pode ser overridden definindo uma nova instance.

-- A type class `Coe` descreve maneiras overloaded de fazer coercion de um
-- tipo para outro:

#check Coe

-- Uma instance de `Coe Pos Nat` eh suficiente para permitir que o codigo
-- anterior funcione:

instance : Coe Pos Nat where
  coe x := x.toNat

-- Usar #check mostra o resultado da busca de instance que foi usada.
#eval [1, 2, 3, 4].drop (2 : Pos)
#check [1, 2, 3, 4].drop (2 : Pos)

-- A instance eh definida apenas para `Pos → Nat` nao `Nat → Pos`.
#check ((1 : Pos) : Nat)
#check ((1 : Nat) : Pos)


-- 3.6.3. Chaining Coercions

-- Ao buscar por coercions, Lean tentara montar uma coercion a partir de uma
-- cadeia de coercions menores. Por exemplo, ja existe uma coercion de Nat para Int.
-- Por causa dessa instance, combinada com a instance `Coe Pos Nat`, o seguinte
-- codigo eh aceito:

def oneInt : Int := Pos.one

-- Essa definicao usa duas coercions: de Pos para Nat, e entao de Nat para Int.

-- O compilador do Lean nao trava na presenca de coercions circulares. Por exemplo,
-- mesmo se dois tipos A e B podem ser coercidos um para o outro, suas coercions
-- mutuas podem ser usadas para encontrar um caminho:

inductive A where
  | a

inductive B where
  | b

instance : Coe A B where
  coe _ := B.b

instance : Coe B A where
  coe _ := A.a

instance : Coe Unit A where
  coe _ := A.a

def coercedToB : B := ()

-- Lembrete: os parenteses duplos () eh abreviacao para o construtor Unit.unit.
-- Apos derivar uma instance Repr B:
deriving instance Repr for B

#eval coercedToB
#eval (coercedToB : A)

-- A biblioteca padrao do Lean define uma coercion de qualquer tipo α para
-- Option α que envolve o valor em some. Isso permite que o some seja omitido.

-- Por exemplo, a funcao List.last? que encontra a ultima entrada em uma lista
-- pode ser escrita sem um some ao redor do valor de retorno x:

def List.last? : List α → Option α
  | [] => none
  | [x] => x
  | _ :: x :: xs => last? (x :: xs)

-- A busca de instance encontra a coercion, e insere uma chamada para coe, que
-- envolve o argumento em some. Essas coercions podem ser encadeadas, de forma
-- que usos aninhados de Option nao requerem construtores some aninhados:

def perhapsPerhapsPerhaps : Option (Option (Option String)) :=
  "Please don't tell me"

-- Coercions so sao ativadas automaticamente quando Lean encontra uma incompatibilidade
-- entre um tipo inferido e um tipo que eh imposto pelo resto do programa. Em casos
-- com outros erros, coercions nao sao ativadas.

-- Por exemplo, se o erro eh que uma instance esta faltando, coercions nao serao usadas:

def perhapsPerhapsPerhapsNat : Option (Option (Option Nat)) :=
  392

-- Numerais sao polimorficos em Lean, mas o numeral `392` nao pode ser usado em
-- um contexto onde o tipo esperado eh Option (Option (Option Nat)) devido a
-- ausencia da instance OfNat correspondente.

-- Isso pode ser contornado indicando manualmente o tipo desejado para ser usado
-- com OfNat:

def perhapsPerhapsPerhapsNat' : Option (Option (Option Nat)) :=
  (392 : Nat)

-- Adicionalmente, coercions podem ser inseridas manualmente usando uma seta
-- para cima (↑):

def perhapsPerhapsPerhapsNat'' : Option (Option (Option Nat)) :=
  ↑(392 : Nat)

-- Em alguns casos, isso pode ser usado para garantir que Lean encontre as
-- instances corretas. Tambem pode tornar as intencoes do programador mais claras.


-- 3.6.4. Non-Empty Lists and Dependent Coercions

-- Uma instance de `Coe α β` faz sentido quando o tipo β tem um valor que pode
-- representar cada valor do tipo α. Fazer coercion de Nat para Int faz sentido,
-- porque o tipo Int contem todos os numeros naturais, mas uma coercion de Int
-- para Nat eh uma ma ideia porque Nat nao contem os numeros negativos.

-- Similarmente, uma coercion de listas nao-vazias para listas comuns faz sentido
-- porque o tipo List pode representar toda lista nao-vazia:

instance : Coe (NonEmptyList α) (List α) where
  coe xs := xs.head :: xs.tail

-- Isso permite que listas nao-vazias sejam usadas com toda a API de List.

-- Por outro lado, eh impossivel escrever uma instance de `Coe (List α) (NonEmptyList α)`,
-- porque nao existe uma lista nao-vazia que possa representar a lista vazia.
-- Essa limitacao pode ser contornada usando outra versao de coercions, chamadas
-- de _dependent coercions_.

-- Dependent coercions podem ser usadas quando a habilidade de fazer coercion
-- de um tipo para outro depende de qual valor particular esta sendo convertido.
-- Assim como a type class OfNat recebe o Nat particular sendo overloaded como
-- parametro, dependent coercion recebe o valor sendo convertido como parametro:

#check CoeDep

-- Por exemplo, qualquer List que tem pelo menos um valor (`x :: xs`) pode
-- ser convertida para uma NonEmptyList:

instance : CoeDep (List α) (x :: xs) (NonEmptyList α) where
  coe := { head := x, tail := xs }

-- A implementacao/contrutor do tipo (`x :: xs` nesse caso) que tem coercion
-- valida eh passado como argumento para a instance, assim, a instance funciona
-- apenas para o caso valido (e nao para `nil`).

def validCoercion : NonEmptyList Nat := [1, 2, 3]
def invalidCoercion : NonEmptyList Nat := []


-- 3.6.5. Coercing to Types

-- Em matematica, eh comum ter um conceito que consiste de um conjunto equipado
-- com estrutura adicional. Por exemplo, um monoide eh algum conjunto S, um
-- elemento s de S, e um operador binario associativo em S, tal que s eh neutro
-- a esquerda e a direita do operador. S eh referido como o "carrier set"
-- (conjunto subjacente) do monoide.

-- Os numeros naturais com zero e adicao formam um monoide.

-- Monoides tambem sao amplamente usados em programacao funcional: listas, a
-- lista vazia, e o operador append formam um monoide, assim como strings, a
-- string vazia, e string append:

structure Monoid where
  Carrier : Type
  neutral : Carrier
  op : Carrier → Carrier → Carrier

def natMulMonoid : Monoid :=
  { Carrier := Nat, neutral := 1, op := (· * ·) }

def natAddMonoid : Monoid :=
  { Carrier := Nat, neutral := 0, op := (· + ·) }

def stringMonoid : Monoid :=
  { Carrier := String, neutral := "", op := String.append }

def listMonoid (α : Type) : Monoid :=
  { Carrier := List α, neutral := [], op := List.append }

-- Dado um monoide, eh possivel escrever a funcao foldMap que, em uma unica
-- passagem, transforma as entradas em uma lista para o carrier set do monoide
-- e entao as combina usando o operador do monoide.

-- Como monoides tem um elemento neutro, ha um resultado natural para retornar
-- quando a lista esta vazia, e como o operador eh associativo, clientes da
-- funcao nao precisam se preocupar se a funcao recursiva combina elementos da
-- esquerda para direita ou da direita para esquerda.

def foldMap (M : Monoid) (f : α → M.Carrier) (xs : List α) : M.Carrier :=
  let rec go (soFar : M.Carrier) : List α → M.Carrier
    | [] => soFar
    | y :: ys => go (M.op soFar (f y)) ys
  go M.neutral xs

def words := ["hello", "world", "!"]

#eval foldMap natAddMonoid String.length words
-- = String.length "hello" + String.length "world" + String.length "!"
-- = 5 + 5 + 1

-- Embora um monoide consista de tres pecas separadas de informacao, eh comum
-- apenas referir ao nome do monoide para referir ao seu conjunto. Ao inves de
-- dizer "Seja A um monoide e sejam x e y elementos de seu carrier set", eh
-- comum dizer "Seja A um monoide e sejam x e y elementos de A".

-- Essa pratica pode ser codificada em Lean definindo um novo tipo de coercion,
-- do monoide para seu carrier set.

-- A classe `CoeSort` eh como a classe Coe, com a excecao de que o alvo da
-- coercion deve ser um _sort_, ou seja, Type ou Prop.

-- O termo "sort" em Lean refere-se a esses tipos que classificam outros tipos:
-- - Type classifica tipos que eles mesmos classificam dados
-- - Prop classifica proposicoes que elas mesmas classificam evidencia de sua verdade

-- Assim como Coe eh checado quando um type mismatch ocorre, CoeSort eh usado
-- quando algo diferente de um sort eh fornecido em um contexto onde um sort
-- seria esperado.

#check CoeSort

-- A coercion de um monoide para seu carrier set extrai o carrier:

instance : CoeSort Monoid Type where
  coe m := m.Carrier

-- Com essa coercion, as assinaturas de tipo se tornam menos burocraticas:

def foldMap' (M : Monoid) (f : α → M) (xs : List α) : M :=
  let rec go (soFar : M) : List α → M
    | [] => soFar
    | y :: ys => go (M.op soFar (f y)) ys
  go M.neutral xs

-- Outro exemplo util de CoeSort eh usado para preencher a lacuna entre Bool e Prop.
-- Como discutido na secao sobre ordenacao e igualdade, a expressao `if` do Lean
-- espera que a condicao seja uma proposicao decidivel ao inves de um Bool.

-- Programas tipicamente precisam poder fazer branch baseado em valores Boolean.
-- Ao inves de ter dois tipos de expressao if, a biblioteca padrao do Lean define
-- uma coercion de Bool para a proposicao de que o Bool em questao eh igual a true:

instance : CoeSort Bool Prop where
  coe b := b = true

-- Neste caso, o sort em questao eh Prop ao inves de Type.


-- 3.6.6. Coercing to Functions

-- Muitos datatypes que ocorrem regularmente em programacao consistem de uma
-- funcao junto com alguma informacao extra sobre ela. Por exemplo, uma funcao
-- pode ser acompanhada por um nome para mostrar em logs ou por alguns dados
-- de configuracao.

-- Adicionalmente, colocar um tipo em um campo de uma structure, similar ao
-- exemplo do Monoid, pode fazer sentido em contextos onde ha mais de uma
-- maneira de implementar uma operacao e mais controle manual eh necessario
-- do que type classes permitiriam.

-- Por exemplo, os detalhes especificos de valores emitidos por um serializador
-- JSON podem ser importantes porque outra aplicacao espera um formato particular.
-- As vezes, a funcao em si pode ser derivavel apenas dos dados de configuracao.

-- A type class `CoeFun` pode transformar valores de tipos nao-funcao para tipos
-- funcao. CoeFun tem dois parametros:
-- - o primeiro eh o tipo cujos valores devem ser transformados em funcoes
-- - o segundo eh um parametro de saida que determina exatamente qual tipo de
--   funcao esta sendo alvo.

#check CoeFun

-- O segundo parametro eh ele mesmo uma funcao que computa um tipo. Em Lean,
-- tipos sao first-class e podem ser passados para funcoes ou retornados delas,
-- assim como qualquer outra coisa.

-- Por exemplo, uma funcao que adiciona uma quantidade constante ao seu argumento
-- pode ser representada como um wrapper ao redor da quantidade a adicionar, ao
-- inves de definir uma funcao real:

structure Adder where
  howMuch : Nat

-- Uma funcao que adiciona cinco ao seu argumento tem um 5 no campo howMuch:

def add5 : Adder := ⟨5⟩

-- Esse tipo Adder nao eh uma funcao, e aplica-lo a um argumento resulta em erro:

#check add5 3

-- Definir uma instance CoeFun faz o Lean transformar o adder em uma funcao
-- com tipo `Nat → Nat`:

instance : CoeFun Adder (fun _ => Nat → Nat) where
  coe a := (· + a.howMuch)

#eval add5 3

-- Como todos os Adders devem ser transformados em funcoes `Nat → Nat`, o
-- argumento para o segundo parametro de CoeFun foi ignorado.

-- Quando o valor em si eh necessario para determinar o tipo de funcao correto,
-- entao o segundo parametro de CoeFun nao eh mais ignorado.

-- Por exemplo, dada a seguinte representacao de valores JSON:

inductive JSON where
  | true : JSON
  | false : JSON
  | null : JSON
  | string : String → JSON
  | number : Float → JSON
  | object : List (String × JSON) → JSON
  | array : List JSON → JSON

-- Um serializador JSON eh uma structure que rastreia o tipo que ele sabe como
-- serializar junto com o codigo de serializacao em si:

structure Serializer where
  Contents : Type
  serialize : Contents → JSON

-- Um serializador para strings apenas precisa envolver a string fornecida no
-- construtor JSON.string:

def Str : Serializer :=
  { Contents := String,
    serialize := JSON.string
  }

-- Ver serializadores JSON como funcoes que serializam seu argumento requer
-- extrair o tipo interno de dados serializaveis:

instance : CoeFun Serializer (fun s => s.Contents → JSON) where
  coe s := s.serialize

-- Dada essa instance, um serializador pode ser aplicado diretamente a um argumento:

def buildResponse (title : String) (R : Serializer) (record : R.Contents) : JSON :=
  JSON.object [
    ("title", JSON.string title),
    ("status", JSON.number 200),
    ("record", R record)
  ]

-- O serializador pode ser passado diretamente para buildResponse:

#eval buildResponse "Functional Programming in Lean" Str "Programming is fun!"


/- 3.6.6.1. Aside: JSON as a String -/

-- Pode ser um pouco dificil de entender JSON quando codificado como objetos Lean.
-- Para ajudar a garantir que a resposta serializada foi o que era esperado, pode
-- ser conveniente escrever um conversor simples de JSON para String.

-- O primeiro passo eh simplificar a exibicao de numeros. JSON nao distingue entre
-- inteiros e numeros de ponto flutuante, e o tipo Float eh usado para representar
-- ambos. Em Lean, Float.toString inclui varios zeros trailing:

#eval (5 : Float).toString  -- "5.000000"

-- A solucao eh escrever uma pequena funcao que limpa a apresentacao removendo
-- todos os zeros trailing, seguidos por um ponto decimal trailing:

def dropDecimals (numString : String) : String :=
  if numString.contains '.' then
    let noTrailingZeros := numString.dropRightWhile (· == '0')
    noTrailingZeros.dropRightWhile (· == '.')
  else numString

#eval dropDecimals (5 : Float).toString
#eval dropDecimals (5.2 : Float).toString

-- O proximo passo eh definir uma funcao helper para fazer append de uma lista de
-- strings com um separador entre elas:

def String.separate (sep : String) (strings : List String) : String :=
  match strings with
  | [] => ""
  | x :: xs => String.join (x :: xs.map (sep ++ ·))

-- Esta funcao eh util para lidar com elementos separados por virgula em arrays
-- e objetos JSON.
#eval ", ".separate ["1", "2"]
#eval ", ".separate ["1"]
#eval ", ".separate []

-- Na biblioteca padrao do Lean, essa funcao eh chamada:
#check String.intercalate

-- Finalmente, um procedimento de escape de string eh necessario para strings JSON,
-- de forma que a string Lean contendo "Hello!" possa ser exibida como "\"Hello!\"".
-- Felizmente, o compilador Lean contem uma funcao interna para fazer escape de
-- strings JSON, chamada Lean.Json.escape. Para acessar essa funcao, adicione
-- `import Lean` no inicio do arquivo.

-- A funcao que emite uma string de um valor JSON eh declarada partial porque Lean
-- nao consegue ver que ela termina. Isso ocorre porque chamadas recursivas para
-- asString ocorrem em funcoes que estao sendo aplicadas por List.map, e esse
-- padrao de recursao eh complicado o suficiente que Lean nao consegue ver que
-- as chamadas recursivas estao realmente sendo feitas em valores menores.

-- Em uma aplicacao que apenas precisa produzir strings JSON e nao precisa raciocinar
-- matematicamente sobre o processo, ter a funcao sendo partial provavelmente nao
-- causara problemas.

#check Lean.Json.escape

def JSON.asString (val : JSON) : String :=
  match val with
  | true => "true"
  | false => "false"
  | null => "null"
  | string s => "\"" ++ Lean.Json.escape s ++ "\""
  | number n => dropDecimals n.toString
  | object members =>
    let memberToString mem :=
      "\"" ++ Lean.Json.escape mem.fst ++ "\": " ++ asString mem.snd
    "{" ++ ", ".separate (members.map memberToString) ++ "}"
  | array elements =>
    "[" ++ ", ".separate (elements.map asString) ++ "]"

-- Com essa definicao, a saida da serializacao eh mais facil de ler:

#eval (buildResponse "Functional Programming in Lean" Str "Programming is fun!").asString


-- 3.6.7. Messages You May Meet

-- Literais de numeros naturais sao overloaded com a type class OfNat. Como
-- coercions disparam em casos onde tipos nao correspondem, ao inves de em
-- casos de instances faltando, uma instance OfNat faltando para um tipo nao
-- causa uma coercion de Nat para ser aplicada:

def perhapsPerhapsPerhapsNat₁ : Option (Option (Option Nat)) :=
  392

-- A coercion nao eh aplicada porque o problema nao eh um type mismatch (o
-- numeral `392` eh polimorfico e pode ter qualquer tipo), mas sim a ausencia
-- de uma instance OfNat para `Option (Option (Option Nat))`.

-- Para resolver, eh necessario indicar explicitamente o tipo:
def perhapsPerhapsPerhapsNat₂ : Option (Option (Option Nat)) :=
  (392 : Nat)


-- 3.6.8. Design Considerations

-- Coercions sao uma ferramenta poderosa que deve ser usada com responsabilidade.
-- Por um lado, elas pertmitem que uma API siga naturalmente as regras do dominio
-- sendo modelado. Isso pode ser a diferenca entre uma bagunca burocratica de funcoes
-- de conversao manuais e um programa claro.

-- "Programas devem ser escritos para pessoas lerem, e apenas incidentalmente
-- para maquinas executarem."

-- APIs que dependem muito de coercions tem algumas limitacoes. Pense cuidadosamente
-- sobre essas limitacoes antes de usar coercions em suas proprias bibliotecas.

-- 1. Coercions sao aplicadas apenas em contextos onde existe informacao de tipo
-- suficiente para o Lean conhecer todos os tipos envolvidos, porque nao ha
-- parametros de saida nas type classes de coercion.

-- Isso significa que uma anotacao de tipo de retorno em uma funcao pode ser a
-- diferenca entre um erro de tipo e uma coercion aplicada com sucesso.

-- Por exemplo, a coercion de listas nao-vazias para listas faz o seguinte
-- programa funcionar:

def lastSpider : Option String :=
  List.getLast? idahoSpiders

-- Por outro lado, se a anotacao de tipo for omitida, entao o tipo de resultado
-- eh desconhecido, entao o Lean nao consegue encontrar a coercion:

def lastSpider₁ :=
  List.getLast? idahoSpiders

-- 2. Quando uma coercion nao eh aplicada por alguma razao, o usuario
-- recebe o erro de tipo original, o que pode tornar dificil debugar
-- cadeias de coercions.

-- 3. Finalmente, coercions nao sao aplicadas no contexto de `dot notation`.
-- Isso significa que ainda existe uma diferenca importante entre expressoes que
-- precisam ser coercidas e aquelas que nao precisam, e essa diferenca eh
-- visivel para usuarios da sua API.
-- _`x.foo` nao trigga coercions_


-- 3.7. Additional Conveniences


-- 3.7.1. Constructor Syntax for Instances

-- Nos bastidores, type classes sao tipos structure e instances sao valores
-- desses tipos. As unicas diferencas sao que o Lean armazena informacao adicional
-- sobre type classes, como quais parametros sao parametros de saida, e que
-- instances sao registradas para busca.

-- Enquanto valores que tem tipos structure tipicamente sao definidos/criados
-- usando os `⟨...⟩` ou com `{ }` e campos, e instances tipicamente sao definidas
-- usando `where`, ambas as sintaxes funcionam para ambos os tipos de definicao.

-- Por exemplo, uma aplicacao de silvicultura pode representar arvores da
-- seguinte forma:

structure Tree : Type where
  latinName : String
  commonNames : List String

def oak : Tree :=
  ⟨"Quercus robur", ["common oak", "European oak"]⟩

def birch : Tree :=
  { latinName := "Betula pendula",
    commonNames := ["silver birch", "warty birch"]
  }

def sloe : Tree where
  latinName := "Prunus spinosa"
  commonNames := ["sloe", "blackthorn"]

-- Todas as tres sintaxes sao equivalentes.

-- Similarmente, instances de type class podem ser definidas usando todas as
-- tres sintaxes:

class Display (α : Type) where
  displayName : α → String

instance : Display Tree :=
  ⟨Tree.latinName⟩

instance : Display Tree :=
  { displayName := Tree.latinName }

instance : Display Tree where
  displayName t := t.latinName

-- A sintaxe `where` eh tipicamente usada para instances, enquanto structures
-- usam ou a sintaxe com chaves ou a sintaxe `where`.

-- A sintaxe `⟨...⟩` pode ser util quando se enfatiza que um tipo structure eh
-- muito parecido com uma tupla na qual os campos acontecem de ter nomes, mas
-- os nomes nao sao importantes no momento.

-- Porem, ha situacoes onde pode fazer sentido usar outras alternativas. Em
-- particular, uma biblioteca pode fornecer uma funcao que constroi um valor
-- de instance. Colocar uma chamada para essa funcao depois de `:=` em uma
-- declaracao de instance eh a maneira mais facil de usar tal funcao:

-- Exemplo de funcao para instance:

def makeDisplayFromToString [ToString α] : Display α :=
  { displayName := toString }

-- Usando a funcao diretamente:
instance : Display Nat := makeDisplayFromToString

-- Ao inves de escrever manualmente
instance : Display Nat where
  displayName := toString


-- 3.7.2. Examples

-- Ao experimentar com codigo Lean, definicoes podem ser mais convenientes de
-- usar do que comandos #eval ou #check.

-- 1. Definicoes nao produzem output, o que pode ajudar a manter o foco do
-- leitor no output mais interessante.

-- 2. Eh mais facil escrever a maioria dos programas Lean comecando com uma
-- assinatura de tipo, permitindo que o Lean forneca mais assistencia e
-- melhores mensagens de erro enquanto escreve o programa em si.
-- Por outro lado, #eval e #check sao mais faceis de usar em contextos
-- onde o Lean consegue determinar o tipo da expressao fornecida.

-- 3. #eval nao pode ser usado com expressoes cujos tipos nao tem instances
-- ToString ou Repr, como funcoes.

-- 4. Blocos `do` multi-step, let-expressions, e outras formas sintaticas que
-- ocupam multiplas linhas sao particularmente dificeis de escrever com
-- uma anotacao de tipo em #eval ou #check, simplesmente porque a
-- parentetizacao necessaria pode ser dificil de prever.

-- Para contornar esses problemas, Lean suporta a indicacao explicita de
-- _examples_ em um arquivo fonte. Um example eh como uma definicao sem nome.`

example : NonEmptyList String :=
  { head := "Sparrow",
    tail := ["Duck", "Swan", "Magpie", "Eurasian coot", "Crow"]
  }

-- Examples podem definir funcoes aceitando argumentos:

example (n : Nat) (k : Nat) : Bool :=
  n + k == k + n

-- Embora isso crie uma funcao nos bastidores, essa funcao nao tem nome e
-- nao pode ser chamada. Ainda assim, isso eh util para demonstrar como uma
-- biblioteca pode ser usada com valores arbitrarios ou desconhecidos de
-- algum tipo dado.

-- Em source files, declaracoes example sao melhor acompanhadas de comentarios
-- que explicam como o example ilustra os conceitos da biblioteca.


-- 3.8. Summary


-- 3.8.1. Type Classes and Overloading

-- Type classes sao o mecanismo do Lean para fazer overload de funcoes e operadores.
-- Uma funcao polimorfica pode ser usada com multiplos tipos e se comporta da mesma
-- maneira independente de qual tipo eh usado.

-- Uma operacao que eh overloaded com type classes, por outro lado, tambem pode
-- ser usada com multiplos tipos. Porem, cada tipo requer sua propria implementacao
-- da operacao overloaded. Ou seja, o comportamento pode variar baseado em qual
-- tipo eh fornecido.

-- Uma type class tem um nome, parametros e um corpo:
-- - Nome: uma maneira de se referir as operacoes overloaded
-- - Parametros: determinam quais aspectos das definicoes podem ser overloaded
-- - Corpo: fornece os nomes e assinaturas de tipo das operacoes overloadable

-- Cada operacao overloadable eh chamada de _metodo_ da type class.

-- Type classes podem fornecer implementacoes default de alguns metodos em termos
-- de outros.

-- Uma _instance_ de uma type class fornece implementacoes dos metodos para
-- parametros dados. Instances podem ser polimorficas, caso em que podem funcionar
-- para uma variedade de parametros, e podem opcionalmente fornecer implementacoes
-- mais especificas de metodos default em casos onde uma versao mais eficiente
-- existe para algum tipo particular.

-- Parametros de type class sao ou:
-- - Input parameters - default.
-- - Output parameters - anotados por `outParam`.

-- Lean nao comecara a buscar por uma instance ate que todos os parametros de
-- entrada nao sejam mais metavariables, enquanto parametros de saida podem ser
-- resolvidos durante a busca por instances.

-- Parametros para uma type class nao precisam ser tipos - eles tambem podem ser
-- valores comuns. A type class OfNat, usada para fazer overload de literais de
-- numeros naturais, recebe o Nat overloaded como um parametro, o que permite
-- que instances restrinjam os numeros permitidos.

-- Instances podem ser marcadas com um atributo `@[default_instance]`. Quando
-- uma instance eh uma default instance, entao ela sera escolhida como fallback
-- para quando o Lean de outra forma falharia em encontrar uma instance devido a
-- presenca de metavariables.


-- 3.8.2. Type Classes for Common Syntax

-- A maioria dos operadores infix no Lean sao overridden com uma type class.
-- Por exemplo, o operador de adicao (`+`) corresponde a type class chamada `Add`.

-- A maioria desses operadores tem uma versao heterogenea correspondente, na qual
-- os dois argumentos nao precisam ter o mesmo tipo.

#check Add.add
#check HAdd.hAdd

-- A sintaxe de indexacao eh overloaded usando uma type class chamada `GetElem`,
-- que envolve provas. GetElem tem dois parametros de saida:
-- 1. O tipo dos elementos a serem extraidos da colecao
-- 2. Uma funcao que pode ser usada para determinar o que conta como evidencia
--    de que o valor do indice esta dentro dos limites (in bounds) da colecao

#check GetElem

-- Essa evidencia eh descrita por uma proposicao, e o Lean tenta provar essa
-- proposicao quando indexacao de array eh usada.

-- Quando o Lean nao consegue checar que operacoes de acesso a lista ou array
-- estao in bounds em compile time, a checagem pode ser adiada para run time
-- adicionando um `?` a sintaxe de indexacao.

example : List Nat := [1, 2, 3, 4]

-- Compile time check (Lean prova que o indice esta in bounds):
#eval [1, 2, 3, 4][2]

-- Run time check (retorna Option, checagem adiada):
#eval [1, 2, 3, 4][2]?
#eval [1, 2, 3, 4][10]?


-- 3.8.3. Functors

-- Um functor eh um tipo polimorfico que suporta uma operacao de mapeamento.
-- Essa operacao de mapeamento transforma todos os elementos sem alterar a estrutura.
-- Por exemplo, listas sao functors e a operacao de mapeamento nao pode dropar,
-- duplicar, nem misturar entradas na lista.

-- Enquanto functors sao definidos por ter `map` (`<$>`), a type class Functor no Lean
-- contem um metodo default adicional que eh responsavel por mapear a funcao
-- constante sobre um valor, substituindo todos os valores cujo tipo eh dado
-- por uma variavel de tipo polimorfica com o mesmo novo valor.

#check Functor

#eval Functor.mapConst 42 [1, 2, 3, 4]
#eval Functor.mapConst "x" (some "hello")


-- 3.8.4. Deriving Instances

-- Muitas type classes tem implementacoes muito padrao. Por exemplo, a classe de
-- igualdade booleana BEq geralmente eh implementada primeiro checando se ambos
-- os argumentos sao construidos com o mesmo construtor, e entao checando se
-- todos os seus argumentos sao iguais. Instances para essas classes podem ser
-- criadas automaticamente.

-- Ao definir um tipo indutivo ou uma structure, uma clausula `deriving` no final
-- da declaracao fara com que instances sejam criadas automaticamente.

inductive Cor where
  | red
  | green
  | blue
  deriving Repr, BEq, Hashable

#eval Cor.red
#eval Cor.red == Cor.blue
#eval hash Cor.green

-- Adicionalmente, o comando `deriving instance ... for ...` pode ser usado fora
-- da definicao de um datatype para fazer com que uma instance seja gerada.

deriving instance BEq, Hashable for Pos
deriving instance BEq, Hashable for NonEmptyList

-- Como cada classe para a qual instances podem ser derivadas requer tratamento
-- especial, nem todas as classes sao derivaveis.


-- 3.8.5. Coercions

-- Coercions permitem ao Lean se "recuperar" do que normalmente seria um erro de
-- tipo incorreto inserindo uma chamada para uma funcao que transforma dados de
-- um tipo para outro.

-- Por exemplo, a coercion de qualquer tipo α para o tipo Option α permite que
-- valores sejam escritos diretamente, ao inves de com o construtor some, fazendo
-- Option funcionar mais como tipos nullable de linguagens orientadas a objetos.

-- Ha multiplos tipos de coercion. Eles podem se recuperar de diferentes tipos
-- de erros, e sao representados por suas proprias type classes:

-- 1. COE: Quando Lean tem uma expressao de tipo α em um contexto que espera algo
-- com tipo β, Lean primeiro tenta encadear uma cadeia de coercions que podem
-- transformar αs em βs, e apenas exibe o erro quando isso nao pode ser feito.

#check Coe

-- Permite transformar `Pos` em `Nat`:
instance : Coe Pos Nat where
  coe x := x.toNat


-- 2. COEDEP: Coercion dependente do valor:
-- A classe CoeDep recebe o valor especifico do tipo sendo coercido como um
--  parametro extra, permitindo que:
-- - Busca de type class adicional seja feita no valor, ou
-- - Construtores sejam usados na instance para limitar o escopo da conversao

#check CoeDep

-- Coercao de List para uma NonEmptyList soh faz sentido se a List tiver elementos
-- `(x :: xs)` e nao para a lista vazia (`[]`):
instance : CoeDep (List α) (x :: xs) (NonEmptyList α) where
  coe := { head := x, tail := xs }

def validCoercion' : NonEmptyList Nat := [1, 2, 3]
def invalidCoercion' : NonEmptyList Nat := []

-- 3. COESORT: Coercion para sorts (Type ou Prop)
-- Usado quando algo diferente de um sort eh fornecido em um contexto onde um
-- sort seria esperado.

#check CoeSort

instance : CoeSort Monoid Type where
  coe m := m.Carrier

-- Permite usar M ao inves de M.Carrier:
def foldMap'' (M : Monoid) (f : α → M) (xs : List α) : M :=
  let rec go (soFar : M) : List α → M
    | [] => soFar
    | y :: ys => go (M.op soFar (f y)) ys
  go M.neutral xs

-- 4. COEFUN: Coercion para funcoes
-- A classe CoeFun intercepta o que seria um erro "not a function" ao compilar
-- uma aplicacao de funcao, e permite que o valor na posicao de funcao seja
-- transformado em uma funcao real se possivel.

#check CoeFun

instance : CoeFun Adder (fun _ => Nat → Nat) where
  coe a := (· + a.howMuch)

#eval add5 3

-- Exemplo com tipo dependente do valor:
instance : CoeFun Serializer (fun s => s.Contents → JSON) where
  coe s := s.serialize

-- O serializador pode ser aplicado diretamente:
def response := buildResponse "Title" Str "content"
