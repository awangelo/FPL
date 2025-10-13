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
-- mas na declaração do metodo, ofNat tem apenas tipo α.

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
