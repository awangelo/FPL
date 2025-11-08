-- 4. Monads

-- Em linguagens como C# e Kotlin, o operador `?.` permite encadear operacoes em
-- valores potencialmente nulos, onde o primeiro null interrompe a cadeia. Isso
-- eh mais conveniente que ifs aninhados. Exceptions sao mais convenientes que
-- verificar manualmente codigos de erro. Verificacoes de null encadeadas e
-- exceptions requerem que designers de linguagens antecipem esses casos de uso,
-- enquanto frameworks de loggin utilizam efeitos colaterais para desacoplar o
-- codigo que registra logs.


-- 4.1. One API, Many Applications

-- Todos esses recursos e mais podem ser implementados em codigo como instancias
-- de uma API comum chamada Monad. Lean fornece sintaxe dedicada que torna essa
-- API conveniente de usar, mas tambem pode atrapalhar o entendimento do que
-- esta acontecendo por tras das cenas. Este capitulo comeca com a apresentacao
-- detalhada de verificacoes de null aninhadas manualmente e constroi a partir
-- dai ate a API conveniente e geral.


-- 4.1.1. Checking for none: Don't Repeat Yourself

-- Em Lean, pattern matching pde ser usado para encadear verificacoes de null.
-- Obter a primeira entrada de uma lista pode usar notacao de indexacao opcional:

def first (xs : List α) : Option α :=
  xs[0]?

-- O resultado deve ser Option porque listas vazias nao tem primeira entrada.
-- Extrair primeira e terceira entradas requer verificar que cada uma nao eh none:

def firstThird (xs : List α) : Option (α × α) :=
  match xs[0]? with
  | none => none
  | some first =>
    match xs[2]? with
    | none => none
    | some third =>
      some (first, third)

-- Extrair primeira, terceira e quinta entradas requer mais verificacoes:

def firstThirdFifth (xs : List α) : Option (α × α × α) :=
  match xs[0]? with
  | none => none
  | some first =>
    match xs[2]? with
    | none => none
    | some third =>
      match xs[4]? with
      | none => none
      | some fifth =>
        some (first, third, fifth)

-- Ja deu pra entender...

-- O problema fundamental eh que o codigo aborda duas preocupacoes: extrair os
-- numeros e verificar que todos estao presentes. A segunda preocupacao eh
-- abordada copiando e colando o codigo que lida com o caso none. Eh bom estilo
-- elevar um segmento repetitivo em uma funcao auxiliar:

def andThen (opt : Option α) (next : α → Option β) : Option β :=
  match opt with
  | none => none
  | some x => next x

-- Este auxiliar, usado similarmente a ?. em C# e Kotlin, cuida de propagar
-- valores none. Recebe dois argumentos: um valor opcional e uma funcao para
-- aplicar quando o valor nao eh none. Se o primeiro argumento eh none, retorna
-- none. Se nao eh none, a funcao eh aplicada ao conteudo do construtor some.

-- Agora firstThird pode ser reescrito usando andThen em vez de pattern matching:

def firstThird' (xs : List α) : Option (α × α) :=
  andThen xs[0]? fun first =>
    andThen xs[2]? fun third =>
      some (first, third)

-- Em Lean, funcoes nao precisam estar entre parenteses quando passadas como
-- argumentos. Definicao equivalente com mais parenteses:

def firstThird'' (xs : List α) : Option (α × α) :=
  andThen xs[0]? (fun first =>
    andThen xs[2]? (fun third =>
      some (first, third)))

-- O auxiliar andThen fornece uma especie de pipeline atraves do qual valores
-- fluem. Melhorar a sintaxe usada para escrever andThen pode tornar essas
-- computacoes ainda mais faceis de entender.


/- 4.1.1.1. Infix Operators -/

-- Em Lean, operadores infix podem ser declarados usando comandos `infix`,
-- `infixl` e `infixr`, que criam operadores (respectivamente) nao-associativos,
-- associativos a esquerda e associativos a direita. Quando usado multiplas
-- vezes seguidas, um operador associativo a esquerda empilha os parenteses de
-- abertura no lado esquerdo da expressao. O operador de adicao eh associativo a
-- esquerda, entao "w + x + y + z" eh equivalente a "(((w + x) + y) + z)". O
-- operador de exponenciacao eh associativo a direita, entao "w ^ x ^ y ^ z" eh
-- equivalente a "w ^ (x ^ (y ^ z))". Operadores de comparacao como `<` sao nao
-- associativos, entao x < y < z eh erro de sintaxe e requer parenteses manuais.

-- A seguinte declaracao torna andThen um operador infix:

infixl:55 " ~~> " => andThen

-- O numero seguindo os dois pontos declara a precedencia do novo operador infix.
-- Em notacao matematica comum, "x + y * z" eh equivalente a "x + (y * z)" embora
-- ambos `+` e `*` sejam associativos a esquerda. Em Lean, `+` tem precedencia 65
-- e `*` tem precedencia 70. Operadores de maior precedencia sao aplicados antes
-- dos menores. De acordo com a declaracao de `~~>`, ambos `+` e `*` tem maior
-- precedencia e assim se aplicam primeiro.

-- Depois do novo operador infix esta uma seta dupla `=>`, que especifica a
-- funcao nomeada a ser usada para o operador infix. A biblioteca padrao de Lean
-- usa esse recurso para definir + e * como operadores infix que apontam para
-- HAdd.hAdd e HMul.hMul, permitindo que type classes sejam usadas para
-- sobrecarregar os operadores infix. Aqui, andThen eh apenas uma funcao comum.

-- Tendo definido um operador infix para andThen, firstThird pode ser reescrito
-- de uma forma que traz o sentimento de pipeline:

def firstThirdInfix (xs : List α) : Option (α × α) :=
  xs[0]? ~~> fun first =>
  xs[2]? ~~> fun third =>
  some (first, third)

-- Este estilo eh muito mais conciso ao escrever funcoes maiores:

def firstThirdFifthSeventh (xs : List α) : Option (α × α × α × α) :=
  xs[0]? ~~> fun first =>
  xs[2]? ~~> fun third =>
  xs[4]? ~~> fun fifth =>
  xs[6]? ~~> fun seventh =>
  some (first, third, fifth, seventh)


-- 4.1.2. Propagating Error Messages

-- Linguagens funcionais puras como Lean nao tem mecanismo de exception
-- embutido para tratamento de erros, porque lancar ou capturar uma exception
-- esta fora do modelo de avaliacao passo a passo para expressoes. Porem,
-- programas funcionais certamente precisam lidar com erros.

-- Isso eh tipicamente realizado definindo um datatype que pode ser um erro ou
-- um resultado, e traduzindo funcoes com exceptions em funcoes que retornam
-- este datatype:

#check Except

-- A variavel de tipo ε representa o tipo de erros que podem ser produzidos
-- pela funcao. Callers sao esperados lidar tanto com erros quanto sucessos.

-- Similar a Option, Except pode ser usado para indicar falha ao encontrar uma
-- entrada em uma lista. Neste caso, o tipo de erro eh String:

def get (xs : List α) (i : Nat) : Except String α :=
  match xs[i]? with
  | none => Except.error s!"Index {i} not found (maximum is {xs.length - 1})"
  | some x => Except.ok x

def ediblePlants : List String :=
  ["ramsons", "sea plantain", "sea buckthorn", "garden nasturtium"]
#eval get ediblePlants 2
#eval get ediblePlants 4

-- Uma unica busca em lista pode convenientemente retornar um valor ou erro:

def first' (xs : List α) : Except String α :=
  get xs 0

-- Porem, realizar duas buscas em lista requer lidar com falhas potenciais:

def firstThird''' (xs : List α) : Except String (α × α) :=
  match get xs 0 with
  | Except.error msg => Except.error msg -- hell nah
  | Except.ok first =>
    match get xs 2 with
    | Except.error msg => Except.error msg -- hell nah
    | Except.ok third =>
      Except.ok (first, third)

-- Adicionar outra busca requer ainda mais tratamento de erro...

-- Novamente, um padrao comum pode se tornar um auxiliar.
-- Uma nova versao de andThen pode ser definida para Except:

def andThen' (attempt : Except e α) (next : α → Except e β) : Except e β :=
  match attempt with
  | Except.error msg => Except.error msg
  | Except.ok x => next x

-- Assim como com Option, esta versao de andThen permite uma definicao mais
-- concisa de firstThird:

def firstThirdExc (xs : List α) : Except String (α × α) :=
  andThen' (get xs 0) fun first  =>
  andThen' (get xs 2) fun third =>
  Except.ok (first, third)

-- Em ambos os casos Option e Except, ha dois padroes repetitivos: a verificacao
-- de resultados intermediarios em cada passo (simplificada com `andThen`), e o
-- resultado final bem-sucedido (`some` ou `Except.ok`). Para conveniencia,
-- sucesso pode ser fatorado em um auxiliar chamado ok:

def ok (x : α) : Except ε α := Except.ok x

-- Similarmente, falha pode ser fatorada em um auxiliar chamado fail:

def fail (err : ε) : Except ε α := Except.error err

-- Usar ok e fail torna get um pouco mais legivel:

def get' (xs : List α) (i : Nat) : Except String α :=
  match xs[i]? with
  | none => fail s!"Index {i} not found (maximum is {xs.length - 1})"
  | some x => ok x

-- Apos adicionar a declaracao infix para andThen, firstThird pode ser tao
-- concisa quanto a versao que retorna Option:

infixl:55 " ~~> " => andThen'

def firstThirdExc' (xs : List α) : Except String (α × α) :=
  get' xs 0 ~~> fun first =>
  get' xs 2 ~~> fun third =>
  ok (first, third)

-- A tecnica escala similarmente para funcoes maiores:

def firstThirdFifthSeventh' (xs : List α) : Except String (α × α × α × α) :=
  get xs 0 ~~> fun first =>
  get xs 2 ~~> fun third =>
  get xs 4 ~~> fun fifth =>
  get xs 6 ~~> fun seventh =>
  ok (first, third, fifth, seventh)


-- 4.1.3. Logging

-- Um numero eh par se dividir por 2 nao deixa resto:

def isEven (i : Int) : Bool :=
  i % 2 == 0

-- A funcao sumAndFindEvens computa a soma de uma lista enquanto lembra os
-- numeros pares encontrados ao longo do caminho:

def sumAndFindEvens : List Int → List Int × Int
  | [] => ([], 0)
  | i :: is =>
    let (moreEven, sum) := sumAndFindEvens is
    (if isEven i then i :: moreEven else moreEven, sum + i)

-- Esta funcao eh um exemplo simplificado de um padrao comum. Muitos programas
-- precisam atravessar uma estrutura de dados uma vez, enquanto computam um
-- resultado principal e acumulam algum tipo de resultado extra. Um exemplo
-- disso eh logging: um programa que eh uma acao IO sempre pode gravar logs em
-- arquivo no disco, mas porque o disco esta fora do mundo matematico das funcoes
-- Lean, torna-se muito mais dificil provar coisas sobre logs baseados em IO.
-- Outro exemplo eh uma funcao que computa a soma de todos os nos em uma arvore
-- com travessia em ordem, enquanto simultaneamente registra cada no visitado:

inductive BinTree (α : Type) where
  | leaf : BinTree α
  | branch : BinTree α → α → BinTree α → BinTree α

def inorderSum : BinTree Int → List Int × Int
  | BinTree.leaf => ([], 0)
  | BinTree.branch l x r =>
    let (leftVisited, leftSum) := inorderSum l
    let (hereVisited, hereSum) := ([x], x)
    let (rightVisited, rightSum) := inorderSum r
    (leftVisited ++ hereVisited ++ rightVisited,
     leftSum + hereSum + rightSum)

-- Tanto sumAndFindEvens quanto inorderSum tem uma estrutura repetitiva comum.
-- Cada passo de computacao retorna um par que consiste de uma lista de dados
-- que foram salvos junto com o resultado primario. As listas sao entao
-- concatenadas, e o resultado primario eh computado e vira par com as listas
-- A estrutura comum se torna mais aparente com uma pequena reescrita de
-- sumAndFindEvens que separa mais claramente as preocupacoes de salvar numeros
-- pares e computar a soma:

def sumAndFindEvens' : List Int → List Int × Int
  | [] => ([], 0)
  | i :: is =>
    let (moreEven, sum) := sumAndFindEvens' is
    let (evenHere, ()) := (if isEven i then [i] else [], ())
    (evenHere ++ moreEven, sum + i)

-- Para clareza, um par que consiste de um resultado acumulado junto com um
-- valor pode receber seu proprio nome:

structure WithLog (logged : Type) (α : Type) where
  log : List logged
  val : α

-- Similarmente, o processo de salvar uma lista de resultados acumulados
-- enquanto passa um valor para o proximo passo de uma computacao pode ser
-- fatorado em um auxiliar, novamente chamado andThen:

def andThenLog (result : WithLog α β) (next : β → WithLog α γ) : WithLog α γ :=
  let {log := thisOut, val := thisRes} := result
  let {log := nextOut, val := nextRes} := next thisRes
  {log := thisOut ++ nextOut, val := nextRes}

-- No caso de erros, ok representa uma operacao que sempre sucede. Aqui, eh uma
-- operacao que simplesmente retorna um valor sem registrar nada:

def okLog (x : β) : WithLog α β := {log := [], val := x}

-- Assim como Except fornece fail, WithLog deve permitir que itens sejam
-- adicionados a um log. Isso nao tem valor de retorno interessante associado,
-- entao retorna Unit:

def save (data : α) : WithLog α Unit :=
  {log := [data], val := ()}

-- `WithLog`, `andThen`, `ok` e `save` podem ser usados para separar a
-- preocupacao de logging da preocupacao principal (somar) em ambos programas:

def sumAndFindEvensLog : List Int → WithLog Int Int
  | [] => okLog 0
  | i :: is =>
    andThenLog (if isEven i then save i else okLog ()) fun () =>
    andThenLog (sumAndFindEvensLog is) fun sum =>
    okLog (i + sum)

def inorderSumLog : BinTree Int → WithLog Int Int
  | BinTree.leaf => okLog 0
  | BinTree.branch l x r =>
    andThenLog (inorderSumLog l) fun leftSum =>
    andThenLog (save x) fun () =>
    andThenLog (inorderSumLog r) fun rightSum =>
    okLog (leftSum + x + rightSum)

-- E novamente, o operador infix ajuda a colocar foco nos passos corretos:

infixl:55 " ~~> " => andThenLog

def sumAndFindEvensLog' : List Int → WithLog Int Int
  | [] => okLog 0
  | i :: is =>
    (if isEven i then save i else okLog ()) ~~> fun () =>
    sumAndFindEvensLog' is ~~> fun sum =>
    okLog (i + sum)

def inorderSumLog' : BinTree Int → WithLog Int Int
  | BinTree.leaf => okLog 0
  | BinTree.branch l x r =>
    inorderSumLog' l ~~> fun leftSum =>
    save x ~~> fun () =>
    inorderSumLog' r ~~> fun rightSum =>
    okLog (leftSum + x + rightSum)


-- 4.1.4. Numbering Tree Nodes

-- Uma numeracao em ordem de uma arvore associa cada ponto de dados na arvore
-- com o passo em que seria visitado em uma travessia em ordem da arvore. Por
-- exemplo, considere aTree:

open BinTree in
def aTree :=
  branch
    (branch
      (branch leaf "a" (branch leaf "b" leaf))
      "c"
      leaf)
    "d"
    (branch leaf "e" leaf)

--       d
--      / \
--     c   e
--    /
--   a
--    \
--     b

-- Sua numeracao em ordem eh:
-- BinTree.branch
--   (BinTree.branch
--     (BinTree.branch (BinTree.leaf) (0, "a") (BinTree.branch (BinTree.leaf) (1, "b") (BinTree.leaf)))
--     (2, "c")
--     (BinTree.leaf))
--   (3, "d")
--   (BinTree.branch (BinTree.leaf) (4, "e") (BinTree.leaf))

-- Arvores sao mais naturalmente processadas com funcoes recursivas, mas o
-- padrao usual de recursao em arvores torna dificil computar uma numeracao em
-- ordem. Isso porque o numero mais alto atribuido em qualquer lugar da
-- subarvore esquerda eh usado para determinar a numeracao do valor de dados do
-- no, e entao novamente para determinar o ponto inicial para numerar a
-- subarvore direita. Em uma linguagem imperativa, esse problema pode ser
-- contornado usando uma variavel mutavel que contem o proximo numero a ser
-- atribuido.

-- Embora Lean nao tenha variaveis mutaveis, existe uma solucao alternativa. Do
-- ponto de vista do resto do mundo, a variavel mutavel pode ser pensada como
-- tendo dois aspectos relevantes: seu valor quando a funcao eh chamada, e seu
-- valor quando a funcao retorna. Em outras palavras, uma funcao que usa uma
-- variavel mutavel pode ser vista como uma funcao que recebe o valor inicial
-- da variavel mutavel como argumento, retornando um par do valor final da
-- variavel e o resultado da funcao. Este valor final pode entao ser passado
-- como argumento para o proximo passo.

-- Assim como o exemplo Python usa uma funcao externa que estabelece uma
-- variavel mutavel e uma funcao auxiliar interna que muda a variavel, uma
-- versao Lean da funcao usa uma funcao externa que fornece o valor inicial da
-- variavel e retorna explicitamente o resultado da funcao junto com uma funcao
-- auxiliar interna que passa o valor da variavel enquanto computa a arvore
-- numerada:

def number (t : BinTree α) : BinTree (Nat × α) :=
  let rec helper (n : Nat) : BinTree α → (Nat × BinTree (Nat × α))
    | BinTree.leaf => (n, BinTree.leaf)
    | BinTree.branch left x right =>
      let (k, numberedLeft) := helper n left
      let (i, numberedRight) := helper (k + 1) right
      (i, BinTree.branch numberedLeft (k, x) numberedRight)
  (helper 0 t).snd

-- Este codigo, como o codigo de propagacao de none, o codigo de propagacao de
-- erro Except, e o codigo de acumulacao de log WithLog, mistura duas
-- preocupacoes: propagar o valor do contador e realmente atravessar a arvore
-- para encontrar o resultado. Assim como nesses casos, um auxiliar andThen
-- pode ser definido para propagar estado de um passo de uma computacao para
-- outro. O primeiro passo eh dar um nome ao padrao de tomar um estado de
-- entrada como argumento e retornar um estado de saida junto com um valor:

def State (σ : Type) (α : Type) : Type :=
  σ → (σ × α)

-- No caso de State, ok eh uma funcao que retorna o estado de entrada inalterado,
-- junto com o valor fornecido:

def okState (x : α) : State σ α :=
  fun s => (s, x)

-- Ao trabalhar com uma variavel mutavel, ha duas operacoes fundamentais: ler o
-- valor e substitui-lo por um novo. Ler o valor atual eh realizado com uma
-- funcao que coloca o estado de entrada nao modificado no estado de saida, e
-- tambem o coloca no campo de valor:

def getState : State σ σ :=
  fun s => (s, s)

-- Escrever um novo valor consiste em ignorar o estado de entrada e colocar o
-- novo valor fornecido no estado de saida:

def setState (s : σ) : State σ Unit :=
  fun _ => (s, ())

-- Finalmente, duas computacoes que usam estado podem ser sequenciadas
-- encontrando tanto o estado de saida quanto o valor de retorno da primeira
-- funcao, e entao passando ambos para a proxima funcao:

def andThenState (first : State σ α) (next : α → State σ β) : State σ β :=
  fun s =>
    let (s', x) := first s
    next x s'

infixl:55 " ~~> " => andThenState

-- Usando State e seus auxiliares, estado mutavel local pode ser simulado:

def numberState (t : BinTree α) : BinTree (Nat × α) :=
  let rec helper : BinTree α → State Nat (BinTree (Nat × α))
    | BinTree.leaf => okState BinTree.leaf
    | BinTree.branch left x right =>
      helper left ~~> fun numberedLeft =>
      getState ~~> fun n =>
      setState (n + 1) ~~> fun () =>
      helper right ~~> fun numberedRight =>
      okState (BinTree.branch numberedLeft (n, x) numberedRight)
  (helper t 0).snd

-- Como State simula apenas uma unica variavel local, get e set nao precisam
-- se referir a nenhum nome de variavel em particular.


-- 4.1.5. Monads: A Functional Design Pattern

-- Cada um desses exemplos consistiu de:
-- - Um tipo polimórfico, como Option, Except ε, WithLog α, ou State σ.
-- - Um operador andThen que cuida de algum aspecto repetitivo de sequenciar
--     programas que tem esse tipo.
-- - Um operador ok que eh (em certo sentido) a forma mais simples de usar o tipo.
-- - Uma colecao de outras operacoes, como none, fail, save e get, que nomeiam
--     formas de usar o tipo.

-- Este estilo de API eh chamado de monad. Enquanto a ideia de monads eh
-- derivada de um ramo da matematica chamado teoria de categorias, nenhum
-- entendimento de teoria de categorias eh necessario para usa-los na
-- programacao. A ideia chave de monads eh que cada monad codifica um tipo
-- particular de efeito colateral usando as ferramentas fornecidas pela
-- linguagem funcional pura Lean. Por exemplo, Option representa programas que
-- podem falhar retornando none, Except representa programas que podem lancar
-- exceptions, WithLog representa programas que acumulam um log enquanto
-- executam, e State representa programas com uma unica variavel mutavel.


-- 4.2. The Monad Type Class

-- Em vez de ter que importar um operador como ok ou andThen para cada tipo que
-- eh um monad, a biblioteca padrao de Lean contem uma type class que permite
-- que sejam sobrecarregados, de modo que os mesmos operadores possam ser usados
-- para qualquer monad. Monads tem duas operacoes, que sao equivalentes a ok e
-- andThen:

#check Monad
#check pure
#check bind

class MyMonad (m : Type → Type) where
  pure : α → m α
  bind : m α → (α → m β) → m β

-- Esta definicao eh ligeiramente simplificada. A definicao real na biblioteca
-- Lean eh um pouco mais envolvida, e sera apresentada mais tarde.

-- As instancias Monad para `Option` e `Except ε` podem ser criadas adaptando as
-- definicoes de suas respectivas operacoes andThen:

instance : MyMonad Option where
  pure x := some x
  bind opt next :=
    match opt with
    | none => none
    | some x => next x

instance : MyMonad (Except ε) where
  pure x := Except.ok x
  bind attempt next :=
    match attempt with
    | Except.error e => Except.error e
    | Except.ok x => next x

-- Como exemplo, firstThirdFifthSeventh foi definido separadamente para tipos de
-- retorno Option α e Except String α. Agora, pode ser definido polimorficamente
-- para qualquer monad. Requer, porem, uma funcao de busca como argumento,
-- porque diferentes monads podem falhar ao encontrar um resultado de diferentes
-- formas. A versao infix de bind eh >>=, que tem o mesmo papel que ~~> nos exemplos.

def firstThirdFifthSeventhMonad [Monad m] (lookup : List α → Nat → m α)
    (xs : List α) : m (α × α × α × α) :=
  lookup xs 0 >>= fun first =>
  lookup xs 2 >>= fun third =>
  lookup xs 4 >>= fun fifth =>
  lookup xs 6 >>= fun seventh =>
  pure (first, third, fifth, seventh)

-- Dadas listas de exemplo de mamiferos lentos e passaros rapidos, esta
-- implementacao de firstThirdFifthSeventh pode ser usada com Option:

def slowMammals : List String :=
  ["Three-toed sloth", "Slow loris"]

def fastBirds : List String := [
  "Peregrine falcon",
  "Saker falcon",
  "Golden eagle",
  "Gray-headed albatross",
  "Spur-winged goose",
  "Swift",
  "Anna's hummingbird"
]

-- `Option` eh o monad em "List String → Nat → Option String".
#eval firstThirdFifthSeventhMonad (fun xs i => xs[i]?) slowMammals
#eval firstThirdFifthSeventhMonad (fun xs i => xs[i]?) fastBirds

-- Apos renomear a funcao de busca get de Except para algo mais especifico, a
-- mesma implementacao de firstThirdFifthSeventh pode ser usada com Except:

def getOrExcept (xs : List α) (i : Nat) : Except String α :=
  match xs[i]? with
  | none =>
    Except.error s!"Index {i} not found (maximum is {xs.length - 1})"
  | some x =>
    Except.ok x

#eval firstThirdFifthSeventhMonad getOrExcept slowMammals
#eval firstThirdFifthSeventhMonad getOrExcept fastBirds
-- `getOrExcept` eh passado de forma parcial para `firstThirdFifthSeventhMonad`
-- e eh "completada" quando ocorre `lookup xs i >>= fun first =>`.

-- O fato de que m deve ter uma instancia Monad significa que as operacoes `>>=`
-- e `pure` estao disponiveis.


-- 4.2.1. General Monad Operations

-- Como muitos tipos diferentes sao monads, funcoes que sao polimorficas sobre
-- qualquer monad sao muito poderosas. Por exemplo, a funcao mapM eh uma versao
-- de map que usa um Monad para sequenciar e combinar os resultados de aplicar
-- uma funcao:

def mapM [Monad m] (f : α → m β) : List α → m (List β)
  | [] => pure []
  | x :: xs =>
    f x >>= fun hd =>
    mapM f xs >>= fun tl =>
    pure (hd :: tl)

-- O tipo de retorno do argumento de funcao `f` determina qual instancia Monad
-- sera usada. Como o tipo de `f` determina os efeitos disponiveis, eles podem
-- ser rigidamente controlados por designers de API.

-- `State σ α` representa programas que fazem uso de uma variavel mutavel de tipo
-- `σ` e retornam um valor de tipo `α`. _Esses programas sao na verdade funcoes_
-- _de um estado inicial para um par de um valor e um estado final._ A classe
-- Monad requer que seu parametro espere um unico argumento de tipo (Type → Type).
-- Isso significa que a instancia para State deve mencionar o tipo de estado σ,
-- que se torna um parametro para a instancia:

#check @State

instance : Monad (State σ) where
  pure x := fun s => (s, x)
  bind first next :=
    fun s =>
      let (s', x) := first s  -- `s` eh o estado inicial, `s'` eh o novo estado
      next x s'  -- `x` eh o valor retornado, `s'` eh passado adiante

-- Isso significa que o tipo do estado nao pode mudar entre chamadas para get e
-- set que sao sequenciadas usando bind, o que eh uma regra razoavel para
-- computacoes com estado. O operador increment aumenta um estado salvo por uma
-- quantidade dada, retornando o valor antigo:

def increment (howMuch : Int) : State Int Int :=
  getState >>= fun i =>
  setState (i + howMuch) >>= fun () =>
  pure i

#eval increment 1 0

-- Usar mapM com increment resulta em um programa que computa a soma das entradas
-- em uma lista. Mais especificamente, a variavel mutavel contem a soma ate o
-- momento, enquanto a lista resultante contem uma soma acumulada. Em outras
-- palavras, mapM increment tem tipo List Int → State Int (List Int), e
-- expandindo a definicao de State resulta em List Int → Int → (Int × List Int).
-- Recebe uma soma inicial como argumento, que deve ser 0:

#eval mapM increment [1, 2, 3, 4, 5] 0

-- Um efeito de logging pode ser representado usando WithLog. Assim como State,
-- sua instancia Monad eh polimorfica com respeito ao tipo dos dados registrados:

instance : Monad (WithLog logged) where
  pure x := {log := [], val := x}
  bind result next :=
    let {log := thisOut, val := thisRes} := result
    let {log := nextOut, val := nextRes} := next thisRes
    {log := thisOut ++ nextOut, val := nextRes}

-- saveIfEven eh uma funcao que registra numeros pares mas retorna seu argumento
-- inalterado:

def saveIfEven (i : Int) : WithLog Int Int :=
  (if isEven i then
    save i
   else pure ()) >>= fun () =>
  pure i

-- Usar esta funcao com mapM resulta em um log contendo numeros pares emparelhado
-- com uma lista de entrada inalterada:

#eval mapM saveIfEven [1, 2, 3, 4, 5]


-- 4.2.2. The Identity Monad

-- Monads codificam programas com efeitos, como falha, exceptions ou logging, em
-- representacoes explicitas como dados e funcoes. Porem, as vezes uma API sera
-- escrita para usar um monad por flexibilidade, mas o cliente da API pode nao
-- requerer nenhum efeito codificado. O identity monad eh um monad que nao tem
-- efeitos. Ele permite que codigo puro seja usado com APIs monadicas:

def Id' (t : Type) : Type := t

instance : Monad Id' where
  pure x := x
  bind x f := f x

-- O tipo de pure deveria ser `α → Id α`, mas `Id α` reduz para apenas `α`.
-- Similarmente, o tipo de bind deveria ser `α → (α → Id β) → Id β`. Como isso
-- reduz para `α → (α → β) → β`, o segundo argumento pode ser aplicado ao
-- primeiro para encontrar o resultado.

-- Com o identity monad, mapM se torna equivalente a map. Para chama-lo desta
-- forma o Lean precisa uma dica de que o monad pretendido eh o `Id`:

#eval mapM (m := Id') (· + 1) [1, 2, 3, 4, 5]

-- Omitir a dica resulta em erro:
#eval mapM (· + 1) [1, 2, 3, 4, 5]

-- Neste erro, a aplicacao de uma metavariavel a outra indica que Lean nao
-- executa a computacao de nivel de tipo para tras. O tipo de retorno da funcao
-- eh esperado ser o monad aplicado a algum outro tipo. Similarmente, usar mapM
-- com uma funcao cujo tipo nao fornece dicas especificas sobre qual monad deve
-- ser usado resulta em "instance problem is stuck":

#eval mapM (fun (x : Nat) => x) [1, 2, 3, 4, 5]


-- 4.2.3. The Monad Contract

-- Assim como todo par de instancias de BEq e Hashable deve garantir que
-- quaisquer dois valores iguais tenham o mesmo hash, ha um contrato que cada
-- instancia de Monad deve obedecer.

-- Primeiro, pure deve ser uma identidade a esquerda de bind. Isto eh,
-- "bind (pure v) f" deve ser o mesmo que "f v".

-- Segundo, pure deve ser uma identidade a direita de bind, entao "bind v pure"
-- eh o mesmo que "v".

-- Finalmente, bind deve ser associativo, entao "bind (bind v f) g" eh o mesmo
-- que "bind v (fun x => bind (f x) g)".

-- Este contrato especifica as propriedades esperadas de programas com efeitos
-- mais geralmente. Como pure nao tem efeitos, sequenciar seus efeitos com bind
-- nao deveria mudar o resultado. A propriedade associativa de bind basicamente
-- diz que a contabilidade de sequenciamento em si nao importa, desde que a
-- ordem em que as coisas estao acontecendo seja preservada.


-- 4.2.4. Exercises


-- 4.2.4.1. Mapping on a Tree

-- Define a function BinTree.mapM. By analogy to mapM for lists, this function
-- should apply a monadic function to each data entry in a tree, as a preorder
-- traversal. The type signature should be:

def BinTree.mapM [Monad m] (f : α → m β) : BinTree α → m (BinTree β)
  | leaf => pure leaf
  | branch l x r => do
    let x' ← f x
    let l' ← mapM f l
    let r' ← mapM f r
    pure $ branch l' x' r'

-- ou

def BinTree.mapM' [Monad m] (f : α → m β) : BinTree α → m (BinTree β)
  | leaf => pure leaf
  | branch l x r =>
    f x >>= fun x' =>
    mapM f l >>= fun l' =>
    mapM f r >>= fun r' =>
    pure $ branch l' x' r'


-- 4.2.4.2. The Option Monad Contract

-- First, write a convincing argument that the Monad instance for Option
-- satisfies the monad contract.

/-
  First, pure should be a left identity of bind. That is, bind (pure v) f should
    be the same as f v.
  Secondly, pure should be a right identity of bind, so bind v pure is the same
    as v.
  Finally, bind should be associative, so bind (bind v f) g is the same as
    bind v (fun x => bind (f x) g).
-/

def f (v : α) : (Option α) :=
  Option.some v

#eval bind (pure 1) f
#eval (pure 1) >>= f
#eval (pure 1) >>= f = f 1

#eval bind (Option.some 1) pure
#eval Option.some 1
#eval bind (Option.some 1) pure = Option.some 1

-- (v >>= f) >>= g  =  v >>= (fun x => f x >>= g)

-- Then, consider the following instance:

-- instance : Monad Option where
--   pure x := some x
--   bind opt next := none
-- Both methods have the correct type. Why does this instance violate the monad
-- contract?

-- `bind` deveria sequenciar computacoes do tipo `Option`, mas essa instance
-- apenas ignora a computacao anterior e a funcao, apenas retornando um valor
-- constante (`none`).


-- 4.3. Example: Arithmetic in Monads

-- Monads sao uma forma de codificar programas com efeitos colaterais em uma
-- linguagem que nao os possui nativamente. Embora usar a API Monad traga um
-- custo sintatico, ela oferece dois beneficios importantes:
-- 1. Os programas deixam explicito nos tipos quais efeitos usam, facilitando
--    entender o que cada funcao pode fazer apenas olhando sua assinatura.
-- 2. Nem toda linguagem oferece os mesmos efeitos (como exceptions,
--    continuations, etc). Com monads, o programador pode escolher quais efeitos
--    usar, sem ficar limitado ao que a linguagem oferece nativamente.

-- Um exemplo de programa que pode ser escrito de forma generica para varios
-- monads eh um avaliador de expressoes aritmeticas.


-- 4.3.1. Arithmetic Expressions

-- Uma expressao aritmetica pode ser um inteiro literal ou um operador binario
-- aplicado a duas expressoes. Os operadores sao soma, subtracao, multiplicacao
-- e divisao:

inductive Expr (op : Type) where
  | const : Int → Expr op
  | prim : op → Expr op → Expr op → Expr op

inductive Arith where
  | plus
  | minus
  | times
  | div

-- O termo 2 + 3 eh representado assim:
open Expr in
open Arith in
def twoPlusThree : Expr Arith :=
  prim plus (const 2) (const 3)

-- E a expressao 14 / (45 - 5 * 9):
open Expr in
open Arith in
def fourteenDivided : Expr Arith :=
  prim div (const 14)
    (prim minus (const 45)
      (prim times (const 5)
        (const 9)))


-- 4.3.2. Evaluating Expressions

-- Essas expressoes incluem divisao e divisao por zero nao eh definida. Uma
-- maneira de representar essa falha eh usar `Option`.

def evaluateOption : Expr Arith → Option Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 =>
    evaluateOption e1 >>= fun v1 =>
    evaluateOption e2 >>= fun v2 =>
    match p with
    | Arith.plus => pure (v1 + v2)
    | Arith.minus => pure (v1 - v2)
    | Arith.times => pure (v1 * v2)
    | Arith.div => if v2 == 0 then none else pure (v1 / v2)

-- Esta definicao usa o `bind` da instancia `Monad Option` para propagar falhas.
-- Porem, a funcao mistura duas preocupacoes: avaliar subexpressoes (e1, e2) e
-- aplicar um operador binario (p) aos resultados.
-- Pode ser melhorada dividindo em duas funcoes:

def applyPrimOption : Arith → Int → Int → Option Int
  | Arith.plus, x, y => pure (x + y)
  | Arith.minus, x, y => pure (x - y)
  | Arith.times, x, y => pure (x * y)
  | Arith.div, x, y => if y == 0 then none else pure (x / y)

def evaluateOption' : Expr Arith → Option Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 =>
    evaluateOption' e1 >>= fun v1 =>
    evaluateOption' e2 >>= fun v2 =>
    applyPrimOption p v1 v2


#eval evaluateOption' fourteenDivided

-- Funciona como esperado, mas essa mensagem nao eh muito util. Como o codigo foi
-- escrito usando `>>=` em vez de lidar explicitamente com o construtor `none`,
-- uma pequena modificacao pode fornecer uma mensagem de erro na falha:

def applyPrimExcept : Arith → Int → Int → Except String Int
  | Arith.plus, x, y => pure (x + y)
  | Arith.minus, x, y => pure (x - y)
  | Arith.times, x, y => pure (x * y)
  | Arith.div, x, y =>
    if y == 0 then
      Except.error s!"Tried to divide {x} by zero"
    else pure (x / y)

def evaluateExcept : Expr Arith → Except String Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 =>
    evaluateExcept e1 >>= fun v1 =>
    evaluateExcept e2 >>= fun v2 =>
    applyPrimExcept p v1 v2

-- A unica diferenca eh que a assinatura de tipo menciona `Except String` em vez
-- de `Option`, e o caso de falha usa `Except.error` em vez de `none`. Um
-- avaliador que eh polimorfico em seu monad (aceita qualquer monad) e recebe
-- uma funcao apply como argumento, eh capaz de representar erros de varias
-- maneiras:

def evaluateM [Monad m]
    (applyPrim : Arith → Int → Int → m Int) :
    Expr Arith → m Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 =>
    evaluateM applyPrim e1 >>= fun v1 =>
    evaluateM applyPrim e2 >>= fun v2 =>
    applyPrim p v1 v2

-- Usando o apply de tipo Option funciona igual ao primeiro avaliador:
#eval evaluateM applyPrimOption fourteenDivided

-- E o de tipo Except funciona igual a versao com mensagens de erro:
#eval evaluateM applyPrimExcept fourteenDivided

-- O codigo ainda pode ser melhorado. As funcoes applyPrimOption e
-- applyPrimExcept diferem apenas em seu tratamento de divisao, que pode ser
-- extraido em outro parametro para o avaliador:

def applyDivOption (x : Int) (y : Int) : Option Int :=
    if y == 0 then
      none
    else pure (x / y)

def applyDivExcept (x : Int) (y : Int) : Except String Int :=
    if y == 0 then
      Except.error s!"Tried to divide {x} by zero"
    else pure (x / y)

def applyPrim [Monad m]
    (applyDiv : Int → Int → m Int) :
    Arith → Int → Int → m Int
  | Arith.plus, x, y => pure (x + y)
  | Arith.minus, x, y => pure (x - y)
  | Arith.times, x, y => pure (x * y)
  | Arith.div, x, y => applyDiv x y

def evaluateM' [Monad m]
    (applyDiv : Int → Int → m Int) :
    Expr Arith → m Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 =>
    evaluateM' applyDiv e1 >>= fun v1 =>
    evaluateM' applyDiv e2 >>= fun v2 =>
    applyPrim applyDiv p v1 v2

-- Neste codigo refatorado, o fato de que os dois caminhos de codigo diferem
-- apenas em seu tratamento de falha foi tornado completamente aparente.


-- 4.3.3. Further Effects

-- Falha e exceptions nao sao os unicos tipos de efeitos que podem ser
-- interessantes ao trabalhar com um avaliador. Enquanto divisao tem apenas o
-- efeito colateral de falha, adicionar outros operadores primitivos as
-- expressoes torna possivel expressar outros efeitos.

-- O primeiro passo eh uma refatoracao adicional, extraindo divisao do datatype
-- de primitivos:

inductive Prim (special : Type) where
  | plus
  | minus
  | times
  | other : special → Prim special

inductive CanFail where
  | div

-- CanFail eh um tipo separado para operacoes que podem falhar e que serao
-- passadas para Prim como argumento.

-- O segundo passo eh ampliar o escopo do argumento manipulador de divisao em
-- evaluateM para que possa processar qualquer operador especial:

def divOption : CanFail → Int → Int → Option Int
  | CanFail.div, x, y => if y == 0 then none else pure (x / y)

def divExcept : CanFail → Int → Int → Except String Int
  | CanFail.div, x, y =>
    if y == 0 then
      Except.error s!"Tried to divide {x} by zero"
    else pure (x / y)

def applyPrim' [Monad m]
    (applySpecial : special → Int → Int → m Int) :
    Prim special → Int → Int → m Int
  | Prim.plus, x, y => pure (x + y)
  | Prim.minus, x, y => pure (x - y)
  | Prim.times, x, y => pure (x * y)
  | Prim.other op, x, y => applySpecial op x y

def evaluateM'' [Monad m]
    (applySpecial : special → Int → Int → m Int) :
    Expr (Prim special) → m Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 =>
    evaluateM'' applySpecial e1 >>= fun v1 =>
    evaluateM'' applySpecial e2 >>= fun v2 =>
    applyPrim' applySpecial p v1 v2

-- Exemplo 1: Usando com Option (falha silenciosa)
open Expr Prim in
def exampleExpr1 : Expr (Prim CanFail) :=
  prim (other CanFail.div) (const 10) (const 2)

open Expr Prim in
def exampleExpr2 : Expr (Prim CanFail) :=
  prim (other CanFail.div) (const 10) (const 0)

-- Usando com Option:
#eval evaluateM'' divOption exampleExpr1
#eval evaluateM'' divOption exampleExpr2

-- Usando com Except:
#eval evaluateM'' divExcept exampleExpr1
#eval evaluateM'' divExcept exampleExpr2


-- 4.3.3.1. No Effects

-- O tipo Empty nao tem construtores, e portanto nenhum valor, mas Empty ainda
-- eh util como uma indicacao ao sistema de tipos de que uma funcao nao pode ser
-- chamada. Usar a sintaxe `nomatch E` quando E eh uma expressao cujo tipo nao
-- tem construtores indica ao Lean que a expressao atual nao precisa retornar
-- um resultado, porque nunca poderia ter sido chamada.

-- Usar Empty como parametro para Prim indica que nao ha casos adicionais alem
-- de Prim.plus, Prim.minus e Prim.times, porque eh impossivel criar um valor de
-- tipo Empty para colocar no construtor Prim.other. Como uma funcao para
-- aplicar um operador de tipo Empty a dois inteiros nunca pode ser chamada, ela
-- nao precisa retornar um resultado. Assim, pode ser usada em qualquer monad:

def applyEmpty [Monad m] (op : Empty) (_ : Int) (_ : Int) : m Int :=
  nomatch op

-- Isso pode ser usado junto com Id, para avaliar expressoes sem efeitos:

open Expr Prim in
#eval evaluateM'' (m := Id) applyEmpty (prim plus (const 5) (const (-14)))

-- Impossivel:
open Expr Prim in
#eval evaluateM'' (m := Id) applyEmpty (prim (other CanFail.div) (const 5) (const (-14)))

-- Lembrar que a funcao aqui ↑ ↑ ↑ ↑ eh usada apenas no caso do match com
-- `Prim.other`.


-- 4.3.3.2. Nondeterministic Search

-- Em vez de simplesmente falhar ao encontrar divisao por zero, seria sensato
-- fazer backtrack e tentar uma entrada diferente. Dado o monad certo, o mesmo
-- evaluateM pode realizar uma busca nao-deterministica por um conjunto de
-- respostas que nao resultam em falha. Isso requer, alem de divisao, algum meio
-- de especificar uma escolha de resultados. Uma maneira de fazer isso eh
-- adicionar uma funcao choose a linguagem de expressoes que instrui o avaliador
-- a escolher qualquer um de seus argumentos ao buscar resultados sem falha.

-- O resultado do avaliador agora eh um multiset de valores, em vez de um unico
-- valor. As regras para avaliacao em um multiset sao:
-- * Constantes n avaliam para conjuntos singleton {n}.
-- * Operadores aritmeticos (exceto divisao) sao chamados em cada par do produto
--   cartesiano dos operadores, entao X+Y avalia para {x+y | x∈X, y∈Y}.
-- - Divisao X/Y avalia para {x/y | x∈X, y∈Y, y≠0}. Todos os valores 0 em Y
--   sao descartados.
-- - Uma escolha choose(x,y) avalia para {x,y}.

-- Por exemplo:
-- - 1 + choose(2,5) avalia para {3,6}
-- - 1 + 2/0 avalia para {}
-- - 90/(choose(-5,5)+5) avalia para {9}

-- Um monad que representa esse efeito nao-deterministico deve ser capaz de
-- representar uma situacao onde nao ha respostas, e uma situacao onde ha pelo
-- menos uma resposta junto com quaisquer respostas restantes:

inductive Many (α : Type) where
  | none : Many α
  | more : α → (Unit → Many α) → Many α

-- Este datatype parece muito com List. A diferenca eh que onde List.cons
-- armazena o resto da lista, more armazena uma funcao que deve computar os
-- valores restantes sob demanda. Isso permite que um consumidor de Many use
-- lazy evaluation para computar o resto apenas quando nescessario.

-- Um unico resultado eh representado por um construtor more que nao retorna
-- mais resultados:

def Many.one (x : α) : Many α := Many.more x (fun () => Many.none)

-- A uniao de dois multisets de resultados pode ser computada verificando se o
-- primeiro multiset esta vazio. Se sim, o segundo multiset eh a uniao. Se nao,
-- a uniao consiste do primeiro elemento do primeiro multiset seguido pela uniao
-- do resto do primeiro multiset com o segundo multiset:

def Many.union : Many α → Many α → Many α
  | Many.none, ys => ys
  | Many.more x xs, ys => Many.more x (fun () => union (xs ()) ys)

-- Pode ser conveniente iniciar um processo de busca com uma lista de valores.
-- Many.fromList converte uma lista em um multiset de resultados:

def Many.fromList : List α → Many α
  | [] => Many.none
  | x :: xs => Many.more x (fun () => fromList xs)

-- Similarmente, uma vez que uma busca foi especificada, pode ser conveniente
-- extrair um numero de valores, ou todos os valores:

def Many.take : Nat → Many α → List α
  | 0, _ => []
  | _ + 1, Many.none => []
  | n + 1, Many.more x xs => x :: (xs ()).take n

def Many.takeAll : Many α → List α
  | Many.none => []
  | Many.more x xs => x :: (xs ()).takeAll

-- Uma instancia Monad Many requer um operador bind. Em uma busca
-- nao-deterministica, sequenciar duas operacoes consiste em pegar todas as
-- possibilidades do primeiro passo e executar o resto do programa em cada uma
-- delas, pegando a uniao dos resultados. Em outras palavras, se o primeiro
-- passo retorna tres respostas possiveis, o segundo passo precisa ser tentado
-- para todas as tres. Como o segundo passo pode retornar qualquer numero de
-- respostas para cada entrada, pegar sua uniao representa todo o espaco de busca.

def Many.bind : Many α → (α → Many β) → Many β
  | Many.none, _ => Many.none
  | Many.more x xs, f => (f x).union (bind (xs ()) f)

-- Many.one e Many.bind obedecem o contrato monad. Para verificar que
-- "Many.bind (Many.one v) f" eh o mesmo que "f v", comece avaliando a expressao
-- o maximo possivel:
-- ⟹ Many.bind (Many.one v) f
-- ⟹ Many.bind (Many.more v (fun () => Many.none)) f
-- ⟹ (f v).union (Many.bind Many.none f)
-- ⟹ (f v).union Many.none
-- O multiset vazio eh uma identidade a direita de union, entao a resposta eh
-- equivalente a "f v".

-- Para verificar que "Many.bind v Many.one" eh o mesmo que "v", considere que
-- Many.bind pega a uniao de aplicar Many.one a cada elemento de v. Em outras
-- palavras, se v tem a forma {v1, v2, v3, ..., vn}, entao "Many.bind v Many.one"
-- eh {v1} ∪ {v2} ∪ {v3} ∪ ... ∪ {vn}, que eh {v1, v2, v3, ..., vn} (igual).

-- Finalmente, para verificar que Many.bind eh associativo, verifique que
-- "Many.bind (Many.bind v f) g" eh o mesmo que "Many.bind v (fun x => Many.bind (f x) g)".
-- Se v tem a forma {v1, v2, v3, ..., vn}, entao:
-- Many.bind v f = f v1 ∪ f v2 ∪ f v3 ∪ ... ∪ f vn
-- o que significa que:
-- Many.bind (Many.bind v f) g =
--   Many.bind (f v1) g ∪ Many.bind (f v2) g ∪ Many.bind (f v3) g ∪ ... ∪ Many.bind (f vn) g
-- Similarmente:
-- Many.bind v (fun x => Many.bind (f x) g) =
--   (fun x => Many.bind (f x) g) v1 ∪ (fun x => Many.bind (f x) g) v2 ∪ ... ∪ (fun x => Many.bind (f x) g) vn =
--   Many.bind (f v1) g ∪ Many.bind (f v2) g ∪ Many.bind (f v3) g ∪ ... ∪ Many.bind (f vn) g
-- Assim, ambos os lados sao iguais, entao Many.bind eh associativo.

-- A instancia monad resultante eh:

instance : Monad Many where
  pure := Many.one
  bind := Many.bind

-- Um exemplo de busca usando este monad encontra todas as combinacoes de
-- numeros em uma lista que somam 15:

def addsTo (goal : Nat) : List Nat → Many (List Nat)
  | [] =>
    if goal == 0 then
      pure []
    else
      Many.none
  | x :: xs =>
    if x > goal then
      addsTo goal xs
    else
      (addsTo goal xs).union
        (addsTo (goal - x) xs >>= fun answer =>
         pure (x :: answer))

-- O processo de busca eh recursivo sobre a lista. A lista vazia eh uma busca
-- bem-sucedida quando o objetivo eh 0; caso contrario, falha. Quando a lista
-- nao esta vazia, ha duas possibilidades: ou a cabeca da lista eh maior que o
-- objetivo, caso em que nao pode participar de nenhuma busca bem-sucedida, ou
-- nao eh, caso em que pode. Se a cabeca da lista nao eh candidata, a busca
-- prossegue para a cauda da lista. Se a cabeca eh candidata, ha duas
-- possibilidades a serem combinadas com Many.union: ou as solucoes encontradas
-- contem a cabeca, ou nao. As solucoes que nao contem a cabeca sao encontradas
-- com uma chamada recursiva na cauda, enquanto as solucoes que contem resultam
-- de subtrair a cabeca do objetivo, e entao anexar a cabeca as solucoes que
-- resultam da chamada recursiva.

-- O auxiliar printList garante que um resultado seja exibido por linha:

def printList [ToString α] : List α → IO Unit
  | [] => pure ()
  | x :: xs => do
    IO.println x
    printList xs

#eval printList (addsTo 15 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).takeAll

-- Retornando ao avaliador aritmetico que produz multisets de resultados, o
-- operador choose pode ser usado para selecionar nao-deterministicamente um
-- valor, com divisao por zero tornando selecoes anteriores invalidas.

inductive NeedsSearch where
  | div
  | choose

def applySearch : NeedsSearch → Int → Int → Many Int
  | NeedsSearch.choose, x, y =>
    Many.fromList [x, y]
  | NeedsSearch.div, x, y =>
    if y == 0 then
      Many.none
    else Many.one (x / y)

-- Usando esses operadores, os exemplos anteriores podem ser avaliados:

open Expr Prim NeedsSearch

#eval
  (evaluateM'' applySearch
    (prim plus (const 1)
      (prim (other choose) (const 2)
        (const 5)))).takeAll

#eval
  (evaluateM'' applySearch
    (prim plus (const 1)
      (prim (other div) (const 2)
        (const 0)))).takeAll

#eval
  (evaluateM'' applySearch
    (prim (other div) (const 90)
      (prim plus (prim (other choose) (const (-5)) (const 5))
        (const 5)))).takeAll


-- 4.3.3.3. Custom Environments

-- O avaliador pode ser tornado extensivel pelo usuario permitindo que strings
-- sejam usadas como operadores, e entao fornecendo um mapeamento de strings
-- para uma funcao que as implementa. Por exemplo, usuarios poderiam estender o
-- avaliador com um operador de resto ou com um que retorna o maximo de seus
-- dois argumentos. O mapeamento de nomes de funcao para implementacoes de
-- funcao eh chamado um environment.

-- Os ambientes precisam ser passados em cada chamada recursiva. Inicialmente,
-- pode parecer que evaluateM precisa de um argumento extra para manter o
-- ambiente, e que este argumento deveria ser passado a cada invocacao recursiva.
-- No entanto, passar um argumento assim eh outra forma de monad, entao uma
-- instancia Monad apropriada permite que o avaliador seja usado inalterado.

-- Usar funcoes como um monad eh tipicamente chamado de reader monad. Ao avaliar
-- expressoes no reader monad, as seguintes regras sao usadas:
-- * Constantes n avaliam para funcoes constantes λe.n
-- * Operadores aritmeticos avaliam para funcoes que passam seus argumentos
--   adiante, entao f+g avalia para λe.f(e)+g(e)
-- * Operadores customizados avaliam para o resultado de aplicar o operador
--   customizado aos argumentos, entao f OP g avalia para λe.{h(f(e),g(e)) se
--   e contem (OP,h), 0 caso contrario}, com 0 servindo como fallback caso um
--   operador desconhecido seja aplicado.

-- Para definir o reader monad em Lean, o primeiro passo eh definir o tipo
-- Reader e o efeito que permite usuarios obterem o ambiente:

def Reader (ρ : Type) (α : Type) : Type := ρ → α

def read : Reader ρ ρ := fun env => env

-- Por convencao, a letra grega ρ (pronunciada "rho") eh usada para environments.

-- O fato de que constantes em expressoes aritmeticas avaliam para funcoes
-- constantes sugere que a definicao apropriada de pure para Reader eh uma
-- funcao constante:

def Reader.pure (x : α) : Reader ρ α := fun _ => x

-- Por outro lado, bind eh um pouco mais complicado. Seu tipo eh
-- "Reader ρ α → (α → Reader ρ β) → Reader ρ β". Este tipo pode ser mais facil
-- de entender desdobrando a definicao de Reader, que resulta em
-- "(ρ → α) → (α → ρ → β) → (ρ → β)". Deve receber uma funcao que aceita
-- ambiente como seu primeiro argumento, enquanto o segundo argumento deve
-- transformar o resultado da funcao que aceita ambiente em ainda outra funcao
-- que aceita ambiente. O resultado de combinar estes eh em si uma funcao,
-- esperando por um ambiente.

-- Eh possivel usar Lean interativamente para obter ajuda escrevendo esta funcao.
-- O primeiro passo eh escrever os argumentos e tipo de retorno, sendo muito
-- explicito para obter o maximo de ajuda possivel, com um underscore para o
-- corpo da definicao:

def Reader.bind₁ {ρ : Type} {α : Type} {β : Type}
  (result : ρ → α) (next : α → ρ → β) : ρ → β :=
  _

-- Lean fornece uma mensagem que descreve quais variaveis estao disponiveis no
-- escopo, e o tipo esperado para o resultado. O simbolo ⊢, chamado turnstile ou
-- catraca devido a sua semelhanca com entradas de metro, separa as variaveis
-- locais do tipo alvo

-- Como o tipo de retorno eh uma funcao, um bom primeiro passo eh envolver um
-- fun ao redor do underscore:

def Reader.bind₂ {ρ : Type} {α : Type} {β : Type}
  (result : ρ → α) (next : α → ρ → β) : ρ → β :=
  fun env => _

-- A mensagem resultante agora mostra o `env` (argumento da funcao) como
-- variavel local (acima do goal).

-- A unica coisa no contexto que pode produzir um β eh next, e ele requer dois
-- argumentos para fazer isso. Cada argumento pode em si ser um underscore:

def Reader.bind₃ {ρ : Type} {α : Type} {β : Type}
  (result : ρ → α) (next : α → ρ → β) : ρ → β :=
  fun env => next _ _

-- Atacando o primeiro underscore, apenas uma coisa no contexto pode produzir um
-- α, isto eh result:

def Reader.bind₄ {ρ : Type} {α : Type} {β : Type}
  (result : ρ → α) (next : α → ρ → β) : ρ → β :=
  fun env => next (result _) _

-- Agora, ambos underscores tem a mesma mensagem de erro pedindo ρ. Felizmente,
-- ambos underscores podem ser substituidos por env:

def Reader.bind₅ {ρ : Type} {α : Type} {β : Type}
  (result : ρ → α) (next : α → ρ → β) : ρ → β :=
  fun env => next (result env) env

-- A versao final pode ser obtida desfazendo o desdobramento de Reader e
-- limpando os detalhes explicitos:

def Reader.bind
    (result : Reader ρ α)
    (next : α → Reader ρ β) : Reader ρ β :=
  fun env => next (result env) env

-- Nem sempre eh possivel escrever funcoes corretas simplesmente "seguindo os
-- tipos", e carrega o risco de nao entender o programa resultante. No entanto,
-- pode ser mais facil entender um programa que foi escrito do que um que nao
-- foi, e o processo de preencher os underscores pode trazer insights. Neste
-- caso, Reader.bind funciona como bind para Id, extrai o valor de `env` usando
-- result e aplica a funcao next nele, tambem passando o environment adiante.

-- Reader.pure (que gera funcoes constantes) e Reader.bind obedecem o contrato
-- monad.
-- Para verificar que "Reader.bind (Reader.pure v) f" eh o mesmo que
-- "f v", eh so substituir as definicoes:
-- ⟹ Reader.bind (Reader.pure v) f
-- ⟹ fun env => f ((Reader.pure v) env) env
-- ⟹ fun env => f ((fun _ => v) env) env
-- ⟹ fun env => f v env
-- ⟹ f v
-- Para toda funcao f, "fun x => f x" eh o mesmo que f, entao a primeira parte
-- do contrato eh satisfeita.

-- Para verificar que "Reader.bind r Reader.pure" eh o mesmo que r, uma tecnica
-- similar funciona:
-- ⟹ Reader.bind r Reader.pure
-- ⟹ fun env => Reader.pure (r env) env
-- ⟹ fun env => (fun _ => (r env)) env
-- ⟹ fun env => r env
-- Como acoes reader r sao em si funcoes, isso eh o mesmo que r.

-- Para verificar associatividade, a mesma coisa pode ser feita tanto para
-- "Reader.bind (Reader.bind r f) g" quanto para
-- "Reader.bind r (fun x => Reader.bind (f x) g)":
-- ⟹ Reader.bind (Reader.bind r f) g
-- ⟹ fun env => g ((Reader.bind r f) env) env
-- ⟹ fun env => g ((fun env' => f (r env') env') env) env
-- ⟹ fun env => g (f (r env) env) env

-- "Reader.bind r (fun x => Reader.bind (f x) g)" reduz para a mesma expressao:
-- ⟹ Reader.bind r (fun x => Reader.bind (f x) g)
-- ⟹ Reader.bind r (fun x => fun env => g (f x env) env)
-- ⟹ fun env => (fun x => fun env' => g (f x env') env') (r env) env
-- ⟹ fun env => (fun env' => g (f (r env) env') env') env
-- ⟹ fun env => g (f (r env) env) env

-- Assim, uma instancia "Monad (Reader ρ)" eh justificada:

instance : Monad (Reader ρ) where
  pure x := fun _ => x
  bind x f := fun env => f (x env) env

-- Os environments que serao passados ao avaliador de expressoes podem
-- ser representados como listas de pares, com nome e uma funcao para a operacao
-- binaria.

abbrev Env : Type := List (String × (Int → Int → Int))

-- Por exemplo, exampleEnv contem funcoes de maximo e modulo:

def exampleEnv : Env := [("max", max), ("mod", (· % ·))]

-- Lean ja tem uma funcao List.lookup que encontra o valor associado a uma chave
-- em uma lista de pares, entao applyPrimReader precisa apenas verificar se a
-- funcao customizada esta presente no ambiente. Retorna 0 se a funcao eh
-- desconhecida:

def applyPrimReader (op : String) (x : Int) (y : Int) : Reader Env Int :=
  read >>= fun env =>
  match env.lookup op with
  | none => pure 0
  | some f => pure (f x y)

-- Usar evaluateM com applyPrimReader e uma expressao resulta em uma funcao que
-- espera um ambiente. Felizmente, exampleEnv esta disponivel:

open Expr Prim in
#eval
  evaluateM'' applyPrimReader
    (prim (other "max") (prim plus (const 5) (const 4))
      (prim times (const 3)
        (const 2)))
    exampleEnv

-- Como Many, Reader eh um exemplo de um efeito que eh dificil de codificar na
-- maioria das linguagens, mas type classes e monads o tornam tao conveniente
-- quanto qualquer outro efeito.


-- 4.3.3.4. Exercises


-- 4.3.3.4.1. Checking Contracts

-- Check the monad contract for `State σ` and `Except ε`.

/-
  First, pure should be a left identity of bind. That is, bind (pure v) f should
    be the same as f v.
  Secondly, pure should be a right identity of bind, so bind v pure is the same
    as v.
  Finally, bind should be associative, so bind (bind v f) g is the same as
    bind v (fun x => bind (f x) g).
-/
#check State
#check Except

-- bind (pure v) f
-- ⟹ bind (fun s => (s, x)) f
-- ⟹ fun s =>
--      let (s', x) := (fun s => (s, x)) s
--      f x s'
-- ⟹ fun s =>
--      let (s', x) := (s, v) s
--      f x s'
-- ⟹ fun s => f v s

-- bind (Except.ok v) pure
-- ⟹ match v with
--     | Except.error v => Except.error v
--     | Except.ok v    => pure v
-- ⟹ pure v
-- ⟹ Except.ok v


-- 4.3.3.4.2. Readers with Failure

-- Adapt the reader monad example so that it can also indicate failure when the
-- custom operator is not defined, rather than just returning zero. In other
-- words, given these definitions:

def ReaderOption (ρ : Type) (α : Type) : Type := ρ → Option α
def ReaderExcept (ε : Type) (ρ : Type) (α : Type) : Type := ρ → Except ε α

-- do the following:

/-
  * Write suitable pure and bind functions
  * Check that these functions satisfy the Monad contract
  * Write Monad instances for ReaderOption and ReaderExcept
  * Define suitable applyPrim operators and test them with evaluateM on some example expressions
-/

def ReaderOption.pure (x : α) : ReaderOption ρ α :=
  fun _ => Option.some x

def ReaderOption.bind
    (result : ReaderOption ρ α) (next : α → ReaderOption ρ β) : ReaderOption ρ β :=
  fun env =>
    result env >>= fun x => next x env
  -- ou
  -- fun env =>
  --   match result env with
  --   | Option.some x => next x env
  --   | Option.none   => Option.none

instance : Monad (ReaderOption ρ) where
  pure := ReaderOption.pure
  bind := ReaderOption.bind

def ReaderExcept.pure (x : α) : ReaderExcept ε ρ α :=
  fun _ => Except.ok x

def ReaderExcept.bind
    (result : ReaderExcept ε ρ α)
    (next : α → ReaderExcept ε ρ β) : ReaderExcept ε ρ β :=
  fun env =>
    result env >>= fun x => next x env

instance : Monad (ReaderExcept ε ρ) where
  pure := ReaderExcept.pure
  bind := ReaderExcept.bind

def readOptionEnv : ReaderOption ρ ρ := fun env => Option.some env
def readExceptEnv : ReaderExcept ε ρ ρ := fun env => Except.ok env

def applySpecialReaderOption (op : String) (x : Int) (y : Int) : ReaderOption Env Int :=
  readOptionEnv >>= fun env =>
    match env.lookup op with
    | none => fun _ => none
    | some f => pure (f x y)

def applySpecialReaderExcept (op : String) (x : Int) (y : Int) : ReaderExcept String Env Int :=
  readExceptEnv >>= fun env =>
    match env.lookup op with
    | none => fun _ => Except.error s!"unknown operation: {op}"
    | some f => pure (f x y)

-- Teste 1: Usa operador que EXISTE ("max")
open Expr Prim in
def exprMaxExists : Expr (Prim String) :=
  prim (other "max") (const 10) (const 5)

-- Teste 2: Usa operador que NAO EXISTE ("pow")
open Expr Prim in
def exprPowNotExists : Expr (Prim String) :=
  prim (other "pow") (const 2) (const 3)

-- Teste 3: Combina operadores normais com custom que existe ("mod")
open Expr Prim in
def exprMixedExists : Expr (Prim String) :=
  prim plus (const 10)
    (prim (other "mod") (const 17) (const 5))

-- Teste 4: Combina operadores normais com custom que NAO existe ("div")
open Expr Prim in
def exprMixedNotExists : Expr (Prim String) :=
  prim times (const 2)
    (prim (other "div") (const 10) (const 3))

-- Teste 5: mais complexa com multiplos operadores custom
open Expr Prim in
def exprComplexMixed : Expr (Prim String) :=
  prim (other "max")
    (prim plus (const 5) (const 3))
    (prim (other "mod") (const 20) (const 7))

-- Teste 6: Apenas operadores normais (sem custom)
open Expr Prim in
def exprNormalOnly : Expr (Prim String) :=
  prim plus
    (prim times (const 3) (const 4))
    (prim minus (const 20) (const 5))

-- Eval com ReaderOption:

#eval evaluateM'' applySpecialReaderOption exprMaxExists exampleEnv
#eval evaluateM'' applySpecialReaderOption exprPowNotExists exampleEnv
#eval evaluateM'' applySpecialReaderOption exprMixedExists exampleEnv
#eval evaluateM'' applySpecialReaderOption exprMixedNotExists exampleEnv
#eval evaluateM'' applySpecialReaderOption exprComplexMixed exampleEnv
#eval evaluateM'' applySpecialReaderOption exprNormalOnly exampleEnv

-- Eval com ReaderExcept:

#eval evaluateM'' applySpecialReaderExcept exprMaxExists exampleEnv
#eval evaluateM'' applySpecialReaderExcept exprPowNotExists exampleEnv
#eval evaluateM'' applySpecialReaderExcept exprMixedExists exampleEnv
#eval evaluateM'' applySpecialReaderExcept exprMixedNotExists exampleEnv
#eval evaluateM'' applySpecialReaderExcept exprComplexMixed exampleEnv
#eval evaluateM'' applySpecialReaderExcept exprNormalOnly exampleEnv


-- 4.3.3.4.3. A Tracing Evaluator

-- The WithLog type can be used with the evaluator to add optional tracing of
-- some operations. In particular, the type ToTrace can serve as a signal to
-- trace a given operator:

inductive ToTrace (α : Type) : Type where
  | trace : α → ToTrace α

-- For the tracing evaluator, expressions should have type
-- `Expr (Prim (ToTrace (Prim Empty)))`. This says that the operators in the
-- expression consist of addition, subtraction, and multiplication, augmented
-- with traced versions of each. The innermost argument is Empty to signal that
-- there are no further special operators inside of trace, only the three basic ones.

/-
  * Implement a Monad (WithLog logged) instance
  * Write an applyTraced function to apply traced operators to their arguments,
    logging both the operator and the arguments, with type
    `ToTrace (Prim Empty) → Int → Int → WithLog (Prim Empty × Int × Int) Int`
-/

#check (Prim (ToTrace (Prim Empty)))

def WithLog.pure (x : α) : WithLog logged α :=
  {log := [], val := x}

def WithLog.bind
    (result : WithLog γ α) (next : α → WithLog γ β) : WithLog γ β :=
  let {log := thisOut, val := thisRes} := result
  let {log := nextOut, val := nextRes} := next thisRes
  {log := thisOut ++ nextOut, val := nextRes}

instance : Monad (WithLog logged) where
  pure := WithLog.pure
  bind := WithLog.bind

-- def applyTraced (op : ToTrace (Prim Empty)) (x : Int) (y :Int)
--     : WithLog (Prim Empty × Int × Int) Int :=
--   match op with
--   | ToTrace.trace o =>
--     match o with
--     | Prim.plus => {log := [(o, x, y)], val := x + y}
--     | Prim.minus => {log := [(o, x, y)], val := x - y}
--     | Prim.times => {log := [(o, x, y)], val := x * y}

def applyTraced : ToTrace (Prim Empty) → Int → Int → WithLog (Prim Empty × Int × Int) Int
  | ToTrace.trace o, x, y =>
    save (o, x, y) >>= fun () =>
    applyPrim' (fun e => nomatch e) o x y

-- Dica: valores do tipo Prim Empty vao aparecer no log resultante. Para serem
-- mostrados como resultado do #eval precisam das instancias:

deriving instance Repr for WithLog
deriving instance Repr for Empty
deriving instance Repr for Prim

-- Teste:
open Expr Prim ToTrace in
#eval
  evaluateM'' applyTraced
    (prim (other (trace times))
      (prim (other (trace plus)) (const 1)
        (const 2))
      (prim (other (trace minus)) (const 3)
        (const 4)))

-- Deve virar:
-- { log := [(Prim.plus, 1, 2), (Prim.minus, 3, 4), (Prim.times, 3, -1)], val := -3 }


-- 4.4. do-Notation for Monads

-- Embora APIs baseadas em monads sejam muito poderosas, o uso explicito de `>>=`
-- com funcoes anonimas ainda eh um pouco feio. Entao pode ser usada da mesma
-- do-notation usada para escrever programas em IO (tambem eh um monad).

-- A primeira traducao de `do` eh usada quando a unica declaracao no `do` eh uma
-- expressao E. Neste caso, o `do` eh removido, entao:
-- ⟹ do E
-- ⟹ E
-- _APENAS FACA `E`_

-- A segunda eh usada quando a primeira declaracao do `do` eh um let com seta,
-- vinculando uma variavel local. Isso traduz para um uso de `>>=` junto com
-- uma funcao anonima que usa essa mesma variavel, entao:
-- ⟹ do let x ← E₁
--       Stmt x
--       …
--       Eₙ
-- ⟹ E₁ >>= fun x =>
--     do Stmt
--        …
--        Eₙ
-- _Executa E₁ pega o resultado e usa depois_

-- Quando a primeira declaracao do bloco `do` eh uma expressao, entao eh considerada
-- uma acao monadica que retorna Unit, entao a funcao corresponde ao construtor
-- Unit e:
-- ⟹ do E₁
--       Stmt
--       …
--       Eₙ
-- ⟹ E₁ >>= fun () =>
--     do Stmt
--        …
--        Eₙ
-- _Executa E₁ (que deve retornar `m Unit`), ignora o (), e continua_

-- Finalmente, quando a primeira declaracao do bloco `do` eh um let que usa :=,
-- a forma traduzida eh uma expressao let comum, entao:
-- ⟹ do let x := E₁
--       Stmt
--       …
--       Eₙ
-- ⟹ let x := E₁
--   do Stmt
--      …
--      Eₙ
-- _Mesma coisa `let x := ⋯` dentro ou fora (nao tem efeito)_

-- A definicao de firstThirdFifthSeventh que usa a classe Monad se parece com:

def firstThirdFifthSeventhBind [Monad m] (lookup : List α → Nat → m α)
    (xs : List α) : m (α × α × α × α) :=
  lookup xs 0 >>= fun first =>
  lookup xs 2 >>= fun third =>
  lookup xs 4 >>= fun fifth =>
  lookup xs 6 >>= fun seventh =>
  pure (first, third, fifth, seventh)

-- Usando do-notation, se torna significativamente mais legivel:

def firstThirdFifthSeventhDo [Monad m] (lookup : List α → Nat → m α)
    (xs : List α) : m (α × α × α × α) := do
  let first ← lookup xs 0
  let third ← lookup xs 2
  let fifth ← lookup xs 4
  let seventh ← lookup xs 6
  pure (first, third, fifth, seventh)

-- Sem a type class Monad, a funcao number que numera os nos de uma arvore foi
-- escrita:

def numberOld (t : BinTree α) : BinTree (Nat × α) :=
  let rec helper : BinTree α → State Nat (BinTree (Nat × α))
    | BinTree.leaf => okState BinTree.leaf
    | BinTree.branch left x right =>
      helper left ~~> fun numberedLeft =>
      getState ~~> fun n =>
      setState (n + 1) ~~> fun () =>
      helper right ~~> fun numberedRight =>
      okState (BinTree.branch numberedLeft (n, x) numberedRight)
  (helper t 0).snd

-- Com Monad e `do`, sua definicao eh muito menos feia:

def numberDo (t : BinTree α) : BinTree (Nat × α) :=
  let rec helper : BinTree α → State Nat (BinTree (Nat × α))
    | BinTree.leaf => pure BinTree.leaf
    | BinTree.branch left x right => do
      let numberedLeft ← helper left
      let n ← getState
      setState (n + 1)
      let numberedRight ← helper right
      pure (BinTree.branch numberedLeft (n, x) numberedRight)
  (helper t 0).snd

-- Todas as conveniencias do `do` com IO tambem estao disponiveis ao usa-lo com
-- outros monads. Por exemplo, acoes aninhadas tambem funcionam em qualquer monad.
-- A definicao original de mapM era:

def mapMBind [Monad m] (f : α → m β) : List α → m (List β)
  | [] => pure []
  | x :: xs =>
    f x >>= fun hd =>
    mapMBind f xs >>= fun tl =>
    pure (hd :: tl)

-- Com do-notation, pode ser escrita:

def mapMDo [Monad m] (f : α → m β) : List α → m (List β)
  | [] => pure []
  | x :: xs => do
    let hd ← f x
    let tl ← mapMDo f xs
    pure (hd :: tl)

-- Usando acoes aninhadas a torna quase tao curta quanto o map original
-- nao-monadico:

def mapMNested [Monad m] (f : α → m β) : List α → m (List β)
  | [] => pure []
  | x :: xs => do
    pure ((← f x) :: (← mapMNested f xs))

-- Usando acoes aninhadas, number pode ser tornado muito mais conciso:

def incrementDo : State Nat Nat := do
  let n ← getState
  setState (n + 1)
  pure n

def numberNested (t : BinTree α) : BinTree (Nat × α) :=
  let rec helper : BinTree α → State Nat (BinTree (Nat × α))
    | BinTree.leaf => pure BinTree.leaf
    | BinTree.branch left x right => do
      pure
        (BinTree.branch
          (← helper left)
          ((← incrementDo), x)
          (← helper right))
  (helper t 0).snd


-- 4.4.1. Exercises

/-
  * Rewrite evaluateM, its helpers, and the different specific use cases using
    do-notation instead of explicit calls to >>=.

  * Rewrite firstThirdFifthSeventh using nested actions.
-/

def evaluateMDo [Monad m] (applyPrim : Arith → Int → Int → m Int) :
    Expr Arith → m Int
  | Expr.const i => pure i
  | Expr.prim p e1 e2 => do
    let v1 ← evaluateMDo applyPrim e1
    let v2 ← evaluateMDo applyPrim e2
    applyPrim p v1 v2
    -- applyPrim p (← evaluateMDo applyPrim e1) (← evaluateMDo applyPrim e2)

def firstThirdFifthSeventhMonadDo [Monad m] (lookup : List α → Nat → m α)
    (xs : List α) : m (α × α × α × α) := do
  pure ((← lookup xs 0), (← lookup xs 2), (← lookup xs 4), (← lookup xs 6))


-- 4.5. The IO Monad
