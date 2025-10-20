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
