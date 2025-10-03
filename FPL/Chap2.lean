-- 2. Hello, World!


-- 2.1. Running a Program

def main₁ : IO Unit := IO.println "Hello, world!"
-- lean --run FPL/Chap2.lean

-- Quando chamado com a flag `--run`, Lean invoca a definicao `main`, que
-- caso nao receba nenhum argumento, sera do tipo IO Unit. Isso significa
-- que main nao eh uma funcao.

-- `IO α` eh o monad que faz efeito colateral ou um erro.


-- 2.1.2. Functional Programming vs Effects

-- Como uma linguagem funcional pode suportar side effects?
/-
Imagine um cafe que tem dois funcionarios: um cozinheiro que atende aos
pedidos e um funcionario no balcao que interage com os clientes e anota
os pedidos. O cozinheiro eh muito bom em fazer as comidas pelas quais o
cafe eh conhecido, mas para fazer isso, o cozinheiro precisa de paz e
tranquilidade, entao prefere nao ter nenhum contato com o mundo exterior.
O atendente do balcão, ao contrario, eh simpatico mas completamente
incompetente na cozinha. Entao, os clientes interagem com o atendente,
que delega todo o trabalho de cozinhar ao cozinheiro. Se o cozinheiro
tiver alguma duvida com o pedido do cliente, ele envia um bilhete ao
atendente do balcao, que interage com o cliente e passa um bilhete de
volta ao cozinheiro com a resposta.
-/

-- Nessa analogia o cozinheiro eh o Lean, que trabalha de forma isolada
-- e o atendente eh o sistema de runtime, que interage com o mundo real.
-- No Lean, expressoes puras produzem IO como descricoes de tarefas a
-- serem feitas, nao os efeitos em si.


-- 2.1.3. Real-World Functional Programming

-- Side effects tambem podem ser tratados como funcoes IO que recebem um
-- "mundo" como argumento e retornam um valor junto com um "novo mundo".
-- Assim, ler do stdin eh puro porque um mundo diferente eh fornecido a
-- cada chamada. E escrever para stdout eh puro porque o mundo retornado
-- eh diferente do inicial.

-- Na pratica, o Lean usa um token abstrato que representa o mundo.
-- Este token eh um valor do tipo RealWorld que nunca eh inspecionado ou
-- reordenado pelo codigo Lean.

-- Programas reais podem ser pensados como uma sequencias de efeitos.
-- Lean tem `do` notation para compor acoes IO primitivas em programas maiores.


-- 2.1.4. Combining IO Actions

def main₂: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  stdout.putStrLn "How would you like to be addressed?"
  let input ← stdin.getLine
  let name := input.dropRightWhile Char.isWhitespace

  stdout.putStrLn s!"Hello, {name}!"

-- `main₂` descreve uma sequencia de acoes que sao executadas em ordem,
-- utilizando o `do` block.

-- Em um `do` block, `let` tem significado ligeiramente diferente:
-- Bindings com `let` ficam disponiveis em todas as declaracoes restantes
-- do bloco, nao apenas na proxima.
--   `let name := expr` usa `:=` para valores comuns
--   `let name ← expr` usa `←` quando expr eh uma acao IO a ser executada


-- `IO.getStdin` e `IO.getStdout` sao acoes IO (nao variaveis globais como em C)
-- para permitir que stdin/stdout sejam localmente substituidos no programa.


-- 2.2. Step By Step

/- do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  stdout.putStrLn "How would you like to be addressed?"
  let input ← stdin.getLine
  let name := input.dropRightWhile Char.isWhitespace
  stdout.putStrLn s!"Hello, {name}!"
-/


-- 2.2.1. Standard IO

-- Para executar `let stdin ← IO.getStdin`:
-- 1. Avalia a expressao na direita da seta (IO.getStdin)
-- 2. Performa a acao IO resultante
-- 3. Associa o valor retornado (tipo IO.FS.Stream) ao nome `stdin`
--
-- `let stdout ← IO.getStdout` funciona na mesma logica:
-- Avalia IO.getStdout, executa a acao, associa `stdout` ao nome.


-- 2.2.2. Asking a Question

-- `stdout.putStrLn "How would you like to be addressed?"`
--   putStrLn tem tipo IO.FS.Stream → String → IO Unit
--   Usa accessor notation (.) para chamada de funcao
--   Expressao eh avaliada para uma acao IO, depois executada
--   Escreve string + newline para stdout

-- `let input ← stdin.getLine`
--   getLine tem tipo IO.FS.Stream → IO String
--   Executa acao IO, esperando o usuario digitar linha completa

-- `let name := input.dropRightWhile Char.isWhitespace`
--   Usa `:=` em vez de `←`. Nao tem nenhuma acao IO, apenas avaliacao

#eval "Hello!!!".dropRightWhile (· == '!')
#eval "Hello!1!??*".dropRightWhile (not ∘ Char.isAlphanum)


-- 2.2.3. Greeting the User

-- `stdout.putStrLn s!"Hello, {name}!"`
--   String interpolation constroi "Hello, David!"
--   Expressao eh avaliada para um IO


-- 2.2.4. IO Actions as Values

-- Por que separar avaliacao de execucao de acoes IO:

-- 1. Programas devem ser explicitos sobre quais funcoes tem side effects
--    Partes sem efeitos sao mais faceis de entender matematicamente
-- 2. Nem todas acoes IO precisam ser executadas imediatamente
--    Funcoes comuns podem ser usadas como estruturas de controle

def twice (action : IO Unit) : IO Unit := do
  action
  action

#eval twice (IO.println "shy")

/-
Given `a : α`, then `pure a : f α` represents an action that does nothing and returns `a`.
  `(pure "hello" : Option String) = some "hello"`
  `(pure "hello" : Except (Array String) String) = Except.ok "hello"`
-/
#eval (pure "hello" : IO String)
#eval (pure (): IO Unit) -- sem IO e sem retorno
-- `pure` cria uma acao IO que nao possui efeitos colaterais, apenas retorna o seu
-- argumento (Unit). Assim, nao produz nenhuma acao e tambem nao retorna nada.
def nTimes (action : IO Unit) : Nat → IO Unit
  | 0 => pure ()
  | n + 1 => do
    action
    nTimes action n

 #eval nTimes (IO.println "Hello") 3

-- countdown nao tem efeitos colaterais, apenas constroi lista de acoes
-- que podem ser avaliadas e executadas depois.
def countdown : Nat → List (IO Unit)
  | 0 => [IO.println "Blast off!"]
  | n + 1 => IO.println s!"{n + 1}" :: countdown n

-- runActions nao executa as acoes - cria uma NOVA acao que as executara.
def runActions : List (IO Unit) → IO Unit
  | [] => pure ()
  | act :: actions => do
    act
    runActions actions

#eval runActions $ countdown 5

def runRevActions : List (IO Unit) → IO Unit
  | []      => pure ()
  | x :: xs => do
    runRevActions xs
    x

#eval runRevActions $ countdown 3


-- 2.2.5. Exercise

def main₃ : IO Unit := do
  let englishGreeting := IO.println "Hello!"
  IO.println "Bonjour!"
  englishGreeting

/-
Define uma acao de IO que se chama `main₃` e nao retorna nada.
Inicia um bloco de acoes ordenadas utilizado o `do`.

Avalia imediatamente (lean NAO eh lazy) a expressao `IO.println "Hello!"`,
  gerando uma acao IO que eh atribuida ao nome `englishGreeting`.
Avalia a expressao `IO.println "Bonjour!"`,  gerando uma acao IO que tem
  seu side effect executado imediatamente.
Avalia a expressao `englishGreeting`, e executa seu side effect.
-/


-- 2.3. Starting a Project

-- Lake eh configurado em um arquivo TOML que descreve as dependencias e
-- o que sera buildado.


-- 2.3.1. First steps

-- `lake new <nome>` cria um diretorio com os seguintes conteudos:
-- 1. `Main.lean` o arquivo que o compilador Lean vai olhar para
--   a acao princial.
-- 2. `<Nome>.lean` e `Nome/Basic.lean` sao a estrutura de uma
--   biblioteca de apoio para o programa.
-- 3. `lakefile.toml` contem a configuracao que o lake usa para buildar tudo.
-- 4. `lean-toolchain` contem a versao do Lean que eh usada no projeto.

-- `lake new` tambem cria um repo git, e um gitignore.

-- Geralmente, a maioria da logica do programa vai estar em uma colecao de
-- bibliotecas, enquanto o `Main.lean` vai ser um wrapper sobre elas,
-- fazendo parsing de argumentos e executando a logica central.

-- `lake init` cria um projeto em uma pasta que ja existe.


-- 2.3.2. Lakefiles

-- Cada `lakefile.toml` descreve um package, que pode conter varios
-- executaveis.

-- Um lakefile gerado contem:
-- 1. Settings do package.
-- 2. Declaracao de biblioteca.
-- 3. Declaracao do executavel.

/-
name = "projetoExemplo"
version = "0.1.0"
defaultTargets = ["projetoExemplo"]

[[lean_lib]]
name = "ProjetoExemplo"

[[lean_exe]]
name = "projetoExemplo"
root = "Main"
-/

-- Por convencao, o nome do pacote e executavel comeca com letra
--   minuscula e de bibliotecas com maiuscula.
-- Dependencies sao declaracoes de outros pacotes Lean (locais ou remotos)
-- Os itens no arquivo de configuracao permitem configurar localizacao
--   de arquivos fonte, hierarquias de modulos e flags do compilador.

-- Lakefiles em formato Lean podem conter adicionalmente:
--   External libraries: bibliotecas nao-Lean para link estatico
--   Custom targets: alvos que nao se encaixam em biblioteca/executavel
--   Scripts: acoes IO similares a main, mas com acesso aos metadados
--   da configuracao do pacote


-- 2.3.3. Libraries and Imports

-- Uma biblioteca Lean consiste de uma colecao de source organizados
-- hierarquicamente que podem ser importados, chamados modulos.

-- Por padrao, uma biblioteca tem um arquivo raiz que corresponde ao seu nome.
-- Modulos adicionais podem ser adicionados criando um diretorio com o nome
-- da biblioteca e colocando arquivos dentro. Esses nomes podem ser importados
-- substituindo o separador de diretorio por ponto:
-- Arquivo Greeting/Smile.lean pode ser importado como `import Greeting.Smile`

-- Hierarquia de modulos eh DESACOPLADA da hierarquia de namespaces
-- - Modulos sao unidades de DISTRIBUICAO de codigo
-- - Namespaces sao unidades de ORGANIZACAO de codigo
-- Nomes definidos no modulo Greeting.Smile NAO ficam automaticamente
-- no namespace Greeting.Smile - podem ficar em qualquer namespace

-- `import` torna conteudo do arquivo disponivel
-- `open` torna nomes de um namespace disponiveis sem prefixos

-- `open Expression` torna Expression.happy acessivel como apenas `happy`
-- Namespaces podem ser abertos seletivamente: `open Nat (toFloat)`
-- torna apenas Nat.toFloat disponivel como `toFloat`

-- Lib "Greeting"     (declarada no lakefile.toml)
-- ── Greeting.lean      (modulo raiz da biblioteca)
-- ── Greeting/          (pasta para organizar modulos)
--    ├── Smile.lean     (modulo Greeting.Smile)
--    └── Wave.lean      (modulo Greeting.Wave)


-- 2.4. Worked Example: cat


-- 2.4.1. Getting started

-- Criar o package com `lake new feline` e remover a lib.
--   ou usar `lake new feline exe`, que nao cria uma biblioteca.


-- 2.4.2. Concatenating Streams


/- 2.4.2.1. Streams -/

-- 1. Define um buffer de 20kb
-- 2. Cria uma funcao `dumb` que le um buffer por vez de um stream e o escreve
--   no stdout, a funcao precisa ser marcada com `partial` porque o Lean nao
--   percebe reducao na recursao. Quando marcada `partial`, Lean nao precisa
--   de uma prova mostrando que ela termina. Isso deixa a possibilidade de
--   recursao infinita (caso leia `/dev/random` por exemplo).

-- O IO.FS.Stream, representa uma stream POSIX com suas operacoes, o campo
-- `isTty` usa o tipo `BaseIO`, uma variacao do `IO` que nao pode devolver erros.
-- Ele usa o mesmo EIO como base, mas ao contrario do `IO` (que passa um Erro
-- como argumento) ele usa o `Empty`.
#check BaseIO
-- Como a recursao eh a ultima expressao, ou seja, o codigo que "ja passou" e
-- nao mantem nenhum estado, o compilador otimiza usando tail recursion,
-- evitando a possibilidade de stack overflow.

-- 3. Para permitir a leitura de arquivos a funcao fileStream recebe um path
--   de um arquivo e retorna um IO (Option IO.FS.Stream), ou seja, o path pode
--   conter um arquivo valido ou invalido. Caso nao seja valido, usa o `pure`
--   para retornar Unit, se nao, cria um handler para o arquivo em read-mode e
--   o converte, preenchendo uma structure Stream de acordo com o arquivo.

-- `pure` faz "lift" do valor para o monad IO.


/- 2.4.2.2. Handling Input -/

-- O loop principal (`process`) eh outra funcao tail-recursive. Para poder
-- retornar um exit code diferente se nenhum input for lido, ele recebe com
-- argumento indicando o "status" do programa. Alem disso, recebe a lista de
-- arquivos que devem ser processados.

-- 1. Nao tem mais arquivos para ser processados, sai com exitCode normal.
-- 2. O argumento eh "-", le o stdin e processa os proximos arquivos.
-- 3. O argumento eh um arquivo, se nao pode ser lido eh pulado e o exitcode 1.

-- `⟨fileName⟩` eh usado para construir o System.FilePath usando a string.


/- 2.4.2.3. Main -/

-- Em Lean existem tres tipos de main: `main : IO Unit` nao le args e sempre
-- retorna 0, `main : IO UInt32` nao le args e retorna um exit code e
-- `main : List String → IO UInt32` que le args e retorna exit code.


-- 2.4.3. Meow!

-- `lake build feline` e `echo "oi feline" | lake exe feline` para testar e
-- `lake exe feline - Main.lean` para digitar e testar com arquivo.


-- 2.4.4. Exercise

-- Adcionada funcao `printHelp` que trata o argumento "--help" diretamente
-- na main, printando as opcoes do programa.


-- 2.5. Additional Conveniences


-- 2.5.1. Nested Actions

-- Varias funcoes do feline repetem a acao de nomear o resultado de uma acao
-- IO (`let ... ← ...`) que eh usada apenas uma vez.

/-
partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let buf ← stream.read bufsize
  if buf.isEmpty then
    pure ()
  else
    let stdout ← IO.getStdout  -- NOMEIA
    stdout.write buf           -- USA UMA VEZ
    dump stream
-/

-- Quando o Lean compila um `do` block, expressoes com seta esquerda (←)
-- imediatamente sob parenteses sao "elevadas" para o `do` mais proximo
-- e seus resultados sao ligados a um nome unico. Esse nome unico substitui
-- a expressao original.

/-
partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let buf ← stream.read bufSize
  if buf.isEmpty then
    pure ()
  else
    (← IO.getStdout).write buf  -- acao IO aninhada
    dump stream
-/

-- fileStream tambem pode ser simplificado:
def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  if not (← filename.pathExists) then          -- nested action
    (← IO.getStderr).putStrLn s!"File not found: {filename}"  -- nested action
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

-- Acoes IO que o Lean eleva de contexto de expressoes aninhadas
-- sao chamadas "nested actions". Evitam nomes usados apenas uma vez,
-- simplificando o programa. Mas ainda pode ser util nomear resultados
-- intermediarios em expressoes longas e complicadas.

-- IMPORTANTE: Os efeitos colaterais ainda ocorrem na mesma ordem, e
--   execucao de efeitos NAO eh intercalada com avaliacao de expressoes.
-- Por isso, nested actions NAO podem ser elevadas dos ramos de um `if`.

def getNumA : IO Nat := do
  (← IO.getStdout).putStrLn "A"
  pure 5

def getNumB : IO Nat := do
  (← IO.getStdout).putStrLn "B"
  pure 7

-- Nao compila:
def test : IO Unit := do
  let a : Nat := if (← getNumA) == 5 then 0 else (← getNumB)
  (← IO.getStdout).putStrLn s!"The answer is {a}"

-- Se fosse permitido, seria equivalente a:
def testEquivalente : IO Unit := do
  let x ← getNumA  -- SEMPRE executa
  let y ← getNumB  -- SEMPRE executa (mesmo se x == 5)
  let a : Nat := if x == 5 then 0 else y
  (← IO.getStdout).putStrLn s!"The answer is {a}"

-- Isso executaria getNumB mesmo quando getNumA == 5 (estragando o proposito do
-- `if`), causando efeitos colaterais desnecessarios. Para prevenir isso, nested
-- actions nao sao permitidas em `if` que nao seja uma linha do `do` block.

def testCorreto : IO Unit := do
  let x ← getNumA
  let a ← if x == 5 then
    pure 0
  else do
    let y ← getNumB
    pure y
  (← IO.getStdout).putStrLn s!"The answer is {a}"


-- 2.5.2. Flexible Layouts for do

-- Expressoes `do` no Lean sao sensiveis a whitespace. Cada acao IO ou
-- binding local no `do` deve comecar em sua propria linha, e todos devem
-- ter a mesma indentacao. Quase todos os usos de `do` devem ser escritos
-- dessa forma.

-- Em contextos raros, pode ser necessario controle manual sobre whitespace
-- e indentacao, ou pode ser conveniente ter multiplas acoes pequenas em
-- uma unica linha. Nesses casos:
--   Newlines podem ser substituidas por ponto e virgula (;)
--   Indentacao pode ser substituida por chaves {}

-- Todas essas versoes sao equivalentes:

-- Versao padrao (usando apenas layout sensivel a whitespace):
def mainLayout : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  stdout.putStrLn "How would you like to be addressed?"
  let name := (← stdin.getLine).trim
  stdout.putStrLn s!"Hello, {name}!"

-- Versao explicita com chaves e ponto e virgula:
def mainExplicit : IO Unit := do {
  let stdin ← IO.getStdin;
  let stdout ← IO.getStdout;
  stdout.putStrLn "How would you like to be addressed?";
  let name := (← stdin.getLine).trim;
  stdout.putStrLn s!"Hello, {name}!"
}

-- Versao hibrida (duas acoes na mesma linha):
def mainHybrid : IO Unit := do
  let stdin ← IO.getStdin; let stdout ← IO.getStdout
  stdout.putStrLn "How would you like to be addressed?"
  let name := (← stdin.getLine).trim
  stdout.putStrLn s!"Hello, {name}!"

-- Codigo Lean idiomatico usa chaves com `do` muito raramente.
-- A versao com whitespace eh preferida.


-- 2.5.3. Running IO Actions With #eval

-- O comando `#eval` do Lean pode ser usado para executar acoes IO,
-- nao apenas avalia-las. Normalmente, `#eval` avalia a expressao fornecida,
-- converte o valor resultante para string e mostra como tooltip.

-- Em vez de falhar porque acoes IO nao podem ser convertidas para strings,
-- `#eval` as executa, realizando seus efeitos colaterais. Se o resultado
-- da execucao eh Unit (), nenhuma string eh mostrada, mas se eh um tipo
-- que pode ser convertido para string, o Lean exibe o valor resultante.

#eval runActions (countdown 3)
-- Saída:
-- 3
-- 2
-- 1
-- Blast off!

-- Esta saida eh produzida pela execucao da acao IO, nao alguma representacao
-- opaca da acao em si. `#eval` tanto avalia a expressao fornecida quanto
-- executa o valor da acao resultante.

-- Testar rapidamente acoes IO com `#eval` pode ser muito mais conveniente
-- que compilar e executar programas inteiros. Porem, ha algumas limitacoes:

-- 1. Ler do standard input simplesmente retorna input vazio.
-- 2. A acao IO eh re-executada sempre que o Lean precisa atualizar
--    informacoes de diagnostico, e isso pode acontecer em momentos
--    imprevisiveis.
-- 3. Uma acao que le e escreve arquivos pode fazer isso inesperadamente.


-- 2.6. Summary


-- 2.6.1. Evaluation vs Execution

-- Side effects sao aspectos da execucao de programas que vao alem da
-- avaliacao de expressoes matematicas. Enquanto a maioria das linguagens
-- permite side effects durante a avaliacao, o Lean nao permite.

-- Em vez disso, Lean tem um tipo chamado `IO` que representa DESCRICOES
-- de programas que usam side effects. Essas descricoes sao entao executadas
-- pelo sistema de runtime da linguagem, que invoca o avaliador de expressoes
-- Lean para realizar computacoes especificas.

-- Valores do tipo `IO α` sao chamados "IO actions". A mais simples eh `pure`,
-- que retorna seu argumento e nao tem side effects reais.

-- Uma acao IO `main` eh executada quando o programa inicia.
-- `main` pode ter um de tres tipos:

-- `main : IO Unit`: Sem argumentos e sem exit code.
-- `main : IO UInt32`: Sem argumentos e com exit code.
-- `main : List String → IO UInt32`: Com argumentos e com exit code.


--  2.6.2. do Notation

-- IO Actions basicas sao compostas usando do notation, uma expressao `do`
-- contem uma sequencia de statements que podem ser:
--   Expressoes que representam acoes IO.
--   Definicoes com `let` e `:=` onde o nome se refere ao valor de uma
--     expresao.
--   Definicoes com `let` e `←` onde o nome se refere ao resultado de
--     executar o valor da expressao fornecida.

-- IO Actions escritas com `do` sao executadas um statement por vez.

-- Expressoes `if` e `match` que ocorrem imediatamente sob um `do` sao
-- implicitamente consideradas como tendo seu proprio `do` em cada ramo.
-- Dentro de uma expressao `do`, nested actions sao expressoes com seta
-- esquerda imediatamente sob parenteses. O compilador Lean implicitamente
-- as eleva para o `do` mais proximo, que pode ser implicitamente parte de
-- um ramo de uma expressao `match` ou `if`, e lhes da um nome unico.
-- Esse nome unico entao substitui o local de origem da nested action.

def exemploIf : IO Unit := do
  if ← (← IO.getStdout).isTty then
    IO.println "Running in terminal"
  else
    IO.println "Running in pipe/redirect"

-- Equivale a:
def exemploIfExplicito : IO Unit := do
  let stdout ← IO.getStdout
  let isTty ← stdout.isTty
  if isTty then
    IO.println "Running in terminal"
  else
    IO.println "Running in pipe/redirect"


-- 2.6.3. Compiling and Running Programs

-- Um programa que consiste em um unico arquivo com uma definicao "main" pode
--   ser rodado com `lake --run FILE`.
-- Projetos Lean sao organizados em packages, que sao colecoes de bibliotecas
--   (libraries) e executaveis juntos com informacoes sobre dependencias e
--   configuracao de build.
-- Packages sao descritos usando a build tool lake, use `lake new <nome> <template>`
--   para criar um package em um novo diretorio ou `lake init` para criar um
--   no diretorio atual e `lake build` para buildar um projeto.


-- 2.6.4. Partiality

-- No modelo matematico, toda expressao deve ter um valor. Isso exclui
-- pattern matches incompletos e programas que podem entrar em loop infinito.
-- O Lean garante que todos os `match` cubram todos os casos e que todas
-- as funcoes recursivas sejam estruturalmente recursivas ou tenham prova
-- explicita de terminacao.

-- Porem, alguns programas reais precisam da possibilidade de loop infinito
-- para lidar com dados potencialmente infinitos (ex: streams POSIX).
-- O Lean fornece uma "saida" para isso: funcoes marcadas com `partial`
-- nao precisam seguir estes principios.

-- Isso tem um custo:
-- 1. Funcoes parciais nao sao avaliadas durante type checking (para evitar
--    loop infinito no type checker)
-- 2. Provas matematicas nao podem inspecionar definicoes de funcoes parciais,
--    tornando programas que as usam muito menos amenos a prova formal

-- Usar `partial` apenas quando necessario para lidar com dados infinitos.
