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
