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

Avalia imediatamente (lean NAO eh lazy) a expressao `IO.println "Hello!"`, gerando uma acao IO
  que eh atribuida ao nome `englishGreeting`.
Avalia a expressao `IO.println "Bonjour!"`,  gerando uma acao IO que tem seu
  side effect executado imediatamente.
Avalia a expressao `englishGreeting`, e executa seu side effect.
-/


-- 2.3. Starting a Project
