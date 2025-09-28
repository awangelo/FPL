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
