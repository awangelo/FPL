-- Interlude: Propositions, Proofs, and Indexing


-- Lean pode usar square brackets para indexar arrays e lista:

def woodlandCritters : List String :=
  ["hedgehog", "deer", "snail"]

#eval woodlandCritters[1]

-- Tentar extrair um quarto elemento resulta em um compile-time error,
-- em vez de um run-time error.

#eval woodlandCritters[3]

-- O erro diz que o Lean tentou provar matematicamente que
-- `3 < woodlandCritters.length`, o que significaria que a operacao eh segura,
-- mas nao conseguiu. Entender como isso funciona requer o entendimento de
-- `propositions`, `proofs` e `tactics`.


-- Propositions and Proofs

-- Uma proposition eh um statement que pode ser verdadeiro ou falso, todos os
-- seguintes sao proposicoes:

/-
 - 1 + 1 = 2
 - Addition is commutative.
 - There are infinitely many prime numbers.
 - 1 + 1 = 15
 - Paris is the capital of France.
 - Buenos Aires is the capital of South Korea.
 - All birds can fly.
-/

-- Por outro lado, coisas que nao fazem sentido nao sao proposicoes:

/-
 - 1 + green = ice cream
 - All capital cities are prime numbers.
 - At least one gorg is a fleep.
-/

-- Existem duas variedades de proposicoes: as que sao puramente matematicas,
-- baseadas apenas em nossas definicoes de conceitos, e aquelas que sao
-- fatos sobre o mundo. Theorem provers como o Lean se preocupam com a
-- primeira categoria e nao tem nada a dizer sobre as capacidades de voo
-- de pinguins ou o status legal de cidades.

-- Uma prova eh um argumento convincente de que uma proposicao eh verdadeira.
-- Para proposicoes matematicas, esses argumentos fazem uso das definicoes
-- dos conceitos envolvidos, bem como das regras de argumentacao logica.
-- A maioria das provas eh escrita para pessoas entenderem e deixa de fora
-- muitos detalhes tediosos.

-- Theorem provers como o Lean sao projetados para permitir que matematicos
-- escrevam provas omitindo muitos detalhes, e eh responsabilidade do software
-- preencher os passos explicitos ausentes. Esses passos podem ser verificados
-- mecanicamente, o que diminui a probabilidade de negligencias ou erros.

-- No Lean, o tipo de um programa descreve as maneiras pelas quais ele pode
-- ser usado. Por exemplo, um programa do tipo `Nat → List String` eh uma
-- funcao que recebe um argumento Nat e produz uma lista de strings.

-- No Lean, proposicoes sao tipos (`Prop`). Elas especificam o que conta como
-- evidencia de que a declaracao eh verdadeira. A proposicao eh provada
-- fornecendo essa evidencia, que eh verificada pelo Lean. Se a proposicao
-- eh falsa, sera impossivel construir essa evidencia.

-- Exemplo: a proposicao `1 + 1 = 2` pode ser escrita diretamente no Lean.
-- A evidencia para essa proposicao eh o construtor `rfl` (reflexivity).
-- Em matematica, uma relacao eh reflexiva se todo elemento se relaciona
-- consigo mesmo - requisito basico para ter uma nocao sensata de igualdade.

def onePlusOneIsTwo : 1 + 1 = 2 := rfl

-- Como `1 + 1` computa para `2`, eles sao realmente a mesma coisa.
-- Por outro lado, `rfl` nao prova a proposicao falsa `1 + 1 = 15`:

def onePlusOneIsFifteen : 1 + 1 = 15 := rfl

-- O erro indica que `rfl` pode provar que duas expressoes sao iguais quando
-- ambos os lados da igualdade ja sao o mesmo numero. Como `1 + 1` avalia
-- diretamente para `2`, eles sao considerados iguais.

-- Assim como `Type` descreve tipos como Nat, String, List, `Prop` descreve
-- proposicoes. Quando uma proposicao foi provada, eh chamada de teorema.
-- Por convencao, teoremas usam a palavra-chave `theorem` em vez de `def`:

def OnePlusOneIsTwo : Prop := 1 + 1 = 2
theorem onePlusOneIsTwo_theorem : OnePlusOneIsTwo := rfl

-- Isso ajuda leitores a ver quais declaracoes sao provas matematicas vs
-- definicoes. Com provas, importa QUE existe evidencia, nao QUAL evidencia.
-- Com definicoes, importa muito qual valor particular eh selecionado.


-- Tactics

-- Provas sao geralmente escritas usando `tactics` ao inves de evidencia direta.
-- Tactics sao pequenos programas que constroem evidencia para uma proposicao.
-- Esses programas executam em um "proof state" que rastreia a declaracao
-- a ser provada (goal) junto com as suposicoes disponiveis para prova-la.
-- Executar uma tactic em um goal resulta em um novo proof state que contem
-- novos goals. A prova esta completa quando todos os goals foram provados.

-- Para escrever uma prova com tactics, comece a definicao com `by`.
-- Escrever `by` coloca o Lean em "tactic mode" ate o fim do proximo bloco
-- indentado. No tactic mode, Lean fornece feedback continuo sobre o proof
-- state atual.

theorem onePlusOneIsTwo_tactic : 1 + 1 = 2 := by
  decide

-- A tactic `decide` invoca um "decision procedure", que eh um programa
-- que pode verificar se uma declaracao eh verdadeira ou falsa, retornando
-- uma prova adequada em qualquer caso. Eh usado principalmente ao trabalhar
-- com valores concretos como 1 e 2.

-- A outra tactic importante eh `simp` (simplify), que eh a base das provas
-- Lean. Ela reescreve o goal para a forma mais simples possivel. Em muitos
-- casos, essa reescrita simplifica tanto a declaracao que pode ser provada
-- automaticamente. Por tras das cenas, uma prova formal detalhada eh
-- construida, mas usar `simp` esconde essa complexidade.

-- Vantagens das tactics:
-- 1. Muitas provas sao complicadas e tediosas quando escritas em detalhes
--    minimos, e tactics podem automatizar essas partes desinteressantes.
-- 2. Provas escritas com tactics sao mais faceis de manter ao longo do tempo,
--    porque automacao flexivel pode contornar pequenas mudancas em definicoes.
-- 3. Como uma unica tactic pode provar muitos teoremas diferentes, o Lean
--    pode usar tactics por tras das cenas para liberar usuarios de escrever
--    provas manualmente.

-- Por exemplo, acesso a array requer prova de que o indice esta dentro
-- dos limites, e uma tactic pode tipicamente construir essa prova sem
-- o usuario precisar se preocupar com isso.

-- Por tras das cenas, notacao de indexacao usa uma tactic para provar
-- que a operacao de lookup do usuario eh segura. Essa tactic considera
-- muitos fatos sobre aritmetica, combinando-os com fatos conhecidos
-- localmente para tentar provar que o indice esta dentro dos limites.


-- Connectives

-- Os blocos de construcao basicos da logica, como "and", "or", "true",
-- "false" e "not", sao chamados conectivos logicos. Cada conectivo define
-- o que conta como evidencia de sua verdade.

-- A maioria desses conectivos sao definidos como datatypes e tem construtores.
-- Se A e B sao proposicoes, entao `A ∧ B` eh uma proposicao.
-- Evidencia para `A ∧ B` consiste do construtor And.intro, que tem tipo
-- A → B → A ∧ B.

theorem addAndAppend : 1 + 1 = 2 ∧ "Str".append "ing" = "String" := by
  decide

-- "A or B" (A ∨ B) tem dois construtores, porque uma prova de "A or B"
-- requer apenas que uma das duas proposicoes subjacentes seja verdadeira:

#check Or

-- Implicacao (se A entao B) eh representada usando funcoes. Uma funcao que
-- transforma evidencia para A em evidencia para B eh ela mesma evidencia
-- de que A implica B. Isso eh diferente da descricao usual de implicacao
-- (A → B como ¬A ∨ B), mas as duas formulacoes sao equivalentes.

-- Proposicoes sao tipos, evidencias sao valores desses tipos.

-- Como evidencia para "and" eh um construtor, pode ser usada com pattern matching:
theorem andImpliesOr : A ∧ B → A ∨ B :=
  fun andEvidence =>  -- Extrai a evidencia de `A ∧ B` (para usar o a como nova evidencia para o `or`)
    match andEvidence with
    | And.intro a b => Or.inl a

-- ┌─────────────┬──────────────┬─────────────────────────────────────────────────┐
-- │ Conectivo   │ Sintaxe Lean │ Evidencia                                       │
-- ├─────────────┼──────────────┼─────────────────────────────────────────────────┤
-- │ True        │ True         │ True.intro : True                               │
-- │ False       │ False        │ Sem evidência                                   │
-- │ A and B     │ A ∧ B        │ And.intro : A → B → A ∧ B                       │
-- │ A or B      │ A ∨ B        │ Or.inl : A → A ∨ B ou Or.inr : B → A ∨ B        │
-- │ A implies B │ A → B        │ Funcao que transforma evidencia de A em B       │
-- │ not A       │ ¬A           │ Funcao que transformaria evidencia de A em False│
-- └─────────────┴──────────────┴─────────────────────────────────────────────────┘

-- A tactic `decide` pode provar teoremas que usam esses conectivos:
theorem onePlusOneOrLessThan : 1 + 1 = 2 ∨ 3 < 5 := by decide
theorem notTwoEqualFive : ¬(1 + 1 = 5) := by decide
theorem trueIsTrue : True := by decide
theorem falseImpliesTrue : False → True := by decide


-- Evidence as Arguments

-- Em alguns casos, indexar com seguranca em uma lista requer que a lista
-- tenha algum tamanho minimo, mas a lista em si eh uma variavel em vez de
-- um valor concreto. Para que essa busca seja segura, deve haver alguma
-- evidencia de que a lista eh longa o suficiente.

-- Uma das maneiras mais faceis de tornar a indexacao segura eh fazer com que
-- a funcao que realiza uma busca em uma estrutura de dados receba a evidencia
-- necessaria de seguranca como argumento.

-- Por exemplo, esta funcao que retorna a terceira entrada de uma lista
-- nao eh geralmente segura porque listas podem conter zero, uma ou duas entradas:

def third₁ (xs : List α) : α := xs[2]

-- Porem, a obrigacao de mostrar que a lista tem pelo menos tres entradas
-- pode ser imposta ao chamador adicionando um argumento que consiste em
-- evidencia de que a operacao de indexacao eh segura:

def third₂ (xs : List α) (ok : xs.length > 2) : α := xs[2]

-- Neste exemplo, `xs.length > 2` NAO eh um programa que verifica se xs
-- tem mais de 2 entradas. Eh uma PROPOSICAO que pode ser verdadeira ou falsa,
-- e o argumento `ok` deve ser EVIDENCIA de que ela eh verdadeira.

-- Quando a funcao eh chamada em uma lista concreta, seu tamanho eh conhecido.
-- Nesses casos, `by decide` pode construir a evidencia automaticamente:

#eval third₂ woodlandCritters (by decide)


-- Indexing Without Evidence

-- Outras formas de usar indexacao segura:
-- `xs[i]!`  → Verifica em runtime, produz 'Panic' se indice invalido
-- `xs[i]?`  → Retorna Option, Some(valor) ou None
-- `xs[i]'h` → Usa prova `h` de que o indice eh valido


-- Messages You May Meet

-- Alem de provar que uma declaracao eh verdadeira, a tactic `decide` tambem
-- pode provar que eh falsa.

#eval third ["rabbit"] (by decide)

-- As tactics `simp` e `decide` NAO fazem unfold automatico de definicoes com `def`.
-- Tentar provar OnePlusOneIsTwo usando simp falha:

theorem onePlusOneIsStillTwo : OnePlusOneIsTwo := by simp
-- Usar `decide` tambem falha:
theorem onePlusOneIsStillTwo₂ : OnePlusOneIsTwo := by decide

-- Definir OnePlusOneIsTwo com `abbrev` corrige o problema marcando a definicao
-- para unfold automatico.

-- Funcoes polimorficas que usam indexacao insegura podem produzir:
def unsafeThird (xs : List α) : α := xs[2]!

#check Inhabited

-- Isso eh devido a uma restricao tecnica para manter o Lean utilizavel tanto
-- como logica para provar teoremas quanto como linguagem de programacao.
-- Apenas programas cujos tipos contem pelo menos um valor podem crashar.
--   O problema nao seria o tipo que crasha existir, e sim a possibilidade de
--   pode crashar ser usado como evidencia (o que o tornaria falso). Isso poderia
--   ser usado para provar qualquer coisa.
-- Suponha que o Lean permitisse isto (que ele NAO permite):
-- def fakeProof : 1 + 1 = 5 := panic! "mentira"
--                 ^^^^^^^^^^
--              proposicao falsa sendo "provada" com crash
-- Isso porque uma proposicao no Lean eh um tipo que classifica evidencia
-- de sua verdade. Proposicoes falsas nao tem tal evidencia. Se um programa
-- com tipo vazio pudesse crashar, esse programa poderia ser usado como
-- "falsa evidencia" para uma proposicao falsa.

-- Internamente, Lean contem uma tabela de tipos que tem pelo menos um valor.
-- O erro diz que um tipo arbitrario α nao esta necessariamente nessa tabela.


-- Exercises

-- Prove the following theorems using rfl: 2 + 3 = 5, 15 - 8 = 7,
-- "Hello, ".append "world" = "Hello, world". What happens if rfl is used to
-- prove 5 < 18? Why?

theorem twoPlusThreeEqFive : 2 + 3 = 5 := by rfl

theorem scnd : 15 - 8 = 7 := by rfl

def helloAndWorld := "Hello, ".append "world" = "Hello, world"
theorem helloWorld : helloAndWorld := by rfl

theorem test₁ : 5 < 18 := by rfl
-- Nao eh possivel pois nao tem `=`.

-- Prove the following theorems using by decide: 2 + 3 = 5, 15 - 8 = 7,
-- "Hello, ".append "world" = "Hello, world", 5 < 18.

theorem twoPlusThreeEqFive₂ : 2 + 3 = 5 := by decide

theorem scnd₂ : 15 - 8 = 7 := by decide

theorem helloWorld₂ : "Hello, ".append "world" = "Hello, world" := by decide

theorem lt : 5 < 18 := by decide

-- Write a function that looks up the fifth entry in a list. Pass the evidence
-- that this lookup is safe as an argument to the function.

def get5elem (xs : List α) (ok : 4 < xs.length) :=
  xs[4]
