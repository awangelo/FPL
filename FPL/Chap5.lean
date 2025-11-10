-- 5. Functors, Applicative Functors, and Monads

-- Functor e Monad descrevem operacoes para tipos que ainda estao esperando um
-- argumento de tipo (Type → Type). Uma forma de entende-los eh que Functor
-- descreve containers nos quais os dados contidos podem ser transformados, e
-- Monad descreve uma codificacao de programas com efeitos colaterais. Mas esse
-- entendimento eh incompleto. Afinal, Option tem instancias tanto para
-- Functor quanto Monad, e simultaneamente representa um valor opcional e uma
-- computacao que pode falhar em retornar um valor.

-- Da perspectiva de estruturas de dados, Option eh como um tipo nullable. Da
-- perspectiva de estruturas de controle, Option representa uma computacao que
-- pode terminar cedo sem resultado. Tipicamente, programas que usam a instancia
-- Functor sao mais faceis de pensar como usando Option como estrutura de dados,
-- enquanto programas que usam a instancia Monad sao mais faceis de pensar como
-- usando Option para permitir falha antecipada. Aprender a usar ambas perspectivas
-- fluentemente eh importante para se tornar proficiente em programacao funcional.

-- Ha uma relacao mais profunda entre functors e monads. Todo monad eh um
-- functor. Alem disso, ha uma abstracao intermediaria adicional, chamada
-- applicative functors, que tem poder suficiente para escrever muitos programas
-- interessantes e ainda permite bibliotecas que nao podem usar a interface Monad.
-- A type class Applicative fornece as operacoes sobrecarregaveis de applicative
-- functors. Todo monad eh um applicative functor, e todo applicative functor
-- eh um functor, mas nao o contrario.


-- 5.1. Structures and Inheritance

-- Para entender as definicoes completas de Functor, Applicative e Monad, outro
-- recurso de Lean eh necessario: structure inheritance. Heranca de estruturas
-- permite que um tipo de estrutura forneca a interface de outro, junto com
-- campos adicionais. Isso pode ser util ao modelar conceitos que tem uma relacao
-- taxonomica clara. Por exemplo, um modelo de criaturas miticas. Algumas sao
-- grandes, outras pequenas:

structure MythicalCreature where
  large : Bool
deriving Repr

-- Por tras das cenas, definir a estrutura MythicalCreature cria um tipo indutivo
-- com um unico construtor chamado mk:

#check MythicalCreature.mk

-- Similarmente, uma funcao MythicalCreature.large eh criada que extrai o campo
-- do construtor:

#check MythicalCreature.large

-- Na maioria das historias antigas, cada monstro pode ser derrotado de alguma
-- forma. Uma descricao de um monstro deve incluir essa informacao, junto com se
-- eh grande:

structure Monster extends MythicalCreature where
  vulnerability : String
deriving Repr

-- O "extends MythicalCreature" no cabecalho declara que todo monstro tambem eh
-- mitico. Para definir um Monster, tanto os campos de MythicalCreature quanto
-- os de Monster devem ser fornecidos. Um troll eh um monstro grande vulneravel
-- a luz solar:

def troll : Monster where
  large := true
  vulnerability := "sunlight"

-- Por tras das cenas, heranca eh implementada usando composicao. O construtor
-- Monster.mk recebe um MythicalCreature como argumento:

#check Monster.mk

-- Alem de definir funcoes para extrair o valor de cada novo campo, uma funcao
-- Monster.toMythicalCreature eh definida com tipo Monster → MythicalCreature.
-- Pode ser usada para extrair a criatura subjacente.

-- Em Lean, porem, subir a hierarquia de heranca APAGA a informacao
-- subjacente. Para ver isso em acao, considere o resultado de avaliar
-- troll.toMythicalCreature:

#eval troll.toMythicalCreature

-- Apenas os campos de MythicalCreature permanecem.

-- Assim como a sintaxe where, notacao de chaves com nomes de campos tambem
-- funciona com heranca de estrutura:

def troll₂ : Monster := {large := true, vulnerability := "sunlight"}

-- No entanto, a notacao anonima de colchetes angulares que delega ao construtor
-- subjacente revela os detalhes internos:

def trollError : Monster := ⟨true, "sunlight"⟩

-- Um conjunto extra de colchetes angulares eh necessario, que invoca
-- MythicalCreature.mk em true:

def troll₃ : Monster := ⟨⟨true⟩, "sunlight"⟩

-- A dot notation eh capaz de levar heranca em conta. Entao o
-- MythicalCreature.large existente pode ser usado com um Monster, e o Lean
-- automaticamente insere a chamada a Monster.toMythicalCreature antes
-- da chamada a MythicalCreature.large. No entanto, isso so ocorre ao usar
-- dot notation, e aplicar a funcao de busca de campo usando sintaxe de
-- chamada de funcao normal resulta em erro de tipo:

#eval troll.large
#eval MythicalCreature.large troll

-- A dot notation tambem pode levar heranca em conta para funcoes definidas
-- pelo usuario. Uma criatura pequena eh uma que nao eh grande:

def MythicalCreature.small (c : MythicalCreature) : Bool := !c.large

-- Avaliar troll.small resulta em false, enquanto tentar avaliar
-- MythicalCreature.small troll resulta em erro de tipo similar ao anterior.

#eval troll.small
#eval MythicalCreature.small troll


-- 5.1.1. Multiple Inheritance

-- Um helper eh uma criatura mitica que pode fornecer assistencia quando recebe
-- o pagamento correto:

structure Helper extends MythicalCreature where
  assistance : String
  payment : String
deriving Repr

-- Por exemplo, um nisse eh um tipo de elfo pequeno conhecido por ajudar em
-- casa quando recebe mingau saboroso:

def nisse : Helper where
  large := false
  assistance := "household tasks"
  payment := "porridge"

-- Se domesticados, trolls fazem excelentes helpers. Sao fortes o suficiente
-- para arar um campo inteiro em uma unica noite, embora precisem de cabras
-- de brinquedo para mante-los satisfeitos. Um assistente monstruoso eh um
-- monstro que tambem eh um helper:

structure MonstrousAssistant extends Monster, Helper where
deriving Repr

-- Um valor deste tipo de estrutura deve preencher todos os campos de ambas
-- estruturas pai:

def domesticatedTroll : MonstrousAssistant where
  large := true
  assistance := "heavy labor"
  payment := "toy goats"
  vulnerability := "sunlight"

-- Ambos os tipos de estrutura pai estendem MythicalCreature (Monster e Helper).
-- Se heranca multipla fosse implementada ingenuamente, isso poderia levar a um
-- "problema do diamante", onde nao ficaria claro qual caminho para large deveria
-- ser tomado dado um MonstrousAssistant. Deveria pegar large do Monster ou do
-- Helper? Em Lean, a resposta eh que o primeiro caminho especificado para a
-- estrutura avo eh seguido, e os campos das estruturas pai adicionais sao
-- copiados em vez de ter a nova estrutura incluindo ambos os pais diretamente.

-- Isso pode ser visto examinando a assinatura do construtor para
-- MonstrousAssistant:

#check MonstrousAssistant.mk

-- Ele recebe um Monster como argumento, junto com os dois campos que Helper
-- introduz alem de MythicalCreature. Similarmente, enquanto
-- MonstrousAssistant.toMonster meramente extrai o Monster do construtor,
-- MonstrousAssistant.toHelper nao tem Helper para extrair. O comando #print
-- expoe sua implementacao:

#print MonstrousAssistant.toHelper

-- Esta funcao constroi um Helper a partir dos campos de MonstrousAssistant.
-- O atributo @[reducible] tem o mesmo efeito que escrever abbrev.


-- 5.1.1.1. Default Declarations

-- Default Declarations permitem calcular automaticamente os campos da estrutura
-- pai a partir dos campos da estrutura filha.

inductive Size where
  | small
  | medium
  | large
deriving BEq

structure SizedCreature extends MythicalCreature where
  size : Size                  -- campo novo
  large := size == Size.large  -- default declaration

-- As definicoes na estrutura filha sao usadas apenas quando nenhum valor
-- especifico para large eh fornecido, o que permite resultados sem sentido:

def nonsenseCreature : SizedCreature where
  large := false
  size := .large

-- Se a estrutura filha nao deve se desviar da estrutura pai, ha algumas opcoes:
-- 1. Documentar a relacao, como eh feito para BEq e Hashable
-- 2. Definir uma proposicao de que os campos estao relacionados apropriadamente,
--    e projetar a API para requerer evidencia de que a proposicao eh verdadeira
-- 3. Nao usar heranca

-- A segunda opcao poderia parecer assim:

abbrev SizesMatch (sc : SizedCreature) : Prop :=
  sc.large = (sc.size == Size.large)

-- Note que um unico sinal de igualdade eh usado para indicar a proposicao de
-- igualdade, enquanto um sinal duplo de igualdade eh usado para indicar uma
-- funcao que verifica igualdade e retorna um Bool. SizesMatch eh definido como
-- abbrev porque deve ser automaticamente desdobrado em provas, para que decide
-- possa ver a igualdade que deve ser provada.

-- Para uma criatura mitica de tamanho medio, os dois campos de tamanho em
-- correspondem um ao outro:

def huldre : SizedCreature where
  size := .medium

example : SizesMatch huldre := by
  decide

#eval huldre.large

-- Default declaration sempre sao aplicadas automaticamente na criacao, uma Porp
-- pode ser usada para verificar se ela faz sentido. Em uma API que usa esse
-- modelo seria responsabilidade do usuario provar a consistencia.


-- 5.1.1.2. Type Class Inheritance

-- Por tras das cenas, type classes sao structures. Definir uma nova type class
-- define uma nova estrutura, e definir uma instancia cria um valor desse tipo
-- de estrutura. Eles sao entao adicionados a tabelas internas no Lean que
-- permitem encontra-los sob demanda. Uma consequencia disso eh que type classes
-- podem herdar de outras type classes.

-- Por usar as mesmas features, heranca de type class suporta todos os recursos
-- da heranca de estrutura, incluindo heranca multipla, implementacoes padrao
-- dos metodos dos tipos pai, e colapso automatico de diamantes (escolhe o
-- primeiro lado). Isso eh util em muitas das mesmas situacoes em que heranca de
-- multiplas interfaces eh util em linguagens como Java, C# e Kotlin. Ao
-- projetar cuidadosamente hierarquias de heranca de type class, programadores
-- podem obter o melhor dos dois mundos: uma colecao refinada de abstracoes
-- implementaveis independentemente, e construcao automatica dessas abstracoes
-- especificas a partir de abstracoes maiores e mais gerais.


-- 5.2. Applicative Functors

-- Um applicative functor eh um functor que tem duas operacoes adicionais
-- disponiveis: pure e seq. pure eh o mesmo operador usado em Monad, porque
-- Monad na verdade herda de Applicative. seq eh muito parecido com map: permite
-- que uma funcao seja usada para transformar o conteudo de um datatype. No
-- entanto, com seq, a funcao em si esta contida no datatype:
-- `f (α → β) → (Unit → f α) → f β`. Ter a funcao sob o tipo f permite que a
-- instancia Applicative controle como a funcao eh aplicada, enquanto
-- Functor.map incondicionalmente aplica uma funcao. O segundo argumento tem um
-- tipo que comeca com `Unit →` para permitir que a definicao de seq
-- curto-circuite em casos onde a funcao nunca sera aplicada.

#check Applicative
#check (· <$> ·)
#check (· <*> ·)

-- O valor desse comportamento de curto-circuito pode ser visto na instancia de
-- Applicative Option:

instance : Applicative Option where
  pure x := .some x
  seq f x :=
    match f with
    | none => none
    | some g => g <$> x ()

-- Neste caso, se nao ha funcao para seq aplicar, entao nao ha necessidade de
-- computar seu argumento, entao x nunca eh chamado. A mesma consideracao informa
-- a instancia de Applicative para Except:

instance : Applicative (Except ε) where
  pure x := .ok x
  seq f x :=
    match f with
    | .error e => .error e
    | .ok g => g <$> x ()

-- Ate agora igual ao monad...

-- Este comportamento de curto-circuito depende apenas das estruturas Option ou
-- Except que envolvem a funcao, em vez da funcao em si.

-- Monads podem ser vistos como uma forma de capturar a nocao de executar
-- declaracoes sequencialmente em uma linguagem funcional pura. O resultado de
-- uma declaracao pode afetar quais declaracoes posteriores executam. Isso pode
-- ser visto no tipo de bind: `m α → (α → m β) → m β`. O valor resultante da
-- primeira declaracao eh uma entrada para uma funcao que computa a proxima
-- declaracao a executar. Usos sucessivos de bind sao como uma sequencia de
-- declaracoes em uma linguagem de programacao imperativa, e bind eh poderoso o
-- suficiente para implementar estruturas de controle como condicionais e loops.

-- Seguindo esta analogia, Applicative captura aplicacao de funcao em uma
-- linguagem que tem efeitos colaterais. Os argumentos para uma funcao em
-- linguagens como Kotlin ou C# sao avaliados da esquerda para a direita.
-- Efeitos colaterais realizados por argumentos anteriores ocorrem antes
-- daqueles realizados por argumentos posteriores. Uma funcao por si so nao eh
-- poderosa o suficiente para implementar operadores de curto-circuito
-- customizados que dependem do valor especifico de um argumento.

-- Tipicamente, seq nao eh invocado diretamente. Em vez disso, o operador <*> eh
-- usado. Este operador envolve seu segundo argumento em `fun () => ...`,
-- simplificando o local de chamada. Em outras palavras, `E1 <*> E2` eh
-- syntactic sugar para `Seq.seq E1 (fun () => E2)`.

-- Applicative provavelmente vai substituir os Monads que usavam `fun () =>...`
-- antes.

-- Currying permite seq ser usado com multiplos argumentos. Por exemplo:
-- `some Plus.plus` pode ter o tipo `Option (Nat → Nat → Nat)`. Fornecer um
-- argumento, `some Plus.plus <*> some 4`, resulta no tipo `Option (Nat → Nat)`.
-- Isso pode ser usado com seq novamente, entao
-- `some Plus.plus <*> some 4 <*> some 7` tem o tipo `Option Nat`.

-- Nem todo functor eh applicative. Pair eh parecido com o tipo Prod:

structure Pair (α β : Type) : Type where
  first : α
  second : β

-- Como Except, Pair tem tipo `Type → Type → Type`. Isso significa que `Pair α`
-- tem tipo `Type → Type`, e uma instancia Functor eh possivel:

instance : Functor (Pair α) where
  map f x := ⟨x.first, f x.second⟩

-- Esta instancia obedece o contrato Functor.

-- As duas propriedades a verificar sao que `id <$> Pair.mk x y = Pair.mk x y` e
-- que `f <$> g <$> Pair.mk x y = (f ∘ g) <$> Pair.mk x y`. A primeira
-- propriedade pode ser verificada apenas percorrendo a avaliacao do lado
-- esquerdo:
-- ⟹ id <$> Pair.mk x y
-- ⟹ Pair.mk x (id y)
-- ⟹ Pair.mk x y

-- A segunda pode ser verificada percorrendo ambos os lados, e notando que
-- produzem o mesmo resultado:
-- ⟹ f <$> g <$> Pair.mk x y
-- ⟹ f <$> Pair.mk x (g y)
-- ⟹ Pair.mk x (f (g y))
-- ⟹ (f ∘ g) <$> Pair.mk x y
-- ⟹ Pair.mk x ((f ∘ g) y)
-- ⟹ Pair.mk x (f (g y))

-- Tentar definir uma instancia Applicative, no entanto, nao funciona tao bem.
-- Requer uma definicao de pure:

#check pure

def Pair.pure (x : β) : Pair α β := _

-- Ja tem um valor do tipo β no escopo (x), e a mensagem de erro do
-- underscore sugere que o proximo passo eh usar o construtor Pair.mk:

def Pair.pure' (x : β) : Pair α β := Pair.mk _ x

-- Mas nao tem um `α` disponivel. Como pure precisaria funcionar para todos
-- os tipos possiveis α para definir uma instancia de `Applicative (Pair α)`,
-- isso eh impossivel. Afinal, um chamador poderia escolher α para ser Empty,
-- que nao tem valores.

-- Nem todo Functor pode ser um Aplicative.


-- 5.2.1. A Non-Monadic Applicative
