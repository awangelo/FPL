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
