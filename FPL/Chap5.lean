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

-- Ao validar input de usuario em um formulario, eh considerado melhor fornecer
-- varios erros de uma vez. Isso permite que o usuario tenha uma visao geral do
-- que eh necessario.

-- Idealmente, validar input de usuario sera visivel no tipo da funcao que esta
-- fazendo a validacao (deve retornar um datatype especifico). Verificar que uma
-- caixa de texto contem um numero deve retornar um tipo numerico real, por
-- exemplo. Uma rotina de validacao poderia lancar uma exception quando o input
-- nao passa na validacao. Exceptions tem uma desvantagem importante: terminam
-- o programa no primeiro erro, tornando impossivel acumular uma lista de erros.

-- Por outro lado, o padrao comum de acumular uma lista de erros e entao falhar
-- quando nao esta vazia tambem eh problematico. Uma longa sequencia aninhada de
-- ifs que validam cada subsecao dos dados de input eh dificil de manter, e eh
-- facil perder mensagens de erro. Idealmente, validacao pode ser realizada
-- usando uma API que permite que um novo valor seja retornado e ainda
-- automaticamente rastreia e acumula mensagens de erro.

-- Um applicative functor chamado Validate fornece uma forma de implementar este
-- estilo de API. Como o monad Except, Validate permite que um novo valor seja
-- construido que caracteriza os dados validados com precisao. Diferente de
-- Except, permite que multiplos erros sejam acumulados, sem risco de esquecer
-- de verificar se a lista esta vazia.


-- 5.2.1.1. User Input

-- Como exemplo de input de usuario, considere a seguinte estrutura:

structure RawInput where
  name : String
  birthYear : String

-- A logica de negocio a ser implementada eh a seguinte:
-- 1. O nome nao pode estar vazio
-- 2. O ano de nascimento deve ser numerico e nao-negativo
-- 3. O ano de nascimento deve ser maior que 1900, e menor ou igual ao ano
--    em que o formulario eh validado

-- Representar essas condicoes como um datatype requer um novo recurso, chamado
-- subtypes. Com essa ferramenta em maos, um framework de validacao pode ser
-- escrito que usa um applicative functor para rastrear erros, e essas regras
-- podem ser implementadas no framework.


-- 5.2.1.2. Subtypes

-- Representar essas condicoes eh mais facil com um tipo adicional do Lean,
-- chamado Subtype:

#check Subtype
#check GetElem

-- Esta estrutura tem dois parametros de tipo: um parametro implicito que eh o
-- tipo de dados α, e um parametro explicito p que eh um predicado sobre α. Um
-- predicado eh uma declaracao logica com uma variavel que pode ser substituida
-- por um valor para produzir uma declaracao real, como o parametro para GetElem
-- que descreve o que significa para um indice estar in bounds para uma busca.
-- No caso de Subtype, o predicado recorta algum subconjunto dos valores de α
-- para os quais o predicado eh verdadeiro. Os dois campos da estrutura sao,
-- respectivamente, um valor de α e evidencia de que o valor satisfaz o
-- predicado p.

-- Representar numeros positivos como tipos indutivos eh claro e facil de
-- programar. No entanto, tem uma desvantagem chave. Embora Nat e Int tenham a
-- estrutura de tipos indutivos comuns da perspectiva de programas Lean, o
-- compilador os trata especialmente e usa bibliotecas de numeros de precisao
-- arbitraria rapidas para implementa-los. Isso nao eh o caso para tipos
-- adicionais definidos pelo usuario. No entanto, um subtype de Nat que o
-- restringe a numeros nao-zero permite que o novo tipo use a representacao
-- eficiente enquanto ainda descarta zero em tempo de compilacao:

def FastPos : Type := {x : Nat // x > 0}

-- O menor numero positivo rapido ainda eh um. Agora, em vez de ser um
-- construtor de um tipo indutivo, eh uma instancia de uma estrutura construida
-- com colchetes angulares. O primeiro argumento eh o Nat subjacente, e o
-- segundo argumento eh a evidencia de que esse Nat eh maior que zero:

def one : FastPos := ⟨1, by decide⟩

-- A proposicao `1 > 0` eh decidivel, entao a tatic decide produz a evidencia
-- necessaria. A instancia OfNat eh muito parecida com a de Pos, exceto que usa
-- uma prova tatica curta para fornecer evidencia de que `n + 1 > 0`:

instance : OfNat FastPos (n + 1) where
  ofNat := ⟨n + 1, by simp⟩

#check (1 : FastPos)

-- Aqui, simp eh necessario porque decide requer valores concretos, mas a
-- proposicao em questao eh `n + 1 > 0`.

-- Subtypes sao uma espada de dois gumes. Permitem representacao eficiente de
-- regras de validacao, mas transferem o fardo de manter essas regras para os
-- usuarios da biblioteca, que tem que provar que nao estao violando invariantes
-- importantes. Geralmente, eh uma boa ideia usa-los internamente a uma
-- biblioteca, fornecendo uma API para usuarios que automaticamente garante que
-- todos os invariantes sao satisfeitos, com quaisquer provas necessarias sendo
-- internas a biblioteca.

-- Verificar se um valor de tipo α esta no subtype `{x : α // p x}` geralmente
-- requer que a proposicao `p x` seja decidivel. A secao sobre classes de
-- igualdade e ordenacao descreve como proposicoes decidaveis podem ser usadas
-- com if. Quando if eh usado com uma proposicao decidivel, um nome pode ser
-- fornecido. No ramo then, o nome eh vinculado a evidencia de que a proposicao
-- eh verdadeira, e no ramo else, eh vinculado a evidencia de que a proposicao
-- eh falsa. Isso eh util ao verificar se um dado Nat eh positivo:

def Nat.asFastPos? (n : Nat) : Option FastPos :=
  if h : n > 0 then
    some ⟨n, h⟩
  else none

-- No ramo then, h eh a evidencia de que `n > 0`, e esta evidencia pode ser
-- usada como o segundo argumento para o construtor de Subtype.


-- 5.2.1.3. Validated Input

-- O input de usuario validado eh uma estrutura que expressa a logica de negocio
-- usando multiplas tecnicas:
-- 1. O tipo da estrutura em si codifica o ano em que foi verificado para
--    validade, entao `CheckedInput 2019` nao eh o mesmo tipo que
--    `CheckedInput 2020`
-- 2. O ano de nascimento eh representado como Nat em vez de String
-- 3. Subtypes sao usados para restringir os valores permitidos nos campos name
--    e birthYear

structure CheckedInput (thisYear : Nat) : Type where
  name : {n : String // n ≠ ""}
  birthYear : {y : Nat // y > 1900 ∧ y ≤ thisYear}

-- Um validador de input deve receber o ano atual e um RawInput como argumentos,
-- retornando ou um input verificado ou pelo menos uma falha de validacao. Isso
-- eh representado pelo tipo Validate:

structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α
instance : HAppend (NonEmptyList α) (NonEmptyList α) (NonEmptyList α) where
  hAppend xs ys := { head := xs.head, tail := xs.tail ++ ys.head :: ys.tail }

inductive Validate (ε α : Type) : Type where
  | ok : α → Validate ε α
  | errors : NonEmptyList ε → Validate ε α

-- Parece muito com Except. A unica diferenca eh que o construtor errors pode
-- conter mais de uma falha.

-- Validate eh um functor. Mapear uma funcao sobre ele transforma qualquer valor
-- bem-sucedido que possa estar presente, assim como na instancia Functor para
-- Except:

instance : Functor (Validate ε) where
  map f
   | .ok x => .ok (f x)
   | .errors errs => .errors errs

-- A instancia Applicative para Validate tem uma diferenca importante da
-- instancia para Except: enquanto a instancia para Except termina no primeiro
-- erro encontrado, a instancia para Validate tem cuidado em acumular todos os
-- erros tanto do ramo da funcao quanto do ramo do argumento:

instance : Applicative (Validate ε) where
  pure := .ok
  seq f x :=
    match f with
    | .ok g => g <$> (x ())
    | .errors errs =>
      match x () with
      -- Se ja ocorreram erros mas o atual eh correto, nao computa
      | .ok _ => .errors errs
      | .errors errs' => .errors (errs ++ errs')

-- Usar .errors junto com o construtor para NonEmptyList eh um pouco verboso.
-- Helpers como reportError tornam o codigo mais legivel. Nesta aplicacao,
-- relatorios de erro consistirao de nomes de campo emparelhados com mensagens:

def Field := String
def InputError := (Field × String)

def reportError (f : Field) (msg : String) : Validate InputError α :=
  .errors { head := (f, msg), tail := [] }

-- A instancia Applicative para Validate permite que os procedimentos de
-- verificacao para cada campo sejam escritos independentemente e entao
-- compostos. Verificar um nome consiste em garantir que uma string nao esta
-- vazia, entao retornar evidencia deste fato na forma de um Subtype. Isso usa
-- a versao de if que vincula evidencia:

def checkName (name : String) :
    Validate InputError {n : String // n ≠ ""} :=
  if h : name = "" then
    reportError "name" "Required"
  else pure ⟨name, h⟩

-- Certamente alguns erros de validacao tornam outras verificacoes impossiveis.
-- Por exemplo, nao faz sentido verificar se o campo ano de nascimento eh maior
-- que 1900 se um usuario confuso escreveu a palavra "syzygy" em vez de um
-- numero. Verificar o intervalo permitido do numero so eh significativo apos
-- garantir que o campo de fato contem um numero. Isso pode ser expresso usando
-- a funcao andThen:

def Validate.andThen (val : Validate ε α)
    (next : α → Validate ε β) : Validate ε β :=
  match val with
  | .errors errs => .errors errs
  | .ok x => next x
-- `next` eh tipo um predicado.

-- Embora a assinatura de tipo desta funcao a torne adequada para ser usada como
-- bind em uma instancia Monad, ha boas razoes para nao fazer isso. Elas sao
-- descritas na secao que descreve o contrato Applicative.

-- Para verificar que o ano de nascimento eh um numero, uma funcao embutida
-- chamada `String.toNat? : String → Option Nat` eh util. Eh mais amigavel ao
-- usuario eliminar espacos em branco iniciais e finais primeiro usando
-- String.trim:

def checkYearIsNat (year : String) : Validate InputError Nat :=
  match year.trim.toNat? with
  | none => reportError "birth year" "Must be digits"
  | some n => pure n

-- Para verificar que o ano fornecido esta no intervalo esperado, usos aninhados
-- da forma de if que fornece evidencia estao em ordem:

def checkBirthYear (thisYear year : Nat) :
    Validate InputError {y : Nat // y > 1900 ∧ y ≤ thisYear} :=
  if h : year > 1900 then
    if h' : year ≤ thisYear then
      -- usa as todas hipoteses disponiveis em lemmas marcados com `[simp]`
      pure ⟨year, by simp [*]⟩  -- [h, h']
    else reportError "birth year" s!"Must be no later than {thisYear}"
  else reportError "birth year" "Must be after 1900"

-- Finalmente, esses tres componentes podem ser combinados usando <*>:

def checkInput (year : Nat) (input : RawInput) :
    Validate InputError (CheckedInput year) :=
  pure CheckedInput.mk <*>
    checkName input.name <*>
    (checkYearIsNat input.birthYear).andThen fun birthYearAsNat =>
      checkBirthYear year birthYearAsNat
    -- Essa parte poderia ser encadeada com `>>=`, mas provavelmente vai ser
    -- dito depois porque nao usar.

-- Testar checkInput mostra que pode de fato retornar multiplos feedbacks:
--
instance [Repr α] : Repr (NonEmptyList α) where
  reprPrec nel _ :=
    s!"NonEmptyList.mk {repr nel.head} {repr nel.tail}"
instance : Repr (CheckedInput year) where
  reprPrec ci _ :=
    s!"CheckedInput.mk (name := {repr ci.name.val}) (birthYear := {repr ci.birthYear.val})"
--

#eval checkInput 2023 {name := "David", birthYear := "1984"}

#eval checkInput 2023 {name := "", birthYear := "2045"}

#eval checkInput 2023 {name := "David", birthYear := "syzygy"}

-- Validacao de formulario com checkInput ilustra uma vantagem chave de
-- Applicative sobre Monad. Como >>= fornece poder suficiente para modificar o
-- resto da execucao do programa baseado no valor do primeiro passo, deve
-- receber um valor do primeiro passo para passar adiante. Se nenhum valor eh
-- recebido (por exemplo, porque um erro ocorreu), entao >>= nao pode executar
-- o resto do programa. Validate demonstra por que pode ser util executar o
-- resto do programa de qualquer forma: em casos onde os dados anteriores nao
-- sao necessarios, executar o resto do programa pode produzir informacoes uteis
-- (neste caso, mais erros de validacao). O <*> de Applicative pode executar
-- ambos seus argumentos antes de recombinar os resultados. Similarmente, >>=
-- forca execucao sequencial. Cada passo deve completar antes que o proximo
-- possa executar. Isso eh geralmente util, mas torna impossivel ter execucao
-- paralela de threads diferentes que naturalmente emerge das dependencias de
-- dados reais do programa. Uma abstracao mais poderosa como Monad aumenta a
-- flexibilidade disponivel para o consumidor da API, mas diminui a
-- flexibilidade disponivel para o implementador da API.

-- Monad eh mais poderoso que Applicative. Decide com base nos valores
-- anteriores. Nao pode ser paralelizado por causa disso.

-- Applicative nao eh forcado a rodar sequencialmente. Eh mais flexivel por nao
-- forcar continuidade.


-- 5.3. The Applicative Contract

-- Assim como Functor, Monad, e tipos que implementam BEq e Hashable, Applicative
-- tem um conjunto de regras que todas as instancias devem aderir.

-- Ha quatro regras que um applicative functor deve seguir:

-- 1. Deve respeitar identidade, entao `pure id <*> v = v`

-- 2. Deve respeitar composicao de funcao, entao
--    `pure (· ∘ ·) <*> u <*> v <*> w = u <*> (v <*> w)`

-- 3. Sequenciar operacoes puras deve ser uma no-op, entao
--    `pure f <*> pure x = pure (f x)`

-- 4. A ordenacao de operacoes puras nao importa, entao
--    `u <*> pure x = pure (fun f => f x) <*> u`

-- Para verificar essas regras para a instancia Applicative Option, comece
-- expandindo pure em some.

-- A primeira regra declara que `some id <*> v = v`. A definicao de seq para
-- Option declara que isso eh o mesmo que `id <$> v = v`, que eh uma das regras
-- Functor que ja foram verificadas.

-- A segunda regra declara que
-- `some (· ∘ ·) <*> u <*> v <*> w = u <*> (v <*> w)`. Se qualquer um de u, v,
-- ou w eh none, entao ambos os lados sao none, entao a propriedade eh valida.
-- Assumindo que u eh `some f`, que v eh `some g`, e que w eh `some x`, entao
-- isso eh equivalente a dizer que
-- `some (· ∘ ·) <*> some f <*> some g <*> some x = some f <*> (some g <*> some x)`.
-- Avaliar os dois lados produz o mesmo resultado:
-- ⟹ some (· ∘ ·) <*> some f <*> some g <*> some x
-- ⟹ some (f ∘ ·) <*> some g <*> some x
-- ⟹ some (f ∘ g) <*> some x
-- ⟹ some ((f ∘ g) x)
-- ⟹ some (f (g x))
-- ⟹ some f <*> (some g <*> some x)
-- ⟹ some f <*> (some (g x))
-- ⟹ some (f (g x))

-- A terceira regra segue diretamente da definicao de seq:
-- ⟹ some f <*> some x
-- ⟹ f <$> some x
-- ⟹ some (f x)

-- No quarto caso, assuma que u eh `some f`, porque se eh none, ambos os lados
-- da equacao sao none. `some f <*> some x` avalia diretamente para
-- `some (f x)`, assim como `some (fun g => g x) <*> some f`.


-- 5.3.1. All Applicatives are Functors

-- Os dois operadores para Applicative sao suficientes para definir map:

def mapFromApplicative [Applicative f] (g : α → β) (x : f α) : f β :=
  pure g <*> x

-- Isso so pode ser usado para implementar Functor se o contrato para Applicative
-- garante o contrato para Functor. A primeira regra de Functor eh que
-- `id <$> x = x`, que segue diretamente da primeira regra para Applicative. A
-- segunda regra de Functor eh que `map (f ∘ g) x = map f (map g x)`.
-- Desdobrando a definicao de map aqui resulta em
-- `pure (f ∘ g) <*> x = pure f <*> (pure g <*> x)`. Usando a regra de que
-- sequenciar operacoes puras eh uma no-op, o lado esquerdo pode ser reescrito
-- para `pure (· ∘ ·) <*> pure f <*> pure g <*> x`. Esta eh uma instancia da
-- regra que declara que applicative functors respeitam composicao de funcao.

-- Isso justifica uma definicao de Applicative que estende Functor, com uma
-- definicao padrao de map dada em termos de pure e seq:

class Applicative' (f : Type → Type) extends Functor f where
  pure : α → f α
  seq : f (α → β) → (Unit → f α) → f β
  map g x := seq (pure g) (fun () => x)

-- Todo Applicative eh um Functor, e a implementacao padrao de map pode ser
-- derivada de pure e seq.


-- 5.3.2. All Monads are Applicative Functors

-- Uma instancia de Monad ja requer uma implementacao de pure. Junto com bind,
-- isso eh suficiente para definir seq:

def seqFromMonad [Monad m] (f : m (α → β)) (x : Unit → m α) : m β := do
  let g ← f
  let y ← x ()
  pure (g y)

-- Mais uma vez, verificar que o contrato Monad implica o contrato Applicative
-- permitira que isso seja usado como uma definicao padrao para seq se Monad
-- estende Applicative.

-- O resto desta secao consiste em um argumento de que esta implementacao de seq
-- baseada em bind de fato satisfaz o contrato Applicative. Uma das coisas
-- bonitas sobre programacao funcional eh que este tipo de argumento pode ser
-- trabalhado em um pedaco de papel com um lapis, usando os tipos de regras de
-- avaliacao da secao inicial sobre avaliar expressoes. Pensar sobre os
-- significados das operacoes ao ler esses argumentos pode as vezes ajudar com
-- o entendimento.

-- Substituir do-notation com usos explicitos de >>= torna mais facil aplicar
-- as regras Monad:

def seqFromMonad' [Monad m] (f : m (α → β)) (x : Unit → m α) : m β :=
  f >>= fun g =>
  x () >>= fun y =>
  pure (g y)

-- Para verificar que esta definicao respeita identidade, verifique que
-- `seq (pure id) (fun () => v) = v`. O lado esquerdo eh equivalente a
-- `pure id >>= fun g => (fun () => v) () >>= fun y => pure (g y)`. A funcao
-- unit no meio pode ser eliminada imediatamente, produzindo
-- `pure id >>= fun g => v >>= fun y => pure (g y)`. Usando o fato de que pure
-- eh uma identidade esquerda de >>=, isso eh o mesmo que
-- `v >>= fun y => pure (id y)`, que eh `v >>= fun y => pure y`. Como
-- `fun x => f x` eh o mesmo que `f`, isso eh o mesmo que `v >>= pure`, e o
-- fato de que pure eh uma identidade direita de >>= pode ser usado para obter
-- `v`.

-- Este tipo de raciocinio informal pode ser tornado mais facil de ler com um
-- pouco de reformatacao. No seguinte grafico, leia "EXPR1 ={ RAZAO }= EXPR2"
-- como "EXPR1 eh o mesmo que EXPR2 porque RAZAO":

-- ⟹ pure id >>= fun g => v >>= fun y => pure (g y)
-- ={ pure eh identidade esquerda de >>= }=
-- ⟹ v >>= fun y => pure (id y)
-- ={ Reduz a chamada a id }=
-- ⟹ v >>= fun y => pure y
-- ={ fun x => f x eh o mesmo que f }=
-- ⟹ v >>= pure
-- ={ pure eh identidade direita de >>= }=
-- ⟹ v

-- ...

-- Isso justifica uma definicao de Monad que estende Applicative, com uma
-- definicao padrao de seq:

class Monad' (m : Type → Type) extends Applicative m where
  bind : m α → (α → m β) → m β
  seq f x :=
    bind f fun g =>
    bind (x ()) fun y =>
    pure (g y)

-- A propria definicao padrao de map de Applicative significa que toda instancia
-- Monad automaticamente gera instancias Applicative e Functor tambem.

-- Resumo da hierarquia:
-- Functor ⊂ Applicative ⊂ Monad
--
-- * Todo Monad eh um Applicative (seq pode ser derivado de bind e pure)
-- * Todo Applicative eh um Functor (map pode ser derivado de pure e seq)


-- 5.3.3. Additional Stipulations

-- Alem de aderir aos contratos individuais associados a cada type class,
-- implementacoes combinadas de Functor, Applicative e Monad devem funcionar
-- de forma equivalente a essas implementacoes padrao. Em outras palavras, um
-- tipo que fornece instancias tanto de Applicative quanto de Monad nao deve
-- ter uma implementacao de seq que funciona diferentemente da versao que a
-- instancia Monad gera como implementacao padrao. Isso eh importante porque
-- funcoes polimorficas podem ser refatoradas para substituir um uso de >>= com
-- um uso equivalente de <*>, ou um uso de <*> com um uso equivalente de >>=.
-- Esta refatoracao nao deve mudar o significado de programas que usam este
-- codigo.

-- Esta regra explica por que Validate.andThen nao deve ser usado para
-- implementar bind em uma instancia Monad. Por si so, obedece o contrato monad.
-- No entanto, quando eh usado para implementar seq, o comportamento nao eh
-- equivalente ao seq em si. Para ver onde diferem, considere o exemplo de duas
-- computacoes, ambas retornando erros. Comece com um exemplo de um caso onde
-- dois erros deveriam ser retornados, um da validacao de uma funcao (que
-- poderia muito bem ter resultado de um argumento anterior para a funcao), e
-- um da validacao de um argumento:

def notFun : Validate String (Nat → String) :=
  .errors { head := "First error", tail := [] }

def notArg : Validate String Nat :=
  .errors { head := "Second error", tail := [] }

-- Combina-los com a versao de <*> da instancia Applicative de Validate resulta
-- em ambos os erros sendo reportados ao usuario:

#eval notFun <*> notArg
-- ⟹
-- match notFun with
-- | .ok g => g <$> notArg
-- | .errors errs =>
--   match notArg with
--   | .ok _ => .errors errs
--   | .errors errs' => .errors (errs ++ errs')
-- ⟹
-- match notArg with
-- | .ok _ => .errors { head := "First error", tail := [] }
-- | .errors errs' => .errors ({ head := "First error", tail := [] } ++ errs')
-- ⟹
-- .errors ({ head := "First error", tail := [] } ++
--          { head := "Second error", tail := []})
-- ⟹
-- .errors { head := "First error", tail := ["Second error"] }

-- Usando a versao de seq que foi implementada com >>=, aqui reescrita para
-- andThen, resulta em apenas o primeiro erro estar disponivel:

#eval Seq.seq notFun (fun () => notArg)
-- ⟹
-- notFun.andThen fun g =>
-- notArg.andThen fun y =>
-- pure (g y)
-- ⟹
-- match notFun with
-- | .errors errs => .errors errs
-- | .ok val =>
--   (fun g =>
--     notArg.andThen fun y =>
--     pure (g y)) val
-- ⟹
-- .errors { head := "First error", tail := [] }

-- * Usando `<*>` diretamente: acumula todos os erros.
-- * Usando `>>=` (que seria andThen): para no primeiro erro.

-- Isso violaria a regra de que "refatorar entre <*> e >>= nao deve mudar o
-- comportamento". Por isso Validate NAO deve ter uma instancia Monad - eh
-- apenas um Applicative.

-- * Validate eh um Applicative que acumula erros
-- * Validate.andThen parece bind, mas nao acumula erros
-- * Se fizessemos Validate um Monad com andThen como bind, violariamos a
--   consistencia entre Applicative e Monad.


-- 5.4. Alternatives


-- 5.4.1. Recovery from Failure
